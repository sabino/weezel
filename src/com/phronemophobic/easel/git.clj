(ns com.phronemophobic.easel.git
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.diff :as diff]
            [com.phronemophobic.easel.process :as process]
            [com.phronemophobic.membrandt :as ant]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(defn command [repository & args]
  (into ["git" "--no-pager" "--literal-pathspecs" "-C" (str repository)] args))

(defn git [repository & args]
  (process/checked (apply command repository args)))

(defn repository-root [directory]
  (str/replace (git directory "rev-parse" "--show-toplevel") #"\r?\n$" ""))

(defn parse-status [output]
  (loop [records (seq (str/split output #"\u0000")) files []]
    (if-let [record (first records)]
      (if (< (count record) 3)
        (recur (next records) files)
        (let [index (subs record 0 1)
              worktree (subs record 1 2)
              renamed? (or (#{"R" "C"} index) (#{"R" "C"} worktree))]
          (recur (if renamed? (nnext records) (next records))
                 (conj files (cond-> {:index index :worktree worktree :path (subs record 3)}
                               renamed? (assoc :original-path (second records)))))))
      files)))

(defn status [repository]
  (parse-status (git repository "status" "--porcelain=v1" "-z" "--untracked-files=all")))

(defn has-head? [repository]
  (zero? (:exit (process/run (command repository "rev-parse" "--verify" "HEAD")))))

(defn history [repository]
  (if (has-head? repository)
    (->> (git repository "log" "-50" "--format=%h%x00%s%x00%an%x00%aI")
         str/split-lines
         (mapv #(zipmap [:hash :subject :author :time] (str/split % #"\u0000"))))
    []))

(defn refresh [directory]
  (let [repository (repository-root directory)]
    {:repository repository
     :branch (let [branch (process/run (command repository "symbolic-ref" "--quiet" "--short" "HEAD"))]
               (if (zero? (:exit branch))
                 (str/trim (:out branch))
                 (str "detached at " (str/trim (git repository "rev-parse" "--short" "HEAD")))))
     :files (status repository)
     :history (history repository)}))

(defn paths [entry]
  (cond-> [(:path entry)] (:original-path entry) (conj (:original-path entry))))

(defn stage! [repository entry]
  (apply git repository "add" "--" (paths entry)))

(defn unstage! [repository entry]
  (if (has-head? repository)
    (apply git repository "restore" "--staged" "--" (paths entry))
    (apply git repository "rm" "--cached" "--ignore-unmatch" "--" (paths entry))))

(defn commit! [repository message]
  (when (str/blank? message)
    (throw (ex-info "Enter a commit message" {})))
  (git repository "commit" "-m" message))

(defn file-patch [repository entry staged?]
  (if (and (= "?" (:index entry)) (not staged?))
    (diff/added-file-diff (io/file repository (:path entry)))
    (apply git repository "diff" "--no-ext-diff" "--no-textconv" "--no-color"
           (concat (when staged? ["--cached"]) ["--"] (paths entry)))))

(defeffect ::refresh [$state repository]
  (applet/task! dispatch! $state #(refresh repository)))

(defeffect ::change [$state repository action argument]
  (applet/task! dispatch! $state
                #(do
                   (case action
                     :stage (stage! repository argument)
                     :unstage (unstage! repository argument)
                     :commit (commit! repository argument))
                   (cond-> (refresh repository) (= action :commit) (assoc :message "")))))

(defeffect ::diff [$state repository entry staged?]
  (applet/task! dispatch! $state
    (fn []
      (let [patch (file-patch repository entry staged?)]
        (dispatch! :com.phronemophobic.easel/add-applet
                   {:make-applet #(diff/diff-applet % {:patch patch
                                                      :file (io/file repository (:path entry))})})
        {}))))

(defeffect ::show-commit [$state repository revision]
  (applet/task! dispatch! $state
                #(let [patch (git repository "show" "--format=fuller" "--no-ext-diff"
                                  "--no-textconv" "--no-color" revision "--")]
                   (dispatch! :com.phronemophobic.easel/add-applet
                              {:make-applet (fn [handler] (diff/diff-applet handler {:patch patch}))})
                   {})))

(defui git-view [{:keys [state size]}]
  (let [{:keys [repository files branch history busy? error]} state
        message (:message state "")
        directory (:directory state "")]
    (ui/vertical-layout
     (ui/horizontal-layout
      (ant/text-input {:text directory :$text [$state '(keypath :directory)] :size :small
                       :placeholder "Repository directory"})
      (ant/button {:text "Refresh" :size :small :disabled? busy?
                   :on-click #(vector [::refresh $state directory])}))
     (ui/label (str repository " | Branch: " branch (when busy? " | Working…")))
     (when error (ui/label error))
     (ui/horizontal-layout
      (ant/text-input {:text message :$text [$state '(keypath :message)] :size :small
                       :placeholder "Commit message"})
      (ant/button {:text "Commit staged" :size :small
                   :disabled? (or busy? (str/blank? message)
                                   (not-any? #(not (#{" " "?" "!"} (:index %))) files))
                   :on-click #(vector [::change $state repository :commit message])}))
     (basic/scrollview
      {:scroll-bounds [(max 0 (first size)) (max 0 (- (second size) 105))]
       :$body nil
       :body
       (apply ui/vertical-layout
              (concat
               (for [entry files]
                 (ui/vertical-layout
                  (ui/label (str (:index entry) (:worktree entry) " " (:path entry)))
                  (ui/horizontal-layout
                   (ant/button {:text "Stage" :size :small :disabled? busy?
                                :on-click #(vector [::change $state repository :stage entry])})
                   (ant/button {:text "Unstage" :size :small :disabled? busy?
                                :on-click #(vector [::change $state repository :unstage entry])})
                   (ant/button {:text "Working diff" :size :small
                                :on-click #(vector [::diff $state repository entry false])})
                   (ant/button {:text "Staged diff" :size :small
                                :on-click #(vector [::diff $state repository entry true])}))))
               [(ui/label "Recent commits")]
               (for [commit history]
                 (ui/on-click #(vector [::show-commit $state repository (:hash commit)])
                              (ui/label (str (:hash commit) " " (:subject commit) " | " (:author commit)))))))}))))

(defn git-applet
  ([handler] (git-applet handler (System/getProperty "user.dir")))
  ([handler directory]
   (applet/tool-applet "Git" #'git-view {:directory (str directory) :files [] :history []}
                       (fn [$state]
                         (applet/task! handler $state #(refresh directory))
                         nil))))
