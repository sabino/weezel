(ns com.phronemophobic.easel.git
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.easel.diff :as diff]
            [com.phronemophobic.easel.process :as process]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(defn command [repository & args]
  (into ["git" "--no-pager" "--literal-pathspecs" "-C" (str repository)] args))

(defn git [repository & args]
  (process/checked (apply command repository args) {:env {"GIT_TERMINAL_PROMPT" "0"}}))

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
  (let [{:keys [exit err]} (process/run (command repository "rev-parse" "--verify" "--quiet" "HEAD"))]
    (case exit
      0 true
      1 false
      (throw (ex-info err {:exit exit})))))

(defn history [repository]
  (if (has-head? repository)
    (->> (git repository "log" "-50" "--format=%h%x00%s%x00%an%x00%aI")
         str/split-lines
         (mapv #(zipmap [:hash :subject :author :time] (str/split % #"\u0000"))))
    []))

(defn refresh [directory]
  (let [repository (repository-root directory)]
    {:repository repository :loaded-directory (str directory) :offset 0
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
  (dispatch! :update $state dissoc :repository :loaded-directory :files :history :branch)
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
                #(hash-map :patch (file-patch repository entry staged?)
                           :file (when-not staged? (io/file repository (:path entry))))
                (fn [info]
                  (dispatch! :com.phronemophobic.easel/add-applet
                             {:make-applet #(diff/diff-applet % info)}))))

(defeffect ::show-commit [$state repository revision]
  (applet/task! dispatch! $state
                #(hash-map :patch (git repository "show" "--format=fuller" "--no-ext-diff"
                                        "--no-textconv" "--no-color" revision "--"))
                (fn [info]
                  (dispatch! :com.phronemophobic.easel/add-applet
                             {:make-applet #(diff/diff-applet % info)}))))

(def page-size 100)

(defui git-view [{:keys [state size]}]
  (let [{:keys [repository files branch history busy? error]} state
        message (:message state "")
        directory (:directory state "")
        stale? (not= directory (:loaded-directory state))
        disabled? (or busy? stale? (nil? repository))
        offset (min (:offset state 0) (* page-size (quot (max 0 (dec (count files))) page-size)))
        actions-layout (if (< (first size) 200) ui/vertical-layout ui/horizontal-layout)
        header
        (ui/vertical-layout
         (eui/field {:text directory :$text [$state '(keypath :directory)] :size :small
                          :disabled? busy? :placeholder "Repository directory"})
         (applet/button {:text "Refresh" :size :small :disabled? busy?
                      :on-click #(vector [::refresh $state directory])})
         (eui/label (str "Branch: " branch (when busy? " | Working…")))
         (when (and stale? (not busy?)) (eui/label "Refresh to load this directory"))
         (when error (eui/label error))
         (eui/field {:text message :$text [$state '(keypath :message)] :size :small
                          :disabled? busy? :placeholder "Commit message"})
         (applet/button {:text "Commit staged" :size :small
                      :disabled? (or disabled? (str/blank? message)
                                      (not-any? #(not (#{" " "?" "!"} (:index %))) files))
                      :on-click #(vector [::change $state repository :commit message])})
         (when (> (count files) page-size)
           (ui/vertical-layout
            (eui/label (str (inc offset) "–" (min (count files) (+ offset page-size)) " of " (count files) " files"))
            (ui/horizontal-layout
             (applet/button {:text "Previous" :size :small :disabled? (zero? offset)
                          :on-click #(vector [:set [$state '(keypath :offset)] (max 0 (- offset page-size))]
                                             [:set [$extra '(keypath ::body)] {}])})
             (applet/button {:text "Next" :size :small :disabled? (>= (+ offset page-size) (count files))
                          :on-click #(vector [:set [$state '(keypath :offset)] (+ offset page-size)]
                                             [:set [$extra '(keypath ::body)] {}])})))))]
    (ui/vertical-layout
     header
     (basic/scrollview
      {:scroll-bounds [(max 0 (first size)) (max 0 (- (second size) (ui/height header)))]
       :extra (get extra ::body) :$extra [$extra '(keypath ::body)]
       :$body nil
       :body
       (apply ui/vertical-layout
              (concat
               (for [entry (take page-size (drop offset files))]
                 (ui/vertical-layout
                  (eui/label (str (:index entry) (:worktree entry) " " (:path entry)))
                  (actions-layout
                   (applet/button {:text "Stage" :size :small :disabled? (or disabled? (= " " (:worktree entry)))
                                :on-click #(vector [::change $state repository :stage entry])})
                   (applet/button {:text "Unstage" :size :small :disabled? (or disabled? (#{" " "?" "!"} (:index entry)))
                                :on-click #(vector [::change $state repository :unstage entry])}))
                  (actions-layout
                   (applet/button {:text "Working diff" :size :small
                                :disabled? (or disabled? (= " " (:worktree entry)))
                                :on-click #(vector [::diff $state repository entry false])})
                   (applet/button {:text "Staged diff" :size :small
                                :disabled? (or disabled? (#{" " "?" "!"} (:index entry)))
                                :on-click #(vector [::diff $state repository entry true])}))))
               [(eui/label "Recent commits")]
               (for [commit history]
                 (ui/on-click #(when-not disabled? [[::show-commit $state repository (:hash commit)]])
                              (eui/label (str (:hash commit) " " (:subject commit) " | " (:author commit)))))))}))))

(defn git-applet
  ([handler] (git-applet handler (System/getProperty "user.dir")))
  ([handler directory]
   (applet/tool-applet "Git" #'git-view {:directory (str directory) :files [] :history []}
                       (fn [$state]
                         (applet/task! handler $state #(refresh directory))
                         nil))))
