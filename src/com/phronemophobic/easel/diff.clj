(ns com.phronemophobic.easel.diff
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.process :as process]
            [com.phronemophobic.membrandt :as ant]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn file-diff [before after]
  (let [{:keys [exit out err]}
        (process/run ["git" "--no-pager" "diff" "--no-index" "--no-ext-diff"
                      "--no-textconv" "--no-color" "--" (str before) (str after)])]
    (when-not (#{0 1} exit)
      (throw (ex-info err {:exit exit})))
    out))

(defn text-diff [before after]
  (let [dir (.toFile (Files/createTempDirectory "easel-diff-" (make-array FileAttribute 0)))
        a (io/file dir "before")
        b (io/file dir "after")]
    (try
      (spit a before :encoding "UTF-8")
      (spit b after :encoding "UTF-8")
      (-> (file-diff a b)
          (str/replace (str dir "/") ""))
      (finally
        (.delete a)
        (.delete b)
        (.delete dir)))))

(defn added-file-diff [file]
  (let [empty-file (.toFile (Files/createTempFile "easel-empty-" ".tmp" (make-array FileAttribute 0)))]
    (try
      (file-diff empty-file file)
      (finally (.delete empty-file)))))

(defn patch-lines [patch]
  (loop [lines (str/split-lines patch) old nil new nil result []]
    (if-let [line (first lines)]
      (if-let [[_ a b] (re-find #"^@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@" line)]
        (recur (next lines) (Long/parseLong a) (Long/parseLong b)
               (conj result {:text line :kind :hunk}))
        (let [kind (cond
                     (str/starts-with? line "diff --git ") :file
                     (and new (str/starts-with? line "+")) :add
                     (and old (str/starts-with? line "-")) :remove
                     (and new (str/starts-with? line " ")) :context
                     :else :header)]
          (recur (next lines)
                 (cond (= :file kind) nil (#{:remove :context} kind) (inc old) :else old)
                 (cond (= :file kind) nil (#{:add :context} kind) (inc new) :else new)
                 (conj result {:text line :kind kind
                               :old-line (when (#{:remove :context} kind) old)
                               :new-line (when (#{:add :context} kind) new)}))))
      result)))

(defeffect ::compare-files [$state before after]
  (applet/task! dispatch! $state
                #(let [a (.getCanonicalFile (io/file before))
                       b (.getCanonicalFile (io/file after))]
                   {:patch (file-diff a b) :file b})))

(defui diff-view [{:keys [state size]}]
  (let [{:keys [patch file compare? before after error busy?]} state]
    (ui/vertical-layout
     (when compare?
       (ui/vertical-layout
        (ant/text-input {:text (or before "") :$text [$state '(keypath :before)] :size :small :placeholder "Before file"})
        (ant/text-input {:text (or after "") :$text [$state '(keypath :after)] :size :small :placeholder "After file"})
        (ant/button {:text "Compare files" :size :small :disabled? busy?
                     :on-click #(vector [::compare-files $state before after])})))
     (when error (ui/label error))
     (when busy? (ui/label "Comparing…"))
     (basic/scrollview
      {:scroll-bounds [(max 0 (first size)) (max 0 (- (second size) (if compare? 115 15)))]
       :$body nil
       :body
       (if (seq patch)
         (apply ui/vertical-layout
                (for [{:keys [text kind old-line new-line]} (patch-lines patch)]
                  (ui/on-click
                   (fn []
                     (when (and file new-line (.isFile (io/file file)))
                       [[:com.phronemophobic.easel/add-applet
                         {:make-applet (fn [handler]
                                         ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
                                          handler {:file (io/file file) :line new-line}))}]]))
                   (ui/with-color (case kind
                                    :add [0.1 0.5 0.15]
                                    :remove [0.7 0.15 0.15]
                                    :hunk [0.2 0.3 0.7]
                                    [0.15 0.15 0.15])
                     (ui/label (format "%5s %5s %s" (or old-line "") (or new-line "") text)
                               (ui/font "Menlo" 11))))))
         (ui/label "No differences"))}))))

(defn diff-applet [_handler info]
  (applet/tool-applet "Diff" #'diff-view info))
