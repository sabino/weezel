(ns com.phronemophobic.easel.diff
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.easel.process :as process]
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
                   (let [patch (file-diff a b)]
                     {:patch patch :lines (patch-lines patch) :offset 0 :file b}))))

(def page-size 200)

(defui diff-view [{:keys [state size]}]
  (let [{:keys [patch file compare? before after error busy?]} state
        lines (or (:lines state) (patch-lines (or patch "")))
        offset (min (:offset state 0) (* page-size (quot (max 0 (dec (count lines))) page-size)))
        footer (ui/vertical-layout
                (eui/label (str " " (if (seq lines) (inc offset) 0) "–" (min (count lines) (+ offset page-size))
                               " of " (count lines) " lines "))
                (ui/horizontal-layout
                 (applet/button {:text "Previous" :size :small :disabled? (zero? offset)
                              :on-click #(vector [:set [$state '(keypath :offset)] (max 0 (- offset page-size))]
                                                 [:set [$extra '(keypath ::body)] {}])})
                 (applet/button {:text "Next" :size :small :disabled? (>= (+ offset page-size) (count lines))
                              :on-click #(vector [:set [$state '(keypath :offset)] (+ offset page-size)]
                                                 [:set [$extra '(keypath ::body)] {}])})))
        header (ui/vertical-layout
     (when compare?
       (ui/vertical-layout
        (eui/field {:text (or before "") :$text [$state '(keypath :before)] :size :small :placeholder "Before file"})
        (eui/field {:text (or after "") :$text [$state '(keypath :after)] :size :small :placeholder "After file"})
        (applet/button {:text "Compare files" :size :small
                     :disabled? (or busy? (str/blank? before) (str/blank? after))
                     :on-click #(vector [::compare-files $state before after])})))
     (when error (eui/label error))
     (when busy? (eui/label "Comparing…")))]
    (ui/vertical-layout
     header
     (basic/scrollview
      {:scroll-bounds [(max 0 (first size)) (max 0 (- (second size) (ui/height header) (ui/height footer)))]
       :extra (get extra ::body) :$extra [$extra '(keypath ::body)]
       :$body nil
       :body
       (if (seq patch)
         (apply ui/vertical-layout
                (for [{:keys [text kind old-line new-line]} (take page-size (drop offset lines))]
                  (ui/on-click
                   (fn []
                     (when (and file new-line (.isFile (io/file file)))
                       [[:com.phronemophobic.easel/add-applet
                         {:make-applet (fn [handler]
                                         ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
                                          handler {:file (io/file file) :line new-line}))}]]))
                   (eui/label (format "%5s %5s %s" (or old-line "") (or new-line "") text)
                              {:role :code :size 12 :color (case kind :add :color/live :remove :color/error
                                                                     :hunk :color/accent :color/text)}))))
         (eui/label "No differences"))})
     footer)))

(defn diff-applet [_handler info]
  (applet/tool-applet "Diff" #'diff-view
                      (assoc info :lines (or (:lines info) (patch-lines (or (:patch info) ""))))))
