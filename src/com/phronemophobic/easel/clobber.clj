(ns com.phronemophobic.easel.clobber
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.easel.evaluation :as evaluation]
   [com.phronemophobic.easel.inspector :as inspector]
   [com.phronemophobic.easel.theme :as theme]
   [com.phronemophobic.easel.keymap :as keymap]
   [com.phronemophobic.easel.ui :as eui]
   [com.phronemophobic.clobber.util :as clobber-util]
   [membrane.basic-components :as basic]
   [membrane.skia.paragraph :as para]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [clojure.string :as str]
   [com.phronemophobic.easel.splitpane :as splitpane]
   [com.phronemophobic.viscous :as viscous]
   [com.phronemophobic.clobber.modes.clojure.ui :as cui]
   [com.phronemophobic.clobber.editor :as clobber-editor]  
   [com.phronemophobic.easel.file-watcher :as watcher]
   [clojure.zip :as z]
   [com.phronemophobic.clobber.modes.clojure :as clojure-mode]
   [com.phronemophobic.clobber.modes.text :as text-mode]
   [com.phronemophobic.clobber.util.ui.key-binding :as key-binding]
   [com.phronemophobic.clobber.util.ui :as util.ui]))

(defn evaluation-source [editor scope]
  (let [tree (:tree editor)
        rope (:rope editor)
        node (when tree
               (case scope
                 :last (clobber-util/previous-named-child-for-byte tree (-> editor :cursor :byte))
                 :top (let [cursor (org.treesitter.TSTreeCursor. (.getRootNode ^org.treesitter.TSTree tree))]
                        (when-not (= -1 (.gotoFirstChildForByte cursor (-> editor :cursor :byte)))
                          (.currentNode cursor)))
                 :buffer nil))]
    (cond
      (= :buffer scope) {:source (str rope) :line 1 :result-line 0}
      node {:source (clobber-util/node->str rope node)
            :line (inc (-> node .getStartPoint .getRow))
            :result-line (-> node .getEndPoint .getRow)})))

(defn evaluate-editor! [dispatch! {:keys [editor $editor]} scope]
  (future
    (when-let [{:keys [source line result-line]} (evaluation-source editor scope)]
      (let [{:keys [value exception]}
            (evaluation/evaluate source {:ns (:eval-ns editor)
                                         :file (some-> (:file editor) .getCanonicalPath)
                                         :line line})]
        (if exception
          (dispatch! ::cui/temp-status {:$editor $editor :msg (str "Exception: " (.getMessage ^Throwable exception))})
          (do
            (dispatch! :update $editor update :line-val
                       (fn [values]
                         {(:rope editor) (assoc (get values (:rope editor)) result-line (viscous/wrap value))}))
            (dispatch! ::cui/temp-status
                       {:$editor $editor
                        :msg (if (= scope :buffer)
                               "buffer loaded."
                               (inspector/preview value))})))))))

(defeffect ::eval-last [m] (evaluate-editor! dispatch! m :last))
(defeffect ::eval-top [m] (evaluate-editor! dispatch! m :top))
(defeffect ::eval-buffer [m] (evaluate-editor! dispatch! m :buffer))

(defui buffer-selector [{:keys [base-style
                                buffer-select-state
                                focused?
                                width]
                         :as this}]
  (let [
        base-style (or base-style 
                       #:text-style
                       {:font-families ["Menlo"]
                        :font-size 12
                        :height 1.2
                        :height-override true})

        applets (:applets buffer-select-state)
        offset (get buffer-select-state :offset 0)
        search-str (get buffer-select-state :search-str "")
        search-str-lower (str/lower-case search-str)

        matching-applets
        (into 
         []
         (comp (filter (fn [applet]
                         (str/includes? (str/lower-case (or (:label applet) ""))
                                        search-str-lower)))
               (drop offset))
         applets)

        ps (into 
            ["Applet: "
             search-str
             " | "]
            (comp (map-indexed 
                   (fn [i applet]
                     (let [style (if (zero? i)
                                   (assoc base-style 
                                          :text-style/font-style
                                          {:font-style/weight :bold})
                                   base-style)]
                       {:style style
                        :text (:label applet)})))
                  (interpose " | "))
            matching-applets)

        p (para/paragraph
           ps
           width
           {:paragraph-style/text-style base-style})
        
        p (if focused?
            (ui/on
             :key-event
             (fn [key scancode action mods]
               (when (#{:press :repeat} action)
                 (let [ctrl? (not (zero? (bit-and ui/CONTROL-MASK mods)))]
                   (cond
                     (and ctrl?
                          (= (char key) \G))
                     [[:set $buffer-select-state nil]]

                     
                     (and ctrl?
                          (= (char key) \S))
                     [[:update $offset 
                       (fn [offset]
                         (if (<= (count matching-applets) 1)
                           0
                           (inc offset)))]]))))
             
             :key-press
             (fn [s]
               (cond
                 
                 (= s :enter)
                 (when-let [applet (first matching-applets)]
                   (let [
                       applet-id (:id applet)
                       focus (:focus context)]
                   [[:com.phronemophobic.easel/set-pane-applet-id {:applet-id applet-id}]
                    [:set $buffer-select-state nil]
                    [:set $focus applet-id]]))
                 
                 (= s :backspace)
                 [[:update $search-str
                   (fn [s]
                     (subs s 0 (max 0 (- (count s) 1))))]
                  [:set $offset 0]]
                 
                 (string? s)
                 [[:update $search-str str s]
                  [:set $offset 0]]))
             p)
            ;; else
            p)]
    p))

(declare clobber-applet)

(defn editor-palette [context]
  (merge {[0 0 0] (theme/token context :color/text)
          [0.98 0.98 0.98] (theme/token context :color/surface)
          [0.85 0.85 0.85] (theme/token context :color/header)
          [1.0 0.8431372549019608 0.5294117647058824 0.7] (theme/token context :color/selection)
          [1 0 1 0.2] (theme/token context :color/selection)
          [0.3725490196078431 0.8431372549019608 0.8431372549019608] (theme/token context :color/selection)}
         (into {} (for [[capture token] {"number" :color/code-string "string" :color/code-string
                                         "constant.builtin" :color/code-definition "constant" :color/code-keyword
                                         "comment" :color/code-comment "defn" :color/code-definition}]
                    [(get clojure-mode/text-colors capture) (theme/token context token)]))))

(defui editor-key-bindings [{:keys [editor body]}]
  (ui/on
   ::key-binding/miss
   (fn [{:keys [key-binding]}]
     (when (= 1 (count key-binding))
       (let [{:keys [key meta? ctrl? super?]} (first key-binding)]
         (when (and key (not (or meta? ctrl? super? (keyword? key))))
           [[::cui/update-editor {:op #(text-mode/editor-self-insert-command % (str key)) :$editor $editor}]]))))
   ::key-binding/press
   (fn [{:keys [intent]}]
     (if (keyword? intent)
       [[intent {:editor editor :$editor $editor}]]
       [[::cui/update-editor {:op intent :editor editor :$editor $editor}]]))
   (key-binding/wrap-key-tree
    {:body body :$body nil
     :key-tree (key-binding/key-bindings->key-tree (:key-bindings editor))})))

(defui code-editor [{:keys [editor focused?]}]
  (let [body (cui/editor-view {:editor editor :focused? focused?})
        body (cond
               (not focused?) body
               (::util.ui/file-picker-state editor)
               (ui/vertical-layout body (util.ui/file-picker {:editor editor :update-editor-intent ::cui/update-editor}))
               (::text-mode/search editor)
               (ui/vertical-layout body (util.ui/search-bar {:editor editor :update-editor-intent ::cui/update-editor}))
               :else (editor-key-bindings {:editor editor :body body :$body nil}))
        body (if focused?
               (->> body
                    (ui/on-clipboard-copy (fn [] [[::cui/editor-copy {:editor editor :$editor $editor}]]))
                    (ui/on-clipboard-paste (fn [s] [[::cui/editor-paste {:editor editor :$editor $editor :s s}]])))
               body)
        row-height (* (get-in editor [:base-style :text-style/height])
                      (get-in editor [:base-style :text-style/font-size]))
        start-line (get-in editor [:viewport :start-line] 0)
        lines (take (get-in editor [:viewport :num-lines] 1)
                    (drop start-line (str/split (str (:rope editor)) #"\n" -1)))
        gutter (apply ui/vertical-layout
                      (map-indexed (fn [index _]
                                     (ui/fixed-bounds [40 row-height]
                                       (eui/text {:text (str (+ start-line index 1)) :role :code
                                                   :color :color/muted :size 11}))) lines))
        initial-value (when (= (:rope editor) (:easel/initial-rope editor)) (:easel/initial-value editor))
        value (or (some->> (get (:line-val editor) (:rope editor)) (sort-by key) last val) initial-value)
        preview (when value
                  (ui/on :mouse-down
                    (fn [_] [[:com.phronemophobic.easel/add-applet {:make-applet #(inspector/inspector-applet % @value)}]])
                    (ui/vertical-layout
                     (eui/text {:text (if (sequential? @value)
                                        (let [n (bounded-count 25 @value)] (str "▾ " n (when (= n 25) "+") " values")) "⇒ value")
                                 :role :code :size 12 :color :color/secondary})
                     (ui/spacer 0 8)
                     (eui/text {:text (if (sequential? @value)
                                        (str (str/join "\n" (map (fn [v]
                                                                    (if (and (vector? v) (= 2 (count v)) (every? number? v))
                                                                      (format "[%.1f %.1f]" (double (first v)) (double (second v)))
                                                                      (inspector/preview v))) (take 5 @value))) "\n…")
                                        (inspector/preview @value))
                                 :role :code :size 12 :width (max 1 (- (:width editor) 72))}))))]
    (ui/wrap-on :mouse-down
      (fn [handler pos] (cons [::cui/request-focus] (handler pos)))
      (ui/fixed-bounds [(:width editor) (:height editor)]
        [(eui/surface context (:width editor) (:height editor))
         (ui/translate 16 18 gutter)
         (ui/translate 52 18 (eui/adapt-colors body (editor-palette context)))
         (when preview
           (ui/translate 52 (+ 34 (* row-height (count lines))) preview))]))))

(defui clobber-ui* [{:keys [this]}]
  (let [focus (:focus context)
        state (:state this)
        editor (:editor state)
        focused? (= (:id this) focus)]
    (when editor
      (let [buffer-select-state (::buffer-select-state this)
            body (ui/on
                  ::show-select-buffer
                  (fn [m]
                    [[::show-select-buffer (assoc this
                                                  :$focus $focus)]])
                  ::hide-pane
                  (fn [m]
                    [[::focus-next {:this this
                                    :$focus $focus}]
                     [:com.phronemophobic.easel/hide-pane {}]])
                  ::delete-pane
                  (fn [m]
                    [[::focus-next {:this this
                                    :$focus $focus}]
                     [:com.phronemophobic.easel/delete-pane {}]])
                  ::close-other-panes
                  (fn [m]
                    [[:com.phronemophobic.easel/close-other-panes {}]])
                  ::split-pane
                  (fn [{:keys [editor]}]
                    (let [forked-editor (-> editor
                                            (dissoc ::cui/auto-reload-unwatch)
                                            (update :tree
                                                    (fn [^org.treesitter.TSTree tree]
                                                      (when tree
                                                        (.copy tree)))))]
                      [[:com.phronemophobic.easel/add-applet
                        {:make-applet
                         #(clobber-applet % {:editor forked-editor
                                             :label (str (:label editor) "*")
                                             :ui (:ui this)})}]]))
                  ::focus-next
                  (fn [m]
                    [[::focus-next {:this this
                                    :$focus $focus}]])
                  ::cui/request-focus
                  (fn []
                    [[:set $focus (:id this)]])
                  (let [ui (if (identical? (:ui this) #'cui/code-editor) #'code-editor (:ui this))
                        extra (:extra state)]
                    (ui {:editor editor
                         :$editor $editor
                         :focused? focused?
                         :extra extra
                         :$extra $extra :context context :$context $context})))
            body (if buffer-select-state
                   (ui/on
                    :mouse-down
                    (fn [_]
                      [[:set $focus (:id this)]])
                    (ui/vertical-layout
                     (ui/no-events body)
                     (let [e (:extra state)]
                       (buffer-selector {:buffer-select-state buffer-select-state
                                         :extra (get e ::buffer-selector)
                                         :focused? focused?}))))
                   body)]
        body))))

(defn clobber-ui [this $context context]
  (clobber-ui* {:this this
                :$this [(:$ref this)]
                :context context
                :$context $context}))


(defn resize-editor [editor [width height]]
  (-> editor
      (assoc :base-style (eui/text-style nil {:role :code}))
      (cui/editor-set-height (max 0 (- height 36)))
      (assoc :width width :height height)
      text-mode/editor-update-viewport))

(defn apply-keymap [editor descriptor]
  (let [base (or (:easel/base-key-bindings editor) (:key-bindings editor))]
    (assoc editor :easel/base-key-bindings base
                  :key-bindings (keymap/editor-bindings base descriptor (:easel/clojure? editor)))))

(defn load-editor [dispatch! $ref editor-info size]
  (let [
        {:keys [editor ui]} (if (:editor editor-info)
                              editor-info
                              (clobber-editor/guess-mode editor-info))

        editor (if-let [line (:line editor-info)]
                 (if (> line 1)
                   (text-mode/editor-goto-line editor (dec line))
                   (assoc editor :cursor {:byte 0 :char 0 :point 0 :row 0 :column-byte 0}))
                 editor)

        editor (cond-> editor
                 (and (:ns editor-info) (identical? ui #'cui/code-editor))
                 (assoc :eval-ns (the-ns (:ns editor-info))))

        editor (cond-> editor
                 (:initial-value editor-info)
                 (assoc :easel/initial-value (:initial-value editor-info) :easel/initial-rope (:rope editor)))
        editor (-> editor (assoc :label (:label editor-info) :easel/clojure? (identical? ui #'cui/code-editor))
                   (apply-keymap (keymap/current (dispatch! :get :easel))))

        $editor [$ref '(keypath :state) '(keypath :editor)]]
    (dispatch!
     :update
     $ref
     (fn [applet]
       (-> applet
           (assoc :ui ui)
           (assoc-in [:state :editor] (resize-editor editor (or (:size applet) size)))
           (assoc-in [:state :$editor] $editor))))
    (when-let [applet (dispatch! :get $ref)]
      (when-let [file (:file editor)]
        (let [active? (:active? applet)
              stopped? (atom false)
              watcher (watcher/watch
                       (fn [{:keys [type]}]
                         (when (and @active? (= type :modify))
                           (#'cui/file-changed dispatch! $editor)))
                       (.getCanonicalPath ^java.io.File file))
              unwatch #(when (compare-and-set! stopped? false true) (watcher/stop watcher))]
          (when-let [previous (first (swap-vals! (:unwatch* applet) (constantly unwatch)))]
            (previous))
          (dispatch! :update $editor assoc ::cui/auto-reload-unwatch unwatch)
          (when-not @active? (unwatch)))))))

(defrecord ClobberApplet [dispatch! editor-info]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [active? (atom true)]
      (assoc this
             :extra {}
             :tap-vals []
             :$ref $ref
             :size size
             :active? active?
             :unwatch* (atom nil)
             ::model/queue
             [(fn []
                (when @active?
                  (load-editor dispatch! $ref editor-info size)
                  (dispatch! :repaint!)))])))
  (-stop [this]
    (when-let [active? (:active? this)] (reset! active? false))
    (when-let [unwatch* (:unwatch* this)]
      (when-let [unwatch (first (swap-vals! unwatch* (constantly nil)))]
        (unwatch)))
    nil)
  model/IUI
  (-ui [this $context context]
    (clobber-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (cond-> (assoc this :size size)
      (get-in this [:state :editor]) (update-in [:state :editor] resize-editor size))))

(defn ^:private truncate-string-end [s n]
  (if (> (count s) n)
    (subs s 0 n)
    s))

(defn ^:private truncate-string-begin [s n]
  (if (> (count s) n)
    (subs s (- (count s) n))
    s))



(defn clobber-applet [handler {:keys [file ns string label] :as m}]
  (let [m (cond-> m (some? string) (assoc :source string))
        name (cond
               label label

               file
               (-> (.getCanonicalPath file)
                   (truncate-string-begin 16))
               
               ns
               (-> (str ns)
                   (truncate-string-begin 16))
               
               :else "Clobber")
        m (assoc m :label name)]
    (-> (->ClobberApplet handler m)
      (assoc :label (str name)))))


(defeffect ::show-select-buffer [{:keys [$ref $focus id]}]
  (let [applets (dispatch! :com.phronemophobic.easel/get-applets)
        clobber-applets (into []
                              (keep (fn [[id applet]]
                                      (when (instance? ClobberApplet applet)
                                        applet)))
                              applets)]
    (dispatch! :update $ref assoc
               ::buffer-select-state {:applets clobber-applets})
    (dispatch! :set $focus id)))


(defn ^:private zfind
  "Finds first loc that matches pred. Returns nil if no match found."
  [loc pred]
  (loop [loc loc]
    (if (z/end? loc)
      nil
      (if (pred (z/node loc))
        loc
        (recur (z/next loc))))))

(defeffect ::focus-next [{:keys [this $focus]}]
  (let [applets (dispatch! :com.phronemophobic.easel/get-applets)
        root-pane (dispatch! :com.phronemophobic.easel/get-root-pane)
        applet-id (:id this)
        ids (into []
                  (comp (map :applet-id)
                        (filter #(instance? ClobberApplet (get applets %))))
                  (splitpane/flatten-pane root-pane))
        current (.indexOf ^java.util.List ids applet-id)]
    (when (seq ids)
      (dispatch! :set $focus (nth ids (mod (inc current) (count ids)))))))

