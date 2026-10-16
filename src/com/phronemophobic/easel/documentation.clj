(ns com.phronemophobic.easel.documentation
  "Documentation as a view of Clojure metadata, examples, and live applet values."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.evaluation :as evaluation]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.guides :as guides]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.viscous :as viscous]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(def page-size 40)

(defn var-symbol [v]
  (when (var? v)
    (let [{:keys [ns name]} (meta v)]
      (symbol (str (ns-name ns)) (str name)))))

(defn resolve-var
  "Resolve a qualified symbol in loaded namespaces, without loading or evaluating."
  [sym]
  (when (and (symbol? sym) (namespace sym))
    (when-let [ns (find-ns (symbol (namespace sym)))]
      (get (ns-interns ns) (symbol (name sym))))))

(defn describe-var [v]
  (let [m (meta v)
        value (when (bound? v) @v)
        protocol? (and (map? value) (:on-interface value) (:sigs value))]
    {:symbol (var-symbol v)
     :kind (cond protocol? :protocol
                 (:protocol m) :protocol-method
                 (:macro m) :macro
                 (:arglists m) :function
                 :else :var)
     :meta m
     :doc (:doc m)
     :arglists (:arglists m)
     :methods (when protocol?
                (mapv #(symbol (str (ns-name (:ns m))) (name %))
                      (sort (keys (:sigs value)))))
     :protocol (var-symbol (:protocol m))}))

(defn- source-meta [v]
  (merge (some-> v meta :protocol meta) (meta v)))

(defn source-location [v]
  (when-let [file (:file (source-meta v))]
    (let [local (io/file file)]
      (if (.isFile local)
        (.toURL (.toURI (.getCanonicalFile local)))
        (io/resource file)))))

(defn source-info
  "Locate a var's full source buffer and its one-based definition line.
  Protocol methods fall back to their protocol's declaration."
  [v]
  (if-let [source (:easel/source (meta v))]
    {:string source :line (or (:line (meta v)) 1) :ns (ns-name (:ns (meta v))) :label "orbit.clj"}
    (when-let [resource (source-location v)]
     (let [{:keys [file line ns]} (source-meta v)
          info {:line (or line 1) :ns (some-> ns ns-name)}]
      (if (= "file" (.getProtocol resource))
        (assoc info :file (io/file (.toURI resource)))
        (assoc info :string (slurp resource) :label file))))))

(defn source-intents [v]
  (when-let [info (source-info v)]
    [[:com.phronemophobic.easel/add-applet
      {:make-applet (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
                       handler info))}]]))

(defn link-target [link]
  (cond
    (map? link) link
    (qualified-symbol? link) {:kind :var :symbol link}
    (keyword? link) {:kind :guide :id link}))

(defn related-links [v]
  (let [{:keys [symbol methods protocol meta]} (describe-var v)]
    (vec
     (distinct
      (concat
       [{:kind :namespace :symbol (ns-name (:ns meta))}]
       (when protocol [{:kind :var :symbol protocol}])
       (map #(hash-map :kind :var :symbol %) methods)
       (keep link-target (:easel/see-also meta))
       (for [guide guides/guides
             :when (some #(= symbol (:symbol %)) (:links guide))]
         {:kind :guide :id (:id guide)}))))))

(defn var-examples [v]
  (into []
        (keep #(cond
                 (string? %) {:source %}
                 (and (map? %) (string? (:source %))) %))
        (:easel/examples (meta v))))

(defn loaded-namespaces []
  (vec (sort-by str (map ns-name (all-ns)))))

(defn lookup-target [text]
  (let [text (str/trim text)]
    (when (and (seq text) (not (re-find #"\s" text)))
      (let [sym (symbol text)]
        (cond
          (resolve-var sym) {:kind :var :symbol sym}
          (find-ns sym) {:kind :namespace :symbol sym})))))

(defn navigate [state target]
  (if (= (:target state) target)
    state
    (-> state
        (update :back events/bounded-conj (:target state) 100)
        (assoc :target target :forward [] :query "" :offset 0 :scroll [0 0] :error nil))))

(defeffect ::navigate [$state target]
  (dispatch! :update $state navigate target))

(defeffect ::history [$state direction]
  (dispatch! :update $state
             (fn [state]
               (if-let [target (peek (get state direction))]
                 (-> state
                     (update direction pop)
                     (update (if (= direction :back) :forward :back) conj (:target state))
                     (assoc :target target :query "" :offset 0 :scroll [0 0] :error nil))
                 state))))

(defeffect ::lookup [$state]
  (let [state (dispatch! :get $state)]
    (if-let [target (lookup-target (:lookup state ""))]
      (dispatch! ::navigate $state target)
      (dispatch! :update $state assoc :error "Choose a loaded namespace or a qualified var. Refresh to discover new namespaces."))))

(defn live-snapshot
  "Project observable applet state without retaining the observer's own state.
  Identity wrappers keep lazy or expensive state values out of equality checks."
  [easel]
  (let [visible (set (keys (get-in easel [:com.phronemophobic.easel/cached-layout :by-applet-id])))]
    (into {}
          (map (fn [[id a]]
                 [id {:id id :label (str (:label a)) :class (.getName (class a))
                      :size (:size a) :visible? (contains? visible id)
                      :generation (:com.phronemophobic.easel/instance-token a)
                      :lifecycle (cond (::applet/error a) :failed
                                       (seq (::model/queue a)) :starting
                                       (:com.phronemophobic.easel/applet-started? a) :started
                                       :else :created)
                      :error (some-> (::applet/error a) applet/error-message)
                      :implementation (var-symbol (or (:component-var a) (:view a)))
                      :protocols (into [] (keep (fn [[protocol sym]]
                                                 (when (satisfies? protocol a) sym)))
                                       [[model/IApplet 'com.phronemophobic.easel.model/IApplet]
                                        [model/IUI 'com.phronemophobic.easel.model/IUI]
                                        [model/IResizable 'com.phronemophobic.easel.model/IResizable]])
                      :documentation? (::documentation? a)
                      :state-key (if (contains? a :state) :state :applet)
                      :state (when-not (::documentation? a)
                               (viscous/wrap (if (contains? a :state) (:state a) a)))}]))
          (:applets easel))))

(defn update-live [state snapshot]
  (let [old (:live state)
        changes (for [id (sort-by str (set/union (set (keys old)) (set (keys snapshot))))
                      :let [before (get old id) after (get snapshot id)]
                      :when (not= before after)]
                  {:time (java.time.Instant/now) :id id :label (:label (or after before))
                   :change (cond (nil? before) :added
                                 (nil? after) :removed
                                 (not= (:generation before) (:generation after)) :restarted
                                 (not= (:lifecycle before) (:lifecycle after)) (:lifecycle after)
                                 (not= (:visible? before) (:visible? after)) (if (:visible? after) :shown :hidden)
                                 (not= (:size before) (:size after)) :resized
                                 :else :state)})]
    (assoc state :live snapshot
           :changes (reduce #(events/bounded-conj %1 %2 100) (:changes state []) changes))))

(defeffect ::inspect-applet [id]
  (when-let [a (get (dispatch! :com.phronemophobic.easel/get-applets) id)]
    (dispatch! :com.phronemophobic.easel/add-applet
               {:make-applet #(inspector/inspector-applet % a)})))

(defn- output-writer [buffer]
  (let [append! (fn [text]
                  (locking buffer
                    (let [remaining (- 8192 (.length ^StringBuilder buffer))]
                      (when (pos? remaining)
                        (.append ^StringBuilder buffer ^String (subs text 0 (min remaining (count text))))))))]
    (proxy [java.io.Writer] []
      (write
        ([value] (append! (cond (number? value) (str (char value))
                               (string? value) value
                               :else (String. ^chars value))))
        ([value offset length]
         (append! (if (string? value)
                    (subs value offset (+ offset length))
                    (String. ^chars value (int offset) (int length))))))
      (flush []) (close []))))

(defn- example-namespace [ns-sym]
  (if (instance? clojure.lang.Namespace ns-sym)
    ns-sym
    (or (find-ns ns-sym)
        (let [ns (create-ns ns-sym)]
          (binding [*ns* ns] (refer 'clojure.core))
          ns))))

(defn example-intents [source ns-sym]
  [[:com.phronemophobic.easel/add-applet
    {:make-applet (fn [handler]
                    (example-namespace ns-sym)
                    ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
                     handler {:string (str "(ns " ns-sym ")\n\n" source)
                              :label "Doc example" :ns ns-sym}))}]])

(defn run-example
  "Evaluate explicitly requested source in a persistent scratch namespace.
  Return bounded output and an identity-wrapped value or exception."
  [source ns-sym]
  (let [ns (example-namespace ns-sym)
        out (StringBuilder.) err (StringBuilder.)
        result (binding [*out* (java.io.PrintWriter. (output-writer out))
                         *err* (java.io.PrintWriter. (output-writer err))]
                 (evaluation/evaluate source {:ns ns :file "easel-example.clj"}))]
    (merge {:out (str out) :err (str err)}
           (if-let [e (:exception result)]
             {:exception (viscous/wrap e)
              :summary (applet/error-message
                        (loop [cause e]
                          (if (and (instance? clojure.lang.Compiler$CompilerException cause)
                                   (.getCause ^Throwable cause))
                            (recur (.getCause ^Throwable cause))
                            cause)))}
             {:value (viscous/wrap (:value result)) :summary (inspector/preview (:value result))}))))

(defeffect ::run-example [$state example-id source]
  (let [{:keys [ready? busy? runner example-ns]} (dispatch! :get $state)]
    (when (and ready? (not busy?))
      (let [runner @runner]
        (locking runner
          (when (:active? @runner)
            (let [ns (example-namespace example-ns)]
              (reset! runner
                      {:active? true
                       :future (applet/task! dispatch! $state
                                             #(run-example source ns)
                                             #(dispatch! :update $state assoc-in [:results example-id] %))}))))))))

(defeffect ::cancel-example [$state]
  (when-let [state (dispatch! :get $state)]
    (dispatch! :update $state assoc :request nil :busy? false)
    (when-let [task (:future @@(:runner state))] (future-cancel task))))

(defn- inspect-intents [obj]
  [[:com.phronemophobic.easel/add-applet
    {:make-applet #(inspector/inspector-applet % obj)}]])

(defn- link-label [{:keys [kind symbol id]}]
  (case kind
    :guide (or (:title (guides/guide id)) (str id))
    :var (str symbol)
    :namespace (str symbol)
    :applet (str "Applet " id)
    :applets "Live applets"
    :namespaces "Namespaces"
    :guides "Guides"
    (str kind)))

(defui link-view [{:keys [state target]}]
  (applet/button {:text (link-label target) :size :small
                  :extra (get extra [::link target])
                  :$extra [$extra (list 'keypath [::link target])]
                  :on-click #(vector [::navigate $state target])}))

(defn- prose [text width]
  (ui/padding 0 4 (eui/label text {:width (max 1 (- width 12))})))

(defn- row-layout [width elements]
  (let [{:keys [rows row]} (reduce (fn [{:keys [row row-width] :as layout} element]
                                   (let [w (ui/width element)]
                                     (if (and (seq row) (> (+ row-width 4 w) (max 1 width)))
                                       (-> layout (update :rows conj row) (assoc :row [element] :row-width w))
                                       (-> layout (update :row conj element) (assoc :row-width (+ row-width 4 w))))))
                                 {:rows [] :row [] :row-width 0}
                                 (remove nil? elements))]
    (ui/flex-layout (map #(ui/flex-layout % {:gap 4}) (cond-> rows (seq row) (conj row)))
                    {:direction :column :gap 4})))

(defui examples-view [{:keys [state target examples width]}]
  (apply ui/vertical-layout
         (map-indexed
          (fn [index {:keys [title source ns]}]
            (let [id [target index]
                  result (get-in state [:results id])
                  ;; Var examples may refer to their defining namespace.
                  source (if ns (str "(clojure.core/refer '" ns ")\n" source) source)]
              (ui/vertical-layout
               (eui/label (or title (str "Example " (inc index))))
               (ui/padding 0 6 (eui/code {:text source :width width :size 12}))
               (row-layout width
                [(applet/button {:text "Run" :size :small
                                  :extra (get extra [::example id :run])
                                  :$extra [$extra (list 'keypath [::example id :run])]
                                  :disabled? (or (not (:ready? state)) (:busy? state))
                                  :on-click #(vector [::run-example $state id source])})
                 (applet/button {:text "Edit in Clobber" :size :small
                                  :extra (get extra [::example id :edit])
                                  :$extra [$extra (list 'keypath [::example id :edit])]
                                  :disabled? (not (:ready? state))
                                  :on-click #(example-intents source (:example-ns state))})
                 (when result
                   (applet/button {:text "Inspect result" :size :small
                                    :extra (get extra [::example id :inspect])
                                    :$extra [$extra (list 'keypath [::example id :inspect])]
                                    :on-click #(inspect-intents @(or (:exception result) (:value result)))}))])
               (when result
                 (ui/vertical-layout
                  (eui/label (str (if (:exception result) "! " "=> ") (:summary result)))
                  (when (seq (:out result)) (eui/label (str "out: " (:out result))))
                  (when (seq (:err result)) (eui/label (str "err: " (:err result)))))))))
          examples)))

(defui page-view [{:keys [state entries render-entry]}]
  (let [query (str/lower-case (:query state ""))
        matches (vec (filter #(str/includes? (str/lower-case (str %)) query) entries))
        offset (min (:offset state 0) (* page-size (quot (max 0 (dec (count matches))) page-size)))
        page (take page-size (drop offset matches))]
    (ui/vertical-layout
     (eui/field {:text (:query state "") :$text [$state '(keypath :query)]
                      :size :small :placeholder "Filter names"})
     (apply ui/vertical-layout (map render-entry page))
     (ui/flex-layout
      [(applet/button {:text "Previous" :size :small :disabled? (zero? offset)
                        :on-click #(vector [:update $state assoc :offset (max 0 (- offset page-size)) :scroll [0 0]])})
       (eui/label (str (+ (if (seq page) 1 0) offset) "–" (+ offset (count page)) " / " (count matches)))
       (applet/button {:text "Next" :size :small :disabled? (>= (+ offset page-size) (count matches))
                        :on-click #(vector [:update $state assoc :offset (+ offset page-size) :scroll [0 0]])})]
      {:gap 4}))))

(defui var-view [{:keys [state target width]}]
  (if-let [v (resolve-var (:symbol target))]
    (let [{:keys [symbol kind meta doc arglists methods protocol]} (describe-var v)
          method-links (set (map #(hash-map :kind :var :symbol %) methods))
          examples (mapv #(assoc % :ns (or (:ns %) (ns-name (:ns meta)))) (var-examples v))]
      (ui/vertical-layout
       (prose (str symbol) width)
       (eui/label (str (name kind) (when (:private meta) " · private") (when (:dynamic meta) " · dynamic")))
       (when arglists (eui/label (pr-str arglists) {:role :code :size 12}))
       (prose (or doc "No docstring. Metadata and source are still available.") width)
       (when-let [file (:file (source-meta v))]
         (eui/label (str file ":" (:line (source-meta v)))))
       (row-layout width
        [(applet/button {:text "Source" :size :small :disabled? (nil? (source-location v))
                          :on-click #(source-intents v)})
         (applet/button {:text "Inspect metadata" :size :small :on-click #(inspect-intents (clojure.core/meta v))})
         (applet/button {:text "Inspect var" :size :small :on-click #(inspect-intents v)})])
       (when (seq methods)
         (ui/vertical-layout
          (prose "Protocol methods" width)
          (apply ui/vertical-layout
                 (map #(link-view {:state state :target {:kind :var :symbol %}}) methods))))
       (examples-view {:state state :target target :examples examples :width width})
       (prose "Related" width)
       (apply ui/vertical-layout (map #(link-view {:state state :target %}) (remove method-links (related-links v))))
       (let [uses (for [[id a] (:live state)
                        :when (or (= symbol (:implementation a))
                                  (some #{(or protocol symbol)} (:protocols a)))]
                    [id a])]
         (when (seq uses)
           (ui/vertical-layout
            (prose "Live uses" width)
            (apply ui/vertical-layout
                   (for [[id a] (sort-by (comp str first) uses)]
                     (applet/button {:text (str (:label a) " · " id) :size :small
                                     :on-click #(vector [::navigate $state {:kind :applet :id id}])}))))))))
    (prose "This var is no longer interned. Refresh or choose another namespace." width)))

(defui namespace-view [{:keys [state target width]}]
  (if-let [ns (find-ns (:symbol target))]
    (let [m (meta ns)]
      (ui/vertical-layout
       (prose (str (ns-name ns)) width)
       (when (:doc m) (prose (:doc m) width))
       (applet/button {:text "Inspect namespace" :size :small :on-click #(inspect-intents ns)})
       (prose "Vars · functions · protocols" width)
       (page-view {:state state :entries (sort-by str (keys (ns-interns ns)))
                   :render-entry (fn [sym]
                    (let [v (get (ns-interns ns) sym)
                          {:keys [kind meta]} (describe-var v)]
                      (applet/button {:text (str sym " · " (name kind) (when (:private meta) " · private"))
                                      :size :small
                                      :extra (get extra [::member (var-symbol v)])
                                      :$extra [$extra (list 'keypath [::member (var-symbol v)])]
                                      :on-click #(vector [::navigate $state {:kind :var :symbol (var-symbol v)}])})))})
       (when (seq (ns-aliases ns))
         (ui/vertical-layout
          (prose "Namespace links" width)
          (apply ui/vertical-layout
                 (for [[alias n] (sort-by (comp str key) (ns-aliases ns))]
                   (applet/button {:text (str alias " → " (ns-name n)) :size :small
                                   :on-click #(vector [::navigate $state {:kind :namespace :symbol (ns-name n)}])})))))))
    (prose "This namespace is no longer loaded." width)))

(defui applet-view [{:keys [state target width]}]
  (if-let [a (get (:live state) (:id target))]
    (ui/vertical-layout
     (prose (str (:label a) " · " (:id a)) width)
     (prose (str (name (:lifecycle a)) " · " (if (:visible? a) "visible" "hidden") " · " (:size a)) width)
     (when (:error a) (prose (:error a) width))
     (eui/label (:class a))
     (applet/button {:text "Inspect current applet" :size :small
                     :on-click #(vector [::inspect-applet (:id a)])})
     (prose (if (= :state (:state-key a)) "State" "Applet value") width)
     (if (:documentation? a)
       (prose "Use Inspect to capture Documentation's state. Its own state updates are excluded from the live feed to keep observation finite." width)
       (ui/vertical-layout
        (prose (inspector/preview @(:state a)) width)
        (applet/button {:text "Inspect state snapshot" :size :small
                        :on-click #(inspect-intents @(:state a))})))
     (when-let [sym (:implementation a)] (link-view {:state state :target {:kind :var :symbol sym}}))
     (apply ui/vertical-layout (map #(link-view {:state state :target {:kind :var :symbol %}}) (:protocols a)))
     (link-view {:state state :target {:kind :guide :id :lifecycle}}))
    (prose "This applet has closed. Its observed lifecycle changes remain in Live applets." width)))

(defui target-view [{:keys [state width]}]
  (let [target (:target state)]
    (case (:kind target)
      :guides (apply ui/vertical-layout
                     (for [guide guides/guides] (link-view {:state state :target {:kind :guide :id (:id guide)}})))
      :guide (if-let [{:keys [title paragraphs examples links]} (guides/guide (:id target))]
               (ui/vertical-layout
                (prose title width)
                (apply ui/vertical-layout (map #(prose % width) paragraphs))
                (examples-view {:state state :target target :examples examples :width width})
                (prose "Explore" width)
                (apply ui/vertical-layout (map #(link-view {:state state :target %}) links)))
               (prose "This guide is not registered." width))
      :namespaces (ui/vertical-layout
                   (prose "Loaded namespaces · Refresh after loading more code" width)
                   (page-view {:state state :entries (:namespaces state)
                               :render-entry #(link-view {:state state :target {:kind :namespace :symbol %}})}))
      :namespace (namespace-view {:state state :target target :width width})
      :var (var-view {:state state :target target :width width})
      :applet (applet-view {:state state :target target :width width})
      :applets (ui/vertical-layout
                (prose "Live applets" width)
                (if (seq (:live state))
                  (apply ui/vertical-layout
                         (for [[id a] (sort-by (comp str key) (:live state))]
                           (applet/button {:text (str (:label a) " · " id " · " (name (:lifecycle a))
                                                     " · " (if (:visible? a) "visible" "hidden"))
                                           :size :small
                                           :on-click #(vector [::navigate $state {:kind :applet :id id}])})))
                  (prose "No applets in this workspace." width))
                (prose "Observed changes · most recent first" width)
                (apply ui/vertical-layout
                       (for [{:keys [time id label change]} (reverse (:changes state))]
                         (eui/label (str time " · " label " (" id ") · " (name change))))))
      (prose "Choose a guide, namespace, or live applet." width))))

(declare documentation-applet)

(defui summary-header [{:keys [applet width]}]
  (let [v (resolve-var (get-in applet [:state :target :symbol]))]
    (when (> width 140)
      (eui/row nil
       [(eui/button {:text "Source" :variant :link :size :small :disabled? (nil? v) :on-click #(source-intents v)})
        (eui/button {:text "Metadata" :variant :link :size :small :disabled? (nil? v) :on-click #(inspect-intents (meta v))})]))))

(defn- open-reference [target]
  [[:com.phronemophobic.easel/add-applet {:make-applet #(documentation-applet % {:target target})}]])

(defui summary-view [{:keys [state size]}]
  (let [[w h] size
        target (:target state)
        v (resolve-var (:symbol target))
        m (meta v)
        example (first (var-examples v))
        id [target 0]
        result (get-in state [:results id])
        source (:source example)
        runnable (when source (str "(clojure.core/refer '" (some-> m :ns ns-name) ")\n" source))
        body (ui/vertical-layout
               (eui/text {:text (str (:name m)) :role :code :size 22 :weight :semi-bold})
               (ui/spacer 0 4)
               (eui/text {:text (inspector/preview (:arglists m)) :role :code :size 12})
               (ui/spacer 0 8)
               (eui/text {:text (or (:doc m) "Inspect a var to read its documentation.") :width (max 1 (- w 40))})
               (ui/spacer 0 12)
               (when source
                 (let [width (max 1 (- w 40))]
                  (ui/fixed-bounds [width 72]
                   [(eui/surface context width 72 :color/header 4)
                    (eui/border context width 72 :color/border 4)
                    (ui/translate 12 10 (eui/code {:text source :width (max 1 (- width 84)) :size 12}))
                    (ui/translate (max 0 (- width 62)) 6
                      (eui/button {:text "Run" :height 28 :disabled? (or (not (:ready? state)) (:busy? state))
                                   :on-click #(vector [::run-example $state id runnable])}))
                  (when result
                    (ui/translate 12 43
                     (ui/on :mouse-down #(inspect-intents @(or (:value result) (:exception result)))
                      (eui/text {:text (str "⇒ " (:summary result)) :role :code :size 12
                                  :color (if (:exception result) :color/error :color/secondary)}))))])))
               (ui/spacer 0 12)
               (eui/wrap-row (max 0 (- w 40))
                 [(eui/text {:text "Related" :size 12 :color :color/secondary})
                  (eui/button {:text "IUI" :variant :link :size :small
                               :on-click #(open-reference {:kind :var :symbol 'com.phronemophobic.easel.model/IUI})})
                  (eui/button {:text "Applet lifecycle" :variant :link :size :small
                               :on-click #(open-reference {:kind :guide :id :lifecycle})})]))]
    (basic/scrollview {:scroll-bounds [(max 0 w) (max 0 h)] :$body nil
                       :body (ui/padding 20 10 body)})))

(defui documentation-view [{:keys [state size]}]
 (if (:compact? state)
  (summary-view {:state state :size size})
  (let [width (max 0 (first size))
        header (ui/vertical-layout
                (row-layout width
                 [(applet/button {:text "Back" :size :small :disabled? (empty? (:back state))
                                   :on-click #(vector [::history $state :back])})
                  (applet/button {:text "Forward" :size :small :disabled? (empty? (:forward state))
                                   :on-click #(vector [::history $state :forward])})
                  (link-view {:state state :target {:kind :guides}})
                  (link-view {:state state :target {:kind :namespaces}})
                  (link-view {:state state :target {:kind :applets}})
                  (applet/button {:text "Refresh" :size :small
                                   :on-click #(vector [:update $state assoc :namespaces (loaded-namespaces)
                                                      :revision (inc (:revision state 0))])})])
                (ui/flex-layout
                 [(eui/field {:text (:lookup state "") :$text [$state '(keypath :lookup)]
                                    :size :small :placeholder "namespace or namespace/var"
                                    :flex-layout.stretch/width (max 1 (- width 65))})
                  (applet/button {:text "Go" :size :small :on-click #(vector [::lookup $state])})]
                 {:gap 4})
                (when (:busy? state)
                  (ui/horizontal-layout
                   (eui/label "Evaluating example… ")
                   (applet/button {:text "Cancel" :size :small :on-click #(vector [::cancel-example $state])})))
                (when-let [error (:error state)] (prose error width)))]
    (basic/scrollview
      {:scroll-bounds [width (max 0 (second size))]
       :offset (:scroll state [0 0]) :$offset [$state '(keypath :scroll)]
       :$body nil
       :body (ui/vertical-layout header (target-view {:state state :width width}))}))))

(defn initial-state []
  {:target {:kind :guide :id :start} :back [] :forward []
   :query "" :lookup "" :offset 0 :scroll [0 0] :results {} :live {} :changes []
   :namespaces (loaded-namespaces) :ready? false
   :example-ns (symbol (str "easel.examples.doc-" (random-uuid)))
   :runner (viscous/wrap (atom {:active? true}))})

(defn documentation-applet
  "Create a native guide, metadata browser, example REPL, and live applet inspector.
  Var metadata may provide :easel/examples and :easel/see-also links."
  {:easel/see-also [com.phronemophobic.easel.model/IApplet
                    com.phronemophobic.easel.applet/tool-applet :documentation]}
  ([handler] (documentation-applet handler {}))
  ([handler options]
  (let [state (merge (initial-state) options)
        runner @(:runner state)]
    (assoc
     (applet/tool-applet
      "Documentation" #'documentation-view state
      (fn [$state]
        (let [release (handler :com.phronemophobic.easel/subscribe-applets
                               live-snapshot #(handler :update $state update-live %))]
          (handler :update $state assoc :ready? true)
          (fn []
            (release)
            (locking runner
              (when-let [task (:future @runner)] (future-cancel task))
              (reset! runner {:active? false}))
            (when (find-ns (:example-ns state)) (remove-ns (:example-ns state)))))))
     :icon "book" :header (when (:compact? options) #'summary-header) ::documentation? true))))
