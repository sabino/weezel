(ns com.phronemophobic.easel.documentation-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.clobber :as clobber]
            [com.phronemophobic.easel.documentation :as doc]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.guides :as guides]
            [com.phronemophobic.easel.model :as model]
            [com.rpl.specter :as specter]
            [membrane.component :as component]
            [membrane.ui :as ui]))

(defn scratch-ns [] (symbol (str "easel.test.examples-" (random-uuid))))

(deftest metadata-and-source-navigation
  (let [protocol (doc/describe-var #'model/IApplet)
        method (doc/describe-var #'model/-start)]
    (is (= :protocol (:kind protocol)))
    (is (= '[com.phronemophobic.easel.model/-start
             com.phronemophobic.easel.model/-stop] (:methods protocol)))
    (is (seq (:doc protocol)))
    (is (= :protocol-method (:kind method)))
    (is (= 'com.phronemophobic.easel.model/IApplet (:protocol method)))
    (is (= '([this $ref size content-scale]) (:arglists method))))
  (is (= :macro (:kind (doc/describe-var #'when))))
  (is (= :var (:kind (doc/describe-var #'guides/guides))))
  (is (= #'model/IUI (doc/resolve-var 'com.phronemophobic.easel.model/IUI)))
  (is (nil? (doc/resolve-var 'clojure.core/not-interned)))
  (is (nil? (doc/resolve-var 'IUI)))
  (let [local (doc/source-info #'model/IApplet)
        jar (doc/source-info #'clojure.core/map)]
    (is (.isFile (:file local)))
    (is (pos? (:line local)))
    (is (= 'com.phronemophobic.easel.model (:ns local)))
    (is (.contains (slurp (:file local)) "(defprotocol IApplet"))
    (is (= local (doc/source-info #'model/-start)))
    (is (.contains (:string jar) "(defn map"))
    (is (= 'clojure.core (:ns jar)))
    (is (pos? (:line jar))))
  (is (some #(= {:kind :guide :id :applets} %) (doc/related-links #'applet/tool-applet)))
  (is (some #(= {:kind :var :symbol 'com.phronemophobic.easel.model/-start} %)
            (doc/related-links #'model/IApplet))))

(deftest guides-have-working-links-and-examples
  (doseq [{:keys [id links examples]} guides/guides]
    (testing (str id)
      (doseq [link links]
        (case (:kind link)
          :var (is (var? (doc/resolve-var (:symbol link))))
          :guide (is (some? (guides/guide (:id link))))
          nil))
      (let [ns-sym (scratch-ns)]
        (try
          (doseq [example examples]
            (let [result (doc/run-example (:source example) ns-sym)]
              (is (nil? (:exception result)) (:summary result))
              (is (contains? result :value))))
          (finally (remove-ns ns-sym))))))
  (let [ns-sym (scratch-ns)]
    (try
      (doseq [example (doc/var-examples #'applet/tool-applet)]
        (let [source (str "(require '[com.phronemophobic.easel.applet :refer :all])\n" (:source example))
              result (doc/run-example source ns-sym)]
          (is (nil? (:exception result)) (:summary result))
          (is (= {:message "Hello, Clojure"} @(:value result)))))
      (finally (remove-ns ns-sym)))))

(deftest examples-preserve-values-and-namespace
  (let [ns-sym (scratch-ns)]
    (try
      (is (nil? @(:value (doc/run-example "nil" ns-sym))))
      (is (false? @(:value (doc/run-example "false" ns-sym))))
      (doc/run-example "(def answer 42)" ns-sym)
      (is (= 42 @(:value (doc/run-example "answer" ns-sym))))
      (let [result (doc/run-example "(println :hello) (binding [*out* *err*] (print :warning)) :ok" ns-sym)]
        (is (= ":hello\n" (:out result)))
        (is (= ":warning" (:err result)))
        (is (= :ok @(:value result))))
      (is (= 8192 (count (:out (doc/run-example "(print (apply str (repeat 20000 \"x\")))" ns-sym)))))
      (let [result (doc/run-example "(throw (ex-info \"example failed\" {:x 1}))" ns-sym)]
        (is (instance? Throwable @(:exception result)))
        (is (= "example failed" (:summary result))))
      (finally (remove-ns ns-sym)))))

(deftest lookup-and-history
  (is (= {:kind :var :symbol 'com.phronemophobic.easel.model/IUI}
         (doc/lookup-target " com.phronemophobic.easel.model/IUI ")))
  (is (= {:kind :namespace :symbol 'com.phronemophobic.easel.model}
         (doc/lookup-target "com.phronemophobic.easel.model")))
  (is (nil? (doc/lookup-target "(throw :oops)")))
  (let [initial (doc/initial-state)
        state (atom {:state initial})
        handler (component/default-handler state)
        target {:kind :var :symbol 'com.phronemophobic.easel.model/IUI}]
    (handler ::doc/navigate '(keypath :state) target)
    (handler ::doc/navigate '(keypath :state) target)
    (is (= [(:target initial)] (get-in @state [:state :back])))
    (handler ::doc/history '(keypath :state) :back)
    (is (= (:target initial) (get-in @state [:state :target])))
    (handler ::doc/history '(keypath :state) :forward)
    (is (= target (get-in @state [:state :target])))))

(deftest live-observers-do-not-observe-themselves
  (let [state (atom {:easel {:applets {}}})
        handler (component/default-handler state)
        start (fn [id] (model/-start (assoc (doc/documentation-applet handler) :id id)
                                     (specter/path :easel :applets (specter/must id))
                                     [600 500] [1 1]))]
    (with-redefs [easel/app-state state]
      (let [a (start :a) b (start :b)
            sample (assoc (applet/tool-applet "Sample" identity {:count 0}) :id :sample)
            ns-sym (get-in a [:state :example-ns])]
        (swap! state assoc-in [:easel :applets] {:a a :b b :sample sample})
        (try
          (run! #(%) (::model/queue a))
          (run! #(%) (::model/queue b))
          (is (= 2 (count (.getWatches state))))
          (is (= {:count 0} @(get-in @state [:easel :applets :a :state :live :sample :state])))
          (swap! state assoc-in [:easel :applets :sample :state :count] 1)
          (doseq [id [:a :b]]
            (is (= {:count 1} @(get-in @state [:easel :applets id :state :live :sample :state]))))
          (let [changes (get-in @state [:easel :applets :a :state :changes])]
            (swap! state assoc-in [:easel :applets :a :state :lookup] "new text")
            (is (= changes (get-in @state [:easel :applets :a :state :changes]))))
          (handler ::doc/run-example (specter/path :easel :applets (specter/must :a) :state) :test "(def live-answer 42)")
          (deref (:future @@(get-in a [:state :runner])) 2000 nil)
          (is (= 42 @(ns-resolve ns-sym 'live-answer)))
          (swap! state update-in [:easel :applets] dissoc :sample)
          (is (= :removed (-> @state :easel :applets :a :state :changes peek :change)))
          (swap! state update-in [:easel :applets] dissoc :a)
          (model/-stop a)
          (is (nil? (find-ns ns-sym)))
          (is (= 1 (count (.getWatches state))))
          (finally (model/-stop a) (model/-stop b))))
      (is (empty? (.getWatches state))))))

(deftest failing-live-subscriptions-release-their-watch
  (let [state (atom nil)]
    (with-redefs [easel/app-state state]
      (let [stop (easel/subscribe-applets! identity (fn [_] (throw (Exception. "bad live subscriber"))))]
        (is (empty? (.getWatches state)))
        (is (= "bad live subscriber" (some-> (events/recent :exception) peek :throwable .getMessage)))
        (stop)
        (stop)))))

(deftest concurrent-live-updates-stay-current
  (let [state (atom {:easel 0}) received (atom [])]
    (with-redefs [easel/app-state state]
      (let [stop (easel/subscribe-applets! identity #(swap! received conj %))]
        (try
          (let [workers (doall (repeatedly 6 #(future (dotimes [_ 50] (swap! state update :easel inc)))))]
            (run! deref workers))
          (is (= 300 (peek @received)))
          (is (apply <= @received))
          (finally (stop)))))))

(deftest concurrent-observers-can-dispatch-without-deadlock
  (let [state (atom {:easel {:count 0}})]
    (with-redefs [easel/app-state state]
      (let [a (easel/subscribe-applets! :count #(swap! state assoc-in [:easel :a] %))
            b (easel/subscribe-applets! :count #(swap! state assoc-in [:easel :b] %))]
        (try
          (let [workers (doall (repeatedly 6 #(future (dotimes [_ 50] (swap! state update-in [:easel :count] inc)))))]
            (is (every? #(not= ::timeout (deref % 2000 ::timeout)) workers)))
          (is (= {:count 300 :a 300 :b 300} (:easel @state)))
          (finally (a) (b)))))))

(deftest example-editor-keeps-the-scratch-namespace
  (let [ns-sym (scratch-ns)
        state (atom {}) handler (component/default-handler state)
        factory (-> (doc/example-intents "(+ 20 22)" ns-sym) first second :make-applet)
        editor (model/-start (factory handler) '(keypath :applet) [400 300] [1 1])]
    (reset! state {:applet editor})
    (try
      (run! #(%) (::model/queue editor))
      (let [value (get-in @state [:applet :state :editor])]
        (is (= ns-sym (ns-name (:eval-ns value))))
        (is (= 42 @(:value (doc/run-example (str (:rope value)) (:eval-ns value))))))
      (finally
        (model/-stop (get @state :applet))
        (when (find-ns ns-sym) (remove-ns ns-sym))))))

(deftest cancelled-examples-do-not-deliver-results
  (let [initial (assoc (doc/initial-state) :ready? true)
        state (atom {:state initial}) handler (component/default-handler state)]
    (try
      (handler ::doc/run-example '(keypath :state) :slow "(Thread/sleep 10000) :late")
      (let [task (:future @@(:runner initial))]
        (handler ::doc/cancel-example '(keypath :state))
        (is (future-cancelled? task))
        (is (false? (get-in @state [:state :busy?])))
        (is (nil? (get-in @state [:state :request])))
        (is (empty? (get-in @state [:state :results]))))
      (finally
        (when (find-ns (:example-ns initial)) (remove-ns (:example-ns initial)))))))

(defn nodes [view] (tree-seq #(seq (ui/children %)) ui/children view))

(deftest namespace-navigation-accepts-keyboard-input
  (let [state (atom {:state (doc/initial-state) :size [700 600]})
        handler (component/default-handler state)
        app (component/make-app #'doc/documentation-view state
                                (fn [intents] (run! #(apply handler %) intents)))
        click (fn [text]
                (let [button (some #(when (and (= text (:text %)) (:on-click %)) %) (nodes (app)))]
                  (run! #(apply handler %) (ui/mouse-down button [5 5]))))]
    (ui/mouse-down (app) [10 36])
    (ui/key-press (app) "com.phronemophobic.easel.model")
    (click "Go")
    (is (= :namespace (get-in @state [:state :target :kind])))
    (ui/mouse-down (app) [10 158])
    (ui/key-press (app) "IApplet")
    (is (= "IApplet" (get-in @state [:state :query])))
    (click "IApplet · protocol")
    (is (= 'com.phronemophobic.easel.model/IApplet (get-in @state [:state :target :symbol])))))

(deftest documentation-views-and-explicit-example-execution
  (let [sample (applet/tool-applet "Sample" identity {:count 0})
        initial (assoc (doc/initial-state) :ready? true
                       :live (doc/live-snapshot {:applets {:sample sample}}))
        ns-sym (:example-ns initial)
        state (atom {:state initial :size [700 600]})
        handler (component/default-handler state)
        app (component/make-app #'doc/documentation-view state
                                (fn [intents] (run! #(apply handler %) intents)))]
    (try
      (doseq [size [[700 600] [160 100] [0 0]]
              target (concat [{:kind :guides} {:kind :namespaces}
                              {:kind :namespace :symbol 'com.phronemophobic.easel.model}
                              {:kind :var :symbol 'com.phronemophobic.easel.model/IApplet}
                              {:kind :var :symbol 'com.phronemophobic.easel.applet/tool-applet}
                              {:kind :applets} {:kind :applet :id :sample} {:kind :applet :id :missing}]
                             (map #(hash-map :kind :guide :id (:id %)) guides/guides))]
        (swap! state assoc :size size)
        (swap! state assoc-in [:state :target] target)
        (is (every? number? (ui/bounds (app)))))
      (is (nil? (find-ns ns-sym)))
      (swap! state assoc :state initial :size [700 600])
      (let [run-button (some #(when (and (= "Run" (:text %)) (:on-click %)) %) (nodes (app)))]
        (is (some? run-button))
        (run! #(apply handler %) (ui/mouse-down run-button [5 5])))
      (deref (:future @@(:runner initial)) 2000 nil)
      (is (find-ns ns-sym))
      (is (contains? (get-in @state [:state :results [{:kind :guide :id :start} 0]]) :value))
      (finally
        (when (find-ns ns-sym) (remove-ns ns-sym))))))
