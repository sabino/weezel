(ns com.phronemophobic.easel.tools-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.builder-test :as views]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.exceptions :as exceptions]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.logging :as logging]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.spreadsheet :as spreadsheet]
            [com.phronemophobic.easel.tap-watcher :as tap]
            [com.phronemophobic.easel.theme :as theme]
            [com.rpl.specter :as specter]
            [liq.buffer :as buffer]
            [membrane.component :as component :refer [defui]]
            [membrane.ui :as ui])
  (:import [java.util.logging Logger]))

(deftest bounded-history
  (is (= [3 4 5] (reduce #(events/bounded-conj %1 %2 3) [] (range 6))))
  (let [received (atom [])
        unsubscribe (events/subscribe! ::test #(reset! received %))]
    (try
      (events/publish! {:kind ::test :message "one"})
      (is (= "one" (:message (peek @received))))
      (unsubscribe)
      (events/publish! {:kind ::test :message "two"})
      (is (= "one" (:message (peek @received))))
      (finally (unsubscribe)))))

(deftest inspector-pagination
  (testing "Infinite sequences are realized one page at a time"
    (let [page (inspector/page (range) 50)]
      (is (= [50 50] (first (:entries page))))
      (is (= 50 (count (:entries page))))
      (is (:more? page))))
  (is (= {:entries [[nil false]] :more? false} (inspector/page {nil false} 0)))
  (let [state (inspector/initial-state {:a [1 nil]})
        child (inspector/descend state :a [1 nil])]
    (is (= [1 nil] @(:obj child)))
    (is (= {:a [1 nil]} @(:obj (first (:parents child))))))
  (is (< (count (inspector/preview (range))) 240)))

(deftest throwable-causes
  (let [a (Exception. "a") b (Exception. "b") s (Exception. "suppressed")]
    (.initCause a b)
    (.initCause b a)
    (.addSuppressed a s)
    (is (= [a s b] (mapv :throwable (exceptions/causes a))))))

(deftest jul-capture-and-cleanup
  (let [logger (Logger/getLogger (str "easel.test." (random-uuid)))
        previous (.getUseParentHandlers logger)
        root (Logger/getLogger "")
        before (count (.getHandlers root))
        release-a (logging/acquire-jul!)
        release-b (logging/acquire-jul!)]
    (try
      (is (= (inc before) (count (.getHandlers root))))
      (.warning logger "capture-test")
      (is (= "capture-test" (:message (peek (events/recent :log)))))
      (release-a)
      (release-a)
      (is (= (inc before) (count (.getHandlers root))))
      (finally
        (release-a)
        (release-b)
        (.setUseParentHandlers logger previous)))
    (is (= before (count (.getHandlers root))))))

(deftest logging-filters
  (let [entries [{:id 1 :level :info :message "first"}
                 {:id 2 :level :error :message "Broken" :logger "app"}]]
    (is (= [2] (mapv :id (logging/matching-events entries :warn "BROKEN" -1))))
    (is (empty? (logging/matching-events entries :trace "" 2)))))

(deftest applet-cleanup
  (let [starts (atom 0) stops (atom 0)
        make #(model/-start (applet/tool-applet "Test" identity {}
                                               (fn [_]
                                                 (swap! starts inc)
                                                 (fn [] (swap! stops inc))))
                            nil [100 100] [1 1])
        first-applet (make)]
    (model/-stop first-applet)
    (run! #(%) (::model/queue first-applet))
    (is (zero? @starts))
    (let [second-applet (make)]
      (run! #(%) (::model/queue second-applet))
      (model/-stop second-applet)
      (model/-stop second-applet)
      (is (= 1 @starts @stops)))))

(deftest tap-retention-and-pause
  (let [state (atom {}) handler (component/default-handler state)
        applet (model/-start (tap/tap-watcher-applet handler) '(keypath :applet) [300 200] [1 1])]
    (reset! state {:applet applet})
    (try
      (dotimes [i 510] ((:tapf applet) i))
      (is (= 500 (count (get-in @state [:applet :tap-vals]))))
      (is (= 10 @(-> @state :applet :tap-vals first :obj)))
      (swap! state assoc-in [:applet :paused?] true)
      ((:tapf applet) :paused)
      (is (= 509 @(-> @state :applet :tap-vals peek :obj)))
      (model/-stop applet)
      ((:tapf applet) :stopped)
      (is (= 509 @(-> @state :applet :tap-vals peek :obj)))
      (finally (model/-stop applet)))))

(deftest failing-subscriber-is-isolated
  (let [received (atom [])
        before (count (.getWatches events/history))
        bad (events/subscribe! ::isolated (fn [_] (throw (AssertionError. "bad listener"))))
        good (events/subscribe! ::isolated #(reset! received %))]
    (try
      (events/publish! {:kind ::isolated :message "still delivered"})
      (is (= "still delivered" (:message (peek @received))))
      (is (= "bad listener" (some-> (events/recent :exception) peek :throwable .getMessage)))
      (finally (bad) (good)))
    (is (= before (count (.getWatches events/history))))))

(deftest reentrant-subscriber
  (let [calls (atom 0)
        stop (events/subscribe! ::reentrant
                                (fn [entries]
                                  (when (seq entries)
                                    (swap! calls inc)
                                    (events/publish! {:kind ::reentrant :message "nested"}))))]
    (try
      (events/publish! {:kind ::reentrant :message "outer"})
      (is (= 1 @calls))
      (is (= "nested" (:message (peek (events/recent ::reentrant)))))
      (finally (stop)))))

(deftest notifications-from-a-new-thread
  (let [kind (random-uuid) finished (promise) received (promise)
        stop (events/subscribe! kind
                                (fn [entries]
                                  (case (:message (peek entries))
                                    "outer" (future (events/publish! {:kind kind :message "inner"})
                                                    (deliver finished true))
                                    "inner" (deliver received true)
                                    nil)))]
    (try
      (events/publish! {:kind kind :message "outer"})
      (is (deref finished 1000 false))
      (is (deref received 1000 false))
      (finally (stop)))))

(deftest asynchronous-task-results
  (let [state (atom {:panel {}}) handler (component/default-handler state)
        path (specter/must :panel)
        entered (promise) release (promise)
        stale (applet/task! handler path #(do (deliver entered true) @release {:value :old}))]
    (is (deref entered 1000 false))
    @(applet/task! handler path (fn [] {:value :new}))
    (deliver release true)
    @stale
    (is (= :new (get-in @state [:panel :value])))
    @(applet/task! handler path #(throw (AssertionError. "task failure")))
    (is (= "task failure" (get-in @state [:panel :error])))
    (is (false? (get-in @state [:panel :busy?])))))

(deftest closing-a-pending-task
  (let [state (atom {:panel {}}) handler (component/default-handler state)
        entered (promise) release (promise) calls (atom 0)
        task (applet/task! handler (specter/must :panel)
                           #(do (deliver entered true) @release {:value :late})
                           (fn [_] (swap! calls inc)))]
    (is (deref entered 1000 false))
    (swap! state dissoc :panel)
    (deliver release true)
    @task
    (is (empty? @state))
    (is (zero? @calls))))

(deftest bounded-and-failing-previews
  (is (= "{:a nil}" (inspector/preview {:a nil})))
  (is (<= (count (inspector/preview {:huge (apply str (repeat 10000 "x"))})) 241))
  (let [bad (proxy [Object] [] (toString [] (throw (IllegalStateException. "unprintable"))))]
    (is (re-find #"could not be printed" (inspector/preview bad))))
  (let [bad (lazy-seq (throw (IllegalStateException. "broken sequence")))]
    (is (re-find #"broken sequence" (:error (inspector/page bad 0))))))

(deftest spreadsheet-evaluates-and-releases-its-worker
  (let [previous @easel/app-state
        before (count (.getWatches easel/app-state))
        applet (model/-start (spreadsheet/spreadsheet-applet easel/handler)
                             '(keypath :sheet) [640 480] [1 1])
        wait-until (fn [f] (loop [n 200] (or (f) (when (pos? n) (Thread/sleep 10) (recur (dec n))))))]
    (try
      (reset! easel/app-state {:sheet applet})
      (run! #(%) (::model/queue applet))
      (let [worker (:thread @(:worker* applet))]
        (is (= (inc before) (count (.getWatches easel/app-state))))
        (easel/handler :update '(keypath :sheet) assoc :ss [{:id :answer :name "answer" :editor :code-editor :src "(+ 20 22)"}])
        (is (wait-until #(contains? (:results (:sheet @easel/app-state)) :answer)))
        (is (= 42 @(get-in @easel/app-state [:sheet :results :answer])))
        (model/-stop applet)
        (model/-stop applet)
        (.join ^Thread worker 2000)
        (is (not (.isAlive ^Thread worker)))
        (is (= before (count (.getWatches easel/app-state)))))
      (finally (model/-stop applet) (reset! easel/app-state previous)))))

(defui sheet-view [{:keys [sheet]}]
  (model/-ui sheet $context context))

(deftest spreadsheet-can-edit-themed-formulas-and-recalculate-dependent-cells
  (let [previous @easel/app-state
        sheet (model/-start (spreadsheet/spreadsheet-applet easel/handler)
                            '(keypath :sheet) [640 480] [1 1])
        wait-until (fn [f] (loop [n 300] (or (f) (when (pos? n) (Thread/sleep 10) (recur (dec n))))))
        result #(some-> (get-in @easel/app-state [:sheet :results %]) deref)
        app (component/make-app #'sheet-view easel/app-state easel/handler)]
    (try
      (reset! easel/app-state {:sheet sheet})
      (run! #(%) (::model/queue sheet))
      (doseq [id [:easel/studio-light :easel/studio-dark]]
        (easel/handler :set [:membrane.component/context ::theme/value] (theme/resolve-theme (theme/builtin id)))
        (easel/handler :update '(keypath :sheet) assoc :ss
                       [{:id :answer :name "answer" :editor :code-editor
                         :src (buffer/buffer "(+ 20 22)" {:mode :insert})}
                        {:id :scale :name "scale" :editor :number-slider :src 3}
                        {:id :total :name "total" :editor :code-editor
                         :src (buffer/buffer "(* answer scale)" {:mode :insert})}])
        (is (wait-until #(= 126 (result :total))))
        (let [{:keys [x y] :as formula} (first (views/frames (app) #(= "(" (:paragraph %))))]
          (is (some? formula))
          (when formula
            (ui/mouse-down (app) [(+ x 2) (+ y 2)])
            (dotimes [_ 12] (ui/key-press (app) :right))
            (dotimes [_ 12] (ui/key-press (app) :backspace))
            (ui/clipboard-paste (app) "100")
            (is (= "100" (buffer/text (get-in @easel/app-state [:sheet :ss 0 :src]))))
            (is (wait-until #(= 300 (result :total))))
            (is (= 100 (result :answer)))))
        (doseq [size [[640 480] [240 160] [0 0]]]
          (easel/handler :update '(keypath :sheet) model/-resize size [2 2])
          (let [view (app) nodes (tree-seq #(seq (ui/children %)) ui/children view)]
            (is (= size (ui/bounds view)))
            (is (not-any? #(instance? membrane.ui.Label %) nodes))))
        (easel/handler :update '(keypath :sheet) model/-resize [640 480] [1 1]))
      (finally
        (model/-stop sheet)
        (reset! easel/app-state previous)))))

(deftest spreadsheet-closed-before-start-does-not-create-a-worker
  (let [applet (model/-start (spreadsheet/spreadsheet-applet easel/handler)
                             '(keypath :sheet) [640 480] [1 1])]
    (model/-stop applet)
    (run! #(%) (::model/queue applet))
    (is (nil? @(:worker* applet)))))

(deftest throwables-are-navigable-data
  (let [error (ex-info "install failed" {:package :easel/flowstorm})
        state (inspector/initial-state error)
        data (inspector/display-value error)
        child (inspector/descend state :data (:data data))]
    (is (= "install failed" (:cause data)))
    (is (= {:package :easel/flowstorm} @(:obj child)))
    (is (identical? error @(:obj (peek (:parents child)))))))
