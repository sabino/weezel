(ns com.phronemophobic.easel.tools-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.exceptions :as exceptions]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.logging :as logging]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.tap-watcher :as tap]
            [com.rpl.specter :as specter]
            [membrane.component :as component])
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
