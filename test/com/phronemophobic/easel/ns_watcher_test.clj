(ns com.phronemophobic.easel.ns-watcher-test
  (:require [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.ns-watcher :as watcher]
            [membrane.component :as component]))

(defn await-value [f]
  (loop [remaining 200]
    (or (f)
        (when (pos? remaining)
          (Thread/sleep 10)
          (recur (dec remaining))))))

(deftest independent-namespace-watchers
  (let [ns-sym (symbol (str "easel.test." (gensym)))
        test-ns (create-ns ns-sym)
        v (intern test-ns 'value 1)
        state (atom {}) handler (component/default-handler state)
        a (model/-start (watcher/ns-watcher-applet handler test-ns) '(keypath :a) [300 200] [1 1])
        b (model/-start (watcher/ns-watcher-applet handler test-ns) '(keypath :b) [300 200] [1 1])]
    (reset! state {:a a :b b})
    (try
      (run! #(%) (::model/queue a))
      (run! #(%) (::model/queue b))
      (is (await-value #(= 2 (count (.getWatches v)))))
      (is (fn? (get-in @state [:a :unwatch])))
      (model/-stop a)
      (alter-var-root v (constantly 2))
      (is (await-value #(= 2 @(-> @state :b :interns first second))))
      (is (= 1 @(-> @state :a :interns first second)))
      (finally
        (model/-stop a)
        (model/-stop b)
        (await-value #(empty? (.getWatches v)))
        (remove-ns ns-sym)))))

(deftest close-before-watch-starts
  (let [state (atom {}) handler (component/default-handler state)
        applet (model/-start (watcher/ns-watcher-applet handler *ns*) '(keypath :applet) [300 200] [1 1])]
    (model/-stop applet)
    (run! #(%) (::model/queue applet))
    (is (not (contains? @watcher/watch-list* (:watch-key applet))))))
