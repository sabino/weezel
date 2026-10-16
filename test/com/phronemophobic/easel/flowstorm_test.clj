(ns com.phronemophobic.easel.flowstorm-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel.flowstorm :as flowstorm]))

(defn traced-function [x]
  (let [y (inc x)]
    (* y y)))

(deftest qualified-var-validation
  (is (= 'com.phronemophobic.easel.flowstorm-test/traced-function
         (flowstorm/var-symbol "com.phronemophobic.easel.flowstorm-test/traced-function")))
  (is (thrown? clojure.lang.ExceptionInfo (flowstorm/var-symbol "traced-function")))
  (is (thrown? clojure.lang.ExceptionInfo (flowstorm/var-symbol "no.such.namespace/missing"))))

(deftest optional-flowstorm-recording
  (when (io/resource "flow_storm/api.clj")
    (let [target "com.phronemophobic.easel.flowstorm-test/traced-function"]
      (try
        (flowstorm/perform! :start nil)
        ((flowstorm/api 'flow-storm.api/start-recording))
        (is (:recording? (flowstorm/perform! :refresh nil)))
        ((flowstorm/api 'flow-storm.api/stop-recording))
        (is (false? (:recording? (flowstorm/perform! :refresh nil))))
        (flowstorm/perform! :record nil)
        (flowstorm/perform! :instrument target)
        (is (= 16 (traced-function 3)))
        (let [threads ((flowstorm/api 'flow-storm.runtime.indexes.api/all-threads))
              [flow-id thread-id] (first threads)
              timeline (fn [] ((flowstorm/api 'flow-storm.runtime.indexes.api/get-full-maps-timeline)
                               flow-id thread-id))
              recorded (count (timeline))]
          (is (pos? recorded))
          (flowstorm/perform! :pause nil)
          (traced-function 4)
          (is (= recorded (count (timeline)))))
        (finally
          (flowstorm/perform! :stop nil)))
      (is (empty? (:instrumented @flowstorm/runtime-state)))
      (is (= 25 (traced-function 4))))))
