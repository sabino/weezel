(ns com.phronemophobic.easel.evaluation-test
  (:require [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel.clobber :as editor]
            [com.phronemophobic.easel.evaluation :as evaluation]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.clobber.modes.clojure.ui :as cui]
            [com.phronemophobic.clobber.modes.text :as text]
            [membrane.component :as component]))

(deftest evaluation-results-and-output
  (is (= false (:value (evaluation/evaluate "false" {:ns *ns*}))))
  (is (contains? (evaluation/evaluate "nil" {:ns *ns*}) :value))
  (let [output (with-out-str
                 (is (= 6 (:value (evaluation/evaluate "(println :hello) (+ 1 2 3)" {:ns *ns*})))))]
    (is (= ":hello\n" output))
    (is (= ":hello" (:message (peek (events/recent :log))))))
  (let [result (evaluation/evaluate "(throw (ex-info \"evaluation failed\" {:answer 42}))"
                                     {:ns *ns* :file "example.clj" :line 10})]
    (is (instance? Throwable (:exception result)))
    (is (= (:exception result) (:throwable (peek (events/recent :exception)))))))

(deftest editor-source-ranges
  (let [source "(def a 1)\n\n(+ a 2)"
        e (-> (cui/make-editor {:source source :ns *ns*})
              (text/editor-goto-line 2))
        top (editor/evaluation-source e :top)]
    (is (= source (:source (editor/evaluation-source e :buffer))))
    (is (= "(+ a 2)" (:source top)))
    (is (= 3 (:line top)))
    (is (= 2 (:result-line top)))))

(deftest resize-before-editor-load
  (let [state (atom {}) handler (component/default-handler state)
        applet (-> (editor/clobber-applet handler {:source "1\n2\n3" :line 1})
                   (model/-start '(keypath :applet) [300 200] [1 1])
                   (model/-resize [150 100] [1 1]))]
    (reset! state {:applet applet})
    (editor/load-editor handler '(keypath :applet) (:editor-info applet) [300 200])
    (is (= 150 (get-in @state [:applet :state :editor :width])))
    (is (= 100 (get-in @state [:applet :state :editor :height])))
    (is (zero? (get-in @state [:applet :state :editor :cursor :row])))
    (model/-stop applet)))
