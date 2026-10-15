(ns com.phronemophobic.easel.splitpane-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.splitpane :as pane]))

(deftest constrained-layout
  (doseq [direction [:row :column]
          available [0 1 10 100]
          children [[{} {}]
                    [{:width 100 :height 100} {:width 200 :height 200} {}]
                    [{:flex.grow/width 0 :flex.grow/height 0} {}]
                    [{:width -10 :height -10} {:flex.grow/width -2 :flex.grow/height -2}]
                    [{:flex.grow/width 0 :flex.grow/height 0} {:flex.grow/width 0 :flex.grow/height 0}]]]
    (let [sizes (pane/child-sizes children direction available)]
      (is (every? #(<= 0 % available) sizes))
      (is (<= (reduce + sizes) available)))))

(deftest nested-layout
  (let [root {:id :root :width 500 :height 400 :direction :row
              :panes [{:id :a :direction :column :panes [{:id :b} {:id :c}]} {:id :d}]}
        layout (pane/layout-pane-nested root 20)
        leaves (pane/flatten-pane layout)]
    (is (= #{:b :c :d} (into #{} (map :id) leaves)))
    (is (every? #(and (<= 0 (:x %) 500) (<= 0 (:y %) 400)
                      (<= (+ (:x %) (:width %)) 500)
                      (<= (+ (:y %) (:height %)) 400)) leaves))
    (is (= root (pane/isolate-pane root :root)))
    (is (= root (pane/isolate-pane root :missing)))
    (is (= [:a] (mapv :id (:panes (pane/isolate-pane root :a)))))
    (let [relative (pane/relative-sizes root layout)
          larger (pane/layout-pane-nested (assoc relative :width 1000) 20)]
      (is (= [500 500] (mapv :width (:panes larger)))))))

(deftest applet-visibility
  (let [root (-> (easel/make-easel)
                 (assoc :$ref '(keypath :easel))
                 (model/-resize [600 400] [1 1]))
        with-applet (model/-add-applet root {:make-applet (fn [_]
                                                            (applet/tool-applet "Test" identity {}))})
        id (:id (first (model/-visible-applets with-applet)))
        hidden (model/-hide-applet with-applet id)
        shown (model/-show-applet hidden id)]
    (is (some? id))
    (is (empty? (model/-visible-applets hidden)))
    (is (= 1 (count (model/-visible-applets shown))))
    (is (= shown (model/-show-applet shown id)))
    (is (= shown (model/-show-applet shown :missing)))
    (is (= [600 400] (:size (easel/remove-all-widgets shown))))))

(deftest unique-applet-placement
  (let [root {:id :root :panes [{:id :a :applet-id 1} {:id :b :applet-id 2}]}
        moved (pane/assign-applet root :b 1)]
    (is (nil? (:applet-id (pane/find-pane-by-id moved :a))))
    (is (= 1 (:applet-id (pane/find-pane-by-id moved :b))))
    (is (= root (pane/assign-applet root :missing 1)))
    (is (= root (pane/assign-applet root :root 1)))))
