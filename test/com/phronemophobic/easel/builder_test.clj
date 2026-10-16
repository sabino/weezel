(ns com.phronemophobic.easel.builder-test
  (:require [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.composition :as composition]
            [com.phronemophobic.easel.composition-test :as fixtures]
            [com.phronemophobic.easel.help-test :as helpers]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.package-ui :as manager]
            [com.phronemophobic.easel.schematic2 :as builder]
            [com.phronemophobic.easel.shell :as shell]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.schematic.model :as sm]
            [membrane.component :as component]
            [membrane.ui :as ui]))

(defn frames
  ([view predicate] (frames view predicate [0 0]))
  ([view predicate [x y]]
   (let [[ox oy] (ui/origin view) x (+ x ox) y (+ y oy)
         [dx dy] (cond (instance? membrane.ui.Padding view) [(:left view) (:top view)]
                       (instance? membrane.ui.ScrollView view) (:offset view)
                       :else [0 0])]
     (into (if (predicate view) [{:view view :x x :y y :size (ui/bounds view)}] [])
           (mapcat #(frames % predicate [(+ x dx) (+ y dy)])) (ui/children view)))))

(defn drag [app source target]
  (ui/mouse-down (app) source)
  (ui/mouse-move (app) (mapv + source [12 12]))
  (ui/mouse-move (app) target)
  (ui/mouse-move-global (app) target)
  (ui/mouse-up (app) target))

(deftest existing-ui-tools-open-as-a-bundled-workspace-and-share-live-data
  (fixtures/isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [registry (packages/catalog)]
       (doseq [id [:easel/ui-builder :easel/ui-components :easel/ui-preview :easel/ui-tree :easel/ui-properties]]
         (is (packages/installed? (get registry id))))
       (manager/open-package! easel/handler (get registry :easel/ui-builder)))
     (is (helpers/await-applets))
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)
           original (:easel @easel/app-state)
           canvas-id (some (fn [[id a]] (when (= :easel/ui-preview (:package/id a)) id)) (:applets original))
           pane (some #(when (= canvas-id (:applet-id %)) %) (get-in original [::easel/cached-layout :all-panes]))
           g (shell/geometry (:viewport original) (:appearance original) original (composition/current original))
           target [(+ (:content-x g) (:x pane) 100)
                   (+ (:content-y g) (:y pane) (theme/token :metric/pane-header) 142)]
           button (first (frames (app) #(= "button" (:paragraph %))))
           root #(get-in @easel/app-state [:membrane.component/context ::builder/elem])]
       (is (some? button))
       (drag app [(+ (:x button) 5) (+ (:y button) 5)] target)
       (is (= ::sm/relative-layout (:element/type (root))))
       (is (= [::sm/button] (mapv :element/type (:element/children (root)))))
       (is (= #{(-> (root) :element/children first :element/id)}
              (get-in @easel/app-state [:membrane.component/context ::builder/selection])))
       (is (nil? (get-in @easel/app-state [:membrane.component/context :drop-object])))
       (easel/handler :set [:membrane.component/context ::builder/elem :element/children 0 :element/text :element/code] "Live button")
       (is (some #(= "Live button" (:paragraph %)) (helpers/nodes (app))))
       (let [ns-symbol (symbol (str "easel.test.canvas-" (random-uuid)))]
         (try
           (let [view (binding [*ns* (create-ns ns-symbol)]
                        (refer 'clojure.core)
                        (eval (builder/source-form (root) [640 480])))]
             (is (some #(= "Live button" (:paragraph %)) (helpers/nodes view))))
           (finally (remove-ns ns-symbol))))
       (doseq [id [:easel/studio-light :easel/studio-dark]]
         (packages/select-theme! easel/handler id false)
         (let [view (app)
               palette-label (some #(when (= "button" (:paragraph %)) %) (helpers/nodes view))]
           (is (every? number? (ui/bounds view)))
           (is (= (theme/token {::theme/value (get-in @easel/app-state [:easel :appearance])} :color/text)
                  (get-in palette-label [:paragraph-style :paragraph-style/text-style :text-style/color])))
           (is (not-any? #(instance? membrane.ui.Label %) (helpers/nodes view)))))
       (let [source (first (frames (app) #(= "Source" (:paragraph %))))]
         (is (some? source))
         (ui/mouse-down (app) [(+ (:x source) 4) (+ (:y source) 4)])
         (is (helpers/await-applets))
         (is (some #(and (= "ui.clj" (:label %))
                         (.contains (str (get-in % [:state :editor :rope])) "Live button"))
                   (vals (get-in @easel/app-state [:easel :applets])))))))))

(deftest canvas-placement-is-data-and-moving-an-element-keeps-its-identity
  (let [a {:element/id (random-uuid) :element/type ::sm/checkbox}
        b {:element/id (random-uuid) :element/type ::sm/button}
        root (-> nil (builder/place-element a [20 30]) (builder/place-element b [80 90]))
        moved (builder/place-element root a [100 120])]
    (is (= 2 (count (:element/children moved))))
    (is (= #{(:element/id a) (:element/id b)} (set (map :element/id (:element/children moved)))))
    (is (= {:element/x 100 :element/y 120} (get-in moved [:relative/layout (:element/id a)])))
    (is (= {:element/x 20 :element/y 30} (get-in root [:relative/layout (:element/id a)])))))
