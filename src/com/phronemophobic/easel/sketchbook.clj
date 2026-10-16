(ns com.phronemophobic.easel.sketchbook
  (:require [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.evaluation :as evaluation]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.viscous :as viscous]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(def source
  "(defonce scene*\n  (atom {:radius 96 :count 24}))\n\n(defn points\n  \"Return points around a circle.\"\n  [{:keys [radius count]}]\n  (for [i (range count)\n        :let [a (* 2 Math/PI (/ i count))]]\n    [(* radius (Math/cos a))\n     (* radius (Math/sin a))]))\n\n(points @scene*)\n")

(defn session
  ([] (session 'sketchbook.orbit))
  ([ns-sym]
   (let [ns (or (find-ns ns-sym)
                (let [ns (create-ns ns-sym)]
                  (binding [*ns* ns] (refer 'clojure.core)) ns))]
     (when-not (ns-resolve ns 'scene*)
       (when-let [error (:exception (evaluation/evaluate source {:ns ns :file "orbit.clj"}))]
         (throw error)))
     (when-let [v (ns-resolve ns 'points)]
       (alter-meta! v assoc :easel/source source
                    :easel/examples ["(count (points @scene*))"]
                    :easel/see-also '[com.phronemophobic.easel.model/IUI :applets]))
     {:ns ns-sym :source* @(ns-resolve ns 'scene*)
      :function (ns-resolve ns 'points)
      :result (viscous/wrap ((ns-resolve ns 'points) @@(ns-resolve ns 'scene*)))
      :source (str "(ns " ns-sym ")\n\n" source)})))

(defn scene-value [source*]
  (let [{:keys [radius count] :as value} @source*]
    (when-not (and (map? value) (number? radius) (Double/isFinite (double radius))
                   (<= 0 radius 100000) (integer? count) (<= 1 count 4096))
      (throw (ex-info "Scene needs a finite :radius and an integer :count from 1 to 4096" {})))
    value))

(defn scene-points [scene]
  (or (:points scene)
      (mapv (fn [i] (let [a (* 2 Math/PI (/ i (:count scene)))]
                 [(* (:radius scene) (Math/cos a)) (* (:radius scene) (Math/sin a))]))
        (range (:count scene)))))

(defn- subscribe [handler $state source* points-var]
  (let [key (random-uuid)
        update! (fn []
                  (try (let [scene (scene-value source*)
                             points (vec (take 4097 (if points-var (points-var scene) (scene-points scene))))]
                         (when-not (and (<= (count points) 4096)
                                        (every? #(and (sequential? %) (= 2 (bounded-count 3 %))
                                                       (every? (fn [n] (and (number? n) (Double/isFinite (double n)))) %)) points))
                           (throw (ex-info "points must return at most 4096 finite [x y] pairs" {})))
                         (handler :update $state assoc :scene (assoc scene :points points) :error nil))
                       (catch Throwable e (handler :update $state assoc :error (applet/error-message e)))))]
    (add-watch source* key (fn [_ _ _ _] (update!)))
    (when points-var (add-watch points-var key (fn [_ _ _ _] (update!))))
    (update!)
    #(do (remove-watch source* key) (when points-var (remove-watch points-var key)))))

(defn- dot [context x y radius color]
  (ui/translate (- x radius) (- y radius)
                (ui/with-color (theme/token context color)
                  (ui/with-style ::ui/style-fill
                    (ui/rounded-rectangle (* 2 radius) (* 2 radius) radius)))))

(defn- line [context color points]
  (ui/with-color (theme/token context color)
    (ui/with-style ::ui/style-stroke (apply ui/path points))))

(defeffect ::select-point [$state index]
  (when-let [source* (some-> (dispatch! :get $state) :source* deref)]
    (swap! source* assoc :selected index)))

(defeffect ::set-radius [$state value]
  (when-let [source* (some-> (dispatch! :get $state) :source* deref)]
    (swap! source* assoc :radius (max 0 (min 300 value)))))

(defui plot [{:keys [scene width height mode yaw tilt on-select]}]
  (let [w (max 0 width) h (max 0 height)
        cx (/ w 2) cy (/ h 2)
        radius (:radius scene 96)
        scale (/ (* 0.47 (min w (if (= mode :3d) (/ h 0.6) h))) 120)
        project (fn [[x y]]
                  (let [[x y] (if (= mode :3d)
                                [(+ (* x (Math/cos (or yaw 0.3))) (* y (Math/sin (or yaw 0.3))))
                                 (* (Math/sin (or tilt 0.35))
                                    (- (* y (Math/cos (or yaw 0.3))) (* x (Math/sin (or yaw 0.3)))))]
                                [x y])]
                    [(+ cx (* scale x)) (- cy (* scale y))]))
        points (scene-points scene)
        positions (mapv project points)
        selected (:selected scene 0)
        extent (max 130 (* radius 1.15))
        grid (vec (for [n (range -120 121 20)
                   pair [[[-120 n] [120 n]] [[n -120] [n 120]]]]
               (line context :color/hover (map project pair))))
        ring (when (seq points) (map project (conj points (first points))))]
    (ui/fixed-bounds [w h]
     [(eui/surface context w h :color/canvas)
      grid
      (line context :color/border [(project [(- extent) 0]) (project [extent 0])])
      (line context :color/border [(project [0 (- extent)]) (project [0 extent])])
      (when ring (line context :color/border ring))
      (when (= mode :3d)
        (line context :color/muted [[cx cy] [cx (max 8 (- cy (* scale 110)))]]))
      (ui/translate (- w 22) (- cy 12) (eui/text {:text "x" :size 11 :color :color/muted}))
      (ui/translate (+ cx 8) 8 (eui/text {:text (if (= mode :3d) "z" "y") :size 11 :color :color/muted}))
      (vec (for [[index [x y]] (map-indexed vector positions)]
        (ui/translate (- x 8) (- y 8)
          (ui/on :mouse-down (fn [_] (when on-select (on-select index)))
                 (ui/fixed-bounds [16 16]
                   (dot context 8 8 (if (= index selected) 5 3.5)
                        (if (= index selected) :color/accent :color/text)))))))])))

(defui orbit-controls [{:keys [state]}]
  (let [mode (:mode state :2d)]
    (eui/row nil
                       [(eui/button {:text "2D" :variant :text :selected? (= mode :2d) :height 28
                                      :on-click #(vector [:set [$state '(keypath :mode)] :2d])})
                        (eui/button {:text "3D" :variant :text :selected? (= mode :3d) :height 28
                                      :on-click #(vector [:set [$state '(keypath :mode)] :3d])})
                        (ui/spacer 12 0)
                        (eui/text {:text "●" :color :color/live :size 11})
                        (eui/text {:text "Live" :size 12 :color :color/secondary})])))

(defui orbit-header [{:keys [applet width]}]
  (when (> width 160) (orbit-controls {:state (:state applet)})))

(defui interactive-plot [{:keys [state scene width height mode]}]
  (let [view (plot {:scene scene :width width :height height :mode mode
                   :yaw (:yaw state 0.3) :tilt (:tilt state 0.35)
                   :on-select #(vector [::select-point $state %])})]
    (if (= mode :3d)
      (ui/wrap-on
       :mouse-down (fn [handler [x y :as pos]]
                     (or (seq (handler pos))
                         [[:set [$state '(keypath :camera-drag)] [x y (:yaw state 0.3) (:tilt state 0.35)]]]))
       :mouse-up (fn [handler pos] (concat (handler pos) [[:set [$state '(keypath :camera-drag)] nil]]))
       :mouse-move (fn [handler [x y :as pos]]
                     (if-let [[dx dy yaw tilt] (:camera-drag state)]
                       [[:update $state assoc :yaw (+ yaw (* 0.01 (- x dx)))
                         :tilt (max -1.4 (min 1.4 (+ tilt (* 0.01 (- y dy)))))]]
                       (handler pos))) view)
      view)))

(defui orbit-view [{:keys [state size]}]
  (let [[w h] size
        mode (:mode state :2d)
        scene (:scene state {:radius 96 :count 24})
        source* @(:source* state)
        bar (when (< w 360) (ui/padding 16 6 (orbit-controls {:state state})))
        remaining (max 0 (- h (ui/height bar) 30))
        primary (* remaining 0.66)
        secondary (- remaining primary)]
    (dnd/on-drop
     (fn [_ value]
       (when-let [wrapped (:x value)]
         (let [value @wrapped]
           (when (and (map? value) (:radius value) (:count value))
             (scene-value (atom value))
             [[:com.phronemophobic.easel/dispatch-fn #(reset! source* value)]]))))
     (if-let [error (:error state)]
       (ui/padding 20 20 (eui/text {:text error :width (max 1 (- w 40)) :color :color/error}))
       (ui/vertical-layout
        bar
        (interactive-plot {:state state :scene scene :width w :height primary :mode mode})
        (eui/rule context w)
        (interactive-plot {:state state :scene scene :width w :height secondary :mode (if (= mode :2d) :3d :2d)})
        (ui/padding 16 4
          (ui/on :mouse-down
            (fn [_] [[::dnd/drag-start {::dnd/obj {:x (viscous/wrap @source*)}}]])
            (eui/text {:text (str (:count scene) " points   r = " (:radius scene) "     ◇ scene*")
                        :size 11 :color :color/secondary}))))))))

(defui radius-slider [{:keys [value width dragging? on-change]}]
  (let [w (max 1 width)
        change (fn [[x _]] (when on-change (on-change (Math/round (double (max 0 (min 300 (* 300 (/ x w)))))))))]
    (ui/on :mouse-down (fn [pos] (cons [:set $dragging? true] (change pos)))
           :mouse-up (fn [pos] (cons [:set $dragging? false] (when dragging? (change pos))))
           :mouse-move (fn [pos] (when dragging? (change pos)))
      (ui/fixed-bounds [w 24]
        [(ui/translate 0 10 (eui/surface context w 4 :color/border 2))
         (ui/translate 0 10 (eui/surface context (* w (/ (min 300 value) 300)) 4 :color/accent 2))
         (dot context (* w (/ (min 300 value) 300)) 12 6 :color/accent)]))))

(defui scene-inspector-view [{:keys [state size]}]
  (let [[w h] size
        scene (:scene state {:radius 96 :count 24})
        points (scene-points scene)
        selected (if (seq points) (mod (:selected scene 0) (count points)) 0)
        source* @(:source* state)
        body (ui/vertical-layout
               (eui/row (max 0 (- w 40))
                        [(eui/text {:text "scene*" :role :code :weight :semi-bold})
                         (ui/spacer (max 0 (- w 190)) 0)
                         (eui/text {:text "Atom" :color :color/muted :size 11})])
               (ui/spacer 0 12)
               (eui/text {:text (str "▾ {:radius " (:radius scene) ", :count " (:count scene) ", …}") :role :code :size 12})
               (ui/spacer 0 12)
               (eui/row nil [(ui/fixed-bounds [110 26] (eui/text {:text "  :radius" :role :code :color :color/accent}))
                            (ui/fixed-bounds [54 26] (eui/text {:text (str (:radius scene)) :role :code}))
                            (radius-slider {:value (:radius scene) :width (max 1 (- w 225))
                                             :on-change #(vector [::set-radius $state %])})])
               (ui/spacer 0 8)
               (eui/text {:text (str "  :count       " (:count scene)) :role :code :color :color/accent})
               (ui/spacer 0 8) (eui/rule context (max 0 (- w 40))) (ui/spacer 0 8)
               (eui/text {:text (str "▸ :points      [" (count points) "]") :role :code})
               (ui/spacer 0 8) (eui/rule context (max 0 (- w 40))) (ui/spacer 0 8)
               (eui/text {:text (str "▾ :points / " selected) :role :code :color :color/accent})
               (ui/spacer 0 8)
               (eui/text {:text (str "     0    " (some-> (get-in points [selected 0]) double str)) :role :code :size 12})
               (eui/text {:text (str "     1    " (some-> (get-in points [selected 1]) double str)) :role :code :size 12})
               (ui/spacer 0 8) (eui/rule context (max 0 (- w 40)))
               (eui/button {:text "Meta   nil" :variant :link :size :small
                            :on-click #(vector [:com.phronemophobic.easel/add-applet
                                                {:make-applet (fn [handler] (inspector/inspector-applet handler (meta source*)))}])}))]
    (basic/scrollview {:scroll-bounds [(max 0 w) (max 0 h)] :$body nil :body (ui/padding 20 16 body)})))

(defn orbit-applet [handler source* & [points-var]]
  (assoc (applet/tool-applet "Orbit preview" #'orbit-view
                             {:source* (viscous/wrap source*) :scene (scene-value source*) :mode :2d}
                             #(subscribe handler % source* points-var))
         :icon "experiment" :header #'orbit-header :description "Live 2D and 3D values"))

(defn scene-inspector-applet [handler source* & [points-var]]
  (assoc (applet/tool-applet "Inspector" #'scene-inspector-view
                             {:source* (viscous/wrap source*) :scene (scene-value source*)}
                             #(subscribe handler % source* points-var))
         :icon "database"))
