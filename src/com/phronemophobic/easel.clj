(ns com.phronemophobic.easel
  (:require [membrane.skia :as skia]
            [clojure.core.async :as async]
            [membrane.skia.paragraph :as para]
            [membrane.ui :as ui]
            [membrane.alpha.stretch :as stretch]
            [tiara.data :as tiara]
            [com.rpl.specter :as specter]
            [clojure.java.io :as io]
            [clojure.zip :as z]
            [com.phronemophobic.easel.list-applets :as
             list-applets]
            [com.phronemophobic.easel.splitpane :as splitpane]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.composition :as composition]
            [com.phronemophobic.easel.keymap :as keymap]
            [com.phronemophobic.easel.commands :as command]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.easel.shell :as shell]
            [com.phronemophobic.easel.help :as help]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon.ui :as icon.ui]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.events :as events]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [com.phronemophobic.schematic.view.util
             :as schematic-util]
            [membrane.basic-components :as basic]
            [membrane.component
             :refer
             [defui defeffect]])
  (:import (java.util.concurrent
            ExecutorService
            Executors)))

(defonce live-windows (atom 0))
(defonce window-ready? (atom false))
(defn repaint! [] (when @window-ready? (#'skia/glfw-post-empty-event)))

(defn native-scroll [window _handle dx dy]
  (ui/scroll @(:ui window) [(* -2 dx) (* -2 dy)] @(:mouse-position window))
  (skia/repaint! window))

(defonce app-state
  (atom nil))

(defn in-runtime [f]
  (if-let [run (resolve 'com.phronemophobic.easel.packages/with-runtime)] (run f) (f)))

(defn runtime-view [view]
  (let [event (fn [handler & args] (in-runtime #(apply handler args)))]
    (ui/wrap-on :mouse-event event :mouse-move event :mouse-move-global event
                :key-event event :key-press event :scroll event view)))

(def handler
  (let [dispatch (membrane.component/default-handler app-state)]
    (fn [& args] (in-runtime #(apply dispatch args)))))
(def ^java.util.Queue main-queue
  (java.util.concurrent.ConcurrentLinkedQueue.))
(defeffect :repaint! []
  ;; needs debounce
  (repaint!)

  )

(defeffect :watch [key $ref f]
  (let [path (membrane.component/path->spec $ref)]
    (add-watch app-state key
               (fn [key ref old new]
                 (let [new-val (specter/select-one path new)
                       old-val (specter/select-one path old)]
                   (when (not= new-val old-val)
                     (f key ref old-val new-val)))))
    #(remove-watch app-state key)))



(defonce app-starter-executor
  (delay
    (Executors/newSingleThreadExecutor
     (reify java.util.concurrent.ThreadFactory
       (newThread [_ runnable]
         (doto (Thread. runnable "easel-applets") (.setDaemon true)))))))

(defonce queue-scheduled? (atom false))

(def ^:private QUEUES-PATH
  (specter/path
   [:easel
    (specter/multi-path
     [:applets specter/MAP-VALS (specter/must ::model/queue)]
     (specter/must ::queue))]))

(defn purge-queue [state]
  (specter/setval
   QUEUES-PATH
   specter/NONE
   state))

(defn ^:private run-queue []
  (let [[old new] (swap-vals! app-state purge-queue)
        queue (concat
               (for [[id applet] (get-in old [:easel :applets]) work (::model/queue applet)]
                 [id (::instance-token applet) work])
               (for [work (get-in old [:easel ::queue])] [nil nil work]))]
    (run! (fn [[id token work]]
            (try
              (in-runtime work)
              (catch Throwable e
                (events/report! e)
                (when id
                  (swap! app-state
                         #(if (and (get-in % [:easel :applets id])
                                   (= token (get-in % [:easel :applets id ::instance-token])))
                            (assoc-in % [:easel :applets id ::applet/error] e)
                            %))))))
          queue)))

(defn- schedule-queue []
  (when (compare-and-set! queue-scheduled? false true)
    (.submit ^ExecutorService @app-starter-executor
             ^Runnable
             (fn []
               (try
                 (run-queue)
                 (finally
                   (reset! queue-scheduled? false)
                   (when (seq (specter/select [QUEUES-PATH specter/ALL] @app-state))
                     (schedule-queue))))))))

(add-watch app-state ::applet-queue
           (fn [key ref old new]
             (let [has-queue? (not= specter/NONE (specter/select-any QUEUES-PATH new))]
               (when has-queue?
                 (schedule-queue)))))

(defeffect ::add-applet [m]
  (dispatch!
   :update :easel
   (fn [easel]
     (model/-add-applet easel m)))
  nil)

(defn add-component! [key f]
  (handler :update
           '[(keypath :membrane.component/context)
             (keypath :com.phronemophobic.easel.schematic2/component-picker-components)]
           (fn [components]
             (assoc components key f)))
  nil)

(defeffect ::add-component! [key f]
  (add-component! key f))

(defeffect ::get-applets []
  (dispatch! :get (specter/path :easel :applets)))

(defn subscribe-applets!
  "Observe (project easel), immediately and when its value changes.
  Return an idempotent cleanup function. Projections should exclude the
  observer's changing state; callbacks may dispatch updates to that state."
  [project callback]
  (let [key (random-uuid)
        active? (atom true)
        previous (volatile! (Object.))
        pending (java.util.concurrent.atomic.AtomicInteger. 0)
        stop #(do (reset! active? false) (remove-watch app-state key))
        notify (fn []
                 ;; One drain per observer, without holding a lock across dispatch.
                 ;; Reentrant and concurrent notifications coalesce to current state.
                 (when (zero? (.getAndIncrement pending))
                   (loop [processed 1]
                     (when @active?
                       (try
                         (let [value (project (:easel @app-state))]
                           (when (not= value @previous)
                             (vreset! previous value)
                             (callback value)))
                         (catch Throwable e
                           (stop)
                           (events/report! e))))
                     (let [remaining (.addAndGet pending (- processed))]
                       (when (pos? remaining) (recur remaining))))))]
    (add-watch app-state key (fn [_ _ _ _] (notify)))
    (notify)
    stop))

(defeffect ::subscribe-applets [project callback]
  (subscribe-applets! project callback))

(defeffect ::get-root-pane []
  (dispatch! :get (specter/path :easel :root-pane)))

(defrecord ComponentApplet [label component-var initial-state]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [component-meta (meta component-var)
          arglists (:arglists component-meta)
          first-arglist (first arglists)
          arg-map (first first-arglist)
          args (:keys arg-map)
          $args (into {}
                      (comp
                       (remove '#{extra context})
                       (map (fn [arg]
                              (let [kw (keyword (name arg))
                                    $kw (keyword (str "$" (name arg)))]
                                [$kw [$ref '(keypath :state) (list 'keypath kw)]]))))
                      args)
          state (into (assoc initial-state
                             :extra {}
                             :$extra [$ref '(keypath :state) '(keypath :extra)])
                      $args)

          $scroll-state [$ref '(keypath :scroll-state)]]
      (assoc this
             ;; :dispatch! dispatch!
             :$ref $ref
             :state state
             :scroll-state {:$extra [$ref '(keypath :scroll-state :extra)]
                            :extra {}
                            :offset [0 0]
                            :$offset [$scroll-state '(keypath :offset)]}
             :size size)))
  (-stop [this])
  model/IUI
  (-ui [this $context context]
    (let [context (assoc context
                         :membrane.stretch/container-size
                         (:size this))
          ui (component-var
              (assoc (:state this)
                     :context context
                     :$context $context))

          scroll-state (:scroll-state this)
          $ref (:$ref this)

          [cw ch] (:size this)
          scroll-bounds [(max 0 (- cw 7 4))
                         (max 0 (- ch 7 4))]
          ui (basic/scrollview
              (assoc scroll-state
                     :body ui
                     :context context
                     :$context $context
                     :scroll-bounds scroll-bounds))]
      (ui/translate 4 4 ui)))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn add-component-as-applet
  "Open a Membrane defui var as an applet, with writable paths for its arguments.
  Redefining the var updates its view without replacing the applet's state."
  [component-var initial-state]
  (handler :com.phronemophobic.easel/add-applet
           {:make-applet
            (fn [_]
              (map->ComponentApplet
               {:label (or (-> component-var
                               meta
                               :name)
                           "Component")
                :component-var component-var
                :initial-state initial-state}))}))
(defeffect ::add-component-as-applet [component-var initial-state]
  (add-component-as-applet component-var initial-state))

(defn on-main-callback []
  (loop []
    (when-let [workf (.poll main-queue)]
      (try
        (workf)
        (catch Throwable e
          (events/report! e)
          (prn e)))
      (recur))))

(defeffect :dispatch-main [f]
  (.add main-queue f)
  (repaint!))


(defn col-width [easel visible-count]
  (let [[w h] (:size easel)
        col-width (if (pos? visible-count)
                    (quot w visible-count)
                    w)]
    col-width))


(defui top-bar [{:keys [pane]}]
  (let [drop-object (:drop-object context)
        pane-id (:id pane)
        bar
        (ui/horizontal-layout
         (ui/on-click
          (fn []
            [[::toggle-pane-direction {:pane-id pane-id}]])
          (if (= (:direction pane) :column)
            (icon.ui/icon {:name "column-height"})
            (icon.ui/icon {:name "column-width"})))
         (ui/on-click
          (fn []
            [[::splitpane {:pane-id pane-id}]])
          (icon.ui/icon {:name "plus"}))
         (when (not= ::root-pane pane-id)
           (ui/on-click
            (fn []
              [[::hide-pane {:pane-id pane-id}]])
            (icon.ui/icon {:name "close"})))
         (ui/on-click
            (fn []
              [[::clear-pane {:pane-id pane-id}]])
            (icon.ui/icon {:name "minus-square"}))
         (ant/button {:text "Balance" :size :small
                      :on-click #(vector [::balance-panes {:pane-id pane-id}])})
         (when (= ::root-pane pane-id)
           (ui/on-click
            (fn []
              [[::toggle-pane-resize {:pane-id pane-id}]])
            (icon.ui/icon {:name "edit"}))))
        height (ui/height bar)

        drag-object (get extra ::drag-object)]
    [(ui/on
      :mouse-down
      (fn [_]
        (when (empty? (:panes pane))
          [[::dnd/drag-start {::dnd/obj {::pane pane}}]]))
      (schematic-util/on-drag-hover
       {:$body nil
        :extra (get extra ::drag-extra)
        :body
        [(when (and (empty? (:panes pane))
                    (::pane drag-object)
                    drop-object)
           (dnd/on-drop
            (fn [_pos obj]
              (when-let [drop-pane (::pane obj)]
                [[::swap-panes {:from-pane-id (:id drop-pane)
                                :to-pane-id (:id pane)}]]))
            (ui/filled-rectangle
             [0.8 0.8 0.8]
             (:width pane) height)))
         (ui/with-style ::ui/style-stroke
           (ui/with-color [0.33 0.33 0.33]
            (ui/rectangle (:width pane) height)))]
        :object drag-object}))
     bar]))

(def top-bar-height (theme/token :metric/pane-header))

(defn relayout* [easel]
  (let [root-pane (:root-pane easel)]
    (model/-resize easel [(:width root-pane) (:height root-pane)] (:content-scale easel))))

(defeffect ::toggle-pane-direction [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             #(splitpane/toggle-direction %))
          relayout*))))

(defeffect ::balance-panes [{:keys [$easel pane-id]}]
  (dispatch! :update $easel
             #(-> %
                  (update :root-pane splitpane/edit-pane-by-id pane-id splitpane/balance-pane)
                  relayout*)))

(defeffect ::toggle-pane-resize [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             #(update % ::pane-resize not))))))

(defn ^:private zfind
  "Finds first loc that matches pred. Returns nil if no match found."
  [loc pred]
  (loop [loc loc]
    (if (z/end? loc)
      nil
      (if (pred (z/node loc))
        loc
        (recur (z/next loc))))))


;; TODO: move more pane code into splitpane namespace.
(defeffect ::begin-resize-drag [{:keys [$easel from-pane to-pane mpos] :as m}]
  ;; resizing a pane may convert relative stretch size to
  ;; absolute size, which may overconstrain a pane
  ;; TODO: check invariants for over constraining.
  (assert (or from-pane to-pane))
  ;; check root-pane-layout in cached layout
  ;; make sure :width or :height is set on from-pane and to-pane
  ;; call relayout*
  (let [setup-drag
        (fn [easel]
          (let [root-pane-layout (-> easel ::cached-layout :root-pane-layout)
                _ (assert root-pane-layout)

                pane-ids (into #{} (remove nil?) [from-pane to-pane])
                pane-layouts-by-id
                (into {}
                      (map (fn [pane]
                             [(:id pane) pane]))
                      (specter/select [splitpane/WALK-PANE
                                       (fn [pane]
                                         (contains? pane-ids (:id pane)))]
                                      root-pane-layout))

                ;; determine direction
                zpane (zfind (splitpane/pane-zip root-pane-layout)
                             #(contains? pane-ids (:id %)))
                _ (assert (and zpane (z/up zpane)))
                direction (or (-> zpane z/up z/node :direction)
                              :row)

                ;; set explicit sizes on from, to panes
                easel (specter/transform
                       [:root-pane
                        splitpane/WALK-PANE
                        #(contains? pane-ids (:id %))]
                       (fn [pane]

                         (let [size (splitpane/get-size (get pane-layouts-by-id (:id pane))
                                                        direction)]
                           (splitpane/set-size pane direction size)))
                       easel)

                ;; store drag state
                coord (if (= direction :column)
                        (nth mpos 1)
                        (nth mpos 0))
                easel (update easel :root-pane
                              (fn [pane]
                                (assoc pane :drag-state
                                       (merge
                                        (select-keys m [:from-pane :to-pane])
                                        {:direction direction
                                         :start-coord coord}
                                        (when to-pane
                                          {:to-size (splitpane/get-size (get pane-layouts-by-id to-pane)
                                                                        direction)})
                                        (when from-pane
                                          {:from-size (splitpane/get-size (get pane-layouts-by-id from-pane)
                                                                          direction)})))))]
            easel))]
    (dispatch!
     :update $easel
     (fn [easel]
       (-> easel
           setup-drag
           relayout*)))))

(defeffect ::resize-drag [{:keys [$easel mpos]}]

  ;; set width and height on panes as appropriate
  ;; call relayout*
  (dispatch!
   :update $easel
   (fn [easel]
     (let [root-pane (:root-pane easel)
           {:keys [to-size from-size start-mpos to-pane from-pane direction start-coord]
            :as drag-state} (:drag-state root-pane)

           coord (if (= direction :column)
                   (nth mpos 1)
                   (nth mpos 0))
           delta (-> (- coord start-coord)
                     (max (if to-size (- to-size) Double/NEGATIVE_INFINITY))
                     (min (if from-size from-size Double/POSITIVE_INFINITY)))

           pane-ids (into #{} (remove nil?) [from-pane to-pane])
           easel
           (specter/transform
            [:root-pane
             splitpane/WALK-PANE
             #(contains? pane-ids (:id %))]
            (fn [pane]

              (cond
                (= from-pane (:id pane))
                (splitpane/set-size pane direction
                                    (- from-size
                                       delta))

                (= to-pane (:id pane))
                (splitpane/set-size pane direction
                                    (+ to-size
                                       delta))))
            easel)]
       (-> easel
           relayout*)))))

(defeffect ::end-resize-drag [{:keys [$easel from-pane to-pane]}]
  ;; resizing a pane may convert relative stretch size to
  ;; absolute size, which may overconstrain a pane
  ;; TODO: check invariants for over constraining.
  (dispatch!
   :update $easel
   (fn [easel]
     (let [id (get-in easel [:root-pane :drag-state :to-pane])
           loc (when id (zfind (splitpane/pane-zip (-> easel ::cached-layout :root-pane-layout))
                               #(= id (:id %))))
           parent (some-> loc z/up z/node)]
       (-> easel
           (update :root-pane
                   (fn [pane]
                     (cond-> (dissoc pane :drag-state)
                       parent (splitpane/edit-pane-by-id (:id parent)
                                                         #(splitpane/relative-sizes % parent)))))
           relayout*)))))

(defeffect ::splitpane [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (->> easel
          (specter/transform [:root-pane splitpane/WALK-PANE #(= pane-id (:id %))]
                             (fn [pane]
                               (if (empty? (:panes pane))
                                 (-> pane
                                     (assoc :applet-id nil)
                                     (splitpane/add-child {:id (random-uuid)
                                                           :applet-id (:applet-id pane)})
                                     (splitpane/add-child {:id (random-uuid)}))
                                 ;; pane already has children
                                 (splitpane/add-child pane {:id (random-uuid)}))))
          relayout*))))

(defeffect ::swap-panes [{:keys [$easel from-pane-id to-pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (-> easel
         (update :root-pane
                 (fn [root-pane]
                   (let [from-pane (splitpane/find-pane-by-id root-pane from-pane-id)
                         to-pane (splitpane/find-pane-by-id root-pane to-pane-id)]
                     (if (and from-pane to-pane
                              (empty? (:panes from-pane)) (empty? (:panes to-pane)))
                       (-> root-pane
                           (splitpane/edit-pane-by-id from-pane-id
                                                      (fn [pane]
                                                        (assoc pane :applet-id (:applet-id to-pane))))
                           (splitpane/edit-pane-by-id to-pane-id
                                                      (fn [pane]
                                                        (assoc pane :applet-id (:applet-id from-pane)))))
                       ;; else
                       root-pane))))
          relayout*))))

(defeffect ::set-pane-applet-id [{:keys [$easel pane-id applet-id] :as m}]
  (dispatch!
   :update $easel
   (fn [easel]
     (if (contains? (:applets easel) applet-id)
       (-> easel
           (update :root-pane splitpane/assign-applet pane-id applet-id)
           relayout*)
       easel))))



(defeffect ::delete-pane [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (let [pane (->> easel
                     ::cached-layout
                     :all-panes
                     (some (fn [pane]
                             (when (= pane-id (:id pane))
                               pane))))]
       (if-let [applet-id (:applet-id pane)]
         (model/-remove-applet easel applet-id)
         (-> easel
             (update :root-pane
                     #(splitpane/delete-pane-by-id % pane-id))
             relayout*))))))

(defeffect ::close-other-panes [{:keys [$easel pane-id]}]
  (dispatch! :update
             $easel
             (fn [easel]
               (-> easel
                   (update :root-pane splitpane/isolate-pane pane-id)
                   relayout*))))

(defeffect ::hide-pane [{:keys [$easel pane-id] :as m}]
  (dispatch!
   :update $easel
   (fn [easel]
     (-> easel
         (update :root-pane
                 #(splitpane/delete-pane-by-id % pane-id))
         relayout*))))

(defeffect ::clear-pane [{:keys [$easel pane-id]}]
  (dispatch!
   :update $easel
   (fn [easel]
     (-> easel
         (update :root-pane
                 (fn [root-pane]
                   (-> root-pane
                       (splitpane/edit-pane-by-id pane-id
                                                  (fn [pane]
                                                    (dissoc pane :applet-id))))))
         relayout*))))



(defui pane-resizer-node [{:keys [pane]}]
  (let [subpanes (:panes pane)
        direction (get pane :direction :row)]
    (when (seq subpanes)
      (ui/translate
       (:x pane 0)
       (:y pane 0)
       [(into []
              (map (fn [subpane]
                     (pane-resizer-node {:pane subpane})))
              subpanes)
        (into []
              (comp
               (map-indexed
                (fn [idx {:keys [x y width height] :as subpane}]
                  (let [hover? (get extra [::hover (:id subpane)])
                        fill-color [1 0 0 (if hover?
                                            0.4
                                            0.2)]
                        [x y w h] (case direction
                                    :row [(+ (:x subpane 0) (:width subpane)) (:y subpane 0)
                                          10 (:height subpane)]
                                    :column [(:x subpane 0 ) (+ (:y subpane 0)
                                                                (:height subpane))
                                             (:width subpane) 10])]
                    (ui/translate x y
                                  (basic/on-hover
                                   {:hover? hover?
                                    :$body nil
                                    :body
                                    (ui/on
                                     :mouse-down
                                     (fn [_]
                                       ;; Resize adjacent panes while preserving their combined size.
                                       [[::begin-resize-drag {:to-pane (:id subpane)
                                                              :from-pane (-> subpanes (nth (inc idx)) :id)}]])
                                     (ui/filled-rectangle fill-color
                                                          w h))}))))))
              (drop-last subpanes))]))))


(defui pane-resizer [{:keys [root-pane]}]
  (let [main-view (ui/fixed-bounds
                   [(:width root-pane) (:height root-pane)]
                   (pane-resizer-node {:pane root-pane}))
        main-view (if (:drag-state root-pane)
                    (ui/on
                     :mouse-move
                     (fn [mpos]
                       [[::resize-drag {:mpos mpos}]])
                     :mouse-up
                     (fn [_]
                       [[::end-resize-drag {:pane-id (:id root-pane)}]]
                       )
                     main-view)
                    ;; else, not resizing
                    (ui/wrap-on
                     :mouse-down
                     (fn [handler mpos]
                       (let [intents (handler mpos)]
                         (if (seq intents)
                           (eduction
                            (map (fn [intent]
                                   (if (= ::begin-resize-drag (first intent))
                                     [::begin-resize-drag (assoc (second intent)
                                                                 :mpos mpos)]
                                     intent)))
                            intents)
                           [[::toggle-pane-resize {:pane-id ::root-pane}]])))
                     main-view))]
    main-view))

(defn ^:private first-empty-pane-id [easel]
  (first
   (eduction
    (filter (fn [pane]
              (and (not (:applet-id pane))
                   (empty? (:panes pane)))))
    (map :id)
    (->> easel
         ::cached-layout
         :all-panes
         reverse))))

(declare command-environment commands)

(defui pane-header [{:keys [pane applet height]}]
  (let [w (:width pane) h (min (:height pane) (or height (theme/token context :metric/pane-header)))
        grip (ui/on :mouse-down
                    (fn [_] [[::dnd/drag-start {::dnd/obj {::pane pane}}]])
                    (eui/row nil [(eui/text {:text "⠿" :size 18 :color :color/muted})
                                  (eui/icon {:name (or (:icon applet) "file") :size 16})]))
        title (eui/text {:text (or (:label applet) "Empty pane") :weight :semi-bold})
        menu (eui/button {:text "⋯" :variant :text :width 32 :height 30
                           :on-click #(vector [::pane-menu (:id pane)])})
        controls (when-let [header (:header applet)]
                   (header {:applet applet :$applet $applet :width (max 0 (- w 240))
                            :context context :$context $context}))
        ns-label (when-let [ns (get-in applet [:state :editor :eval-ns])]
                   (eui/text {:text (str ns) :size 11 :color :color/secondary}))]
    (dnd/on-drop
     (fn [_ object]
       (when-let [from (::pane object)]
         [[::swap-panes {:from-pane-id (:id from) :to-pane-id (:id pane)}]]))
     (ui/fixed-bounds [w h]
       [(eui/surface context w h :color/header)
        (ui/translate 10 (/ (- h (ui/height grip)) 2) grip)
        (ui/translate 50 (/ (- h (ui/height title)) 2)
                      (ui/scissor-view [0 0] [(max 0 (- w 94)) (ui/height title)] title))
        (ui/translate (max 0 (- w 40)) (/ (- h 30) 2) menu)
        (when controls (ui/translate (max 180 (- w (ui/width controls) 54)) (/ (- h (ui/height controls)) 2) controls))
        (when (and ns-label (> w 350))
          (ui/translate 148 (/ (- h (ui/height ns-label)) 2)
                        (ui/scissor-view [0 0] [(- w 200) (ui/height ns-label)] ns-label)))
        (ui/translate 0 (dec h) (eui/rule context w))]))))

(defui pane-actions [{:keys [easel pane-id applet-id width]}]
  (let [environment (assoc (command-environment easel context) :pane-id pane-id :applet-id applet-id)
        registry (command/registry (composition/current easel) environment)]
    (apply ui/vertical-layout
      (for [id (get-in (composition/current easel) [:panes :actions])
            :let [action (get registry id)] :when action]
        (eui/button {:text (:name action) :width width :height 34 :variant :text
                     :disabled? (not (command/enabled? action environment))
                     :on-click #(vector [::invoke-command id pane-id])})))))

(defn easel-ui* [{:keys [applets] :as easel} $extra extra $context context]
  (let [descriptor (composition/current easel)
        header-height (composition/dimension (theme/current context) (get-in descriptor [:panes :header :height] 0))
        header-view (some-> (get-in descriptor [:panes :header :view]) requiring-resolve)
        panes (filter #(empty? (:panes %)) (-> easel ::cached-layout :all-panes))
        menu-pane (some #(when (= (:id %) (:pane-menu easel)) %) panes)]
    [(vec (for [pane panes
           :let [a (get applets (:applet-id pane))
                 w (:width pane) h (:height pane)
                 pane-extra (get extra (:id pane))
                 $pane-extra [$extra (list 'keypath (:id pane))]]]
       (ui/translate (:x pane) (:y pane)
         (ui/fixed-bounds [w h]
           [(eui/surface context w h)
            (when (and header-view (pos? header-height))
              (header-view {:pane pane :applet a :$applet [(:$ref a)] :height header-height
                            :extra pane-extra :$extra $pane-extra :context context :$context $context}))
            (ui/translate 0 header-height
              (ui/scissor-view [0 0] [w (max 0 (- h header-height))]
                (if a
                  (ui/on
                   ::add-applet (fn [m] [[::add-applet (assoc m :from-pane-id (:id pane))]])
                   ::hide-pane (fn [m] [[::hide-pane (assoc m :pane-id (:id pane))]])
                   ::delete-pane (fn [m] [[::delete-pane (assoc m :pane-id (:id pane))]])
                   ::close-other-panes (fn [m] [[::close-other-panes (assoc m :pane-id (:id pane))]])
                   (applet/safe-ui a $context context))
                  (ui/translate 20 20
                    (eui/button {:text "Add applet" :icon "plus"
                                 :context context :$context $context
                                 :extra (get pane-extra ::add-applet)
                                 :$extra [$pane-extra (list 'keypath ::add-applet)]
                                 :on-click #(vector [::add-to-pane (:id pane)]) })))))
            (eui/border context w h)]))))
     (when (::pane-resize (:root-pane easel))
       (pane-resizer {:root-pane (-> easel ::cached-layout :root-pane-layout)
                     :extra (get extra ::pane-resizer)
                     :$extra [$extra (list 'keypath ::pane-resizer)]
                     :context context :$context $context}))
     (when menu-pane
       (let [w (min 240 (first (:size easel)))
             menu (pane-actions {:easel easel :pane-id (:id menu-pane) :applet-id (:applet-id menu-pane)
                                 :extra (get extra ::pane-menu) :$extra [$extra (list 'keypath ::pane-menu)]
                                 :width w :context context :$context $context})
             h (min (second (:size easel)) (ui/height menu))
             x (max 0 (- (+ (:x menu-pane) (:width menu-pane)) w 8))
             y (max 0 (min (+ (:y menu-pane) header-height) (- (second (:size easel)) h)))]
         (ui/translate x y
           [(eui/surface context w h :color/surface 4)
            (basic/scrollview {:scroll-bounds [w h] :$body nil :body menu
                               :extra (get extra ::menu-scroll) :$extra [$extra (list 'keypath ::menu-scroll)]})
            (eui/border context w h :color/border 4)])))]))

(defn- create-applet [factory]
  (try
    (let [applet (factory handler)]
      (when-not (every? #(satisfies? % applet) [model/IApplet model/IUI model/IResizable])
        (throw (ex-info "Applet does not implement the applet protocols" {})))
      applet)
    (catch Throwable e
      (assoc (applet/tool-applet "Applet" identity {}) ::applet/error e))))

(defrecord AEasel [applets
                   last-id $ref
                   root-pane]
  model/IEasel
  (-add-applet [this info]
    (let [{:keys [make-applet pane-id pop-out? from-pane-id dock-slot overlay?]} info
          id (or (:id info)
                 (inc last-id))
          _ (when (contains? applets id)
              (throw (ex-info "Applet id is already in use" {:id id})))
          new-last-id (if (integer? id) (max id last-id) last-id)
          applet (cond-> (assoc (create-applet make-applet) :id id ::factory make-applet
                        ::instance-token (random-uuid) :render-error* (atom nil))
                   dock-slot (assoc :dock-slot dock-slot)
                   overlay? (assoc :overlay? true))
          pane-id (or pane-id (when-not (or pop-out? from-pane-id dock-slot overlay?) (first-empty-pane-id this)))
          target (when pane-id (splitpane/find-pane-by-id root-pane pane-id))
          root-pane (cond
                      (or dock-slot overlay?) root-pane
                      
                      (and (not pop-out?)
                           target (empty? (:panes target)))
                      (splitpane/edit-pane-by-id root-pane pane-id
                                                 #(assoc % :applet-id id))
                      
                      from-pane-id
                      (let [from-zpane (zfind (splitpane/pane-zip root-pane)
                                              (fn [pane]
                                                (= (:id pane) from-pane-id)))]
                        (if (and from-zpane (z/up from-zpane))
                          (-> from-zpane
                              (z/insert-right {:id (random-uuid)
                                               :applet-id id})
                              z/root)
                          
                          ;; else
                          (splitpane/add-child root-pane {:id (random-uuid)
                                                          :applet-id id})))
                      
                      :else
                      (splitpane/add-child root-pane {:id (random-uuid)
                                                      :applet-id id}))

          this-pane (get-in this [::cached-layout :by-applet-id id])
          new-size [(:width this-pane) (:height this-pane)]]

      (-> this
          (assoc :last-id new-last-id)
          (assoc-in [:applets id] applet)
          (assoc :root-pane root-pane)
          (cond-> dock-slot (assoc-in [:dock :applets dock-slot] id)
                  dock-slot (assoc-in [:dock :selected] dock-slot)
                  overlay? (assoc :manager-id id))
          relayout*)))
  (-remove-applet [this id]
    (let [applet (get applets id)
          root-pane (-> root-pane
                        (splitpane/delete-pane-by-pred #(= id (:applet-id %))))]
      (-> this
          (update :applets dissoc id)
          (cond-> (= id (:manager-id this)) (dissoc :manager-id))
          (cond-> (:dock this)
            (update-in [:dock :applets] #(into {} (remove (fn [[_ value]] (= id value))) %)))
          (assoc :root-pane root-pane)
          (update-in [:workspaces :by-id]
                     (fn [workspaces]
                       (into {} (map (fn [[key workspace]]
                                       [key (-> workspace
                                                (cond-> (:dock workspace)
                                                  (update-in [:dock :applets]
                                                             #(into {} (remove (fn [[_ value]] (= id value))) %)))
                                                (update :root-pane
                                                    #(specter/transform [splitpane/WALK-PANE
                                                                         (fn [pane] (= id (:applet-id pane)))]
                                                                        (fn [pane] (dissoc pane :applet-id)) %)))]))
                             workspaces)))
          (update ::queue
                  (fnil conj [])
                  (fn []
                    (when applet
                      (try
                        (model/-stop applet)
                        (catch Throwable e
                          (events/report! e))))))
          relayout*)))
  (-show-applet [this id]
    (if (or (not (contains? applets id))
            (contains? (-> this ::cached-layout :by-applet-id) id))
      this
      (let [;; check if there is an empty pane first
          ;; if so, add it to the empty pane
          ;; otherwise, add a new pane
          empty-pane-id (first-empty-pane-id this)

          this (if empty-pane-id
                 (update this :root-pane
                         (fn [root-pane]
                           (splitpane/edit-pane-by-id root-pane empty-pane-id #(assoc % :applet-id id))))
                 (update this :root-pane
                         #(splitpane/add-child % {:id (random-uuid)
                                                  :applet-id id})))]
        (relayout* this))))
  (-hide-applet [this id]
    (-> this
        (update :root-pane
                (fn [pane]
                  (splitpane/delete-pane-by-pred pane #(= id (:applet-id %)))))
        relayout*))
  (-visible-applets [this]
    (let [by-applet-id (-> this
                           ::cached-layout
                           :by-applet-id)]
      (into []
            (filter (fn [applet]
                      (contains? by-applet-id (:id applet))))
            (vals applets))))

  (-applets [this]
    applets)
  model/IResizable
  (-resize [this [w h] content-scale]
    (let [root-pane (if (empty? (:panes root-pane))
                      ;; ensure that root pane can't be edited.
                      (splitpane/add-child root-pane {:id (random-uuid)})
                      ;; pane already has children
                      root-pane)
          root-pane (-> root-pane
                        (assoc :width (max 0 w)
                               :height (max 0 h)))
          header-height (composition/dimension (:appearance this) (get-in (composition/current this) [:panes :header :height] 0))
          gap (theme/token {::theme/value (:appearance this)} :metric/gap)
          root-pane-layout (splitpane/layout-workspace root-pane gap)
          all-panes (splitpane/flatten-pane-nested root-pane-layout)
          dock-id (when-not (get-in this [:dock :hidden?])
                    (get-in this [:dock :applets (get-in this [:dock :selected])]))
          dock-pane (when (and (get applets dock-id) (pos? (:dock-height this 0)))
                      {:applet-id dock-id :width (max 0 (:dock-width this w))
                       :height (max 0 (- (:dock-height this 0)
                                         (composition/dimension (:appearance this) (get-in (composition/current this) [:dock :header-height] 40)))) :body? true})
          manager-pane (when-let [id (:manager-id this)]
                         {:applet-id id :width (max 0 w)
                          :height (max 0 (or (:manager-height this) h)) :body? true})
          active-panes (concat all-panes (when dock-pane [dock-pane]) (when manager-pane [manager-pane]))
          cached-layout {:all-panes all-panes
                         :root-pane-layout root-pane-layout
                         :by-applet-id
                         (into {}
                               (keep (fn [pane]
                                       (when-let [applet-id (:applet-id pane)]
                                         [applet-id pane])))
                               active-panes)}

          this (assoc this
                      :size [(max 0 w) (max 0 h)]
                      :root-pane root-pane
                      ::cached-layout cached-layout
                      :content-scale content-scale)
          this (let [start-or-resize
                     (fn [pane applet]
                       (let [new-size [(:width pane)
                                       (max 0
                                            (- (:height pane) (if (:body? pane) 0 header-height)))]]
                         (if (::applet/error applet)
                           (assoc applet :size new-size)
                           (try
                             (if (::applet-started? applet)
                               (model/-resize applet
                                              new-size
                                              content-scale)
                               (-> applet
                                   (model/-start (specter/path
                                                 (membrane.component/path->spec $ref)
                                                 (specter/keypath :applets)
                                                 (specter/must (:id applet))
                                                 (specter/pred #(= (::instance-token applet) (::instance-token %))))
                                                new-size
                                                content-scale)
                                   (assoc ::applet-started? true)))
                             (catch Throwable e
                               (assoc applet :size new-size ::applet/error e))))))
                     applets
                     (transduce
                      (filter (fn [pane]
                                (get applets (:applet-id pane))))
                      (completing
                       (fn [applets pane]
                         (update applets (:applet-id pane)
                                 #(start-or-resize pane %))))
                      applets
                      active-panes)]
                 (assoc this :applets applets))]
      this))
  model/IUI
  (-ui [this $context context]
    (let [extra (get this ::extra)
          $extra [$ref (list 'keypath ::extra)]]
      (easel-ui* this $extra extra $context context))))

(defeffect ::retry-applet [{:keys [$easel id]}]
  (dispatch! :update $easel
             (fn [easel]
               (if-let [old (get-in easel [:applets id])]
                 (if-let [factory (::factory old)]
                   (-> easel
                       (assoc-in [:applets id] (assoc (create-applet factory) :id id ::factory factory
                                                    ::instance-token (random-uuid)
                                                    :render-error* (atom nil)))
                       (update ::queue (fnil conj []) #(model/-stop old))
                       relayout*)
                   easel)
                 easel))))

(defn make-easel
  "Return an empty Easel value with a root pane and workspace collection."
  []
  (-> (map->AEasel
       {:applets (tiara/ordered-map)
        :last-id 0
        ::extra {}
        :workspaces {:by-id {}
                     :last-id 0}
        :root-pane {:id ::root-pane
                    :width 1
                    :height 1
                    :panes [{:id (random-uuid)}]}})
      relayout*))

(def tab-height 30)
(defui tab-view [{:keys [tabs selected width]}]
  [(ui/spacer width 0)
   (stretch/vlayout
    (map (fn [tab]
           (let [background (ui/rectangle width tab-height)
                 background (if (selected (:id tab))
                              (->> background
                                   (ui/with-style ::ui/style-stroke)
                                   (ui/with-color [0.33 0.33 0.33]))
                              (->> background
                                   (ui/with-color [0.8 0.8 0.8])
                                   (ui/with-style ::ui/style-fill)))
                 lbl (ui/translate 
                      0 2
                      (ui/center
                       (para/paragraph (:label tab))
                       (ui/bounds background)))
                 close (ui/on
                        :mouse-down
                        (fn [_]
                          [[:stop (:id tab)]])
                        (icon.ui/icon {:name "delete"
                                       :hover? (get extra [:delete-hover? (:id tab)])}))
                 [close-width close-height] (ui/bounds close)]
             [(ui/on
               :mouse-down
               (fn [_]
                 [[:toggle (:id tab)]])
               [background
                lbl])
              (ui/translate
               (- width 20)
               (- (/ tab-height 2)
                  (/ close-height 2))
               close)])))
    tabs)])

(defeffect ::save-workspace [{:keys [$easel]}]
  (dispatch! :update
             $easel
             (fn [easel]
               (update easel :workspaces
                       (fn [{:keys [last-id by-id] :as workspaces}]
                         (let [next-id (inc last-id)

                               root-pane (:root-pane easel)
                               new-workspace {:root-pane (dissoc root-pane :width :height :drag-state ::pane-resize)
                                              :dock (:dock easel) :eval-ns (:eval-ns easel)
                                              :name (str next-id)
                                              :id next-id}]
                           (-> workspaces
                               (update :by-id assoc next-id new-workspace)
                               (update :last-id inc))))))))

(defeffect ::clear-workspace [{:keys [$easel]}]
  (dispatch! :update
             $easel
             (fn [easel]
               (-> easel
                   (update :root-pane dissoc :panes)
                   relayout*))))

(defeffect ::select-workspace [{:keys [$easel workspace-id]}]
  (dispatch! :update $easel
             (fn [easel]
               (if-let [workspace (-> easel :workspaces :by-id (get workspace-id))]
                 (let [
                     workspace-root-pane (:root-pane workspace)

                     {:keys [width height]} (:root-pane easel)
                     workspace-root-pane (assoc workspace-root-pane
                                                :width width
                                                :height height)]
                 (-> easel
                     (assoc :root-pane workspace-root-pane)
                     relayout*))
                 easel))))

(defeffect ::delete-workspace [{:keys [$easel workspace-id]}]
  (dispatch! :update
             $easel
             (fn [easel]
               (update-in easel [:workspaces :by-id]
                          dissoc workspace-id))))

(defui string-editor [{:keys [string edit-string on-select
                              last-click
                              ^:membrane.component/contextual
                              focus] :as m}]
  (let [focus? (= focus $string) ]
    (if focus?
      (ui/wrap-on
       :key-press
       (fn [handler s]
         (case s
           :enter [[:set $focus nil] [:set $string edit-string]]
           :escape [[:set $focus nil]]
           (handler s)))
       (ui/horizontal-layout
        (ui/on
         ::basic/request-focus
         (constantly nil)
         (basic/textarea-view {:text edit-string
                               :focus? focus?
                               :cursor (get extra :cursor 0)
                               :border? true}))))
      (ui/on
       :mouse-down
       (fn [_]
         ;; TODO: correctly implement double click
         ;;       and select workspace after 500ms timeout.
         (let [t (.getTime ^java.util.Date (java.util.Date.))]
           (if (and last-click
                    (< (- t last-click)
                       500))
             [[:set $focus $string]
              [:set $edit-string string]]
             (into [[:set $last-click t]] (when on-select (on-select))))))
       (ui/label string)))))

(defui workspace-view [{:keys [workspaces width]}]
  (ui/vertical-layout
   (ui/horizontal-layout
    (ui/on-click
     (fn []
       [[::save-workspace {}]])
     (icon.ui/icon {:name "save"}))
    (ui/on-click
     (fn []
       [[::clear-workspace {}]])
     (icon.ui/icon {:name "minus-circle"})))
   (let [by-id (:by-id workspaces)]
     (stretch/vlayout
      (map (fn [workspace-id]
             (let [workspace (get by-id workspace-id)
                   background (ui/rectangle width tab-height)
                   background (->> background
                                   (ui/with-style ::ui/style-stroke)
                                   (ui/with-color [0.33 0.33 0.33]))

                   editing-name? (get extra [::editing? (:id workspace)])

                   lbl (string-editor {:string (get workspace :name (str workspace-id))
                                       :$string [$workspaces '(keypath :by-id) (list 'keypath workspace-id) '(keypath :name)]
                                       :on-select #(vector [::select-workspace {:workspace-id workspace-id}])})
                   lbl (ui/center lbl
                                  (ui/bounds background))

                   close (ui/on
                          :mouse-down
                          (fn [_]
                            [[::delete-workspace {:workspace-id (:id workspace)}]])
                          (icon.ui/icon {:name "delete"
                                         :hover? (get extra [:delete-hover? (:id workspace)])}))
                   [close-width close-height] (ui/bounds close)]
               [(ui/on
                 :mouse-down
                 (fn [_]
                   [[::select-workspace {:workspace-id (:id workspace)}]])
                 background)
                lbl
                (ui/translate
                 (- width 20)
                 (- (/ tab-height 2)
                    (/ close-height 2))
                 close)])))
      (->> by-id
           (map first)
           sort)))))

(defn add-$easel
  "Adds the :$easel key to the set of intents."
  [$easel intents body]
  (ui/on-bubble
   (fn [intents']
     (specter/transform
      [specter/ALL
       (fn [intent]
         (intents
          (first intent)))]
      (fn [intent]
        [(first intent) (assoc (second intent) :$easel $easel)])
      intents'))
   body))

(def tab-width 180)

(defn resize-window [easel size content-scale]
  (let [appearance (or (:appearance easel) (theme/current))
        geometry (shell/geometry size appearance easel (composition/current easel))]
    (-> easel
        (assoc :viewport size :appearance appearance :composition/frames (:frames geometry)
               :dock-height (:dock-height geometry) :dock-width (:dock-width geometry)
               :manager-height (:workspace-height geometry))
        (model/-resize [(:content-width geometry) (:workspace-height geometry)] content-scale))))

(defn refresh-window [easel] (resize-window easel (:viewport easel [1536 1024]) (:content-scale easel [1 1])))

(defeffect ::dispatch-fn [f] (f))
(defeffect ::pane-menu [id] (dispatch! :update :easel assoc :pane-menu id))
(defeffect ::close-applet [id] (dispatch! :update :easel model/-remove-applet id))
(defeffect ::save-preset []
  (try (let [name ((requiring-resolve 'com.phronemophobic.easel.workspace/save!) dispatch!)]
         (dispatch! :update :easel assoc :message (str "Saved " name)))
       (catch Throwable e
         (events/report! e)
         (dispatch! :update :easel assoc :message (applet/error-message e)))))

(defeffect ::show-help []
  (dispatch! ::hide-commands)
  (let [easel (dispatch! :get :easel)]
    (dispatch! :update :easel
               #(-> % (dissoc :pane-menu)
                      (assoc :help
                             (merge {:previous-focus (dispatch! :get '[(keypath :membrane.component/context) (keypath :focus)])}
                                    (:help easel) {:page :start :offset [0 0]}))))
    (dispatch! :update :easel refresh-window)))

(defeffect ::help-page [page]
  (when (some #(= page (first %)) help/pages)
    (dispatch! :update :easel update :help assoc :page page :offset [0 0])))

(defeffect ::finish-help []
  (when-let [state (:help (dispatch! :get :easel))]
    (when (:first-run? state)
      (try (settings/update-settings! assoc :onboarding/version help/version)
           (catch Throwable e
             (events/report! e)
             (dispatch! :update :easel assoc :message (str "Could not save help preference: " (applet/error-message e))))))
    (dispatch! :update :easel #(refresh-window (dissoc % :help)))
    (dispatch! :set '[(keypath :membrane.component/context) (keypath :focus)] (:previous-focus state))))

(defeffect ::new-editor-in-pane [pane-id]
  (dispatch! ::finish-help)
  (dispatch! ::close-tools)
  (let [id (random-uuid)]
    (dispatch! ::add-applet
               {:id id :pane-id pane-id
                :make-applet #(assoc ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
                                      % {:ns 'user :string "" :label "untitled.clj"})
                                     :package/id :easel/clobber :package/args [{:ns 'user :string "" :label "untitled.clj"}])})
    (dispatch! ::focus-applet id)))

(defeffect ::new-editor [] (dispatch! ::new-editor-in-pane nil))

(defeffect ::open-documentation []
  (dispatch! ::finish-help)
  (dispatch! ::close-tools)
  (dispatch! ::add-applet
             {:make-applet #(assoc ((requiring-resolve 'com.phronemophobic.easel.documentation/documentation-applet)
                                    % {:target {:kind :guide :id :start}})
                                   :package/id :easel/documentation)}))

(defeffect ::focus-applet [id]
  (dispatch! ::finish-help)
  (dispatch! :update :easel
             #(if (get-in % [:applets id :overlay?])
                (resize-window (assoc % :manager-id id) (:viewport %) (:content-scale %))
                (model/-show-applet (dissoc % :manager-id) id)))
  (dispatch! :set '[(keypath :membrane.component/context) (keypath :focus)] id))
(defeffect ::split-direction [{:keys [pane-id direction]}]
  (dispatch! ::splitpane {:$easel '(keypath :easel) :pane-id pane-id})
  (dispatch! :update :easel
             #(-> % (update :root-pane splitpane/edit-pane-by-id pane-id (fn [pane] (assoc pane :direction direction)))
                  relayout*)))
(defeffect ::open-workspace [id]
  (dispatch! ::finish-help)
  (dispatch! :update :easel
             (fn [easel]
               (if-let [current (:workspace-id easel)]
                 (update-in easel [:workspaces :by-id current] merge
                            (select-keys easel [:root-pane :dock :eval-ns]))
                 easel)))
  (let [easel (dispatch! :get :easel)
        workspace (get-in easel [:workspaces :by-id id])]
    (if (:root-pane workspace)
      (dispatch! :update :easel
                 #(resize-window (merge % (select-keys workspace [:root-pane :dock :eval-ns])
                                        {:workspace-id id :manager-id nil})
                                 (:viewport %) (:content-scale %)))
      ((requiring-resolve 'com.phronemophobic.easel.workspace/open!) handler (or (:preset workspace) :easel/empty) id))))

(defeffect ::toggle-dock []
  (dispatch! ::finish-help)
  (if (:dock (dispatch! :get :easel))
    (dispatch! :update :easel
               #(let [easel (update-in % [:dock :hidden?] not)]
                  (resize-window easel (:viewport easel) (:content-scale easel))))
    (dispatch! ::select-dock :repl)))

(defeffect ::show-commands [query]
  (dispatch! :update :easel
             #(if (:commands %) %
                (assoc % :commands {:query query :selection 0
                                     :previous-focus (dispatch! :get '[(keypath :membrane.component/context) (keypath :focus)])})))
  (dispatch! :set '[(keypath :membrane.component/context) (keypath :focus)]
             :com.phronemophobic.easel.shell/command-query))
(defeffect ::hide-commands []
  (let [commands (:commands (dispatch! :get :easel))]
    (dispatch! :update :easel dissoc :commands)
    (when commands
      (dispatch! :set '[(keypath :membrane.component/context) (keypath :focus)] (:previous-focus commands)))))

(defeffect ::add-to-pane [pane-id]
  (dispatch! ::show-commands "")
  (dispatch! :update :easel update :commands assoc :pane-id pane-id :category :applet))

(defeffect ::select-dock [slot]
  (dispatch! ::finish-help)
  (let [easel (dispatch! :get :easel)
        item (or (some #(when (= slot (:id %)) %) (shell/dock-items easel))
                 (throw (ex-info "No dock tool is configured for this slot" {:slot slot})))
        dock? (some #(= :dock (:role %)) (composition/nodes (composition/current easel)))
        existing (get-in easel [:dock :applets slot])]
    (when-not existing
      (let [factory (fn [handler]
                      (if-let [tool (:tool item)]
                        ((requiring-resolve 'com.phronemophobic.easel.packages/make-applet)
                         handler tool (if (:workspace-ns? item) [(or (:eval-ns easel) 'user)] (:args item)))
                        (apply (requiring-resolve (:entry item)) handler (:args item))))]
        (dispatch! ::add-applet {:make-applet factory :dock-slot slot})))
    (if dock?
      (dispatch! :update :easel
                 #(refresh-window (-> % (assoc-in [:dock :selected] slot) (assoc-in [:dock :hidden?] false))))
      (when-let [id (get-in (dispatch! :get :easel) [:dock :applets slot])] (dispatch! ::focus-applet id)))))

(defeffect ::open-tools []
  (dispatch! ::finish-help)
  (let [easel (dispatch! :get :easel)
        existing (some #(when (:tool-manager? %) (:id %)) (vals (:applets easel)))]
    (if existing
      (dispatch! :update :easel assoc :manager-id existing)
      (dispatch! ::add-applet {:overlay? true
                              :make-applet (requiring-resolve 'com.phronemophobic.easel.packages/package-applet)}))
    (dispatch! :update :easel #(resize-window % (:viewport %) (:content-scale %)))))
(defeffect ::close-tools []
  (dispatch! :update :easel #(refresh-window (dissoc % :manager-id))))

(defeffect ::manage-packages [kind]
  (dispatch! ::open-tools)
  (when-let [id (:manager-id (dispatch! :get :easel))]
    (dispatch! :update [:easel :applets (list 'keypath id) :state]
               assoc :kind kind :page :installed :query "" :selected nil)))

(defn command-environment [easel context]
  (let [panes (filter #(empty? (:panes %)) (get-in easel [::cached-layout :all-panes]))
        focus (if (:commands easel) (get-in easel [:commands :previous-focus]) (:focus context))
        pane (or (some #(when (= (:id %) (or (:pane-menu easel) (get-in easel [:commands :pane-id]))) %) panes)
                 (some #(when (= (:applet-id %) focus) %) panes) (first panes))]
    {:easel easel :context context :pane-id (:id pane) :applet-id (:applet-id pane)
     :easel-path '(keypath :easel)}))

(defn key-scopes [easel context]
  (keymap/scopes easel context))

(defeffect ::open-package [id pane-id]
  (dispatch! ::finish-help)
  (dispatch! ::close-tools)
  (dispatch! ::add-applet
             {:pane-id pane-id
              :make-applet #((requiring-resolve 'com.phronemophobic.easel.packages/make-applet) % id nil)}))

(defeffect ::use-package [id]
  (let [package (get ((requiring-resolve 'com.phronemophobic.easel.packages/catalog)) id)]
    ((requiring-resolve 'com.phronemophobic.easel.package-ui/open-package!) dispatch! package)))

(defn package-commands [{:keys [easel pane-id]}]
  (let [catalog ((requiring-resolve 'com.phronemophobic.easel.packages/catalog))
        installed? (requiring-resolve 'com.phronemophobic.easel.packages/installed?)]
    (concat
     (for [p (sort-by :name (vals catalog)) :when (and (= :applet (:kind p)) (not (#{:easel/clobber :easel/repl} (:id p))))]
       {:id (keyword (str "easel.open." (namespace (:id p))) (name (:id p)))
        :name (str (if (installed? p) "Open " "Install ") (:name p)) :icon (or (:icon p) "plus")
        :category :applet :description (:description p) :enabled? (not (:restart? p))
        :disabled-reason "Restart Easel to load this package version."
        :intents (if (installed? p)
                   [[::open-package (:id p) pane-id]]
                   [[::open-tools] [:com.phronemophobic.easel.package-ui/select (:id p)]])})
     (for [p (sort-by :name (vals catalog)) :when (#{:theme :composition :keymap :workspace} (:kind p))]
       {:id (keyword (str "easel.use." (namespace (:id p))) (name (:id p))) :name (str (case (:kind p) :theme "Apply theme: " :composition "Use composition: "
                                                                                                  :keymap "Use keymap: " :workspace "New workspace: ") (:name p))
        :category (:kind p) :description (:description p) :icon (:icon p)
        :intents [[::use-package (:id p)]]})
     (for [a (vals (:applets easel)) :when (not (:overlay? a))]
       {:id (keyword "easel.focus" (str (:id a))) :name (str "Focus " (:label a)) :category :focus :description "Show the existing applet; keeps its state." :icon (or (:icon a) "file")
        :intents [(if-let [slot (:dock-slot a)] [::select-dock slot] [::focus-applet (:id a)])]}))))

(defn commands [easel context]
  (let [environment (command-environment easel context)]
    (for [[id descriptor] (sort-by (comp :name val) (command/registry (composition/current easel) environment))
          :when (not (:hidden? descriptor))]
      (assoc descriptor :disabled? (not (command/enabled? descriptor environment))
                        :shortcut (keymap/shortcut (keymap/current easel) (key-scopes easel context) id)
                        :intents [[::invoke-command id (:pane-id environment)]]))))

(defeffect ::invoke-command [id pane-id]
  (let [easel (dispatch! :get :easel)
        context (dispatch! :get '(keypath :membrane.component/context))
        environment (cond-> (command-environment easel context)
                      pane-id (assoc :pane-id pane-id
                                     :applet-id (:applet-id (splitpane/find-pane-by-id (:root-pane easel) pane-id))))
        descriptor (get (command/registry (composition/current easel) environment) id)]
    (when-let [intents (command/intents descriptor environment)]
      (when-not (= id :easel/cancel)
        (dispatch! ::hide-commands)
        (dispatch! ::pane-menu nil))
      (doseq [intent intents] (apply dispatch! intent)))))

(defeffect ::cancel []
  (let [easel (dispatch! :get :easel)]
    (cond (:commands easel) (dispatch! ::hide-commands)
          (:help easel) (dispatch! ::finish-help)
          (:pane-menu easel) (dispatch! ::pane-menu nil)
          (::pane-resize (:root-pane easel)) (dispatch! ::toggle-pane-resize {:$easel '(keypath :easel) :pane-id ::root-pane}))
    (dispatch! :update :easel dissoc :key-prefix)))

(defui easel-view [{:keys [easel]}]
  (let [appearance (or (:appearance easel) (theme/current))
        shared-context (assoc context ::theme/value appearance ::keymap/value (keymap/current easel)
                                      :composition/frames (:composition/frames easel))
        dock-id (get-in easel [:dock :applets (get-in easel [:dock :selected])])
        dock-applet (get-in easel [:applets dock-id])
        manager (get-in easel [:applets (:manager-id easel)])
        body (add-$easel
               $easel #{::splitpane ::swap-panes ::toggle-pane-resize ::balance-panes ::hide-pane
                        ::close-other-panes ::clear-pane ::delete-pane ::retry-applet
                        ::begin-resize-drag ::resize-drag ::end-resize-drag}
               (model/-ui easel $context shared-context))
        view (dnd/drag-and-drop {:$body nil :body (shell/workspace-view
               {:easel easel :body body :commands (commands easel context)
                :dock-body (when dock-applet (applet/safe-ui dock-applet $context shared-context))
                :manager-body (when manager (applet/safe-ui manager $context shared-context))
                :context shared-context :$context $context})})]
    (ui/wrap-on :key-press
      (fn [next-handler key]
        (let [live (handler :get $easel)
              code (when (keyword? key) (some (fn [[code value]] (when (= value key) code)) skia/keymap))
              environment (command-environment easel context)
              registry (command/registry (composition/current easel) environment)
              match (when code (keymap/match-event (keymap/current easel) (key-scopes easel context) nil code 0
                                                   #(command/enabled? (get registry %) environment)))]
          (cond (and (:key-consumed live) (or (string? key) (= code (:key-consumed live)))) []
                (:command match) [[::invoke-command (:command match) nil]]
                :else (next-handler key))))
      (ui/wrap-on :key-event
        (fn [next-handler key scancode action mods]
          (cond
            (<= 340 key 347) (next-handler key scancode action mods)
            (= :release action)
            (into [[:update $easel #(if (= key (:key-consumed %)) (dissoc % :key-consumed) %)]]
                  (next-handler key scancode action mods))
            (#{:press :repeat} action)
            (let [environment (command-environment easel context)
                  registry (command/registry (composition/current easel) environment)
                  scopes (key-scopes easel context)
                  match (keymap/match-event (keymap/current easel) scopes (:key-prefix easel) key mods
                                           #(command/enabled? (get registry %) environment))
                  editor-prefix? (and (:pending match) (keymap/editor-prefix? easel (:focus context) (:pending match)))
                  forwarded? (and (:key-forwarded? easel) (= (:key-prefix-focus easel) (:focus context)))
                  clear-editor (when (and (:key-forwarded? easel) (or (not forwarded?) (not editor-prefix?))
                                          (get-in easel [:applets (:key-prefix-focus easel) :state :editor]))
                                 [:set [$easel '(keypath :applets) (list 'keypath (:key-prefix-focus easel)) '(keypath :extra)] {}])]
              (cond (:command match) (cond-> [[:update $easel #(-> % (dissoc :key-prefix :key-prefix-focus :key-forwarded?) (assoc :key-consumed key))]]
                                       clear-editor (conj clear-editor)
                                       true (conj [::invoke-command (:command match) nil]))
                    (:pending match) (into (cond-> [[:update $easel assoc :key-prefix (:pending match)
                                                     :key-prefix-focus (:focus context) :key-forwarded? editor-prefix? :key-consumed key]]
                                               clear-editor (conj clear-editor))
                                           (when editor-prefix? (next-handler key scancode action mods)))
                    (:key-prefix easel) (into (cond-> [[:update $easel #(cond-> (dissoc % :key-prefix :key-prefix-focus :key-forwarded? :key-consumed)
                                                                                        (not forwarded?) (assoc :key-consumed key))]]
                                               clear-editor (conj clear-editor))
                                             (when forwarded? (next-handler key scancode action mods)))
                    :else (into [[:update $easel dissoc :key-consumed]] (next-handler key scancode action mods))))
            :else (next-handler key scancode action mods))) view))))

(defn initialize!
  ([] (initialize! {}))
  ([{:keys [size preset onboarding?] :or {size [1536 1024]}}]
   (let [fresh? (nil? @app-state)]
     (swap! app-state #(or % {:easel (assoc (make-easel) :$ref '(keypath :easel))}))
     (swap! app-state update :easel resize-window size [1 1])
     (when-not (get-in @app-state [:easel :packages-restored?])
       ((requiring-resolve 'com.phronemophobic.easel.packages/restore!) handler)
       (handler :update :easel assoc :packages-restored? true))
     (when (and preset (nil? (get-in @app-state [:easel :workspace-id])))
       ((requiring-resolve 'com.phronemophobic.easel.workspace/initialize!) handler preset))
     (when (and fresh? onboarding? (not= help/version (:onboarding/version (settings/read-settings))))
       (handler :update :easel #(refresh-window (assoc % :help {:page :start :first-run? true :offset [0 0]}))))
     nil)))

(declare remove-all-widgets!)

(defn run
  "Open an empty native workspace asynchronously. Options include :size, :preset, and :onboarding?."
  {:easel/see-also [com.phronemophobic.easel/make-easel
                    com.phronemophobic.easel/add-component-as-applet :start]}
  ([] (run {}))
  ([{:keys [size preset onboarding?] :or {size [1536 1024] onboarding? true}}]
   (initialize! {:size size :preset (or preset :easel/empty) :onboarding? onboarding?})
   (let [app (membrane.component/make-app #'easel-view app-state handler)
         options {:include-container-info true :window-title "Easel"
       :window-start-width (first size) :window-start-height (second size)
       ::skia/on-main (fn [] (reset! window-ready? true) (in-runtime on-main-callback))
       :handlers {:scroll native-scroll
                  :reshape
                  (fn [window handle width height]
                    (let [[sx sy] (skia/get-window-content-scale-size handle)]
                      (swap! app-state update :easel resize-window
                             [(int (/ width sx)) (int (/ height sy))] [sx sy]))
                    (skia/-reshape window handle width height))}}
         _ (swap! live-windows inc)
         closed (async/thread
                  (try (skia/run-sync (fn [& args] (in-runtime #(runtime-view (apply app args)))) options)
                       (finally
                         (when (zero? (swap! live-windows dec))
                           (reset! window-ready? false)
                           (try
                             (when-let [stop (resolve 'com.phronemophobic.easel.flowstorm/shutdown!)] (stop))
                             (when-let [suspend (resolve 'com.phronemophobic.easel.packages/suspend!)] (suspend handler))
                             (catch Throwable e (events/report! e))
                             (finally
                               (handler :update :easel dissoc :packages-restored?)
                               (remove-all-widgets!)))))))]
     {::skia/repaint repaint! ::closed closed})))

(defn -main [& _]
  (async/<!! (::closed (run)))
  (shutdown-agents))

(defn ^:private headless
  ([] (headless {}))
  ([opts]
   (initialize! opts)
   (let [app (membrane.component/make-app #'easel-view app-state handler)]
     (app {:container-size (get-in @app-state [:easel :viewport]) :content-scale [1 1] :container nil}))))

(defn add-term []
  (swap! app-state
         update :easel
         model/-add-applet {:make-applet (fn [_]
                                           ((requiring-resolve 'com.phronemophobic.easel.term/termlet) handler))})
  nil)

(defn add-browser [url]
  (swap! app-state
         update :easel
         model/-add-applet {:make-applet (fn [dispatch!]
                                          ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
                                           dispatch! url))})
  nil)

(defn remove-all-widgets [easel]
  (let [easel (reduce
               model/-remove-applet
               easel
               (keys (:applets easel)))
        easel (model/-resize easel
                             (:size easel)
                             (:content-scale easel))]
    easel))

(defn remove-all-widgets! []
  (swap! app-state
         (fn [state]
           (update state :easel remove-all-widgets)))
  nil)

(defn remove-widget! [id]
  (swap! app-state
         (fn [state]
           (update state :easel
                   (fn [easel]
                     (model/-remove-applet easel id)))))
  nil)

(defn reset-easel! []
  (remove-all-widgets!)
           
  ,)

(defn reset-run []
  (try
    (remove-all-widgets!)
    (catch Exception e
      (prn e)
      nil))
  (reset! app-state nil)
  (run))


(comment
  (run)
  (skia/run #(/ 1 0))

  (reset-run)

  (tap> (easel-view @app-state))
  (tap> @app-state)

  ,)

