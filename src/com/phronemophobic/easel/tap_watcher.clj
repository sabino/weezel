(ns com.phronemophobic.easel.tap-watcher
  (:require
   [membrane.component :refer
    [defui
     defeffect]]
   [membrane.ui :as ui]
   [membrane.alpha.component.drag-and-drop :as dnd]
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.easel.ui :as eui]
   [membrane.basic-components :as basic]
   [clojure.core.async :as async]
   [clojure.set :as set]
   [clojure.string :as str]
   [com.phronemophobic.easel.events :as events]
   [com.phronemophobic.easel.inspector :as inspector]
   [com.phronemophobic.easel.exceptions :as exceptions]
   [com.phronemophobic.viscous :as viscous]
   [com.phronemophobic.membrandt :as ant]))

(defeffect ::tap-drop-object [drop-object]
  (when-let [x (:x drop-object)]
    (tap> @x)))


(defui on-drag-hover
  "Component for adding a hover? state."
  [{:keys [object body]}]
  (if object
    (ui/wrap-on
     :mouse-move-global
     (fn [handler [x y :as pos]]
       (let [[w h] (ui/bounds body)
             child-intents (handler pos)]
         (if (or (neg? x)
                 (> x w)
                 (neg? y)
                 (> y h))
           (conj child-intents
                 [:set $object nil])
           child-intents)))
     body)
    (dnd/on-drop-move
     (fn [_ o]
       [[:set $object o]])
     body)))


(defui drag-target [{:keys [drag-object body]}]
  (dnd/on-drop
   (fn [pos obj]
     (let [intents [[:set $drag-object nil]]]
       (conj intents [::tap-drop-object obj])))
   (on-drag-hover
    {:$body nil
     :object drag-object
     :body
     (let [body [body (ui/spacer 200 200)]
           body (if drag-object
                  (ui/fill-bordered
                   [1 0 0 0.2]
                   0
                   body)
                  body)]
       body)})))

(defui tap-view [{:keys [tap-vals size paused? query]}]
  (basic/scrollview
   {:scroll-bounds size
    :$body nil
    :body
    (drag-target
     {:$body nil
      :body 
      (apply
       ui/vertical-layout
       (ui/horizontal-layout
        (eui/button {:size :small
                     :text "Clear"
                     :on-click (fn [] [[:set $tap-vals []]])})
        (eui/button {:size :small
                     :text (if paused? "Resume" "Pause")
                     :on-click (fn [] [[:update $paused? not]])})
        (eui/label (str " " (count tap-vals) " taps"))
        (eui/field {:text query :size :small :placeholder "Filter taps"}))
       (eduction
        (filter (fn [entry]
                  (str/includes? (str/lower-case (:summary entry))
                                 (str/lower-case (or query "")))))
        (map-indexed
         (fn [i {:keys [obj time id]}]
           (let [inspector-extra (get extra [::inspector id])]
             (ui/vertical-layout
              (ui/horizontal-layout
               (eui/label (str time " "))
               (eui/button {:size :small :text "Inspect"
                            :on-click #(vector [:com.phronemophobic.easel/add-applet
                                                {:make-applet (fn [handler]
                                                                (inspector/inspector-applet handler @obj))}])})
               (when (instance? Throwable @obj)
                 (eui/button {:size :small :text "Stacktrace"
                              :on-click #(vector [:com.phronemophobic.easel/add-applet
                                                  {:make-applet (fn [handler]
                                                                  (exceptions/exception-applet handler @obj))}])})))
              (viscous/inspector
               {:obj obj
                :width (get inspector-extra :width 40)
                :height (get inspector-extra :height 1)
                :show-context? (get inspector-extra :show-context?)
                :extra inspector-extra})))))
        (reverse tap-vals)))})}))

(defn tap-ui [this $context context]
  (let [state (-> this
                  (assoc :context context
                         :$context $context
                         :extra (:extra this)
                         :$extra [(:$ref this) '(keypath :extra)]))]
    (tap-view state)))


(defrecord TapWatcher [dispatch!]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [active? (atom true)
          tapf (fn [o]
                 (when @active?
                   (let [entry {:id (random-uuid) :obj (viscous/wrap o)
                                :time (java.time.Instant/now)
                                :summary (inspector/preview o)}]
                     (dispatch! :update $ref
                                (fn [state]
                                  (if (or (not @active?) (:paused? state))
                                    state
                                    (-> state
                                        (update :tap-vals events/bounded-conj entry 500)
                                        (update :extra
                                                (fn [extra]
                                                  (let [ids (into #{} (map :id) (:tap-vals state))]
                                                    (into {} (remove (fn [[k _]]
                                                                       (and (vector? k)
                                                                            (= ::inspector (first k))
                                                                            (not (contains? ids (second k))))))
                                                          extra)))))))))))]
      (assoc this
             :extra {}
             :tap-vals []
             :paused? false
             :query ""
             :active? active?
             :tapf tapf
             :$ref $ref
             :size size
             ::model/queue
             [(fn []
                (when @active?
                  (add-tap tapf)
                  (when-not @active? (remove-tap tapf))))])))
  (-stop [this]
    (reset! (:active? this) false)
    (when-let [tapf (:tapf this)]
      (remove-tap tapf))
    nil)
  model/IUI
  (-ui [this $context context]
    (tap-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn tap-watcher-applet [handler]
  (-> (->TapWatcher handler)
      (assoc :label (str "tap watcher") )))


