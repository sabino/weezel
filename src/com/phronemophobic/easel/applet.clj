(ns com.phronemophobic.easel.applet
  (:require [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.membrandt :as ant]
            [membrane.component :refer [defui]]
            [membrane.skia.paragraph :as para]
            [membrane.ui :as ui]))

(defui button [{:keys [text size disabled? on-click]}]
  (ant/button {:text text :size size :disabled? disabled?
               :on-click (when-not disabled? on-click)}))

(defn error-message [e]
  (let [message (or (.getMessage ^Throwable e) (str e))]
    (if (> (count message) 1000) (str (subs message 0 1000) "…") message)))

(defn- report-once! [this e]
  (let [fingerprint [(class e) (.getMessage ^Throwable e)]
        reported (:render-error* this)]
    (when (and reported (not= fingerprint @reported))
      (reset! reported fingerprint)
      (events/report! e))))

(defn safe-ui [this $context context]
  (let [failure (fn [e]
                  (report-once! this e)
                  (ui/vertical-layout
                   (ui/label (str (:label this) " could not run"))
                   (para/paragraph (error-message e)
                                   (max 1 (first (:size this))))
                   (ant/button {:text "Retry" :size :small
                                :on-click #(vector [:com.phronemophobic.easel/retry-applet {:id (:id this)}])})))
        report-event (fn [handler & args]
                       (try
                         (doall (apply handler args))
                         (catch Throwable e (report-once! this e) nil)))]
    (if-let [e (::error this)]
      (failure e)
      (try
        (let [view (model/-ui this $context context)]
          (ui/bounds view)
          (ui/try-draw
           (ui/wrap-on :mouse-event report-event :key-event report-event
                       :key-press report-event
                       :scroll report-event view)
           (fn [draw e] (draw (failure e)))))
        (catch Throwable e (failure e))))))

(defn task!
  ([dispatch! $state f] (task! dispatch! $state f nil))
  ([dispatch! $state f on-success]
   (let [request (random-uuid)
         current? #(= request (:request (dispatch! :get $state)))
         finish! (fn [result]
                   (dispatch! :update $state
                              (fn [state]
                                (if (= request (:request state))
                                  (merge state result {:request request :busy? false})
                                  state))))]
     (dispatch! :update $state assoc :request request :busy? true :error nil)
     (when (current?)
       (future
         (try
           (let [result (f)]
             (finish! (if on-success {} result))
             (when (and on-success (current?)) (on-success result)))
           (catch Throwable e
             (events/report! e)
             (finish! {:error (error-message e)}))))))))

(defrecord ToolApplet [label view state on-start]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [active? (atom true)
          cleanup (atom nil)
          $state [$ref '(keypath :state)]]
      (assoc this
             :$ref $ref
             :size size
             :active? active?
             :cleanup cleanup
             ::model/queue
             [(fn []
                (when (and on-start @active?)
                  (let [result (on-start $state)
                        stop (when (fn? result) result)]
                    (reset! cleanup stop)
                    (when-not @active?
                      (when-let [stop (first (swap-vals! cleanup (constantly nil)))]
                        (stop))))))])))
  (-stop [this]
    (when-let [active? (:active? this)] (reset! active? false))
    (when-let [cleanup (:cleanup this)]
      (when-let [stop (first (swap-vals! cleanup (constantly nil)))]
        (stop))))
  model/IUI
  (-ui [this $context context]
    (view {:state (:state this)
           :$state [(:$ref this) '(keypath :state)]
           :size (:size this)
           :extra (:extra this)
           :$extra [(:$ref this) '(keypath :extra)]
           :context context
           :$context $context}))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this :size size)))

(defn tool-applet
  ([label view state]
   (tool-applet label view state nil))
  ([label view state on-start]
   (->ToolApplet label view state on-start)))
