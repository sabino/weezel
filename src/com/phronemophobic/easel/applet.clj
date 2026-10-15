(ns com.phronemophobic.easel.applet
  (:require [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.events :as events]))

(defn task! [dispatch! $state f]
  (let [request (random-uuid)]
    (dispatch! :update $state assoc :request request :busy? true :error nil)
    (future
      (let [result (try
                     (f)
                     (catch Exception e
                       (events/report! e)
                       {:error (or (.getMessage e) (str e))}))]
        (dispatch! :update $state
                   (fn [state]
                     (if (= request (:request state))
                       (merge state result {:busy? false})
                       state))))))
  nil)

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
                  (let [stop (on-start $state)]
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
