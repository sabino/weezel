(ns com.phronemophobic.easel.applet
  (:require [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.easel.theme :as theme]
            [membrane.component :refer [defui]]
            [membrane.ui :as ui]))

(defui button [{:keys [text size disabled? on-click]}]
  (eui/button {:text text :size size :disabled? disabled?
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
  (let [context (assoc context ::theme/applet (:package/id this))
        failure (fn [e]
                  (report-once! this e)
                  (ui/vertical-layout
                   (eui/text {:text (str (:label this) " could not run") :context context :$context $context})
                   (eui/text {:text (error-message e) :width (max 1 (first (:size this)))
                              :context context :$context $context :color :color/error})
                   (eui/button {:text "Retry" :size :small :context context :$context $context
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
  "Run f off the UI thread and merge its result into $state.
  A newer request, removal, or retry supersedes an older result. With on-success,
  deliver the current result to that callback instead of merging it."
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
    (eui/legacy-view context (:size this)
     (view {:state (:state this)
           :$state [(:$ref this) '(keypath :state)]
           :size (:size this)
           :extra (:extra this)
           :$extra [(:$ref this) '(keypath :extra)]
           :context context
           :$context $context})))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this :size size)))

(defn tool-applet
  "Build an applet around a view var and a state map.
  The view receives :state, :$state, :size, :context, and :$context.
  Optional on-start receives $state and may return a cleanup function."
  {:easel/examples ["(require '[membrane.ui :as ui])\n(defn hello-view [{:keys [state]}] (ui/label (:message state)))\n(:state (tool-applet \"Hello\" #'hello-view {:message \"Hello, Clojure\"}))"]
   :easel/see-also [com.phronemophobic.easel.model/IApplet
                    com.phronemophobic.easel.model/IUI
                    com.phronemophobic.easel.model/IResizable :applets :lifecycle]}
  ([label view state]
   (tool-applet label view state nil))
  ([label view state on-start]
   (->ToolApplet label view state on-start)))
