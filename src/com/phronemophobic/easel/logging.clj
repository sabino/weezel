(ns com.phronemophobic.easel.logging
  (:require [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.membrandt :as ant]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui]]
            [membrane.ui :as ui])
  (:import [java.util.logging Handler Level LogRecord Logger SimpleFormatter]))

(def levels {:trace 0 :debug 1 :info 2 :warn 3 :error 4})

(defn log!
  ([level message] (log! level message nil))
  ([level message data]
   (events/publish! {:kind :log :level level :message (str message) :data data})))

(defonce ^:private jul-state (atom {:users 0}))

(defn acquire-jul! []
  (locking jul-state
    (when (zero? (:users @jul-state))
      (let [formatter (SimpleFormatter.)
            handler (proxy [Handler] []
                      (publish [^LogRecord record]
                        (let [value (.intValue (.getLevel record))
                              level (cond
                                      (>= value (.intValue Level/SEVERE)) :error
                                      (>= value (.intValue Level/WARNING)) :warn
                                      (>= value (.intValue Level/INFO)) :info
                                      (>= value (.intValue Level/FINE)) :debug
                                      :else :trace)]
                          (events/publish! {:kind :log :level level
                                            :message (.formatMessage formatter record)
                                            :logger (.getLoggerName record)
                                            :throwable (.getThrown record)})
                          (when-let [e (.getThrown record)] (events/report! e))))
                      (flush [])
                      (close []))]
        (.setLevel handler Level/ALL)
        (.addHandler (Logger/getLogger "") handler)
        (swap! jul-state assoc :handler handler)))
    (swap! jul-state update :users inc))
  (let [released? (atom false)]
    (fn []
      (when (compare-and-set! released? false true)
        (locking jul-state
          (let [{:keys [users handler]} (swap! jul-state update :users dec)]
            (when (zero? users)
              (.removeHandler (Logger/getLogger "") handler)
              (.close ^Handler handler)
              (swap! jul-state dissoc :handler))))))))

(defn matching-events [entries level query after-id]
  (filter (fn [entry]
            (and (> (:id entry) after-id)
                 (>= (get levels (:level entry) 2) (get levels level 0))
                 (str/includes? (str/lower-case (str (:logger entry) " " (:message entry)))
                                (str/lower-case query))))
          entries))

(defui logging-view [{:keys [state size]}]
  (let [{:keys [entries paused?]} state
        query (:query state "")
        level (:level state :trace)
        visible (matching-events (if paused? (:snapshot state) entries)
                                 level query (:after-id state -1))]
    (ui/vertical-layout
     (ui/horizontal-layout
      (ant/button {:text (if paused? "Resume" "Pause") :size :small
                   :on-click #(vector [:update $state assoc :paused? (not paused?) :snapshot entries])})
      (ant/button {:text "Clear" :size :small
                   :on-click #(vector [:set [$state '(keypath :after-id)]
                                       (dec (:next-id @events/history))])})
      (ui/label (str " " (count visible) " logs")))
     (apply ui/horizontal-layout
            (for [l [:trace :debug :info :warn :error]]
              (ant/button {:text (str (when (= l level) "• ") (name l)) :size :small
                           :on-click #(vector [:set [$state '(keypath :level)] l])})))
     (ant/text-input {:text query :$text [$state '(keypath :query)] :size :small :placeholder "Filter logs"})
     (basic/scrollview
      {:scroll-bounds [(max 0 (first size)) (max 0 (- (second size) 95))]
       :$body nil
       :body
       (apply ui/vertical-layout
              (for [entry (reverse visible)]
                (ui/horizontal-layout
                 (ant/button {:text "Inspect" :size :small
                              :on-click #(vector [:com.phronemophobic.easel/add-applet
                                                  {:make-applet (fn [handler]
                                                                  (inspector/inspector-applet handler entry))}])})
                 (ui/with-color (if (#{:error :warn} (:level entry)) [0.7 0.2 0.1] [0.1 0.1 0.1])
                   (ui/label (str (:time entry) " " (name (:level entry)) " "
                                  (:logger entry) " " (:message entry)))))))}))))

(defn logging-applet [handler]
  (applet/tool-applet
   "Logs" #'logging-view {:entries [] :query ""}
   (fn [$state]
     (let [release (acquire-jul!)
           unsubscribe (events/subscribe! :log #(handler :update $state assoc :entries %))]
       (fn [] (unsubscribe) (release))))))
