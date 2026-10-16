(ns com.phronemophobic.easel.events)

(def history-limit 1000)

(defonce history (atom {:next-id 0 :events []}))

(def ^:dynamic *notifying* #{})

(defn bounded-conj [xs x limit]
  (let [xs (conj (vec xs) x)]
    (if (> (count xs) limit)
      (into [] (take-last limit xs))
      xs)))

(defn publish! [event]
  (let [event (assoc event :time (java.time.Instant/now))]
    (swap! history
           (fn [{:keys [next-id events]}]
             {:next-id (inc next-id)
              :events (bounded-conj events (assoc event :id next-id) history-limit)})))
  nil)

(defn recent [kind]
  (into [] (filter #(= kind (:kind %))) (:events @history)))

(defn subscribe! [kind callback]
  (let [key (random-uuid)
        active? (atom true)
        notify (fn []
                 (let [invocation [key (Thread/currentThread)]]
                   (when (and @active? (not (contains? *notifying* invocation)))
                     (binding [*notifying* (conj *notifying* invocation)]
                       (try
                         (callback (recent kind))
                         (catch Throwable e
                           (reset! active? false)
                           (remove-watch history key)
                           (publish! {:kind :exception :throwable e})))))))]
    (add-watch history key (fn [_ _ _ _] (notify)))
    (notify)
    (fn []
      (reset! active? false)
      (remove-watch history key))))

(defn report! [throwable]
  (publish! {:kind :exception :throwable throwable}))
