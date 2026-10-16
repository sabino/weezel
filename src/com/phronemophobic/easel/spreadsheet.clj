(ns com.phronemophobic.easel.spreadsheet
  (:require
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.easel.events :as events]
   [com.phronemophobic.easel.ui :as eui]
   [com.phronemophobic.easel.clobber :as clobber]
   [clojure.core.async :as async]
   [com.phronemophobic.membrane.spreadsheet
    :as ss]))

(defn spreadsheet-ui [this $context context]
  (let [state(-> this
                 (assoc :context
                        (-> context
                            (assoc :membrane.stretch/container-size (:size this))))
                 (assoc :$context $context
                        :extra (:extra this)
                        :$extra [(:$ref this) '(keypath :extra)]
                        :$ss [(:$ref this) '(keypath :ss)]
                        :$results [(:$ref this) '(keypath :results)]
                        :$ns-info [(:$ref this) '(keypath :ns-info)]
                        :drop-object (:drop-object context)
                        :$drop-object [$context '(keypath :drop-object)]))]
    (eui/adapt-colors (ss/spreadsheet-editor state) (clobber/editor-palette context))))

(defn run-results [dispatch! $ref active?]
  (let [input (async/chan (async/sliding-buffer 1))
        worker (doto
                (Thread.
                 (bound-fn []
                   (try
                     (loop []
                       (when-let [sheet (async/<!! input)]
                         (when @active?
                           (try
                             (let [{:keys [cache side-effects ns-info]} (dispatch! :get $ref)
                                   result (ss/calc-spreadsheet (the-ns (:name ns-info))
                                                               (or cache {}) (or side-effects {}) sheet)]
                               (dispatch! :update $ref
                                          (fn [state]
                                            (if (and @active? (= sheet (:ss state)))
                                              (merge state result {:last-ns-info ns-info}) state)))
                               (dispatch! :repaint!))
                             (catch InterruptedException _)
                             (catch Throwable e (when @active? (events/report! e))))
                           (recur))))
                     (catch InterruptedException _)))
                 "easel-spreadsheet")
                 (.setDaemon true))
        unwatch (dispatch! :watch [$ref (random-uuid)] $ref
                           (fn [_ _ old new]
                             (when (and @active? (some? (:ss new)) (not= (:ss old) (:ss new)))
                               (async/put! input (:ss new)))))]
    (.start worker)
    (when-some [sheet (:ss (dispatch! :get $ref))] (async/put! input sheet))
    {:thread worker
     :stop (fn []
             (when unwatch (unwatch))
             (async/close! input)
             (.interrupt worker))}))

(defrecord SpreadSheet [dispatch! eval-ns]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [active? (atom true) worker* (atom nil)]
      (assoc this
           :active? active?
           :worker* worker*
           :ss []
           ::ss/id (random-uuid)
           :ns-info {:name eval-ns
                     :require '([membrane.ui :as ui
                                 :refer [vertical-layout
                                         horizontal-layout]]
                                [clojure.java.io :as io]
                                [com.phronemophobic.membrane.spreadsheet :as ss]
                                [clojure.edn :as edn]
                                clojure.set
                                [clojure.string :as str]
                                [clojure.data.json :as json])
                     :import '(java.io.PushbackReader
                               java.time.Instant)}
           :$ref $ref
           :size size

           ::model/queue
           [(fn []
              (locking active?
                (when @active?
                  (reset! worker* (run-results dispatch! $ref active?)))))])))
  (-stop [this]
    (when-let [active? (:active? this)]
      (locking active?
        (reset! active? false)
        (when-let [worker (first (swap-vals! (:worker* this) (constantly nil)))]
          ((:stop worker))))))
  model/IUI
  (-ui [this $context context]
    (spreadsheet-ui this $context context))
  model/IResizable
  (-resize [this size _content-scale]
    (assoc this
           :size size)))

(defn spreadsheet-applet [handler]
  (-> (->SpreadSheet handler 'com.phronemophobic.easel)
      (assoc :label "Spreadsheet")))

