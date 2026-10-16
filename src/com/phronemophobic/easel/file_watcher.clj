(ns com.phronemophobic.easel.file-watcher
  (:require [clojure.java.io :as io]
            [com.phronemophobic.easel.events :as events])
  (:import [io.methvin.watcher DirectoryChangeListener DirectoryWatcher]
           [java.util.concurrent Executors ThreadFactory]
           [java.util.function BiConsumer]))

(defonce executor
  (delay
    (Executors/newCachedThreadPool
     (reify ThreadFactory
       (newThread [_ work] (doto (Thread. work "easel-file-watcher") (.setDaemon true)))))))

(defn watch [callback & paths]
  (let [watcher (-> (DirectoryWatcher/builder)
                    (.paths (map #(.toPath (io/file %)) paths))
                    (.listener
                     (reify DirectoryChangeListener
                       (onEvent [_ event]
                         (try
                           (callback {:type (keyword (.toLowerCase (.name (.eventType event)) java.util.Locale/ROOT))
                                      :path (.path event)})
                           (catch Throwable e (events/report! e))))
                       (onException [_ error] (events/report! error))))
                    .build)]
    (.whenComplete (.watchAsync watcher @executor)
                   (reify BiConsumer
                     (accept [_ _ error] (when error (events/report! error)))))
    watcher))

(defn stop [^DirectoryWatcher watcher] (.close watcher))
