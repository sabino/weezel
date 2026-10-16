(ns com.phronemophobic.easel.file-watcher-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel.file-watcher :as watcher])
  (:import [java.util.concurrent CountDownLatch ForkJoinPool TimeUnit]))

(deftest file-edits-arrive-while-the-common-pool-is-busy
  (let [pool (ForkJoinPool/commonPool)
        count (.getParallelism pool)
        started (CountDownLatch. count) release (CountDownLatch. 1)
        jobs (mapv (fn [_] (.submit pool ^Runnable #(do (.countDown started) (.await release)))) (range count))
        directory (.toFile (java.nio.file.Files/createTempDirectory "easel-watcher-"
                                                                   (make-array java.nio.file.attribute.FileAttribute 0)))
        file (io/file directory "config.edn") observed (promise) resource (atom nil)]
    (try
      (is (.await started 3 TimeUnit/SECONDS))
      (spit file "{:value 1}")
      (reset! resource (watcher/watch #(when (= (.toPath file) (:path %)) (deliver observed true)) (str directory)))
      (spit file "{:value 2}")
      (is (deref observed 3000 false))
      (finally
        (when @resource (watcher/stop @resource))
        (.countDown release)
        (doseq [job jobs] (.get job 3 TimeUnit/SECONDS))
        (io/delete-file file true) (io/delete-file directory true)))))
