(ns com.phronemophobic.easel.process
  (:require [clojure.java.io :as io])
  (:import [java.util.concurrent TimeUnit]))

(defn- read-output [stream limit]
  (with-open [reader (io/reader stream :encoding "UTF-8")]
    (let [buffer (char-array 8192)
          output (StringBuilder.)]
      (loop [truncated? false]
        (let [n (.read reader buffer)]
          (if (neg? n)
            {:text (.toString output) :truncated? truncated?}
            (let [remaining (max 0 (- limit (.length output)))
                  keep (min remaining n)]
              (.append output buffer 0 (int keep))
              (recur (or truncated? (> n remaining))))))))))

(defn run
  ([command] (run command {}))
  ([command {:keys [timeout-ms max-output-chars env]
             :or {timeout-ms 30000 max-output-chars 2097152}}]
   (let [builder (ProcessBuilder. ^java.util.List (vec command))
         _ (when env (.putAll (.environment builder) env))
         process (.start builder)
         deadline (+ (System/nanoTime) (* timeout-ms 1000000))
         remaining #(max 0 (long (/ (- deadline (System/nanoTime)) 1000000)))
         out (future (read-output (.getInputStream process) max-output-chars))
         err (future (read-output (.getErrorStream process) max-output-chars))
         timed-out (fn [] (ex-info "Command timed out" {:command command :timeout-ms timeout-ms}))]
     (try
       (.close (.getOutputStream process))
       (when-not (.waitFor process (remaining) TimeUnit/MILLISECONDS)
         (throw (timed-out)))
       (let [stdout (deref out (remaining) ::timeout)
             stderr (deref err (remaining) ::timeout)]
         (when (or (= ::timeout stdout) (= ::timeout stderr))
           (throw (timed-out)))
         (when (or (:truncated? stdout) (:truncated? stderr))
           (throw (ex-info "Command output exceeded the display limit"
                           {:command command :max-output-chars max-output-chars})))
         {:exit (.exitValue process) :out (:text stdout) :err (:text stderr)})
       (finally
         (when (.isAlive process)
           (with-open [children (.descendants process)]
             (doseq [^java.lang.ProcessHandle child (reverse (vec (.toArray children)))]
               (.destroyForcibly child)))
           (.destroyForcibly process))
         (future-cancel out)
         (future-cancel err))))))

(defn checked
  ([command] (checked command {}))
  ([command options]
   (let [{:keys [exit err] :as result} (run command options)]
     (when-not (zero? exit)
       (throw (ex-info (if (seq err) err (str "Command exited with " exit))
                       (assoc result :command command))))
     (:out result))))
