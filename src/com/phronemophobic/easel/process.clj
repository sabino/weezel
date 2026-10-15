(ns com.phronemophobic.easel.process
  (:require [clojure.java.io :as io])
  (:import [java.util.concurrent TimeUnit]))

(defn run
  ([command] (run command {}))
  ([command {:keys [timeout-ms] :or {timeout-ms 30000}}]
   (let [process (.start (ProcessBuilder. ^java.util.List (vec command)))
         out (future (with-open [r (io/reader (.getInputStream process) :encoding "UTF-8")]
                       (slurp r)))
         err (future (with-open [r (io/reader (.getErrorStream process) :encoding "UTF-8")]
                       (slurp r)))]
     (try
       (.close (.getOutputStream process))
       (when-not (.waitFor process timeout-ms TimeUnit/MILLISECONDS)
         (throw (ex-info "Command timed out" {:command command :timeout-ms timeout-ms})))
       {:exit (.exitValue process) :out @out :err @err}
       (finally
         (.destroyForcibly process))))))

(defn checked [command]
  (let [{:keys [exit err] :as result} (run command)]
    (when-not (zero? exit)
      (throw (ex-info (if (seq err) err (str "Command exited with " exit))
                      (assoc result :command command))))
    (:out result)))
