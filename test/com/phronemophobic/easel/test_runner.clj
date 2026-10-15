(ns com.phronemophobic.easel.test-runner
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn -main [& _]
  (let [namespaces (for [file (file-seq (io/file "test"))
                         :when (str/ends-with? (.getName file) "_test.clj")]
                     (-> (.getPath file)
                         (str/replace #"^test/|\.clj$" "")
                         (str/replace "/" ".")
                         (str/replace "_" "-")
                         symbol))]
    (try
      (run! require namespaces)
      (let [{:keys [fail error]} (apply test/run-tests namespaces)]
        (shutdown-agents)
        (System/exit (if (zero? (+ fail error)) 0 1)))
      (catch Throwable e
        (.printStackTrace e)
        (shutdown-agents)
        (System/exit 1)))))
