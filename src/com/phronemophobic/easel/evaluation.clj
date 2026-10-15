(ns com.phronemophobic.easel.evaluation
  (:require [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.logging :as logging])
  (:import [java.io PrintWriter StringReader Writer]
           [clojure.lang LineNumberingPushbackReader]))

(defn log-writer [^Writer output level]
  (let [buffer (StringBuilder.)
        flush-line (fn []
                     (when (pos? (.length buffer))
                       (logging/log! level (.toString buffer))
                       (.setLength buffer 0)))
        write-text (fn [text]
                     (locking buffer
                       (.write output ^String text)
                       (doseq [ch text]
                         (if (= ch \newline)
                           (flush-line)
                           (do
                             (.append buffer ^char ch)
                             (when (>= (.length buffer) 4096) (flush-line)))))))]
    (PrintWriter.
     (proxy [Writer] []
       (write
         ([value]
          (write-text (cond
                        (string? value) value
                        (number? value) (str (char value))
                        :else (String. ^chars value))))
         ([value offset length]
          (write-text (if (string? value)
                        (subs value offset (+ offset length))
                        (String. ^chars value (int offset) (int length))))))
       (flush []
         (locking buffer
           (flush-line)
           (.flush output)))
       (close []
         (locking buffer (flush-line)))))))

(defn evaluate [source {:keys [ns file line] :or {line 1}}]
  (let [out (log-writer *out* :info)
        err (log-writer *err* :error)]
    (try
      (binding [*ns* (or ns (the-ns 'user)) *out* out *err* err]
        (with-open [reader (doto (LineNumberingPushbackReader. (StringReader. source))
                            (.setLineNumber line))]
          {:value (clojure.lang.Compiler/load reader file file)}))
      (catch Throwable e
        (events/report! e)
        {:exception e})
      (finally
        (.flush out)
        (.flush err)))))
