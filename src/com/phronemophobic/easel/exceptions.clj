(ns com.phronemophobic.easel.exceptions
  (:require [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.inspector :as inspector]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui]]
            [membrane.ui :as ui]))

(def report! events/report!)

(defn causes [throwable]
  (let [seen (java.util.IdentityHashMap.)]
    (loop [queue (list [throwable 0 "Exception"]) result []]
      (if-let [[^Throwable e depth label] (first queue)]
        (if (.containsKey seen e)
          (recur (next queue) result)
          (do
            (.put seen e true)
            (recur (concat (map #(vector % (inc depth) "Suppressed") (.getSuppressed e))
                           (when-let [cause (.getCause e)] [[cause depth "Caused by"]])
                           (next queue))
                   (conj result {:throwable e :depth depth :label label}))))
        result))))

(defn source-info [^StackTraceElement frame]
  (let [filename (.getFileName frame)
        line (.getLineNumber frame)
        classname (first (str/split (.getClassName frame) #"\$"))
        resource (str (str/replace classname "." "/")
                      (when filename (str "." (last (str/split filename #"\.")))))
        source (when filename (or (io/resource resource) (io/resource filename)))
        local (when filename (io/file filename))]
    (when (pos? line)
      (cond
        (and local (.isFile local)) {:file (.getCanonicalFile local) :line line}
        source (if (= "file" (.getProtocol source))
                 {:file (io/file (.toURI source)) :line line}
                 {:string (slurp source) :line line :label filename})))))

(defn source-intents [frame]
  (when-let [info (source-info frame)]
    [[:com.phronemophobic.easel/add-applet
      {:make-applet (fn [handler]
                      ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
                       handler info))}]]))

(defui exception-view [{:keys [state size]}]
  (let [entries (:entries state)
        selected (or (some #(when (= (:selected state) (:id %)) %) entries)
                     (peek entries))
        ^Throwable throwable (:throwable selected)
        hide-library? (:hide-library? state)]
    (ui/vertical-layout
     (ui/horizontal-layout
      (eui/button {:text "Clear" :size :small
                   :on-click #(vector [:update $state assoc :entries [] :after-id
                                       (dec (:next-id @events/history))])})
      (eui/button {:text (if hide-library? "Show library frames" "Hide library frames") :size :small
                   :on-click #(vector [:update $state update :hide-library? not])})
      (when throwable
        (eui/button {:text "Inspect" :size :small
                     :on-click #(vector [:com.phronemophobic.easel/add-applet
                                         {:make-applet (fn [handler]
                                                         (inspector/inspector-applet handler throwable))}])})))
     (basic/scrollview
      {:scroll-bounds [(max 0 (first size)) (max 0 (- (second size) 35))]
       :$body nil
       :body
       (apply ui/vertical-layout
              (concat
               (for [entry (take 20 (reverse entries))]
                 (eui/button {:text (str (:time entry) " " (some-> (:throwable entry) .getMessage))
                              :size :small
                              :on-click #(vector [:set [$state '(keypath :selected)] (:id entry)])}))
               (when throwable
                 (mapcat
                  (fn [{:keys [^Throwable throwable depth label]}]
                    (concat
                     [(eui/label (str (apply str (repeat depth "  ")) label ": "
                                      (.getName (class throwable)) ": " (.getMessage throwable)))
                      (when-let [data (ex-data throwable)]
                        (eui/label (inspector/preview data)))]
                     (for [^StackTraceElement frame (.getStackTrace throwable)
                           :when (or (not hide-library?)
                                     (not (re-find #"^(java\.|jdk\.|sun\.|clojure\.)" (.getClassName frame))))]
                       (ui/on-click #(source-intents frame)
                                    (eui/label (str "  " (repl/demunge (.getClassName frame)) "/"
                                                   (.getMethodName frame) " (" (.getFileName frame) ":"
                                                   (.getLineNumber frame) ")"))))))
                  (causes throwable)))))}))))

(defn exception-applet
  ([handler] (exception-applet handler nil))
  ([handler throwable]
   (when throwable (report! throwable))
   (applet/tool-applet
    "Exceptions" #'exception-view {:entries []}
    (fn [$state]
      (events/subscribe!
       :exception
       #(handler :update $state
                 (fn [state]
                   (assoc state :entries (into [] (filter (fn [entry]
                                                           (> (:id entry) (:after-id state -1)))) %)))))))))
