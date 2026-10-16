(ns com.phronemophobic.easel.flowstorm
  (:require [clojure.string :as str]
            [clojure.java.basis :as basis]
            [clojure.java.io :as io]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.ui :as eui]
            [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]))

(defonce runtime-state (atom {:started? false :recording? false :instrumented #{}}))
(defonce debugger-process (atom nil))

(defn api [sym]
  (try
    (or (requiring-resolve sym)
        (throw (ex-info (str "Missing FlowStorm API: " sym) {})))
    (catch java.io.FileNotFoundException e
      (throw (ex-info "Install FlowStorm from Tools to enable tracing" {} e)))))

(defn var-symbol [text]
  (let [sym (symbol (str/trim text))]
    (when-not (and (qualified-symbol? sym) (find-ns (symbol (namespace sym)))
                   (var? (resolve sym)))
      (throw (ex-info "Enter an existing qualified var, for example my.app/run" {:var text})))
    sym))

(defn- debugger-classpath []
  (str/join java.io.File/pathSeparator
            (distinct
             (map #(.getAbsolutePath (io/file %))
                  (concat (str/split (System/getProperty "java.class.path")
                                    (re-pattern (java.util.regex.Pattern/quote java.io.File/pathSeparator)))
                          (mapcat :paths (vals (:libs (basis/current-basis)))))))))

(defn- stop-debugger! []
  (when-let [{:keys [process directory]} (first (swap-vals! debugger-process (constantly nil)))]
    (.close (.getOutputStream ^Process process))
    (when-not (.waitFor ^Process process 3 java.util.concurrent.TimeUnit/SECONDS)
      (.destroyForcibly ^Process process)
      (.waitFor ^Process process 3 java.util.concurrent.TimeUnit/SECONDS))
    (doseq [file (reverse (file-seq directory))] (io/delete-file file true)))
  (swap! runtime-state assoc :debugger-open? false))

(defn- open-debugger! []
  (when-not (some-> @debugger-process :process .isAlive)
    (stop-debugger!)
    (let [root (doto (settings/file "cache" "flowstorm") .mkdirs)
          directory (.toFile (java.nio.file.Files/createTempDirectory (.toPath root) "session-"
                                                                     (make-array java.nio.file.attribute.FileAttribute 0)))
          log (io/file root "debugger.log")
          process (-> (ProcessBuilder.
                       ^java.util.List
                       [(str (io/file (System/getProperty "java.home") "bin" "java"))
                        "--enable-native-access=ALL-UNNAMED" "-cp" (debugger-classpath)
                        "clojure.main" "-m" "com.phronemophobic.easel.flowstorm-debugger"
                        (pr-str {:directory (.getPath directory)
                                 :theme (if (= :easel/studio-dark (:id (theme/current))) :dark :light)})])
                      (.directory directory) (.redirectErrorStream true) (.redirectOutput log) .start)
          session {:process process :directory directory}]
      (reset! debugger-process session)
      (try
        (let [await! (fn [pred]
                       (loop [remaining 300]
                         (cond (pred) true
                               (or (not (.isAlive process)) (zero? remaining))
                               (throw (ex-info (str "FlowStorm debugger could not connect. See " log) {}))
                               :else (do (Thread/sleep 100) (recur (dec remaining))))))
              ready (io/file directory "ready.edn")]
          (await! #(.isFile ready))
          (let [port (:port (theme/read-data ready))]
            ((api 'flow-storm.runtime.debuggers-api/remote-connect)
             {:debugger-host "127.0.0.1" :debugger-ws-port port}))
          (await! #(.isFile (io/file directory "connected"))))
        (future
          (.waitFor process)
          (when (compare-and-set! debugger-process session nil)
            (doseq [file (reverse (file-seq directory))] (io/delete-file file true))
            (swap! runtime-state assoc :debugger-open? false
                   :started? false :recording? false)))
        (catch Throwable e (stop-debugger!) (throw e)))))
  (swap! runtime-state assoc :started? true :debugger-open? true))

(defn perform! [action target]
  (locking runtime-state
    (case action
      :start (do ((api 'flow-storm.runtime.debuggers-api/start-runtime))
                 (swap! runtime-state assoc :started? true))
      :refresh nil
      :open (open-debugger!)
      :record (do ((api 'flow-storm.api/start-recording))
                  (swap! runtime-state assoc :recording? true))
      :pause (do ((api 'flow-storm.api/stop-recording))
                 (swap! runtime-state assoc :recording? false))
      :instrument (let [sym (var-symbol target)]
                    ((api 'flow-storm.api/instrument-var-clj) sym {})
                    (swap! runtime-state update :instrumented conj sym))
      :uninstrument (let [sym (var-symbol target)]
                      ((api 'flow-storm.api/uninstrument-var-clj) sym)
                      (swap! runtime-state update :instrumented disj sym))
      :stop (do
              (doseq [sym (:instrumented @runtime-state)]
                (try ((api 'flow-storm.api/uninstrument-var-clj) sym)
                     (catch Throwable e (events/report! e))))
              ((api 'flow-storm.runtime.events/clear-dispatch-fn!))
              (when-let [client (some-> (resolve 'flow-storm.remote-websocket-client/remote-websocket-client) deref)]
                (.closeBlocking client))
              (when ((api 'flow-storm.runtime.debuggers-api/runtime-started?))
                ((api 'flow-storm.runtime.debuggers-api/stop-runtime)))
              (stop-debugger!)
              (reset! runtime-state {:started? false :recording? false :instrumented #{}})))
    (let [started? (boolean ((api 'flow-storm.runtime.debuggers-api/runtime-started?)))]
      (swap! runtime-state assoc :started? started?
             :recording? (and started? (boolean ((api 'flow-storm.tracer/recording?))))))))

(defn shutdown! []
  (when (or (:started? @runtime-state) @debugger-process)
    (packages/with-runtime #(perform! :stop nil))))

(defonce shutdown-hook
  (let [hook (Thread. ^Runnable #(try (stop-debugger!) (catch Throwable _)) "easel-flowstorm-shutdown")]
    (.addShutdownHook (Runtime/getRuntime) hook)
    hook))

(defeffect ::perform [$state action target]
  (applet/task! dispatch! $state #(do (perform! action target) {})))

(defui flowstorm-view [{:keys [state size]}]
  (let [{:keys [started? recording? instrumented]} (:runtime state)
        target (:target state "")
        busy? (:busy? state)]
    (basic/scrollview
     {:scroll-bounds size :$body nil
      :body
      (ui/vertical-layout
     (eui/label (str "FlowStorm | " (if started? "Running" "Stopped")
                    " | " (if recording? "Recording" "Paused")))
     (ui/vertical-layout
      (applet/button {:text "Refresh" :size :small :disabled? busy?
                   :on-click #(vector [::perform $state :refresh nil])})
      (applet/button {:text "Start tracing" :size :small :disabled? (or busy? started?)
                   :on-click #(vector [::perform $state :start nil])})
      (applet/button {:text "Open debugger" :size :small :disabled? busy?
                   :on-click #(vector [::perform $state :open nil])})
      (applet/button {:text (if recording? "Pause" "Record") :size :small
                   :disabled? (or busy? (not started?))
                   :on-click #(vector [::perform $state (if recording? :pause :record) nil])})
      (applet/button {:text "Stop" :size :small :disabled? (or busy? (not started?))
                   :on-click #(vector [::perform $state :stop nil])}))
     (eui/field {:text target :$text [$state '(keypath :target)] :size :small
                      :placeholder "namespace/function"})
     (ui/vertical-layout
      (applet/button {:text "Instrument var" :size :small :disabled? (or busy? (not started?) (str/blank? target))
                   :on-click #(vector [::perform $state :instrument target])})
      (applet/button {:text "Uninstrument var" :size :small :disabled? (or busy? (not started?) (str/blank? target))
                   :on-click #(vector [::perform $state :uninstrument target])}))
     (when-let [error (:error state)] (eui/label error))
     (apply ui/vertical-layout (map #(eui/label (str %)) (sort instrumented))))})))

(defn flowstorm-applet [handler]
  (applet/tool-applet
   "FlowStorm" #'flowstorm-view {:target "" :runtime @runtime-state}
   (fn [$state]
     (let [key (random-uuid)]
       (add-watch runtime-state key (fn [_ _ _ state]
                                      (handler :update $state assoc :runtime state)))
       (handler :update $state assoc :runtime @runtime-state)
       #(remove-watch runtime-state key)))))
