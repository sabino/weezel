(ns com.phronemophobic.easel.flowstorm
  (:require [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.ui :as eui]
            [membrane.component :refer [defui defeffect]]
            [membrane.basic-components :as basic]
            [membrane.ui :as ui]))

(defonce runtime-state (atom {:started? false :recording? false :instrumented #{}}))

(defn api [sym]
  (try
    (or (requiring-resolve sym)
        (throw (ex-info (str "Missing FlowStorm API: " sym) {})))
    (catch java.io.FileNotFoundException e
      (throw (ex-info "Start Easel with the :flowstorm alias to enable FlowStorm" {} e)))))

(defn var-symbol [text]
  (let [sym (symbol (str/trim text))]
    (when-not (and (qualified-symbol? sym) (find-ns (symbol (namespace sym)))
                   (var? (resolve sym)))
      (throw (ex-info "Enter an existing qualified var, for example my.app/run" {:var text})))
    sym))

(defn perform! [action target]
  (locking runtime-state
    (case action
      :start (do ((api 'flow-storm.runtime.debuggers-api/start-runtime))
                 (swap! runtime-state assoc :started? true))
      :refresh nil
      :open (do ((api 'flow-storm.api/local-connect) {})
                (swap! runtime-state assoc :started? true :debugger-open? true))
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
                ((api 'flow-storm.api/uninstrument-var-clj) sym))
              (if (:debugger-open? @runtime-state)
                ((api 'flow-storm.api/stop))
                ((api 'flow-storm.runtime.debuggers-api/stop-runtime)))
              (reset! runtime-state {:started? false :recording? false :instrumented #{}})))
    (let [started? (boolean ((api 'flow-storm.runtime.debuggers-api/runtime-started?)))]
      (swap! runtime-state assoc :started? started?
             :recording? (and started? (boolean ((api 'flow-storm.tracer/recording?))))))))

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
