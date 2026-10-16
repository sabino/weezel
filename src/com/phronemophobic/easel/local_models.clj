(ns com.phronemophobic.easel.local-models
  (:require [clojure.java.io :as io]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.viscous :as viscous]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(defn generate! [path prompt emit! current?]
  (when-not (.isFile (io/file path)) (throw (ex-info "Choose an existing GGUF model" {})))
  (packages/with-runtime
   #(let [create (requiring-resolve 'com.phronemophobic.llama/create-context)
          generate (requiring-resolve 'com.phronemophobic.llama/generate)
          template (requiring-resolve 'com.phronemophobic.llama/chat-apply-template)]
      (with-open [context (create path {:n-ctx 2048})]
        (let [prompt (try (template context [{:role "user" :content prompt}])
                          (catch IllegalArgumentException _ prompt)
                          (catch UnsupportedOperationException _ prompt))]
          (transduce (comp (take 256) (take-while (fn [_] (and (current?) (not (.isInterrupted (Thread/currentThread)))))))
                     (completing (fn [output chunk]
                                   (let [output (str output chunk)]
                                     (emit! output)
                                     (if (> (count output) 16384) (reduced output) output))))
                     "" (generate context prompt)))))))

(defeffect ::generate [$state]
  (let [{:keys [model-path prompt runner busy?]} (dispatch! :get $state) runner @runner token (random-uuid)]
    (when-not busy?
      (locking runner
        (when (:active? @runner)
          (dispatch! :update $state assoc :stream-token token :output "")
          (swap! runner assoc :future
                 (applet/task! dispatch! $state
                   #(do (generate! model-path prompt
                                   (fn [output] (when (= token (:stream-token (dispatch! :get $state)))
                                                  (dispatch! :update $state assoc :output output)))
                                   (fn [] (= token (:stream-token (dispatch! :get $state))))) {}))))))))

(defeffect ::cancel [$state]
  (let [runner (some-> (dispatch! :get $state) :runner deref)]
    (dispatch! :update $state assoc :request nil :stream-token nil :busy? false)
    (when-let [task (:future @runner)] (future-cancel task))))

(defui model-view [{:keys [state size]}]
  (let [[w h] size width (max 1 (- w 40))]
    (basic/scrollview
     {:scroll-bounds size :$body nil
      :body (ui/padding 20 20
              (ui/vertical-layout
               (eui/text {:text "Local models" :size 24 :weight :semi-bold})
               (ui/spacer 0 12)
               (eui/text {:text "GGUF model" :color :color/secondary})
               (eui/field {:text (:model-path state "") :$text [$state '(keypath :model-path)] :width width :placeholder "/path/to/model.gguf"})
               (ui/spacer 0 16)
               (eui/field {:text (:prompt state "") :$text [$state '(keypath :prompt)] :width width
                           :placeholder "Prompt…" :on-submit #(vector [::generate $state])})
               (ui/spacer 0 12)
               (eui/row nil [(eui/button {:text "Generate" :variant :primary :disabled? (:busy? state)
                                          :on-click #(vector [::generate $state])})
                            (eui/button {:text "Cancel" :disabled? (not (:busy? state)) :on-click #(vector [::cancel $state])})])
               (ui/spacer 0 20)
               (when-let [error (:error state)] (eui/text {:text error :width width :color :color/error}))
               (eui/text {:text (:output state "") :width width :role :code})))})))

(defn model-applet [handler]
  (let [runner (atom {:active? true})]
    (assoc (applet/tool-applet "Local models" #'model-view
                              {:model-path "" :prompt "" :output "" :runner (viscous/wrap runner)}
                              (fn [_] (fn [] (locking runner
                                                (swap! runner assoc :active? false)
                                                (when-let [task (:future @runner)] (future-cancel task))))))
           :icon "deployment-unit")))
