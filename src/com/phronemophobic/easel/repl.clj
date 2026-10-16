(ns com.phronemophobic.easel.repl
  (:require [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.documentation :as documentation]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.viscous :as viscous]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(defeffect ::evaluate [$state]
  (let [{:keys [source ns entries busy? ready? runner]} (dispatch! :get $state)]
    (when (and ready? (not busy?) (seq (clojure.string/trim source)))
      (let [runner @runner]
        (locking runner
          (when (:active? @runner)
            (reset! runner
                    {:active? true
                     :future (applet/task!
                              dispatch! $state
                              #(let [result (documentation/run-example source ns)]
                                 {:source ""
                                  :entries (events/bounded-conj entries (assoc result :source source) 100)
                                  :history-index nil}) )})))))))

(defeffect ::cancel [$state]
  (let [state (dispatch! :get $state)]
    (dispatch! :update $state assoc :request nil :busy? false)
    (when-let [task (:future @@(:runner state))] (future-cancel task))))

(defui repl-view [{:keys [state size]}]
  (let [[w h] size
        prompt (eui/text {:text (str (:ns state) "> ") :role :code})
        cancel (when (:busy? state)
                 (eui/button {:text "Cancel" :variant :link :size :small
                              :on-click #(vector [::cancel $state])}))
        input (ui/padding 20 8
                (eui/row nil
                  [prompt
                   (eui/field {:text (:source state "") :$text [$state '(keypath :source)]
                               :placeholder "Evaluate a form…" :role :code :variant :bare
                               :disabled? (:busy? state) :height 28
                               :width (max 1 (- w 40 (ui/width prompt) (if cancel (+ 8 (ui/width cancel)) 0)))
                               :on-submit #(vector [::evaluate $state])})
                   (when cancel (ui/spacer 8 0))
                   cancel] 0))
        history (apply ui/vertical-layout
                       (for [entry (:entries state)]
                         (ui/vertical-layout
                          (eui/code {:text (str (:ns state) "> " (:source entry)) :width (max 1 (- w 40))})
                          (ui/on :mouse-down
                            (fn [_]
                              [[:com.phronemophobic.easel/add-applet
                                {:make-applet #(inspector/inspector-applet % @(or (:value entry) (:exception entry)))}]])
                            (eui/text {:text (str "⇒ " (:summary entry)) :role :code
                                        :color (if (:exception entry) :color/error :color/secondary)
                                        :width (max 1 (- w 40))}))
                          (when (seq (:out entry)) (eui/text {:text (:out entry) :role :code :size 12}))
                          (when (seq (:err entry)) (eui/text {:text (:err entry) :role :code :size 12 :color :color/error}))
                          (ui/spacer 0 16))))]
    (ui/vertical-layout
     (basic/scrollview
      {:scroll-bounds [(max 0 w) (max 0 (- h (ui/height input)))] :$body nil
       :body (ui/padding 20 12 history)})
     input)))

(defn repl-applet
  ([handler] (repl-applet handler 'user))
  ([handler ns-sym]
   (let [runner (atom {:active? true})]
     (assoc
      (applet/tool-applet "REPL" #'repl-view
        {:ns ns-sym :source "" :entries [] :runner (viscous/wrap runner) :ready? false}
        (fn [$state]
          (handler :update $state assoc :ready? true)
          (fn []
            (locking runner
              (when-let [task (:future @runner)] (future-cancel task))
              (reset! runner {:active? false})))))
      :icon "code"))))
