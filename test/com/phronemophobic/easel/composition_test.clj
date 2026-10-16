(ns com.phronemophobic.easel.composition-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.composition :as composition]
            [com.phronemophobic.easel.commands :as commands]
            [com.phronemophobic.easel.help-test :as fixtures]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.term :as term]
            [com.phronemophobic.easel.theme :as theme]
            [membrane.component :as component]
            [membrane.ui :as ui]))

(defn await-value [f]
  (loop [n 500]
    (or (f) (when (pos? n) (Thread/sleep 10) (recur (dec n))))))

(defn isolated [f]
  (fixtures/isolated-workspace
   #(try (f) (finally (packages/suspend! easel/handler)))))

(defn badge [{:keys [width height text]}]
  (ui/fixed-bounds [width height] (ui/label text)))

(deftest composition-trees-allocate-only-the-declared-views
  (doseq [id [:easel/minimal :easel/studio]
          width [0 120 600 900 1536] height [0 80 400 1024]
          dock? [true false]]
    (let [descriptor (composition/resolve-composition (composition/builtin id))
          frames (composition/layout descriptor [width height]
                                      {:easel {:dock (when dock? {})} :appearance @theme/default-theme})]
      (doseq [frame frames]
        (is (every? #(and (number? %) (Double/isFinite (double %)) (not (neg? %)))
                    ((juxt :x :y :width :height) frame)))
        (is (<= (+ (:x frame) (:width frame)) (+ width 0.00001)))
        (is (<= (+ (:y frame) (:height frame)) (+ height 0.00001))))))
  (let [frames (composition/layout @composition/default-composition [1000 700] {:easel {}})]
    (is (= [0 0 1000 700] ((juxt :x :y :width :height) (composition/region frames :workspace))))
    (is (not-any? #(#{:sidebar :topbar :status :dock} (:role %)) frames))))

(deftest imported-compositions-can-use-arbitrary-view-vars-and-commands
  (isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [file (settings/file "composition.edn")
           descriptor {:composition/version 1 :id :test/custom :name "Custom" :extends :easel/minimal
                       :commands [{:id :test/change :name "Change label"
                                   :intents [[:set [:easel :root-pane :test/value] 42]]}]
                       :root {:id :custom :layout :row
                              :children [{:id :badge :view 'com.phronemophobic.easel.composition-test/badge
                                          :width 90 :props {:text "Custom view"}}
                                         {:id :workspace :role :workspace :layout :stack
                                          :children [{:id :body :view 'com.phronemophobic.easel.shell/panes-view}]}]}}]
       (settings/write-data! file descriptor)
       (is (= [:test/custom] (take-last 1 (:installed (packages/import! (.getPath file))))))
       (packages/select-composition! easel/handler :test/custom true)
       (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)]
         (is (= [1536 1024] (ui/bounds (app))))
         (is (= 90 (:x (composition/region (get-in @easel/app-state [:easel :composition/frames]) :workspace))))
         (is (some #(= "Custom view" (:text %)) (fixtures/nodes (app)))))
       (easel/handler ::easel/invoke-command :test/change nil)
       (is (= 42 (get-in @easel/app-state [:easel :root-pane :test/value])))
       (is (= descriptor (edn/read-string (pr-str descriptor))))
       (is (= :test/custom (:composition (settings/read-settings))))))))

(deftest changing-compositions-preserves-editor-and-terminal-resources
  (isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (easel/handler ::easel/new-editor)
     (is (fixtures/await-applets))
     (easel/handler ::easel/add-applet {:make-applet #(term/termlet % {:command ["/bin/sleep" "30"]})})
     (is (fixtures/await-applets))
     (let [before (get-in @easel/app-state [:easel :applets])
           editor (some #(when (get-in % [:state :editor]) %) (vals before))
           terminal (some #(when (:resources %) %) (vals before))
           root (get-in @easel/app-state [:easel :root-pane])
           app (component/make-app #'easel/easel-view easel/app-state easel/handler)]
       (is (await-value #(some-> terminal :resources deref :pty)))
       (ui/clipboard-paste (app) "(+ 40 2)")
       (let [rope (get-in @easel/app-state [:easel :applets (:id editor) :state :editor :rope])
             resources @(:resources terminal)
             focus (get-in @easel/app-state [:membrane.component/context :focus])]
         (swap! easel/app-state assoc-in [:easel :applets (:id editor) :state :editor :key-bindings "C-c z"]
                :com.phronemophobic.easel.clobber/eval-top)
         (doseq [id [:easel/studio :easel/minimal :easel/studio :easel/minimal]]
           (packages/select-composition! easel/handler id false)
           (let [after (get-in @easel/app-state [:easel :applets])]
             (is (= (set (keys before)) (set (keys after))))
             (is (= (mapv :applet-id (:panes root)) (mapv :applet-id (get-in @easel/app-state [:easel :root-pane :panes]))))
             (is (identical? rope (get-in after [(:id editor) :state :editor :rope])))
             (is (identical? (:resources terminal) (:resources (get after (:id terminal)))))
             (is (= resources @(:resources terminal)))
             (is (= :com.phronemophobic.easel.clobber/eval-top
                    (get-in after [(:id editor) :state :editor :key-bindings "C-c z"])))
             (is (= focus (get-in @easel/app-state [:membrane.component/context :focus])))
             (fixtures/check-view app))))))))

(deftest keymaps-route-by-scope-and-update-running-editors
  (isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)]
       (easel/handler ::easel/open-tools)
       (is (fixtures/await-applets))
       (is (some #(= "Sources" (:text %)) (fixtures/nodes (app))))
       (is (some #(= "Compositions" (:text %)) (fixtures/nodes (app))))
       (easel/handler ::easel/close-tools)
       (ui/key-event (app) 75 0 :press ui/CONTROL-MASK)
       (is (some? (get-in @easel/app-state [:easel :commands])))
       (ui/key-event (app) 256 0 :press 0)
       (easel/handler ::easel/toggle-pane-resize {:$easel '(keypath :easel) :pane-id ::easel/root-pane})
       (let [old (app)]
         (ui/key-event old 256 0 :press 0)
         (ui/key-press old :escape)
         (is (not (get-in @easel/app-state [:easel :root-pane ::easel/pane-resize]))))
       (easel/handler ::easel/new-editor)
       (is (fixtures/await-applets))
       (ui/clipboard-paste (app) "keep me")
       (ui/key-event (app) 65 0 :press ui/CONTROL-MASK)
       (ui/key-event (app) 75 0 :press ui/CONTROL-MASK)
       (is (nil? (get-in @easel/app-state [:easel :commands])))
       (is (= "" (str (get-in (some #(when (get-in % [:state :editor]) %) (vals (get-in @easel/app-state [:easel :applets]))) [:state :editor :rope]))))
       (ui/key-event (app) 88 0 :press ui/ALT-MASK)
       (ui/key-press (app) "≈")
       (is (= "" (get-in @easel/app-state [:easel :commands :query])))
       (ui/key-event (app) 88 0 :release ui/ALT-MASK)
       (ui/key-press (app) "REPL")
       (is (= "REPL" (get-in @easel/app-state [:easel :commands :query])))
       (is (some? (get-in @easel/app-state [:easel :commands])))
       (ui/key-event (app) 256 0 :press 0)
       (let [file (settings/file "keys.edn")
             descriptor {:keymap/version 1 :id :test/keys :name "Personal keys" :extends :easel/default-keys
                         :scopes {:global {"M-x" nil "C-c h" :easel/help}
                                  :clojure {"C-x C-e" nil "C-c e" :com.phronemophobic.easel.clobber/eval-last}}}]
         (settings/write-data! file descriptor)
         (packages/import! (.getPath file))
         (packages/select-keymap! easel/handler :test/keys true)
         (let [bindings (get-in (some #(when (get-in % [:state :editor]) %) (vals (get-in @easel/app-state [:easel :applets]))) [:state :editor :key-bindings])]
           (is (not (contains? bindings "C-x C-e")))
           (is (= :com.phronemophobic.easel.clobber/eval-last (get bindings "C-c e"))))
         (ui/key-event (app) 67 0 :press ui/CONTROL-MASK)
         (ui/key-event (app) 341 0 :press ui/CONTROL-MASK)
         (ui/key-event (app) 341 0 :release 0)
         (ui/key-event (app) 72 0 :press 0)
         (is (= :start (get-in @easel/app-state [:easel :help :page])))
         (easel/handler ::easel/finish-help)
         (packages/select-composition! easel/handler :easel/studio)
         (is (= :test/keys (get-in @easel/app-state [:easel :keymap :id])))
         (packages/select-keymap! easel/handler :easel/default-keys)
         (let [bindings (get-in (some #(when (get-in % [:state :editor]) %) (vals (get-in @easel/app-state [:easel :applets]))) [:state :editor :key-bindings])]
           (is (not (contains? bindings "C-c e")))
           (is (contains? bindings "C-x C-e"))))))))

(deftest invalid-live-edits-keep-the-last-valid-composition
  (isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [file (settings/file "composition.edn")
           descriptor {:composition/version 1 :id :test/live :name "Live" :extends :easel/minimal}]
       (settings/write-data! file descriptor)
       (packages/import! (.getPath file))
       (packages/select-composition! easel/handler :test/live true)
       (spit file "{:composition/version")
       (is (await-value #(get-in @easel/app-state [:easel :configuration-error])))
       (is (= :test/live (get-in @easel/app-state [:easel :composition :id])))
       (is (= descriptor (get-in (settings/read-settings) [:packages :test/live :data])))
       (settings/write-data! file (assoc-in descriptor [:panes :header :height] :metric/missing))
       (is (await-value #(some-> (get-in @easel/app-state [:easel :configuration-error])
                                 (.contains "finite number"))))
       (is (= descriptor (get-in (settings/read-settings) [:packages :test/live :data])))
       (settings/write-data! file (assoc descriptor :name "Edited live"))
       (is (await-value #(= "Edited live" (get-in @easel/app-state [:easel :composition :name]))))
       (is (nil? (get-in @easel/app-state [:easel :configuration-error])))
       (let [before (:easel @easel/app-state)]
         (settings/update-settings! assoc-in [:packages :test/bad]
                                    (packages/normalize {:composition/version 1 :id :test/bad :name "Bad" :extends :easel/minimal
                                                         :panes {:header {:height 40 :view 'missing.namespace/view}}}))
         (is (thrown? java.io.FileNotFoundException (packages/select-composition! easel/handler :test/bad)))
         (is (= before (:easel @easel/app-state))))))))

(deftest command-registry-stays-readable-as-data
  (isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [easel (:easel @easel/app-state)
           registry (commands/registry (composition/current easel) (easel/command-environment easel {}))]
       (is (= registry (edn/read-string (pr-str registry))))))))

(deftest live-keymap-edits-and-presentation-selection-persist-independently
  (isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [file (settings/file "keys.edn")
           descriptor {:keymap/version 1 :id :test/live-keys :name "Live keys" :extends :easel/default-keys
                       :scopes {:global {"M-q" :easel/help}}}]
       (settings/write-data! file descriptor)
       (packages/import! (.getPath file))
       (packages/select-keymap! easel/handler :test/live-keys true)
       (spit file "{:keymap/version")
       (is (await-value #(get-in @easel/app-state [:easel :configuration-error])))
       (is (= :test/live-keys (get-in @easel/app-state [:easel :keymap :id])))
       (settings/write-data! file (assoc descriptor :name "New keys"))
       (is (await-value #(= "New keys" (get-in @easel/app-state [:easel :keymap :name]))))
       (packages/select-composition! easel/handler :easel/studio true)
       (packages/select-theme! easel/handler :easel/studio-dark true)
       (packages/suspend! easel/handler)
       (easel/remove-all-widgets!)
       (is (fixtures/await-applets))
       (reset! easel/app-state nil)
       (easel/initialize! {:preset :easel/empty})
       (is (= :easel/studio (get-in @easel/app-state [:easel :composition :id])))
       (is (= :test/live-keys (get-in @easel/app-state [:easel :keymap :id])))
       (is (= :easel/studio-dark (get-in @easel/app-state [:easel :appearance :id])))
       (is (empty? (get-in @easel/app-state [:easel :applets])))
       (packages/select-theme! easel/handler :easel/studio-light)))))

(deftest descriptors-reject-ambiguous-or-missing-parts
  (doseq [overrides [{:root {:id :root :layout :stack :children []}}
                    {:root {:id :root :role :workspace :layout :row
                            :children [{:id :root :spacer? true}]}}
                    {:panes {:header {:view 'membrane.ui/label :height -1}}}
                    {:commands [{:id :test/no-name :intents []}]}
                    {:dock {:items [{:id :tool :label "Tool"}]}}]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (composition/resolve-composition (merge {:composition/version 1 :id :test/bad :name "Bad"
                                                           :extends :easel/minimal} overrides)))))
  (is (= 28 (get-in (composition/resolve-composition {:composition/version 1 :id :test/compact :name "Compact"
                                                      :extends :easel/minimal :panes {:header {:height 28}}})
                    [:panes :header :height])))
  (let [a {:composition/version 1 :id :test/a :name "A" :extends :test/b}
        b (assoc a :id :test/b :extends :test/a)]
    (is (thrown? clojure.lang.ExceptionInfo (composition/resolve-composition a {:test/a a :test/b b})))))

(deftest incompatible-presentation-tokens-keep-the-working-theme
  (isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [file (settings/file "theme.edn") profile-file (settings/file "composition.edn")
           theme {:theme/version 1 :id :test/spacing :name "Spacing" :extends :easel/studio-light
                  :tokens {:metric/custom-gap 18}}
           profile (-> (composition/builtin :easel/minimal)
                       (assoc :id :test/spacing-layout :name "Spaced")
                       (assoc-in [:root :padding] :metric/custom-gap))]
       (settings/write-data! file theme)
       (packages/import! (.getPath file))
       (packages/select-theme! easel/handler :test/spacing true)
       (settings/write-data! profile-file profile)
       (packages/import! (.getPath profile-file))
       (packages/select-composition! easel/handler :test/spacing-layout true)
       (let [before (:easel @easel/app-state)]
         (is (thrown? clojure.lang.ExceptionInfo (packages/select-theme! easel/handler :easel/studio-dark true)))
         (is (= before (:easel @easel/app-state)))
         (is (= :test/spacing (:id (theme/current)))))
       (packages/select-composition! easel/handler :easel/minimal)
       (packages/select-theme! easel/handler :easel/studio-light)))))
