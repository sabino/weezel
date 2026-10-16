(ns com.phronemophobic.easel.keyboard-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.clobber :as clobber]
            [com.phronemophobic.easel.composition-test :as fixtures]
            [com.phronemophobic.easel.help :as help]
            [com.phronemophobic.easel.help-test :as ui-fixtures]
            [com.phronemophobic.easel.documentation :as documentation]
            [com.phronemophobic.easel.keymap :as keymap]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.settings :as settings]
            [membrane.component :as component]
            [membrane.ui :as ui]))

(defn press [app code mods]
  (ui/key-event (app) code 0 :press mods)
  (ui/key-event (app) code 0 :release mods))

(defn editor! [app source]
  (easel/handler ::easel/new-editor)
  (is (ui-fixtures/await-applets))
  (ui/clipboard-paste (app) source)
  (press app 69 ui/CONTROL-MASK)
  (get-in @easel/app-state [:membrane.component/context :focus]))

(deftest easel-keeps-the-original-pane-and-evaluation-bindings
  (let [keys (keymap/builtin :easel/default-keys)]
    (is (= {"C-x 3" ::clobber/split-pane "C-x k" ::clobber/delete-pane
            "C-x b" ::clobber/show-select-buffer "C-x o" ::clobber/focus-next
            "C-x 1" ::clobber/close-other-panes "C-x 0" ::clobber/hide-pane}
           (get-in keys [:scopes :editor])))
    (is (= {"C-x C-e" ::clobber/eval-last "C-M-x" ::clobber/eval-top "C-c C-k" ::clobber/eval-buffer}
           (get-in keys [:scopes :clojure]))))
  (doseq [[label chords] [[:last [[88 ui/CONTROL-MASK] [69 ui/CONTROL-MASK]]]
                          [:top [[88 (bit-or ui/CONTROL-MASK ui/ALT-MASK)]]]
                          [:buffer [[67 ui/CONTROL-MASK] [75 ui/CONTROL-MASK]]]]]
    (testing (name label)
      (fixtures/isolated
       (fn []
         (easel/initialize! {:preset :easel/empty})
         (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)
               id (editor! app "(+ 20 22)")
               values #(some-> (get-in @easel/app-state [:easel :applets id :state :editor :line-val]) vals first vals first deref)]
           (doseq [[key mods] chords] (press app key mods))
           (is (fixtures/await-value #(= 42 (values))))))))))

(deftest original-pane-shortcuts-run-through-the-selected-editor
  (fixtures/isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)
           first-id (editor! app "(+ 1 2)")
           chord (fn [key] (press app 88 ui/CONTROL-MASK) (press app key 0))
           pane-ids #(keep :applet-id (tree-seq :panes :panes (get-in @easel/app-state [:easel :root-pane])))]
       (chord 51)
       (is (ui-fixtures/await-applets))
       (is (= 2 (count (pane-ids))))
       (chord 79)
       (let [second-id (get-in @easel/app-state [:membrane.component/context :focus])]
         (is (not= first-id second-id))
         (is (contains? (get-in @easel/app-state [:easel :applets]) second-id))
         (chord 66)
         (is (some? (get-in @easel/app-state [:easel :applets second-id ::clobber/buffer-select-state])))
         (press app 71 ui/CONTROL-MASK)
         (is (nil? (get-in @easel/app-state [:easel :applets second-id ::clobber/buffer-select-state])))
         (chord 49)
         (is (= [second-id] (vec (pane-ids))))
         (chord 51)
         (is (ui-fixtures/await-applets))
         (chord 48)
         (is (= 1 (count (pane-ids))))
         (let [id (first (pane-ids))]
           (easel/handler ::easel/focus-applet id)
           (chord 75)
           (is (not (contains? (get-in @easel/app-state [:easel :applets]) id)))))))))

(deftest help-and-running-editors-use-the-live-effective-bindings
  (fixtures/isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)
           id (editor! app "(+ 20 22)")
           file (settings/file "keys.edn")
           descriptor {:keymap/version 1 :id :test/live-help :name "Live help" :extends :easel/default-keys
                       :scopes {:global {"M-x" nil "F1" nil "M-q" :easel/commands "C-c h" :easel/help}
                                :editor {"C-a" nil "C-c a" 'com.phronemophobic.clobber.modes.text/editor-move-beginning-of-line}
                                :clojure {"C-x C-e" nil "C-c e" ::clobber/eval-last}}}]
       (press app 88 ui/CONTROL-MASK)
       (settings/write-data! file descriptor)
       (packages/import! (.getPath file))
       (packages/select-keymap! easel/handler :test/live-help false false)
       (press app 51 0)
       (is (= 1 (count (get-in @easel/app-state [:easel :applets]))))
       (press app 259 0)
       (press app 67 ui/CONTROL-MASK) (press app 69 0)
       (is (fixtures/await-value #(some-> (get-in @easel/app-state [:easel :applets id :state :editor :line-val]) seq)))
       (let [rows (mapcat :rows (help/shortcut-groups (:easel @easel/app-state)))
             shortcuts (set (map first rows))]
         (is (every? shortcuts ["C-c a" "C-c e" "C-c h" "M-q"]))
         (is (not-any? shortcuts ["C-a" "C-x C-e" "F1" "M-x"]))
         (let [state (assoc (documentation/initial-state) :target {:kind :guide :id :repl})
               view (documentation/target-view {:state state :width 800
                                                 :context {::keymap/value (keymap/current (:easel @easel/app-state))}})
               paragraphs (keep :paragraph (ui-fixtures/nodes view))]
           (is (some #(and (string? %) (str/includes? % "preceding form with C-c e")) paragraphs))
           (is (not-any? #(and (string? %) (str/includes? % "C-x C-e")) paragraphs)))
         (let [texts (keep #(or (:text %) (:paragraph %))
                          (ui-fixtures/nodes (help/introduction-page {:page :start :width 800 :easel (:easel @easel/app-state)})))
               text (str/join " " texts)]
           (is (str/includes? text "C-c e"))
           (is (not (str/includes? text "C-x C-e")))))))))

(deftest workspace-shortcuts-respect-text-input-ownership
  (let [id (random-uuid)
        state {:applets {id {:package/id :easel/inspector}}}]
    (is (= [:global :workspace :easel/inspector] (keymap/scopes state {:focus id})))
    (is (= [:global :easel/inspector] (keymap/scopes (assoc-in state [:applets id :keymap/input?] true) {:focus id})))
    (is (= [:global] (keymap/scopes state {:focus [:inspector :filter]})))))

(deftest pending-prefixes-stay-with-their-editor-and-reset-on-global-keymap-changes
  (fixtures/isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)
           id (editor! app "(+ 20 22)")
           file (settings/file "global-keys.edn")
           descriptor {:keymap/version 1 :id :test/global-prefix :name "Global prefix" :extends :easel/default-keys
                       :scopes {:global {"C-c h" :easel/help}}}]
       (settings/write-data! file descriptor)
       (packages/import! (.getPath file))
       (packages/select-keymap! easel/handler :test/global-prefix false false)
       (press app 67 ui/CONTROL-MASK)
       (easel/handler :set [:membrane.component/context :focus] [:test/field])
       (press app 72 0)
       (is (some? (get-in @easel/app-state [:easel :help])))
       (is (= #{id} (set (keys (get-in @easel/app-state [:easel :applets])))))
       (is (= {} (get-in @easel/app-state [:easel :applets id :extra])))
       (easel/handler ::easel/finish-help)
       (easel/handler ::easel/focus-applet id)
       (press app 67 ui/CONTROL-MASK)
       (packages/select-keymap! easel/handler :easel/default-keys false false)
       (press app 75 ui/CONTROL-MASK)
       (is (empty? (get-in @easel/app-state [:easel :applets id :state :editor :line-val])))
       (is (= "(+ 20 22)" (str (get-in @easel/app-state [:easel :applets id :state :editor :rope]))))))))

(deftest native-special-keys-run-a-command-once
  (fixtures/isolated
   (fn []
     (easel/initialize! {:preset :easel/empty})
     (easel/handler :update :easel assoc :keymap
                    (keymap/resolve-keymap {:keymap/version 1 :id :test/function-key :name "Function key"
                                           :extends :easel/default-keys :scopes {:global {"F1" :easel/toggle-dock}}}))
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)]
       (ui/key-event (app) 290 0 :press 0)
       (ui/key-press (app) :f1)
       (is (false? (boolean (get-in @easel/app-state [:easel :dock :hidden?]))))
       (is (some? (get-in @easel/app-state [:easel :dock])))
       (ui/key-event (app) 290 0 :release 0)
       (ui/key-event (app) 290 0 :press 0)
       (ui/key-press (app) :f1)
       (is (true? (get-in @easel/app-state [:easel :dock :hidden?])))))))
