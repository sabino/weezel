(ns com.phronemophobic.easel.help
  (:require [com.phronemophobic.easel.keymap :as keymap]
            [com.phronemophobic.easel.composition :as composition]
            [com.phronemophobic.easel.commands :as commands]
            [clojure.string :as str]
            [com.phronemophobic.easel.ui :as eui]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui]]
            [membrane.ui :as ui]))

(def version 2)

(def pages [[:start "Start here"] [:emacs "Keys and concepts"] [:shortcuts "Shortcuts"]])

(defn shortcut-groups
  ([] (shortcut-groups {}))
  ([easel]
   (let [descriptor (keymap/current easel)
         names (into {} (map (fn [[id value]] [id (:name value)]))
                     (commands/registry (composition/current easel) {:easel easel}))
         bindings (keymap/editor-keys easel)
         editor-actions {"split-pane" "Split the editor pane side by side."
                         "delete-pane" "Close this applet." "show-select-buffer" "Choose an open buffer."
                         "focus-next" "Focus the next pane." "close-other-panes" "Hide other panes."
                         "hide-pane" "Hide this pane; keep its applet running."
                         "eval-last" "Evaluate the form before the cursor." "eval-top" "Evaluate the surrounding top-level form."
                         "eval-buffer" "Evaluate the Clojure buffer."}]
     (vec (concat
           (for [[scope title] [[:global "Across Easel"] [:workspace "In the workspace, outside text input"]]]
             {:title title :rows (vec (for [[keys id] (sort-by key (get-in descriptor [:scopes scope])) :when id]
                                       [keys (get names id (str id))]))})
           [{:title "Editing text with Clobber"
            :rows (vec (for [[keys intent] (sort-by key bindings)
                             :when (not= "com.phronemophobic.easel.clobber" (namespace (keymap/binding-target intent)))]
                         [keys (keymap/binding-label intent)]))}
            {:title "Pane and evaluation bindings from this keymap"
             :rows (vec (for [[keys intent] (sort-by key bindings)
                              :when (= "com.phronemophobic.easel.clobber" (namespace (keymap/binding-target intent)))]
                          [keys (get editor-actions (name (keymap/binding-target intent)) (keymap/binding-label intent))]))}])))))

(defn action-keys [easel action]
  (str/join " / " (keymap/shortcuts (keymap/current easel) [:editor :clojure] action)))

(defui paragraph [{:keys [text width]}]
  (ui/padding 0 5 (eui/text {:text text :width width :color :color/secondary})))

(defui introduction-page [{:keys [width page easel]}]
  (case page
    :start
    (ui/vertical-layout
     (eui/text {:text "A workspace of your own" :size 26 :weight :semi-bold :width width})
     (paragraph {:width width :text "Easel opens empty. Add the tools you need, arrange their panes, and keep working in the same live Clojure process."})
     (ui/spacer 0 18)
     (eui/text {:text "Everything here is also clickable" :weight :semi-bold :width width})
     (paragraph {:width width :text "Use Add applet or command search to open a tool. Tools manages packages, toolsets, and themes. Each pane's ⋯ menu contains its layout and close actions."})
     (paragraph {:width width :text "The dock holds the REPL, values, logs, and terminal. Its tools start only when you choose them. Find saved workspaces in command search or Tools. Examples are optional presets."})
     (ui/spacer 0 18)
     (eui/text {:text "Try a short feedback loop" :weight :semi-bold :width width})
     (paragraph {:width width :text (str "Open a Clojure buffer, type (+ 1 2), and place the cursor after the closing parenthesis. "
                                        (let [keys (action-keys easel :com.phronemophobic.easel.clobber/eval-last)]
                                          (if (seq keys) (str "Press " keys " to evaluate it. The result is 3. Change the form and evaluate it again.")
                                              "The evaluate-form action is unbound in this keymap; choose a binding in Tools → Keymaps.")))})
     (eui/wrap-row width
       [(eui/button {:text "Open a blank editor" :icon "file-text"
                     :on-click #(vector [:com.phronemophobic.easel/finish-help]
                                        [:com.phronemophobic.easel/new-editor])})
        (eui/button {:text "Open REPL" :icon "code"
                     :on-click #(vector [:com.phronemophobic.easel/finish-help]
                                        [:com.phronemophobic.easel/select-dock :repl])})
        (eui/button {:text "Tools and themes" :icon "appstore"
                     :on-click #(vector [:com.phronemophobic.easel/finish-help]
                                        [:com.phronemophobic.easel/open-tools])})])
     (ui/spacer 0 18)
     (eui/text {:text "Compose the environment" :weight :semi-bold :width width})
     (paragraph {:width width :text "Minimal keeps the workspace free of sidebars and status bars. Studio adds them as an optional composition. Compositions describe views and layout in EDN; themes describe appearance; keymaps describe scoped commands. Switching them keeps your applets running."})
     (eui/wrap-row width
       [(eui/button {:text "Choose a composition" :on-click #(vector [:com.phronemophobic.easel/finish-help]
                                                                     [:com.phronemophobic.easel/manage-packages :composition])})
        (eui/button {:text "Choose a keymap" :on-click #(vector [:com.phronemophobic.easel/finish-help]
                                                                [:com.phronemophobic.easel/manage-packages :keymap])})])
     (ui/spacer 0 20)
     (paragraph {:width width :text (str "Return to this guide with "
                                        (or (keymap/shortcut (keymap/current easel) :easel/help) "the pane menu")
                                        ". Keys and concepts explains the notation; Shortcuts shows the selected keymap.")}))

    :emacs
    (ui/vertical-layout
     (eui/text {:text "Read the keys, then try one" :size 26 :weight :semi-bold :width width})
     (paragraph {:width width :text "Clobber uses Emacs-style editing. A few names and key conventions make it much easier to learn; the rest of Easel still has clickable controls."})
     (ui/spacer 0 14)
     (eui/text {:text "C means Control. M means Meta." :weight :semi-bold :width width})
     (paragraph {:width width :text "C-x means hold Control and press x. On a Mac, this is the Control key, not Command (⌘). M-x means hold Option (Alt) and press x. C-M-x means hold Control and Option together, then press x."})
     (eui/text {:text "A space separates two key presses" :weight :semi-bold :width width})
     (paragraph {:width width :text (str "Spaces separate successive keystrokes. The current evaluate-form binding is "
                                        (let [keys (action-keys easel :com.phronemophobic.easel.clobber/eval-last)] (if (seq keys) keys "unbound"))
                                        ". A multi-key binding waits for its next keystroke; release and press each chord in order.")})
     (ui/spacer 0 14)
     (eui/text {:text "Buffer, pane, and applet" :weight :semi-bold :width width})
     (paragraph {:width width :text "A buffer is text held in memory. It is saved to disk only when you save it. A pane is one area of the workspace; Emacs calls this a window. An applet is a running tool that occupies a pane. Hiding a pane keeps its applet; closing the applet stops it."})
     (eui/text {:text "Point, mark, and kill ring" :weight :semi-bold :width width})
     (paragraph {:width width :text (let [bindings (keymap/editor-keys easel)
                                         hint (fn [action] (let [keys (str/join " / " (keymap/editor-shortcuts bindings action))]
                                                             (if (seq keys) keys "unbound")))]
                                     (str "Point is the cursor. Mark is the other end of a selection ("
                                          (hint 'com.phronemophobic.clobber.modes.text/editor-set-mark)
                                          "); moving the cursor then selects a region. Kill means cut, and yank means paste ("
                                          (hint 'com.phronemophobic.clobber.modes.text/editor-yank)
                                          "). The kill ring holds text cut or copied by the editor."))})
     (ui/spacer 0 14)
     (eui/text {:text "Commands and the live REPL" :weight :semi-bold :width width})
     (paragraph {:width width :text (str "The active command-search bindings are "
                                        (let [keys (str/join " / " (keymap/shortcuts (keymap/current easel) :easel/commands))]
                                          (if (seq keys) keys "unbound"))
                                        ". Workspace bindings apply outside text input. Editor bindings keep their editing actions. "
                                        "Shortcuts lists the effective bindings, including live changes. Clojure buffers and the REPL evaluate in the running process.")})
     (paragraph {:width width :text "Start with ordinary typing, arrow keys, and your mouse. Then learn one command at a time: open, save, evaluate, and cancel are enough to begin."}))))

(defui shortcut-group [{:keys [title rows width]}]
  (let [stacked? (< width 560)
        key-width (min 152 (* width 0.35))]
    (ui/vertical-layout
     (eui/text {:text title :weight :semi-bold :width width})
     (ui/spacer 0 8)
     (apply ui/vertical-layout
       (for [[keys description] rows]
         (ui/padding 0 5
           (if stacked?
             (ui/vertical-layout
              (eui/text {:text keys :role :code :color :color/accent :width width})
              (eui/text {:text description :width width :color :color/secondary}))
             (ui/horizontal-layout
              (ui/fixed-bounds [key-width 23]
                (eui/text {:text keys :role :code :color :color/accent}))
              (eui/text {:text description :width (max 1 (- width key-width)) :color :color/secondary}))))))
     (ui/spacer 0 20))))

(defui help-view [{:keys [help easel width height]}]
  (let [padding (min 36 (* width 0.06))
        content-width (max 1 (min 820 (- width (* 2 padding))))
        page (:page help :start)
        header (ui/vertical-layout
                (eui/text {:text "Getting started" :size 12 :color :color/secondary})
                (ui/spacer 0 8)
                (eui/wrap-row content-width
                  (for [[id title] pages]
                    (eui/button {:text title :variant :text :selected? (= page id)
                                  :on-click #(vector [:com.phronemophobic.easel/help-page id])}))))
        footer (eui/wrap-row content-width
                 [(eui/button {:text (if (:first-run? help) "Start with an empty workspace" "Back to workspace")
                               :variant :primary
                               :on-click #(vector [:com.phronemophobic.easel/finish-help])})
                  (eui/button {:text "Open documentation" :icon "book"
                               :on-click #(vector [:com.phronemophobic.easel/finish-help]
                                                  [:com.phronemophobic.easel/open-documentation])})])
        header-height (min height (+ padding (ui/height header) 18))
        footer-height (min (max 0 (- height header-height)) (+ (ui/height footer) 24))
        body-height (max 0 (- height header-height footer-height))
        content (if (= page :shortcuts)
                  (apply ui/vertical-layout
                    (for [group (shortcut-groups easel)]
                      (shortcut-group (assoc group :width content-width))))
                  (introduction-page {:page page :width content-width :easel easel}))]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height)
       (ui/translate padding padding header)
       (ui/translate 0 header-height
         (basic/scrollview
          {:scroll-bounds [width body-height] :offset (:offset help [0 0]) :$offset [$help '(keypath :offset)]
           :$body nil :body (ui/padding padding 12 content)}))
       (when (pos? footer-height)
         (ui/translate 0 (+ header-height body-height)
           [(eui/rule context width)
            (ui/translate padding 12 footer)]))
       (eui/border context width height)])))
