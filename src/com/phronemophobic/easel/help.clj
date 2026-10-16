(ns com.phronemophobic.easel.help
  (:require [com.phronemophobic.easel.keymap :as keymap]
            [com.phronemophobic.easel.commands :as commands]
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
         names (into {} (map (fn [[id value]] [id (:name value)])) @commands/builtin)
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
             :rows [["C-x C-f / C-x C-s" "Open / save a file."] ["C-s / C-r" "Search forward / backward."]
                    ["C-a / C-e" "Start / end of line."] ["C-k" "Cut to the end of the line. M-x opens Easel commands from the editor."]
                    ["C-SPC" "Set a mark, then move to select text."] ["C-w / M-w / C-y" "Cut / copy / yank from the kill ring."]
                    ["C-g / C-_" "Cancel an editor command / undo."]]}
            {:title "Pane and evaluation bindings from this keymap"
             :rows (vec (for [scope [:editor :clojure] [keys intent] (sort-by key (get-in descriptor [:scopes scope])) :when intent]
                          [keys (get editor-actions (name intent) (str intent))]))}])))))

(defui paragraph [{:keys [text width]}]
  (ui/padding 0 5 (eui/text {:text text :width width :color :color/secondary})))

(defui introduction-page [{:keys [width page]}]
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
     (paragraph {:width width :text "Open a Clojure buffer, type (+ 1 2), and place the cursor after the closing parenthesis. Press Control+x, then Control+e. The result is 3. Change the form and evaluate it again."})
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
     (paragraph {:width width :text "Return to this guide with F1 or the pane menu. Keys and concepts explains the notation; Shortcuts shows the selected keymap."}))

    :emacs
    (ui/vertical-layout
     (eui/text {:text "Read the keys, then try one" :size 26 :weight :semi-bold :width width})
     (paragraph {:width width :text "Clobber uses Emacs-style editing. A few names and key conventions make it much easier to learn; the rest of Easel still has clickable controls."})
     (ui/spacer 0 14)
     (eui/text {:text "C means Control. M means Meta." :weight :semi-bold :width width})
     (paragraph {:width width :text "C-x means hold Control and press x. On a Mac, this is the Control key, not Command (⌘). M-x means hold Option (Alt) and press x. C-M-x means hold Control and Option together, then press x."})
     (eui/text {:text "A space separates two key presses" :weight :semi-bold :width width})
     (paragraph {:width width :text "For C-x C-e, press Control+x, release, then press Control+e. You can also keep Control held and press x followed by e. C-x is a prefix: it waits for the next key. C-g cancels if you get lost."})
     (ui/spacer 0 14)
     (eui/text {:text "Buffer, pane, and applet" :weight :semi-bold :width width})
     (paragraph {:width width :text "A buffer is text held in memory. It is saved to disk only when you save it. A pane is one area of the workspace; Emacs calls this a window. An applet is a running tool that occupies a pane. Hiding a pane keeps its applet; closing the applet stops it."})
     (eui/text {:text "Point, mark, and kill ring" :weight :semi-bold :width width})
     (paragraph {:width width :text "Point is the cursor. Mark is the other end of a selection: C-SPC sets it, then moving the cursor selects a region. Kill means cut, and yank means paste. The kill ring holds text cut or copied by the editor; C-y pastes from it."})
     (ui/spacer 0 14)
     (eui/text {:text "Commands and the live REPL" :weight :semi-bold :width width})
     (paragraph {:width width :text "With the default keymap, M-x finds tools and workspace actions by name. C-k also opens commands when no text input is focused. Inside Clobber, C-k cuts to the end of the line. Shortcuts shows your selected keymap. Clojure buffers and the REPL evaluate in the running process."})
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
                  (introduction-page {:page page :width content-width}))]
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
