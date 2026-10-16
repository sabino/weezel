(ns com.phronemophobic.easel.help
  (:require [com.phronemophobic.easel.ui :as eui]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui]]
            [membrane.ui :as ui]))

(def version 1)

(def pages [[:start "Start here"] [:emacs "Emacs basics"] [:shortcuts "Shortcuts"]])

(defn command-key []
  (if (.startsWith (System/getProperty "os.name") "Mac") "⌘" "Ctrl+"))

(defn shortcut-groups []
  [{:title "Anywhere in Easel"
    :rows [[(str (command-key) "K") "Find a tool or command. Type to filter; use ↑ / ↓ and Enter."]
           [(str (command-key) "J") "Show or hide the bottom dock."]
           ["F1" "Open this help."]
           ["Esc / C-g" "Dismiss command search, help, a pane menu, or pane resizing."]]}
   {:title "In a Clobber editor"
    :rows [["C-x C-f" "Open a file. Choose a file in the prompt and press Enter."]
           ["C-x C-s" "Save the buffer. A new buffer asks for a file name."]
           ["C-x C-e" "Evaluate the form immediately before the cursor."]
           ["C-M-x" "Evaluate the surrounding top-level form."]
           ["C-c C-k" "Evaluate the whole Clojure buffer."]
           ["C-s / C-r" "Search forward / backward."]
           ["C-g" "Cancel an editor command or selection."]
           ["C-_" "Undo the last edit."]]}
   {:title "Move and select in an editor"
    :rows [["← ↑ ↓ →" "Move the cursor. Mouse clicks work too."]
           ["C-a / C-e" "Move to the start / end of the line."]
           ["M-b / M-f" "Move backward / forward one word."]
           ["C-SPC" "Set the selection's starting point, then move the cursor."]
           ["C-w / M-w" "Cut / copy the selected region into the editor's kill ring."]
           ["C-y" "Paste (yank) from the kill ring."]]}
   {:title "Arrange editor panes"
    :rows [["C-x 3" "Split the editor pane side by side."]
           ["C-x o" "Focus the next editor pane."]
           ["C-x b" "Choose an open editor buffer."]
           ["C-x 0" "Hide this pane; the applet stays open."]
           ["C-x 1" "Hide the other panes."]
           ["C-x k" "Close this applet."]]}])

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
     (paragraph {:width width :text "The dock holds the REPL, values, logs, and terminal. Its tools start only when you choose them. Saved workspaces are available in the sidebar; examples are optional presets in Tools."})
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
     (ui/spacer 0 20)
     (paragraph {:width width :text "You can return to this guide at any time with Help or F1. Emacs basics explains the notation before you need to memorize anything."}))

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
     (paragraph {:width width :text (str "In Easel, use command search (" (command-key) "K) to find tools and workspace actions by name. Clojure buffers evaluate forms in the running process. The REPL is another way to evaluate a form: type it into the input and press Enter.")})
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

(defui help-view [{:keys [help width height]}]
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
                    (for [group (shortcut-groups)]
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
