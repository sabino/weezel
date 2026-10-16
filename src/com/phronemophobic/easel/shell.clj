(ns com.phronemophobic.easel.shell
  (:require [clojure.string :as str]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.composition :as composition]
            [com.phronemophobic.easel.keymap :as keymap]
            [com.phronemophobic.easel.help :as help]
            [com.phronemophobic.easel.ui :as eui]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui]]
            [membrane.ui :as ui]))

(defn geometry
  ([size appearance dock?]
   (geometry size appearance {:dock (when dock? {})} @composition/default-composition))
  ([size appearance easel descriptor]
   (let [frames (composition/layout descriptor size {:easel easel :appearance appearance})
         workspace (composition/region frames :workspace)
         dock (composition/region frames :dock)
         sidebar (composition/region frames :sidebar)
         topbar (composition/region frames :topbar)
         status (composition/region frames :status)]
     {:width (max 0 (first size)) :height (max 0 (second size)) :frames frames
      :sidebar (:width sidebar 0) :topbar (:height topbar 0) :status (:height status 0)
      :gap (theme/token {::theme/value appearance} :metric/gap)
      :content-x (:x workspace 0) :content-y (:y workspace 0)
      :content-width (:width workspace 0) :workspace-height (:height workspace 0)
      :dock-height (:height dock 0) :dock-width (:width dock 0)
      :available (:height workspace 0)})))

(defui navigation-row [{:keys [text icon width selected? on-click]}]
  (eui/button {:text text :icon icon :width width :height 42 :variant :text
               :selected? selected? :on-click on-click}))

(defui tools-navigation [{:keys [easel width]}]
  (let [state (get-in easel [:applets (:manager-id easel) :state])
        page (:page state :discover)]
    (ui/vertical-layout
     (ui/padding 12 14 (eui/text {:text "Tools" :weight :semi-bold}))
     (apply ui/vertical-layout
       (for [[id name icon] [[:discover "Discover" "search"] [:installed "Installed" "inbox"]
                             [:toolsets "Toolsets" "layers"] [:updates "Updates" "reload"] [:sources "Sources" "database"]]]
         (navigation-row {:text name :icon icon :width (- width 20) :selected? (= page id)
                           :on-click #(vector [:com.phronemophobic.easel.package-ui/page id])})))
     (ui/spacer 0 20)
     (ui/padding 8 8 (eui/rule context (max 0 (- width 36))))
     (ui/padding 8 8 (eui/text {:text "My toolsets" :size 13 :color :color/secondary}))
     (apply ui/vertical-layout
       (for [p (sort-by :name (filter #(= :toolset (:kind %)) (vals (:registry state))))]
         (navigation-row {:text (:name p) :icon "folder" :width (- width 20)
                           :selected? (= (:selected state) (:id p))
                           :on-click #(vector [:com.phronemophobic.easel.package-ui/page :toolsets]
                                              [:com.phronemophobic.easel.package-ui/select (:id p)])})))
     (navigation-row {:text "New toolset" :icon "plus" :width (- width 20)
                      :on-click #(vector [:com.phronemophobic.easel.package-ui/new-toolset])}))))

(defui sidebar [{:keys [easel width height]}]
  (let [applets (keep (:applets easel) (keep :applet-id (tree-seq :panes :panes (:root-pane easel))))
        active (:focus context)
        body (ui/vertical-layout
               (ui/padding 12 14 (eui/text {:text "Workspace" :weight :semi-bold}))
               (apply ui/vertical-layout
                      (for [a applets]
                        (navigation-row
                         {:text (str (:label a)) :icon (or (:icon a) "file") :width (- width 20)
                          :selected? (= active (:id a))
                          :on-click #(vector [:com.phronemophobic.easel/focus-applet (:id a)])})))
               (navigation-row {:text "Add applet" :icon "plus" :width (- width 20)
                                :on-click #(vector [:com.phronemophobic.easel/show-commands ""] )})
               (ui/spacer 0 20)
               (ui/padding 8 8 (eui/rule context (max 0 (- width 36))))
               (ui/padding 8 8 (eui/text {:text "Workspaces" :size 12 :color :color/secondary}))
               (apply ui/vertical-layout
                      (for [[id workspace] (sort-by (comp str key) (get-in easel [:workspaces :by-id]))]
                        (navigation-row
                         {:text (:name workspace) :width (- width 20)
                          :selected? (= id (:workspace-id easel))
                          :on-click #(vector [:com.phronemophobic.easel/open-workspace id])})))
               (ui/spacer 0 12)
               (navigation-row {:text "Save workspace" :icon "save" :width (- width 20)
                                :on-click #(vector [:com.phronemophobic.easel/save-preset])}))]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height :color/sidebar)
       (ui/translate (max 0 (dec width)) 0 (eui/surface context 1 height :color/border))
               (ui/translate 10 0 (basic/scrollview {:scroll-bounds [(max 0 (- width 20)) height]
                                          :$body nil :body (if (:manager-id easel)
                                                             (tools-navigation {:easel easel :width width}) body)}))])))

(defui brand [{:keys [width height]}]
  (let [label (eui/text {:text "Easel" :size 20 :weight :semi-bold})]
    (ui/translate 12 (max 0 (/ (- height (ui/height label)) 2)) label)))

(defui workspace-tabs [{:keys [easel width height]}]
  (ui/scissor-view [0 0] [width height]
    (eui/row nil
      (for [[id workspace] (sort-by (comp str key) (get-in easel [:workspaces :by-id]))]
        (eui/button {:text (:name workspace) :variant :text :height height
                     :selected? (= id (:workspace-id easel))
                     :on-click #(vector [:com.phronemophobic.easel/open-workspace id])})))))

(defui command-button [{:keys [easel width height command text icon show-shortcut?]}]
  (eui/button {:text (str text (when show-shortcut? (str "     " (keymap/shortcut (keymap/current easel) command))))
               :icon icon :width width :height height :variant :text
               :on-click #(vector [:com.phronemophobic.easel/invoke-command command nil])}))

(defui session-indicator [{:keys [height]}]
  (let [label (eui/row nil [(eui/text {:text "●" :color :color/live :size 11})
                           (eui/text {:text "Local JVM" :size 12})])]
    (ui/translate 0 (max 0 (/ (- height (ui/height label)) 2)) label)))

(defn dock-items [easel] (get-in (composition/current easel) [:dock :items]))

(defui dock [{:keys [easel width height dock-body]}]
  (let [tabs (ui/flex-layout
              (for [{:keys [id label]} (dock-items easel)]
                (eui/button {:text label :variant :text :height 30 :size :small
                             :selected? (= id (get-in easel [:dock :selected]))
                             :on-click #(vector [:com.phronemophobic.easel/select-dock id])}))
              {:gap 6})
        header-height (min height (composition/dimension (theme/current context) (get-in (composition/current easel) [:dock :header-height] 40)))]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height)
       (ui/translate 10 5 tabs)
       (ui/translate (max 0 (- width 70)) 5
                     (eui/button {:text "Hide" :variant :link :size :small
                                  :on-click #(vector [:com.phronemophobic.easel/toggle-dock])}))
       (ui/translate 0 (dec header-height) (eui/rule context width))
       (ui/translate 0 header-height
         (ui/scissor-view [0 0] [width (max 0 (- height header-height))] dock-body))
       (eui/border context width height)])))

(defui statusbar [{:keys [width height easel]}]
  (ui/fixed-bounds [width height]
    [(eui/surface context width height :color/header)
     (ui/translate 20 4
       (eui/row nil [(eui/text {:text "●" :color :color/live :size 10})
                    (eui/text {:text "Local session" :size 11 :color :color/secondary})
                    (ui/spacer 12 0)
                    (eui/text {:text (str (+ (if (:theme-error easel) 1 0)
                                              (count (filter :com.phronemophobic.easel.applet/error (vals (:applets easel))))) " errors")
                                :size 11 :color :color/secondary})]))
     (when (> width 800)
       (ui/translate (- width 340) 4
         (eui/text {:text (or (when (:theme-error easel) "Theme error · previous theme retained")
                             (:message easel) (str (keymap/shortcut (keymap/current easel) :easel/commands) " Commands    "
                                                  (keymap/shortcut (keymap/current easel) :easel/help) " Help"))
                    :width 330 :size 11 :color :color/secondary})))]))

(defui command-palette [{:keys [commands query selection width]}]
  (let [matches (take 12 (filter #(str/includes? (str/lower-case (:name %)) (str/lower-case query)) commands))
        index (max 0 (min (or selection 0) (dec (count matches))))
        panel-width (min 600 (max 200 (- width 40)))
        content (ui/vertical-layout
                  (eui/field {:text query :width (- panel-width 32) :placeholder "Find a tool or command…"
                              :focus-id ::command-query
                              :on-key (fn [key] (case key
                                                 :down [[:set $selection (min (dec (count matches)) (inc index))]]
                                                 :up [[:set $selection (max 0 (dec index))]] nil))
                              :on-submit #(when-let [command (nth (vec matches) index nil)] (:intents command))
                              :on-escape #(vector [:com.phronemophobic.easel/hide-commands])})
                  (ui/spacer 0 12)
                  (apply ui/vertical-layout
                         (for [[i command] (map-indexed vector matches)]
                           (eui/button {:text (:name command) :icon (:icon command) :variant :text
                                        :selected? (= i index) :disabled? (:disabled? command)
                                        :width (- panel-width 32) :height 40 :on-click #(:intents command)}))))
        panel-height (+ 32 (ui/height content))]
    (ui/fixed-bounds [panel-width panel-height]
      [(eui/surface context panel-width panel-height :color/surface 8)
       (eui/border context panel-width panel-height :color/border 8)
       (ui/translate 16 16 content)])))

(defui panes-view [{:keys [body]}] body)
(defui manager-view [{:keys [manager-body]}] manager-body)
(defui help-view [{:keys [easel width height]}]
  (help/help-view {:help (:help easel) :$help [$easel '(keypath :help)] :easel easel
                   :width width :height height}))

(defui commands-overlay [{:keys [easel commands width height]}]
  [(ui/on :mouse-down (fn [_] [[:com.phronemophobic.easel/hide-commands]])
          (ui/filled-rectangle [0 0 0 0.12] width height))
   (ui/translate (max 0 (/ (- width (min 600 (max 200 (- width 40)))) 2)) (min 80 (* height 0.1))
     (command-palette {:commands commands :query (get-in easel [:commands :query] "")
                       :selection (get-in easel [:commands :selection] 0)
                       :$selection [$easel '(keypath :commands) '(keypath :selection)]
                       :$query [$easel '(keypath :commands) '(keypath :query)] :width width}))])

(defui workspace-view [{:keys [easel body dock-body manager-body commands]}]
  (let [[width height :as size] (:viewport easel [1100 800])
        frames (composition/layout (composition/current easel) size
                                    {:easel easel :appearance (theme/current context)})
        shared {:easel easel :$easel $easel :body body :dock-body dock-body :manager-body manager-body
                :commands commands :context context :$context $context}]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height :color/window)
       (vec (for [{:keys [id node x y width height] :as frame} frames
                  :when (and (pos? width) (pos? height) (or (:view node) (:background node)))]
              (ui/translate x y
                (ui/scissor-view [0 0] [width height]
                  [(when-let [background (:background node)] (eui/surface context width height background))
                   (when-let [view (:view node)]
                     ((requiring-resolve view)
                      (merge shared (:props node)
                             {:width width :height height :frame frame :node node
                              :extra (get extra id) :$extra [$extra (list 'keypath id)]})))]))))])))
