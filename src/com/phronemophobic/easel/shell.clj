(ns com.phronemophobic.easel.shell
  (:require [clojure.string :as str]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.ui :as eui]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui]]
            [membrane.ui :as ui]))

(defn geometry [size appearance dock?]
  (let [[width height] size
        width (max 0 width) height (max 0 height)
        context {::theme/value appearance}
        sidebar (if (< width 650) 0 (min (* width 0.25) (theme/token context :metric/sidebar)))
        topbar (min height (theme/token context :metric/topbar))
        status (min (max 0 (- height topbar)) (theme/token context :metric/status))
        available (max 0 (- height topbar status))
        dock (if dock? (min (theme/token context :metric/dock) (* available 0.35)) 0)
        gap (theme/token context :metric/gap)]
    {:width width :height height :sidebar sidebar :topbar topbar :status status
     :gap gap :content-width (max 0 (- width sidebar (* 2 gap)))
     :workspace-height (max 0 (- available dock (* 2 gap)))
     :dock-height dock :available available}))

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
               (ui/padding 8 8 (eui/text {:text "Saved workspaces" :size 12 :color :color/secondary}))
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

(defui topbar [{:keys [easel width height]}]
  (let [workspaces (take (if (< width 1000) 1 3)
                         (sort-by (comp str key) (get-in easel [:workspaces :by-id])))
        tabs (ui/flex-layout
              (for [[id workspace] workspaces]
                (eui/button {:text (:name workspace) :variant :text
                             :selected? (= id (:workspace-id easel)) :height 36
                             :on-click #(vector [:com.phronemophobic.easel/open-workspace id])}))
              {:gap 8})
        name (eui/text {:text "Easel" :size 20 :weight :semi-bold})
        tools (eui/button {:text "Tools" :height 36 :selected? (some? (:manager-id easel))
                           :on-click #(vector [:com.phronemophobic.easel/open-tools])})
        status (eui/row nil [(eui/text {:text "●" :color :color/live :size 11})
                            (eui/text {:text "Local JVM" :size 12})])
        search-width (max 100 (min 400 (- width (ui/width tabs) 400)))
        search (eui/button {:text "Find a tool or command     ⌘K" :width search-width :height 36
                            :icon "search" :variant :default
                            :on-click #(vector [:com.phronemophobic.easel/show-commands ""])})]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height :color/header)
       (ui/translate 20 (/ (- height (ui/height name)) 2) name)
       (when (> width 520) (ui/translate 110 (/ (- height 36) 2) tabs))
       (when (> width 850) (ui/translate (max (+ 140 (ui/width tabs)) (- width search-width 250))
                                                      (/ (- height 36) 2) search))
       (when (> width 650) (ui/translate (- width 190) (/ (- height (ui/height status)) 2) status))
       (ui/translate (max 0 (- width (ui/width tools) 16)) (/ (- height 36) 2) tools)
       (ui/translate 0 (max 0 (dec height)) (eui/rule context width))])))

(def dock-items [[:repl "REPL"] [:values "Values"] [:trace "Trace"] [:logs "Logs"] [:terminal "Terminal"] [:media "Media"]])

(defui dock [{:keys [easel width height body]}]
  (let [tabs (ui/flex-layout
              (for [[id label] dock-items]
                (eui/button {:text label :variant :text :height 30 :size :small
                             :selected? (= id (get-in easel [:dock :selected]))
                             :on-click #(vector [:com.phronemophobic.easel/select-dock id])}))
              {:gap 6})
        header-height 40]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height)
       (ui/translate 10 5 tabs)
       (ui/translate (max 0 (- width 70)) 5
                     (eui/button {:text "Hide" :variant :link :size :small
                                  :on-click #(vector [:com.phronemophobic.easel/toggle-dock])}))
       (ui/translate 0 (dec header-height) (eui/rule context width))
       (ui/translate 0 header-height
         (ui/scissor-view [0 0] [width (max 0 (- height header-height))] body))
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
                             (:message easel) "⌘K Commands    C-x C-e Evaluate    ⌘J Dock")
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
                                        :selected? (= i index)
                                        :width (- panel-width 32) :height 40 :on-click #(:intents command)}))))
        panel-height (+ 32 (ui/height content))]
    (ui/fixed-bounds [panel-width panel-height]
      [(eui/surface context panel-width panel-height :color/surface 8)
       (eui/border context panel-width panel-height :color/border 8)
       (ui/translate 16 16 content)])))

(defui workspace-view [{:keys [easel body dock-body manager-body commands]}]
  (let [appearance (theme/current context)
        {:keys [width height sidebar topbar status gap content-width workspace-height dock-height available]}
        (geometry (:viewport easel [1100 800]) appearance (and (:dock easel) (not (get-in easel [:dock :hidden?]))))
        manager? (some? (:manager-id easel))
        content-x (+ sidebar gap)
        content-y (+ topbar gap)
        command-state (:commands easel)]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height :color/window)
       (com.phronemophobic.easel.shell/topbar {:easel easel :width width :height topbar})
       (when (pos? sidebar)
         (ui/translate 0 topbar
           (com.phronemophobic.easel.shell/sidebar {:easel easel :width sidebar :height available})))
       (ui/translate content-x content-y
         (ui/scissor-view [0 0] [content-width (if manager? (- available (* 2 gap)) workspace-height)]
                          (if manager? manager-body body)))
       (when (and (not manager?) (pos? dock-height))
         (ui/translate content-x (+ content-y workspace-height gap)
           (dock {:easel easel :width content-width :height (max 0 (- dock-height gap)) :body dock-body})))
       (ui/translate sidebar (- height status)
         (statusbar {:width (- width sidebar) :height status :easel easel}))
       (when command-state
         [(ui/on :mouse-down (fn [_] [[:com.phronemophobic.easel/hide-commands]])
                  (ui/with-color [0 0 0 0.12] (ui/filled-rectangle [0 0 0 0.12] width height)))
          (ui/translate (max 20 (/ (- width (min 600 (- width 40))) 2)) (+ topbar 24)
            (command-palette {:commands commands :query (:query command-state "") :selection (:selection command-state 0)
                               :$selection [$easel '(keypath :commands) '(keypath :selection)]
                               :$query [$easel '(keypath :commands) '(keypath :query)] :width width}))])])))
