(ns com.phronemophobic.easel.package-ui
  (:require [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.composition :as composition]
            [com.phronemophobic.easel.ui :as eui]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(defn source-kind [p]
  (cond (some :mvn/version (vals (:deps p))) :clojars
        (or (some :git/url (vals (:deps p))) (str/includes? (:source p "") "github.com")) :git
        :else :local))

(defn matches [state]
  (let [page (:page state :discover) kind (:kind state :applet)
        registry (if (= page :updates) (:updates state) (:registry state))]
    (->> (vals registry)
         (filter #(case page :toolsets (= :toolset (:kind %))
                        :updates true
                        (= kind (:kind %))))
         (filter #(or (not= page :installed) (packages/installed? %)))
         (filter #(or (not= page :discover) (not= kind :applet) (not (str/blank? (:query state))) (:featured? %)))
         (filter #(or (= :all (:source-filter state :all)) (= (:source-filter state) (source-kind %))))
         (filter #(str/includes? (str/lower-case (str (:name %) " " (:description %) " " (:id %)))
                                (str/lower-case (:query state ""))))
         (sort-by #(vector (or (:order %) 100) (:name %))))))

(defn open-package! [handler p]
  (case (:kind p)
    :applet (do (handler :com.phronemophobic.easel/close-tools)
                (handler :com.phronemophobic.easel/add-applet
                         {:make-applet #(packages/make-applet % (:id p) nil)}))
    :theme (packages/select-theme! handler (:id p) true)
    :composition (packages/select-composition! handler (:id p) true)
    :keymap (packages/select-keymap! handler (:id p) true)
    :workspace (let [id (inc (get-in (handler :get :easel) [:workspaces :last-id] 0))]
                 (handler :update :easel #(-> % (assoc-in [:workspaces :by-id id]
                                                        {:id id :name (:name p) :preset (:id p)})
                                              (assoc-in [:workspaces :last-id] id)))
                 (handler :com.phronemophobic.easel/open-workspace id))
    nil))

(defeffect ::page [page]
  (when-let [id (:manager-id (dispatch! :get :easel))]
    (dispatch! :update [:easel :applets (list 'keypath id) :state] assoc :page page :query "")))

(defeffect ::select [package-id]
  (when-let [id (:manager-id (dispatch! :get :easel))]
    (dispatch! :update [:easel :applets (list 'keypath id) :state] assoc :selected package-id :details? true)))

(defeffect ::new-toolset []
  (when-let [id (:manager-id (dispatch! :get :easel))]
    (dispatch! :update [:easel :applets (list 'keypath id) :state] assoc :create? true :page :installed :kind :applet :members #{})))

(defeffect ::refresh [$state]
  (dispatch! :update $state assoc :registry (packages/catalog) :error nil))

(defeffect ::action [$state action id]
  (let [state (dispatch! :get $state)
        registry (merge (packages/catalog) (:updates state))
        p (get registry id)]
    (try
     (case action
      :open (open-package! dispatch! p)
      :pin (do (packages/pin! id (not (get-in (settings/read-settings) [:pins id])))
               (dispatch! :update $state assoc :pins (:pins (settings/read-settings))))
      :preview (do (dispatch! :update $state
                              #(if (:preview-original %) % (assoc % :preview-original (:id (theme/current)))))
                   (packages/select-theme! dispatch! id))
      :apply (do (packages/select-theme! dispatch! id true)
                 (dispatch! :update $state dissoc :preview-original))
      :revert (do (when-let [original (:preview-original state)] (packages/select-theme! dispatch! original))
                  (dispatch! :update $state dissoc :preview-original))
      :edit (packages/edit-data! dispatch! id)
      :install (applet/task! dispatch! $state #(packages/install! registry [id])
                            (fn [result]
                              (dispatch! :update $state assoc :registry (packages/catalog)
                                         :message (if (:restart? result) "Installed. Restart the JVM to load the new version." "Installed."))
                              (when (and (:open? state) (= :applet (:kind p)) (not (:restart? result)))
                                (open-package! dispatch! p))))
      :remove (applet/task! dispatch! $state #(packages/remove! id)
                           (fn [result]
                             (doseq [a (vals (:applets (dispatch! :get :easel))) :when (= id (:package/id a))]
                               (dispatch! :com.phronemophobic.easel/close-applet (:id a)))
                             (when (= id (:id (theme/current)))
                               (packages/select-theme! dispatch! :easel/studio-light true))
                             (when (= id (get-in (dispatch! :get :easel) [:composition :id]))
                               (packages/select-composition! dispatch! :easel/minimal true))
                             (when (= id (get-in (dispatch! :get :easel) [:keymap :id]))
                               (packages/select-keymap! dispatch! :easel/default-keys true))
                             (dispatch! :update $state assoc :registry (packages/catalog)
                                        :message (if (:restart? result) "Removed. Loaded JVM code is released on restart." "Removed.")))))
     (catch Throwable e
       (events/report! e)
       (dispatch! :update $state assoc :error (applet/error-message e))))))

(defeffect ::import [$state]
  (let [source (:source-input (dispatch! :get $state))]
    (applet/task! dispatch! $state #(packages/import! source)
                  (fn [result]
                    (dispatch! :update $state assoc :registry (packages/catalog) :selected (last (:installed result))
                               :page :installed :kind (:kind (get (packages/catalog) (last (:installed result))))
                               :message (if (:restart? result) "Installed. Restart the JVM to load this version." "Installed."))))))

(defeffect ::updates [$state]
  (applet/task!
   dispatch! $state
   #(let [updates (into {}
                         (keep (fn [[id p]]
                                 (when-let [source (:origin p)]
                                   (let [new (-> (packages/read-source source) packages/normalize packages/validate)]
                                     (when-not (= id (:id new)) (throw (ex-info "Update source changed its package id" {:id id})))
                                     (when (not= (:version p) (:version new))
                                       [id (assoc new :origin source)])))))
                         (:packages (settings/read-settings)))]
      {:updates updates :page :updates :message (str (count updates) " updates available.")})))

(defeffect ::save-toolset [$state]
  (let [{:keys [toolset-name members]} (dispatch! :get $state)
        name (str/trim toolset-name)
        slug (str/replace (str/lower-case name) #"[^a-z0-9]+" "-")]
    (when (or (str/blank? name) (str/blank? slug) (empty? members))
      (throw (ex-info "Name the toolset and select at least one package" {})))
    (let [id (keyword "user" slug)
          p {:package/version 1 :id id :name name :version "1" :kind :toolset :requires (vec members)}]
      (applet/task! dispatch! $state #(packages/install! (assoc (packages/catalog) id p) [id])
                    (fn [_] (dispatch! :update $state assoc :registry (packages/catalog)
                                      :create? false :page :toolsets :selected id))))))

(defeffect ::add-to-toolset [$state toolset-id package-id]
  (let [p (get (packages/catalog) toolset-id)
        p (update p :requires #(vec (distinct (conj % package-id))))]
    (applet/task! dispatch! $state #(packages/install! (assoc (packages/catalog) toolset-id p) [toolset-id])
                  (fn [_] (dispatch! :update $state assoc :registry (packages/catalog) :choose-toolset? false)))))

(defeffect ::close [$state]
  (when-let [id (:preview-original (dispatch! :get $state))]
    (packages/select-theme! dispatch! id)
    (dispatch! :update $state dissoc :preview-original))
  (dispatch! :com.phronemophobic.easel/close-tools))

(defui toolset-card [{:keys [package width selected? busy? on-select on-install]}]
  (let [w (max 0 width) installed? (packages/installed? package)]
    (ui/wrap-on :mouse-down (fn [handler pos] (concat (when on-select (on-select)) (handler pos)))
      (ui/fixed-bounds [w 118]
        [(eui/surface context w 118 (if selected? :color/selection :color/surface) 4)
         (eui/border context w 118 :color/border 4)
         (ui/translate 16 18 (eui/icon {:name "folder" :size 28}))
         (ui/translate 58 14 (eui/text {:text (:name package) :weight :semi-bold :width (max 1 (- w 70))}))
         (ui/translate 58 40 (eui/text {:text (:description package) :size 12 :color :color/secondary
                                        :width (max 1 (- w 70))}))
         (ui/translate 58 82
           (if installed? (eui/text {:text "●  Installed" :size 12 :color :color/live})
               (eui/button {:text "Install toolset" :size :small :disabled? busy? :on-click on-install})))]))))

(defui package-row [{:keys [package width selected? busy? on-select on-action]}]
  (let [w (max 0 width) wide? (> w 600)
        state (packages/status package)
        action (case state :installed (if (= :library (:kind package)) "Included" "Open")
                           :restart "Restart JVM" "Install")]
    (ui/wrap-on :mouse-down (fn [handler pos] (concat (when on-select (on-select)) (handler pos)))
      (ui/fixed-bounds [w 52]
        [(when selected? (eui/surface context w 52 :color/selection 4))
         (ui/translate 10 15 (eui/icon {:name (or (:icon package) (case (:kind package) :theme "skin" :workspace "layout" "appstore")) :size 22}))
         (ui/translate 52 15 (eui/text {:text (:name package) :width (if wide? 160 (max 1 (- w 150)))}))
         (when wide?
           (ui/translate 224 15 (eui/text {:text (:description package) :width (max 1 (- w 430)) :size 13 :color :color/secondary})))
         (when (> w 780)
           (ui/translate (- w 198) 16 (eui/text {:text (name (source-kind package)) :size 12 :color :color/secondary})))
         (ui/translate (max 0 (- w 112)) 10
           (eui/button {:text action :width 102 :height 32 :size :small
                        :disabled? (or busy? (= state :restart) (= :library (:kind package)))
                        :on-click on-action}))
         (ui/translate 0 51 (eui/rule context w))]))))

(defui details [{:keys [state package width height compact?]}]
  (let [p package w (max 1 (- width 40)) busy? (:busy? state)
        installed? (packages/installed? p)
        action (fn [a] #(vector [::action $state a (:id p)]))
        coord (first (:deps p))
        body (ui/vertical-layout
               (eui/button {:text "← Back to workspace" :variant :link :size :small
                            :on-click #(vector [::close $state])})
               (when compact?
                 (eui/button {:text "← All tools" :variant :link :size :small
                              :on-click #(vector [:update $state assoc :details? false])}))
               (ui/spacer 0 28)
               (eui/row nil [(eui/icon {:name (or (:icon p) "appstore") :size 46})
                            (ui/spacer 12 0)
                            (eui/text {:text (:name p) :size 27 :weight :semi-bold :width (max 1 (- w 75))})])
               (ui/spacer 0 6)
               (eui/text {:text (str (:version p) "  ·  " (name (source-kind p))) :color :color/secondary})
               (ui/spacer 0 24)
               (eui/text {:text (:description p "") :width w})
               (ui/spacer 0 20)
               (if (= :theme (:kind p))
                 (ui/vertical-layout
                  (ui/flex-layout
                   [(eui/button {:text "Preview" :on-click (action :preview) :disabled? busy?})
                    (eui/button {:text "Apply" :variant :primary :on-click (action :apply) :disabled? busy?})]
                   {:gap 8})
                  (ui/spacer 0 12)
                  (eui/row nil [(eui/button {:text "Edit EDN" :variant :link :on-click (action :edit)})
                                (when (:preview-original state)
                                  (eui/button {:text "Revert preview" :variant :link :on-click (action :revert)}))]))
                 (eui/button {:text (cond (:restart? p) "Restart JVM" installed? (case (:kind p) :workspace "Use workspace" :composition "Use composition" :keymap "Use keymap" "Open") :else "Install")
                              :width 142 :height 40 :variant :primary
                              :disabled? (or busy? (:restart? p) (= :library (:kind p)) (and installed? (= :toolset (:kind p))))
                              :on-click (action (if installed? :open :install))}))
               (when (and installed? (#{:composition :keymap} (:kind p)))
                 (ui/padding 0 12 (eui/button {:text "Edit EDN" :variant :link :on-click (action :edit)})))
               (when (and (= :applet (:kind p)) (not installed?))
                 (ui/on :mouse-down (fn [_] [[:update [$state '(keypath :open?)] not]])
                   (ui/padding 0 16 (eui/text {:text (str (if (:open? state) "☑" "☐") "  Open in workspace")}))))
               (ui/spacer 0 24) (eui/rule context w) (ui/spacer 0 20)
               (eui/text {:text "Package" :weight :semi-bold})
               (ui/spacer 0 8)
               (eui/text {:text (str (or (first coord) (:id p))) :role :code :size 12 :width w})
               (ui/spacer 0 18)
               (eui/text {:text "Version specification" :weight :semi-bold})
               (ui/spacer 0 8)
               (eui/text {:text (pr-str (or (second coord) {:version (:version p)})) :role :code :size 12 :width w})
               (ui/spacer 0 16)
               (when-let [source (:source p)]
                 (eui/button {:text "Source ↗" :variant :link
                              :on-click #(vector [:com.phronemophobic.easel/add-applet
                                                   {:make-applet (fn [handler]
                                                                   ((requiring-resolve 'com.phronemophobic.easel.browser/browslet) handler source))}])}))
               (ui/spacer 0 12)
               (eui/wrap-row w
                 [(eui/button {:text (if (get-in state [:pins (:id p)]) "Unpin version" "Pin version")
                                :disabled? (or busy? (not installed?)) :on-click (action :pin)})
                  (when (= :applet (:kind p))
                    (eui/button {:text "Add to toolset…"
                                 :on-click #(vector [:update $state assoc :choose-toolset? (not (:choose-toolset? state))])}))])
               (when (:choose-toolset? state)
                 (let [toolsets (filter #(and (= :toolset (:kind %)) (not (:bundled? %))) (vals (:registry state)))]
                   (if (seq toolsets)
                     (apply ui/vertical-layout
                       (for [toolset toolsets]
                         (eui/button {:text (:name toolset) :variant :text
                                      :on-click #(vector [::add-to-toolset $state (:id toolset) (:id p)])})))
                     (eui/text {:text "Choose Toolsets, then New toolset to create a collection." :width w :size 12 :color :color/secondary}))))
               (when (and installed? (not (:bundled? p)))
                 (ui/padding 0 12 (eui/button {:text "Remove" :variant :link :disabled? busy? :on-click (action :remove)})))
               (ui/spacer 0 20) (eui/rule context w) (ui/spacer 0 18)
               (eui/text {:text "Easel libraries" :weight :semi-bold})
               (ui/spacer 0 12)
               (apply ui/vertical-layout
                 (for [[name description] [["Membrane" "UI library"] ["Membrandt" "UI components"] ["Bifurcan" "Data structures / ropes"]]]
                   (ui/padding 4 12 (eui/row nil [(eui/icon {:name "block" :size 22})
                                                 (eui/text {:text (str name "    " description) :width (max 1 (- w 34)) :size 13})])))))]
    (ui/fixed-bounds [width height]
      [(eui/surface context width height)
       (eui/border context width height)
       (basic/scrollview {:scroll-bounds [width height] :$body nil :body (ui/padding 20 20 body)})])))

(defui package-view [{:keys [state size]}]
  (let [[width height] size
        detail-width (if (> width 960) (min 400 (* width 0.3)) 0)
        main-width (max 0 (- width detail-width (if (pos? detail-width) 8 0)))
        w (max 1 (- main-width 48))
        selected (or (get (:updates state) (:selected state)) (get (:registry state) (:selected state)))
        items (matches state)
        select (fn [id] #(vector [:update $state assoc :selected id :details? true]))
        action (fn [p] #(vector [::action $state (if (packages/installed? p) :open :install) (:id p)]))
        body (ui/vertical-layout
               (eui/row w [(eui/text {:text "Tools" :size 28 :weight :semi-bold})
                            (ui/spacer (max 0 (- w 200)) 0)
                            (eui/button {:text "Close" :variant :link :on-click #(vector [::close $state])})])
               (when-not (composition/provides? (:composition/frames context) :easel/tools-navigation)
                 (eui/wrap-row w
                   (for [[id label] [[:discover "Discover"] [:installed "Installed"] [:toolsets "Toolsets"]
                                     [:updates "Updates"] [:sources "Sources"]]]
                     (eui/button {:text label :variant :text :selected? (= id (:page state :discover))
                                  :on-click #(vector [::page id])}))))
               (ui/spacer 0 4)
               (eui/text {:text "Small tools. Shared data." :size 16 :color :color/secondary})
               (ui/spacer 0 20)
               (eui/field {:text (:query state "") :$text [$state '(keypath :query)]
                           :placeholder "Search tools and libraries" :width w})
               (ui/spacer 0 12)
               (eui/wrap-row w
                 (for [[id name] [[:all "All"] [:clojars "Clojars"] [:git "Git"] [:local "Local"]
                                  [:applet "Applets"] [:library "Libraries"] [:theme "Themes"] [:workspace "Workspaces"] [:composition "Compositions"] [:keymap "Keymaps"]]]
                   (eui/button {:text name :variant :text :size :small
                                :selected? (or (= id (:source-filter state)) (= id (:kind state)))
                                :on-click #(vector [:update $state assoc (if (#{:all :clojars :git :local} id) :source-filter :kind) id])})))
               (ui/spacer 0 24)
               (when (= :discover (:page state))
                 (ui/vertical-layout
                  (eui/text {:text "Toolsets" :size 18 :weight :semi-bold})
                  (ui/spacer 0 4)
                  (eui/text {:text "Collections of tools for common workflows." :color :color/secondary})
                  (ui/spacer 0 16)
                  (ui/flex-layout
                   (for [id [:easel/essentials :easel/data-graphics :easel/models]
                         :let [p (get (:registry state) id)]]
                     (toolset-card {:package p :width (/ (- w 16) 3) :busy? (:busy? state)
                                    :selected? (= id (:selected state)) :on-select (select id)
                                    :on-install #(vector [::action $state :install id])})) {:gap 8})
                  (ui/spacer 0 24) (eui/rule context w) (ui/spacer 0 20)))
               (when (= :sources (:page state))
                 (ui/vertical-layout
                  (eui/text {:text "Install from EDN" :size 18 :weight :semi-bold})
                  (ui/spacer 0 12)
                  (eui/text {:text "Enter a local manifest, an HTTPS URL, or a Git source with :git/url, :git/sha, and :path. Theme, composition, keymap, and workspace EDN files work directly."
                              :width w :color :color/secondary})
                  (ui/spacer 0 12)
                  (eui/field {:text (:source-input state "") :$text [$state '(keypath :source-input)]
                              :width w :placeholder "/path/to/package.edn" :on-submit #(vector [::import $state])})
                  (ui/spacer 0 12)
                  (eui/button {:text "Install" :variant :primary :disabled? (:busy? state) :on-click #(vector [::import $state])})
                  (ui/spacer 0 24)))
               (when (= :toolsets (:page state))
                 (eui/button {:text "New toolset" :icon "plus" :on-click #(vector [::new-toolset])}))
               (when (:create? state)
                 (ui/vertical-layout
                  (eui/field {:text (:toolset-name state "") :$text [$state '(keypath :toolset-name)] :width w :placeholder "Toolset name"})
                  (ui/spacer 0 8)
                  (eui/text {:text "Select packages below, then save." :color :color/secondary})
                  (eui/button {:text (str "Save toolset (" (count (:members state)) ")")
                               :disabled? (or (:busy? state) (empty? (:members state)) (str/blank? (:toolset-name state)))
                               :on-click #(vector [::save-toolset $state])})
                  (ui/spacer 0 20)))
               (eui/row w [(eui/text {:text (case (:page state) :installed "Installed" :updates "Updates" :toolsets "Toolsets" "Available tools")
                                      :size 18 :weight :semi-bold})
                            (eui/button {:text "Refresh" :variant :link :size :small :on-click #(vector [::refresh $state])})
                            (eui/button {:text "Check updates" :variant :link :size :small :disabled? (:busy? state)
                                         :on-click #(vector [::updates $state])})])
               (ui/spacer 0 12)
               (when (:busy? state) (eui/text {:text "Working…" :color :color/secondary}))
               (when-let [message (:message state)] (eui/text {:text message :width w :size 12 :color :color/secondary}))
               (when-let [error (:error state)] (eui/text {:text error :width w :color :color/error}))
               (when (empty? items) (ui/padding 0 16 (eui/text {:text "No matching packages." :color :color/secondary})))
               (apply ui/vertical-layout
                      (for [p items]
                        (package-row {:package p :width w :busy? (:busy? state)
                                       :selected? (or (= (:id p) (:selected state)) (contains? (:members state #{}) (:id p)))
                                       :on-select (if (:create? state)
                                                    #(vector [:update [$state '(keypath :members)]
                                                              (fn [members] ((if (contains? members (:id p)) disj conj) members (:id p)))])
                                                    (select (:id p)))
                                       :on-action (action p)}))))]
    (if (and (zero? detail-width) selected (:details? state))
      (details {:state state :package selected :width width :height height :compact? true})
    (ui/fixed-bounds [width height]
      [(eui/surface context main-width height)
       (eui/border context main-width height)
       (basic/scrollview {:scroll-bounds [main-width height] :$body nil :body (ui/padding 24 20 body)})
       (when (and selected (pos? detail-width))
         (ui/translate (+ main-width 8) 0 (details {:state state :package selected :width detail-width :height height})))]))))

(defn package-applet [handler]
  (assoc (applet/tool-applet "Tools" #'package-view
                             {:registry (packages/catalog) :page :discover :kind :applet :source-filter :all
                              :query "" :selected :easel/flowstorm :open? true :members #{}
                              :pins (:pins (settings/read-settings))})
         :tool-manager? true :icon "appstore"))
