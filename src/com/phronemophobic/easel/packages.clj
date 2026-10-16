(ns com.phronemophobic.easel.packages
  (:require [clojure.java.basis :as basis]
            [clojure.java.io :as io]
            [clojure.repl.deps :as deps]
            [clojure.string :as str]
            [com.phronemophobic.easel.file-watcher :as watcher]
            [com.phronemophobic.easel.composition :as composition]
            [com.phronemophobic.easel.keymap :as keymap]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.process :as process]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.workspace :as workspace])
  (:import [java.net URI URLConnection]
           [clojure.lang DynamicClassLoader Compiler]))

(defonce runtime-loader (DynamicClassLoader. (clojure.lang.RT/baseLoader)))
(defonce runtime-data-readers (atom {}))
(defonce loaded (atom {}))
(defonce theme-watcher (atom nil))
(defonce data-watchers (atom {}))

(defn with-runtime [f]
  (let [thread (Thread/currentThread) previous (.getContextClassLoader thread)]
    (try (.setContextClassLoader thread runtime-loader)
         (with-bindings {Compiler/LOADER runtime-loader #'*repl* true
                         #'*data-readers* (merge @runtime-data-readers *data-readers*)} (f))
         (finally (.setContextClassLoader thread previous)))))

(defn- add-libs! [coords]
  (when (seq coords)
    (with-runtime
      #(let [previous *data-readers*
             result (deps/add-libs coords)]
         (swap! runtime-data-readers merge
                (into {} (remove (fn [[tag reader]] (= reader (get previous tag)))) *data-readers*))
         result))))

(defn validate [descriptor]
  (when-not (and (map? descriptor) (= 1 (:package/version descriptor))
                 (qualified-keyword? (:id descriptor)) (string? (:name descriptor))
                 (string? (:version descriptor))
                 (#{:applet :library :toolset :theme :workspace :composition :keymap} (:kind descriptor))
                 (or (nil? (:deps descriptor)) (map? (:deps descriptor)))
                 (or (nil? (:requires descriptor))
                     (and (vector? (:requires descriptor)) (every? qualified-keyword? (:requires descriptor)))))
    (throw (ex-info "Invalid package descriptor" {:id (:id descriptor)})))
  (when (and (= :applet (:kind descriptor)) (not (qualified-symbol? (:entry descriptor))))
    (throw (ex-info "An applet package needs a qualified :entry symbol" {:id (:id descriptor)})))
  (when (and (:setup descriptor) (not (qualified-symbol? (:setup descriptor))))
    (throw (ex-info "Package setup must be a qualified symbol" {:id (:id descriptor)})))
  (when (and (= :theme (:kind descriptor)) (or (seq (:deps descriptor)) (:entry descriptor) (:setup descriptor)))
    (throw (ex-info "A theme contains appearance data; executable extensions use applet or library packages" {:id (:id descriptor)})))
  (doseq [[lib coord] (:deps descriptor)]
    (when-not (and (qualified-symbol? lib) (map? coord)
                   (or (and (string? (:mvn/version coord))
                            (not (re-find #"(?i)LATEST|RELEASE|[\[\](),]" (:mvn/version coord))))
                       (and (string? (:git/url coord)) (re-matches #"[a-fA-F0-9]{40}" (or (:git/sha coord) "")))
                       (and (string? (:local/root coord)) (.isDirectory (io/file (:local/root coord))))))
      (throw (ex-info "Dependencies need a fixed Maven version, Git SHA, or existing local root" {:lib lib}))))
  (case (:kind descriptor)
    :theme (theme/validate (:data descriptor))
    :workspace (workspace/validate (:data descriptor))
    :composition (composition/validate (:data descriptor))
    :keymap (keymap/validate (:data descriptor))
    nil)
  descriptor)

(defn normalize [descriptor]
  (cond
    (:theme/version descriptor) {:package/version 1 :id (:id descriptor) :name (:name descriptor)
                                 :version (or (:version descriptor) "1") :kind :theme :data descriptor}
    (:workspace/version descriptor) {:package/version 1 :id (:id descriptor) :name (:name descriptor)
                                     :version (or (:version descriptor) "1") :kind :workspace :data descriptor}
    (:composition/version descriptor) {:package/version 1 :id (:id descriptor) :name (:name descriptor)
                                       :version (or (:version descriptor) "1") :kind :composition :data descriptor}
    (:keymap/version descriptor) {:package/version 1 :id (:id descriptor) :name (:name descriptor)
                                  :version (or (:version descriptor) "1") :kind :keymap :data descriptor}
    :else descriptor))

(defn builtins []
  (into {} (map (juxt :id identity))
        (concat (map-indexed (fn [index p] (cond-> p (< index 7) (assoc :featured? true :order index)))
                             (theme/read-data (io/resource "com/phronemophobic/easel/packages.edn")))
                (for [id [:easel/studio-light :easel/studio-dark]]
                  (assoc (normalize (theme/builtin id)) :bundled? true :description "Native semantic colors and component styles."))
                (for [id (keys composition/builtin-resources)]
                  (assoc (normalize (composition/builtin id)) :bundled? true :description "A composition of native views. Keeps the same running applets."))
                (for [id (keys keymap/builtin-resources)]
                  (assoc (normalize (keymap/builtin id)) :bundled? true :description "Scoped command and editor bindings, independent of appearance."))
                (for [id (keys workspace/builtin-resources)]
                  (assoc (normalize (workspace/builtin id)) :bundled? true :description "A native workspace preset.")))))

(defn catalog [] (merge (builtins) (:packages (settings/read-settings))))
(defn theme-data [id] (or (get-in (catalog) [id :data]) (theme/builtin id)))
(defn composition-data [id] (get-in (catalog) [id :data]))
(defn keymap-data [id] (get-in (catalog) [id :data]))
(defn workspace-data [id] (get-in (catalog) [id :data]))

(defn plan [registry ids]
  (let [seen (atom #{}) result (atom [])]
    (letfn [(visit [id path]
              (when (contains? path id) (throw (ex-info "Package dependency cycle" {:id id})))
              (when-not (@seen id)
                (let [p (or (get registry id) (throw (ex-info "Package dependency is missing" {:id id})))
                      p (validate p)]
                  (doseq [dependency (:requires p)] (visit dependency (conj path id)))
                  (when (= :theme (:kind p))
                    (when-let [parent (get-in p [:data :extends])]
                      (when-not (theme/builtin parent) (visit parent (conj path id))))
                    (theme/resolve-theme (:data p) #(or (theme/builtin %) (get-in registry [% :data]))))
                  (when (#{:composition :keymap} (:kind p))
                    (when-let [parent (get-in p [:data :extends])]
                      (when-not (= (:kind p) (:kind (get registry parent)))
                        (throw (ex-info "An inherited package must have the same kind" {:id id :parent parent})))
                      (visit parent (conj path id)))
                    (let [lookup #(get-in registry [% :data])]
                      (if (= :composition (:kind p))
                        (let [resolved (composition/resolve-composition (:data p) lookup)]
                          (when-let [keys (:keymap resolved)]
                            (when-not (= :keymap (:kind (get registry keys)))
                              (throw (ex-info "A composition needs an installed keymap package" {:id keys})))
                            (visit keys (conj path id))))
                        (keymap/resolve-keymap (:data p) lookup))))
                  (swap! seen conj id) (swap! result conj p))))]
      (doseq [id ids] (visit id #{}))
      @result)))

(defn- coordinates [packages]
  (reduce (fn [result p]
            (reduce-kv (fn [result lib coord]
                         (when (and (contains? result lib) (not= (get result lib) coord))
                           (throw (ex-info "Packages request conflicting library versions" {:lib lib})))
                         (assoc result lib coord)) result (:deps p {}))) {} packages))

(defn installed? [p]
  (or (:bundled? p) (contains? (:packages (settings/read-settings)) (:id p))))

(defn status [p]
  (cond (:restart? p) :restart
        (installed? p) :installed
        :else :available))

(defn install! [registry ids]
  (locking loaded
    (let [packages (plan registry ids)
          coords (coordinates packages)
          previous (settings/read-settings)
          _ (doseq [p packages :let [pin (get-in previous [:pins (:id p)])]
                    :when (and pin (not= pin (:version p)))]
              (throw (ex-info "Package version is pinned" {:id (:id p) :version pin})))
          current (:libs (basis/current-basis))
          changed (into {} (filter (fn [[lib coord]]
                                    (and (contains? current lib)
                                         (not= coord (select-keys (get current lib) (keys coord)))))) coords)
          user-libs (set (keys (coordinates (vals (:packages previous)))))
          _ (doseq [[lib _] changed :when (not (user-libs lib))]
              (throw (ex-info "A dependency conflicts with a library supplied by Easel" {:lib lib})))
          restart? (boolean (seq changed))]
      (if restart?
        (process/checked ["clojure" "-P" "-Sdeps" (pr-str {:deps coords})] {:timeout-ms 180000})
        (add-libs! coords))
      (when-not restart?
        (doseq [p packages :when (:setup p)]
          (with-runtime #((requiring-resolve (:setup p))))))
      (settings/update-settings!
       (fn [config]
         (reduce (fn [config p]
                   (if (:bundled? p) config
                       (assoc-in config [:packages (:id p)] (assoc p :restart? restart?)))) config packages)))
      (when-not restart? (swap! loaded merge coords))
      {:installed (mapv :id packages) :restart? restart?})))

(defn make-applet [handler id args]
  (let [p (or (get (catalog) id) (throw (ex-info "Unknown applet package" {:id id})))]
    (when-not (and (= :applet (:kind p)) (installed? p))
      (throw (ex-info "Install the applet from Tools first" {:id id})))
    (when (:restart? p) (throw (ex-info "Restart the JVM to load this package version" {:id id})))
    (with-runtime
      #(assoc (apply (requiring-resolve (:entry p)) handler (or args (:args p)))
              :package/id id :package/args (or args (:args p)) :icon (:icon p)))))

(defn remove! [id]
  (let [registry (catalog) p (get registry id)]
    (when-not p (throw (ex-info "Unknown package" {:id id})))
    (when (:bundled? p) (throw (ex-info "This package is included with Easel" {:id id})))
    (when-let [dependent (some (fn [[other p]]
                                (when (and (not= other id) (installed? p)
                                           (or (some #{id} (:requires p)) (= id (get-in p [:data :extends])) (= id (get-in p [:data :keymap])))) other)) registry)]
      (throw (ex-info "Another installed package needs this package" {:id id :dependent dependent})))
    (settings/update-settings! #(-> % (update :packages dissoc id) (update :pins dissoc id)))
    {:removed id :restart? (boolean (seq (:deps p)))}))

(defn pin! [id pinned?]
  (if pinned?
    (settings/update-settings! assoc-in [:pins id] (:version (get (catalog) id)))
    (settings/update-settings! update :pins dissoc id)))

(defn- git-source [source]
  (let [{url :git/url sha :git/sha path :path} source]
    (when-not (and (string? url) (re-matches #"[a-fA-F0-9]{40}" (or sha ""))
                   (string? path) (not (.isAbsolute (io/file path))))
      (throw (ex-info "A Git source needs :git/url, a full :git/sha, and a relative :path" {})))
    (let [root (settings/file "sources" (str sha "-" (Integer/toUnsignedString (hash url))))]
      (when-not (.isDirectory root)
        (.mkdirs (.getParentFile root))
        (process/checked ["git" "clone" "--filter=blob:none" "--no-checkout" "--" url (.getPath root)] {:timeout-ms 180000}))
      (process/checked ["git" "-C" (.getPath root) "checkout" "--detach" sha] {:timeout-ms 180000})
      (let [file (.getCanonicalFile (io/file root path))]
        (when-not (.startsWith (.toPath file) (.toPath (.getCanonicalFile root)))
          (throw (ex-info "The manifest must be inside the Git source" {})))
        file))))

(defn read-source [source]
  (let [source (str/trim source)]
    (cond
      (str/starts-with? source "{")
      (read-source (.getPath (git-source (theme/read-data (java.io.StringReader. source)))))
      (re-find #"^https?://" source)
      (let [connection (.openConnection (.toURL (URI. source)))]
        (.setConnectTimeout connection 10000) (.setReadTimeout connection 20000)
        (with-open [input (.getInputStream connection)] (theme/read-data input)))
      :else (let [file (.getCanonicalFile (io/file source))
            descriptor (normalize (theme/read-data file))]
        (update descriptor :deps
                #(into {} (map (fn [[lib coord]]
                                 [lib (if-let [root (:local/root coord)]
                                        (assoc coord :local/root (.getCanonicalPath (io/file (.getParentFile file) root)))
                                        coord)])) %))))))

(defn import! [source]
  (let [p (-> (read-source source) normalize (assoc :origin source :bundled? false) validate)]
    (install! (assoc (catalog) (:id p) p) [(:id p)])))

(declare select-theme!)
(defn- watch-theme! [handler package]
  (when-let [stop (first (swap-vals! theme-watcher (constantly nil)))] (stop))
  (when-let [origin (:origin package)]
    (when (.isFile (io/file origin))
      (reset! theme-watcher
              (theme/watch-file!
               origin
               (fn [resolved]
                (when (= (:id package) (:id (theme/current)))
                 (when-not (= (:id package) (:id resolved))
                   (throw (ex-info "The edited theme id must stay the same" {})))
                 (settings/update-settings! assoc-in [:packages (:id package) :data]
                                            (dissoc resolved ::theme/colors))
                 (handler :update :easel dissoc :theme-error)
                 (select-theme! handler (:id package) false false)))
               theme-data
               (fn [e] (when (= (:id package) (:id (theme/current)))
                         (events/report! e) (handler :update :easel assoc :theme-error (.getMessage e)))))))))

(defn select-theme! [handler id & [persist? watch?]]
  (let [resolved (theme/resolve-theme (or (theme-data id) (throw (ex-info "Theme is not installed" {:id id}))) theme-data)]
    (when-let [easel (handler :get :easel)]
      (let [descriptor (composition/prepare! (composition/current easel) resolved)]
        (composition/layout descriptor (:viewport easel [1536 1024]) {:easel easel :appearance resolved})))
    (theme/apply! resolved)
    (handler :update :easel
             #(let [easel (-> % (assoc :appearance resolved) (dissoc :theme-error))]
                ((requiring-resolve 'com.phronemophobic.easel/resize-window) easel (:viewport easel) (:content-scale easel))))
    (when persist? (settings/update-settings! assoc :theme id))
    (when (not= false watch?) (watch-theme! handler (get (catalog) id)))
    resolved))

(defn edit-theme! [handler id]
  (let [package (get (catalog) id)
        target (when-let [origin (:origin package)] (when (.isFile (io/file origin)) (io/file origin)))
        [id target] (if target [id target]
                        (let [id (keyword "user" (name id))
                              target (settings/file "themes" (str (name id) ".edn"))]
                          (when-not (.exists target)
                            (settings/write-data! target {:theme/version 1 :id id :name (str (:name package) " custom")
                                                          :extends (:id package) :tokens {}}))
                          (import! (.getPath target))
                          [id target]))]
    (select-theme! handler id true)
    (handler :com.phronemophobic.easel/add-applet
             {:make-applet #((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet)
                             % {:file target})})
    (handler :com.phronemophobic.easel/close-tools)))

(defn- update-editor-keys [easel descriptor]
  (if (= (select-keys (:scopes (keymap/current easel)) [:editor :clojure])
         (select-keys (:scopes descriptor) [:editor :clojure]))
    easel
    (update easel :applets
          (fn [applets]
            (reduce-kv (fn [result id applet]
                         (assoc result id
                                (if (get-in applet [:state :editor])
                                  (update-in applet [:state :editor]
                                             (requiring-resolve 'com.phronemophobic.easel.clobber/apply-keymap) descriptor)
                                  applet))) (empty applets) applets)))))

(defn- resolve-keymap! [id registry]
  (let [resolved (keymap/resolve-keymap
                  (or (get-in registry [id :data]) (throw (ex-info "Keymap is not installed" {:id id})))
                  #(get-in registry [% :data]))]
    (doseq [scope [:editor :clojure] [_ intent] (get-in resolved [:scopes scope]) :when (symbol? intent)]
      (when-not (ifn? @(requiring-resolve intent)) (throw (ex-info "An editor binding must reference a function" {:symbol intent}))))
    resolved))

(declare select-composition! select-keymap!)

(defn- watch-data! [handler kind id]
  (when-let [stop (get (first (swap-vals! data-watchers dissoc kind)) kind)] (stop))
  (let [active? (atom true)
        files (keep (fn [p] (when-let [origin (:origin p)]
                               (when (and (#{:composition :keymap} (:kind p)) (.isFile (io/file origin)))
                                 [(.getCanonicalPath (io/file origin)) p]))) (vals (catalog)))
        watchers (mapv (fn [[path package]]
                         (watcher/watch
                          (bound-fn [{event-path :path :keys [type]}]
                            (when (and @active? (#{:modify :create :delete :overflow} type)
                                       (or (= :overflow type)
                                           (= path (.getCanonicalPath (io/file (str event-path))))))
                              (try
                                (when (some #{(:id package)}
                                            (map :id (plan (catalog)
                                                           (cond-> [id]
                                                             (and (= kind :composition) (:keymap/override (handler :get :easel)))
                                                             (conj (:keymap/override (handler :get :easel)))))))
                                 (let [updated (-> (read-source path) normalize validate)
                                      _ (when-not (and (= (:id package) (:id updated)) (= (:kind package) (:kind updated)))
                                          (throw (ex-info "An edited package must keep its id and kind" {})))
                                      registry (assoc (catalog) (:id package) (merge package updated))
                                      _ (plan registry [id])
                                      resolved (when (= kind :composition)
                                                 (composition/prepare! (composition/resolve-composition (get-in registry [id :data]) #(get-in registry [% :data]))))
                                      _ (when resolved
                                          (let [easel (handler :get :easel)]
                                            (composition/layout resolved (:viewport easel) {:easel easel :appearance (:appearance easel)})))
                                      keys-id (if (= kind :keymap) id
                                                  (or (:keymap/override (handler :get :easel)) (:keymap (settings/read-settings)) (:keymap resolved) :easel/default-keys))
                                      _ (resolve-keymap! keys-id registry)]
                                  (when @active?
                                    (settings/update-settings! update-in [:packages (:id package)] assoc :data (:data updated) :name (:name updated))
                                    (if (= kind :composition) (select-composition! handler id false false)
                                        (select-keymap! handler id false false))
                                    (handler :update :easel dissoc :configuration-error))))
                                (catch Throwable e
                                  (when @active?
                                    (events/report! e)
                                    (handler :update :easel assoc :configuration-error (.getMessage e)))))))
                          (.getParent (io/file path)))) files)]
    (swap! data-watchers assoc kind
           #(when (compare-and-set! active? true false) (run! watcher/stop watchers)))))

(defn select-composition! [handler id & [persist? watch?]]
  (with-runtime
    #(let [registry (catalog)
           _ (plan registry [id])
           descriptor (composition/prepare! (composition/resolve-composition
                                             (or (get-in registry [id :data]) (throw (ex-info "Composition is not installed" {:id id})))
                                             (fn [id] (get-in registry [id :data]))))
           keys (resolve-keymap! (or (:keymap/override (handler :get :easel)) (:keymap (settings/read-settings)) (:keymap descriptor) :easel/default-keys) registry)]
       (handler :update :easel
                (fn [easel]
                  (-> easel (update-editor-keys keys)
                      (assoc :composition descriptor :keymap keys)
                      (dissoc :key-prefix :pane-menu :configuration-error)
                      ((requiring-resolve 'com.phronemophobic.easel/refresh-window)))))
       (when persist? (settings/update-settings! assoc :composition id))
       (when (not= false watch?) (watch-data! handler :composition id))
       descriptor)))

(defn select-keymap! [handler id & [persist? watch?]]
  (with-runtime
    #(let [descriptor (resolve-keymap! id (catalog))]
       (handler :update :easel
                (fn [easel] (-> easel (update-editor-keys descriptor)
                                (assoc :keymap descriptor :keymap/override id)
                                (dissoc :key-prefix :configuration-error))))
       (when persist? (settings/update-settings! assoc :keymap id))
       (when (not= false watch?) (watch-data! handler :keymap id))
       descriptor)))

(defn edit-data! [handler id]
  (let [package (get (catalog) id) kind (:kind package)]
    (if (= :theme kind) (edit-theme! handler id)
      (let [target (when-let [origin (:origin package)] (when (.isFile (io/file origin)) (io/file origin)))
            [id target] (if target [id target]
                            (let [id (keyword "user" (name id)) target (settings/file (str (name kind) "s") (str (name id) ".edn"))]
                              (when-not (.exists target)
                                (settings/write-data! target {(keyword (name kind) "version") 1 :id id
                                                              :name (str (:name package) " custom") :extends (:id package)}))
                              (import! (.getPath target)) [id target]))]
        (if (= kind :composition) (select-composition! handler id true) (select-keymap! handler id true))
        (handler :com.phronemophobic.easel/add-applet
                 {:make-applet (fn [handler] ((requiring-resolve 'com.phronemophobic.easel.clobber/clobber-applet) handler {:file target}))})
        (handler :com.phronemophobic.easel/close-tools)))))

(defn suspend! [handler]
  (try
    (when-let [id (some #(when (:tool-manager? %) (get-in % [:state :preview-original]))
                        (vals (:applets (handler :get :easel))))]
      (select-theme! handler id))
    (finally
      (when-let [stop (first (swap-vals! theme-watcher (constantly nil)))] (stop))
      (run! (fn [[_ stop]] (stop)) (first (swap-vals! data-watchers (constantly {})))))))

(defn restore! [handler]
  (try
    (let [config (settings/read-settings)]
      (when (seq (:packages config))
        (let [coords (coordinates (plan (catalog) (keys (:packages config))))
              current (:libs (basis/current-basis))]
          (doseq [[lib coord] coords :when (and (get current lib)
                                               (not= coord (select-keys (get current lib) (keys coord))))]
            (throw (ex-info "Restart with a compatible dependency configuration to load this package" {:lib lib})))
          (add-libs! coords)
          (swap! loaded merge coords))
        (doseq [p (vals (:packages config)) :when (and (:restart? p) (:setup p))]
          (with-runtime #((requiring-resolve (:setup p)))))
        (settings/update-settings! update :packages
                                   #(into {} (map (fn [[id p]] [id (dissoc p :restart?)])) %)))
      (when-let [id (:theme config)] (select-theme! handler id))
      (select-composition! handler (:composition config :easel/minimal))
      (when-let [id (:keymap config)] (select-keymap! handler id)))
    (catch Throwable e (events/report! e))))

(defn package-applet [handler]
  ((requiring-resolve 'com.phronemophobic.easel.package-ui/package-applet) handler))
