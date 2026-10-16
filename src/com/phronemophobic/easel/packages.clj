(ns com.phronemophobic.easel.packages
  (:require [clojure.java.basis :as basis]
            [clojure.java.io :as io]
            [clojure.repl.deps :as deps]
            [clojure.string :as str]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.process :as process]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.workspace :as workspace])
  (:import [java.net URI URLConnection]
           [clojure.lang DynamicClassLoader Compiler]))

(defonce runtime-loader (DynamicClassLoader. (clojure.lang.RT/baseLoader)))
(defonce loaded (atom {}))
(defonce theme-watcher (atom nil))

(defn with-runtime [f]
  (let [thread (Thread/currentThread) previous (.getContextClassLoader thread)]
    (try (.setContextClassLoader thread runtime-loader)
         (with-bindings {Compiler/LOADER runtime-loader #'*repl* true} (f))
         (finally (.setContextClassLoader thread previous)))))

(defn validate [descriptor]
  (when-not (and (map? descriptor) (= 1 (:package/version descriptor))
                 (qualified-keyword? (:id descriptor)) (string? (:name descriptor))
                 (string? (:version descriptor))
                 (#{:applet :library :toolset :theme :workspace} (:kind descriptor))
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
    nil)
  descriptor)

(defn normalize [descriptor]
  (cond
    (:theme/version descriptor) {:package/version 1 :id (:id descriptor) :name (:name descriptor)
                                 :version (or (:version descriptor) "1") :kind :theme :data descriptor}
    (:workspace/version descriptor) {:package/version 1 :id (:id descriptor) :name (:name descriptor)
                                     :version (or (:version descriptor) "1") :kind :workspace :data descriptor}
    :else descriptor))

(defn builtins []
  (into {} (map (juxt :id identity))
        (concat (map-indexed (fn [index p] (cond-> p (< index 7) (assoc :featured? true :order index)))
                             (theme/read-data (io/resource "com/phronemophobic/easel/packages.edn")))
                (for [id [:easel/studio-light :easel/studio-dark]]
                  (assoc (normalize (theme/builtin id)) :bundled? true :description "Native semantic colors and component styles."))
                (for [id (keys workspace/builtin-resources)]
                  (assoc (normalize (workspace/builtin id)) :bundled? true :description "A native workspace preset.")))))

(defn catalog [] (merge (builtins) (:packages (settings/read-settings))))
(defn theme-data [id] (or (get-in (catalog) [id :data]) (theme/builtin id)))
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
        (with-runtime #(deps/add-libs coords)))
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
                                           (or (some #{id} (:requires p)) (= id (get-in p [:data :extends])))) other)) registry)]
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

(defn suspend! [handler]
  (try
    (when-let [id (some #(when (:tool-manager? %) (get-in % [:state :preview-original]))
                        (vals (:applets (handler :get :easel))))]
      (select-theme! handler id))
    (finally
      (when-let [stop (first (swap-vals! theme-watcher (constantly nil)))] (stop)))))

(defn restore! [handler]
  (try
    (let [config (settings/read-settings)]
      (when (seq (:packages config))
        (let [coords (coordinates (plan (catalog) (keys (:packages config))))
              current (:libs (basis/current-basis))]
          (doseq [[lib coord] coords :when (and (get current lib)
                                               (not= coord (select-keys (get current lib) (keys coord))))]
            (throw (ex-info "Restart with a compatible dependency configuration to load this package" {:lib lib})))
          (with-runtime #(deps/add-libs coords))
          (swap! loaded merge coords))
        (doseq [p (vals (:packages config)) :when (and (:restart? p) (:setup p))]
          (with-runtime #((requiring-resolve (:setup p)))))
        (settings/update-settings! update :packages
                                   #(into {} (map (fn [[id p]] [id (dissoc p :restart?)])) %)))
      (when-let [id (:theme config)] (select-theme! handler id)))
    (catch Throwable e (events/report! e))))

(defn package-applet [handler]
  ((requiring-resolve 'com.phronemophobic.easel.package-ui/package-applet) handler))
