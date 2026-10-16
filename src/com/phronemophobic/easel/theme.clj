(ns com.phronemophobic.easel.theme
  "Versioned EDN appearance data shared by Easel and its applets."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel.file-watcher :as watcher]))

(def builtin-resources
  {:easel/base "com/phronemophobic/easel/themes/base.edn"
   :easel/studio-light "com/phronemophobic/easel/themes/studio_light.edn"
   :easel/studio-dark "com/phronemophobic/easel/themes/studio_dark.edn"})

(defn read-data
  ([source] (read-data source (* 1024 1024)))
  ([source limit]
  (let [text (with-open [reader (io/reader source)]
               (let [result (StringBuilder.) buffer (char-array 4096)]
                 (loop []
                   (let [n (.read reader buffer)]
                     (when (pos? n)
                       (when (> (+ (.length result) n) limit)
                         (throw (ex-info "EDN document exceeds its size limit" {:limit limit})))
                       (.append result buffer 0 n)
                       (recur))))
                 (.toString result)))]
    (with-open [reader (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [opts {:readers {} :default (fn [tag _] (throw (ex-info "Unknown EDN tag" {:tag tag})))}
            value (edn/read opts reader)]
        (when-not (= ::end (edn/read (assoc opts :eof ::end) reader))
          (throw (ex-info "Expected one EDN value" {})))
        value)))))

(defn builtin [id]
  (when-let [resource (get builtin-resources id)]
    (read-data (io/resource resource))))

(defn merge-data [base overrides]
  (merge-with (fn [a b] (if (and (map? a) (map? b)) (merge-data a b) b)) base overrides))

(defn color [hex]
  (when-not (and (string? hex) (re-matches #"#[0-9a-fA-F]{6}([0-9a-fA-F]{2})?" hex))
    (throw (ex-info "Expected a color in #RRGGBB or #RRGGBBAA form" {:value hex})))
  (mapv #(/ (Long/parseLong (apply str %) 16) 255.0) (partition 2 (subs hex 1))))

(defn- number-token? [key]
  (or (#{"space" "metric" "radius"} (namespace key))
      (#{:font/size :font/code-size} key)))

(defn- validate-components [recipes tokens]
  (when-not (or (nil? recipes) (map? recipes))
    (throw (ex-info "Component recipes must be a map" {})))
  (doseq [[id recipe] recipes]
    (when-not (and (keyword? id) (map? recipe))
      (throw (ex-info "A component recipe must be a map" {:component id})))
    (doseq [[key value] recipe]
      (when (and tokens (qualified-keyword? value) (not (contains? tokens value)))
        (throw (ex-info "Unknown component token" {:component id :token value})))
      (when (#{:height :width :padding :radius :gap :font-size} key)
        (let [value (if (and tokens (qualified-keyword? value)) (get tokens value) value)]
          (when-not (or (nil? value) (and (nil? tokens) (qualified-keyword? value))
                         (and (number? value) (Double/isFinite (double value)) (<= 0 value 4096)))
            (throw (ex-info "Component dimensions must be finite numbers or dimension tokens" {:component id :key key}))))))))

(defn validate [theme]
  (when-not (and (map? theme) (= 1 (:theme/version theme))
                 (qualified-keyword? (:id theme)) (string? (:name theme))
                 (map? (:tokens theme))
                 (or (nil? (:extends theme)) (qualified-keyword? (:extends theme)))
                 (or (nil? (:components theme)) (map? (:components theme)))
                 (or (nil? (:applets theme)) (map? (:applets theme))))
    (throw (ex-info "Invalid theme descriptor" {:id (:id theme)})))
  (doseq [[key value] (:tokens theme)]
    (when-not (qualified-keyword? key)
      (throw (ex-info "Theme tokens must use qualified keywords" {:token key})))
    (when-not (keyword? value)
      (cond
        (= "color" (namespace key)) (color value)
        (number-token? key)
        (when-not (and (number? value) (Double/isFinite (double value)) (<= 0 value 4096))
          (throw (ex-info "Theme dimensions must be finite values between 0 and 4096" {:token key})))
        (#{:font/ui :font/code} key)
        (when-not (and (vector? value) (seq value) (every? string? value))
          (throw (ex-info "Font families must be a nonempty vector of names" {:token key}))))))
  (validate-components (:components theme) nil)
  (doseq [[id style] (:applets theme)]
    (when-not (and (qualified-keyword? id) (map? style))
      (throw (ex-info "Applet styles need qualified ids and maps" {:id id})))
    (when (:tokens style) (validate (assoc theme :tokens (:tokens style) :applets {}))))
  theme)

(defn- resolve-tokens [tokens]
  (letfn [(resolve-token [key seen]
            (when (contains? seen key) (throw (ex-info "Theme token cycle" {:token key})))
            (when-not (contains? tokens key) (throw (ex-info "Unknown theme token" {:token key})))
            (let [value (get tokens key)]
              (if (keyword? value) (resolve-token value (conj seen key)) value)))]
    (into {} (map (fn [key] [key (resolve-token key #{})])) (keys tokens))))

(defn resolve-theme
  ([descriptor] (resolve-theme descriptor builtin))
  ([descriptor lookup]
   (letfn [(inherit [theme seen]
             (validate theme)
             (when (contains? seen (:id theme))
               (throw (ex-info "Theme inheritance cycle" {:id (:id theme)})))
             (if-let [parent (:extends theme)]
               (if-let [base (lookup parent)]
                 (merge-data (inherit base (conj seen (:id theme))) theme)
                 (throw (ex-info "Parent theme is not installed" {:id parent})))
               theme))]
     (let [theme (inherit descriptor #{})
           tokens (:tokens theme)
           resolved (resolve-tokens tokens)
           applets (into {}
                         (map (fn [[id style]]
                                (let [local (resolve-tokens (merge resolved (:tokens style)))]
                                  (validate (assoc theme :tokens local :applets {}))
                                  (validate-components (:components style) local)
                                  [id (cond-> style (:tokens style) (assoc :tokens (select-keys local (keys (:tokens style)))))])))
                         (:applets theme))]
       (validate (assoc theme :tokens resolved))
       (validate-components (:components theme) resolved)
       (assoc theme :tokens resolved :applets applets
              ::colors (into {} (keep (fn [[key value]]
                                       (when (= "color" (namespace key)) [key (color value)]))) resolved))))))

(def default-theme (delay (resolve-theme (builtin :easel/studio-light))))
(defonce active-theme (atom nil))

(defn current
  ([] (or @active-theme @default-theme))
  ([context] (or (::value context) (current))))

(defn token
  ([key] (token nil key))
  ([context key]
   (let [theme (current context)
         local (get-in theme [:applets (::applet context) :tokens key])
         local (if (keyword? local) (token (dissoc context ::applet) local) local)]
     (or (when (some? local) (if (and (= "color" (namespace key)) (string? local)) (color local) local))
         (get (::colors theme) key)
         (get (:tokens theme) key)
         (get-in @default-theme [::colors key])
         (get-in @default-theme [:tokens key])))))

(defn component [context key]
  (let [theme (current context)
        values (merge (get-in theme [:components key]) (get-in theme [:applets (::applet context) :components key]))]
    (into {} (map (fn [[k v]] [k (if (qualified-keyword? v) (token context v) v)])) values)))

(defn applet-style [context id]
  (get-in (current context) [:applets id] {}))

(defn apply! [resolved]
  (reset! active-theme resolved)
  resolved)

(defn watch-file!
  ([file on-change] (watch-file! file on-change builtin))
  ([file on-change lookup] (watch-file! file on-change lookup events/report!))
  ([file on-change lookup on-error]
  (let [active? (atom true)
        watcher (watcher/watch
                 (fn [{:keys [type]}]
                   (when (and @active? (#{:modify :create} type))
                     (try
                       (let [data (read-data file)
                             descriptor (if (and (= 1 (:package/version data)) (= :theme (:kind data))) (:data data) data)
                             resolved (resolve-theme descriptor lookup)]
                         (when @active? (on-change resolved)))
                       (catch Throwable e (on-error e)))))
                 (.getCanonicalPath (io/file file)))]
    (fn []
      (when (compare-and-set! active? true false) (watcher/stop watcher))))))
