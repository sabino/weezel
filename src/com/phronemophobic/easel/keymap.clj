(ns com.phronemophobic.easel.keymap
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.repl :as repl]
            [com.phronemophobic.clobber.util.ui.key-binding :as binding]
            [com.phronemophobic.easel.theme :as theme]
            [membrane.skia :as skia]
            [membrane.ui :as ui]))

(def builtin-resources
  {:easel/default-keys "com/phronemophobic/easel/keymaps/default.edn"
   :easel/studio-keys "com/phronemophobic/easel/keymaps/studio.edn"})

(defn builtin [id]
  (when-let [resource (builtin-resources id)] (theme/read-data (io/resource resource))))

(defn parse-chord [text]
  (loop [remaining text modifiers {}]
    (if-let [[prefix key] (some (fn [[prefix key]] (when (str/starts-with? remaining prefix) [prefix key]))
                                [["C-" :ctrl?] ["M-" :meta?] ["S-" :super?]])]
      (recur (subs remaining 2) (assoc modifiers key true))
      (let [key (or ((merge binding/special-keys {"ESC" :escape "RET" :enter "SPC" \space "TAB" :tab "DEL" :backspace}) remaining)
                    (when (re-matches #"F(?:[1-9]|1[0-9]|2[0-5])" remaining) (keyword (str/lower-case remaining)))
                    (when (= 1 (count remaining)) (first remaining)))]
        (when-not key (throw (ex-info "Invalid key chord" {:keys text})))
        (assoc modifiers :key key)))))

(defn parse-keys [text]
  (when-not (and (string? text) (not (str/blank? text))) (throw (ex-info "A key binding needs a key sequence" {})))
  (mapv parse-chord (str/split text #"\s+")))

(defn validate [descriptor]
  (when-not (and (= 1 (:keymap/version descriptor)) (qualified-keyword? (:id descriptor)) (string? (:name descriptor))
                 (or (nil? (:extends descriptor)) (qualified-keyword? (:extends descriptor)))
                 (map? (:scopes descriptor {})))
    (throw (ex-info "Invalid keymap descriptor" {})))
  (doseq [[scope bindings] (:scopes descriptor)]
    (when-not (and (keyword? scope) (map? bindings)) (throw (ex-info "A keymap scope needs a binding map" {})))
    (doseq [[keys command] bindings]
      (parse-keys keys)
      (when-not (or (nil? command) (qualified-keyword? command)
                     (and (#{:editor :clojure} scope) (qualified-symbol? command)))
        (throw (ex-info "Bindings name commands, editor intents, or editor function vars" {:scope scope :keys keys})))))
  descriptor)

(defn resolve-keymap
  ([descriptor] (resolve-keymap descriptor builtin))
  ([descriptor lookup]
   (letfn [(inherit [value seen]
             (validate value)
             (when (seen (:id value)) (throw (ex-info "Keymap inheritance cycle" {})))
             (if-let [id (:extends value)]
               (theme/merge-data (inherit (or (lookup id) (throw (ex-info "Parent keymap is missing" {:id id})))
                                         (conj seen (:id value))) value)
               value))]
     (let [result (inherit descriptor #{})]
       (assoc result ::compiled
              (into {} (for [[scope bindings] (:scopes result) :when (not (#{:editor :clojure} scope))]
                         [scope (vec (for [[keys command] bindings :when command]
                                       {:keys keys :chords (parse-keys keys) :command command}))])))))))

(def default-keymap (delay (resolve-keymap (builtin :easel/default-keys))))
(defn current [easel] (or (:keymap easel) @default-keymap))

(defn event-chord [key mods]
  (let [special (get skia/keymap key)
        value (cond (keyword? special) (case special :space \space :kp_enter :enter special)
                    (pos? (bit-and mods ui/SHIFT-MASK)) (get binding/uppercase (char key) (char key))
                    :else (Character/toLowerCase (char key)))]
    (cond-> {:key value}
      (pos? (bit-and mods ui/CONTROL-MASK)) (assoc :ctrl? true)
      (pos? (bit-and mods ui/ALT-MASK)) (assoc :meta? true)
      (pos? (bit-and mods ui/SUPER-MASK)) (assoc :super? true))))

(defn match-event [descriptor scopes pending key mods enabled?]
  (let [sequence (conj (vec pending) (event-chord key mods))
        bindings (mapcat #(get (::compiled descriptor) %) scopes)
        matches (filter #(and (enabled? (:command %)) (= sequence (vec (take (count sequence) (:chords %))))) bindings)]
    (if-let [complete (some #(when (= (count sequence) (count (:chords %))) %) matches)]
      {:command (:command complete)}
      (when (seq matches) {:pending sequence}))))

(defn scopes [easel context]
  (let [focus (:focus context) applet (when-not (or (:help easel) (:manager-id easel)) (get-in easel [:applets focus]))]
    (cond-> [:global]
      (or (nil? focus)
          (and applet (not (:keymap/input? applet)))
          (and (or (uuid? focus) (integer? focus)) (nil? applet))) (conj :workspace)
      (:package/id applet) (conj (:package/id applet)))))

(defn shortcuts
  ([descriptor command] (shortcuts descriptor [:global :workspace] command))
  ([descriptor scopes command]
   (vec (for [scope scopes
              [keys target] (sort-by key (get-in descriptor [:scopes scope])) :when (= command target)] keys))))

(defn shortcut
  ([descriptor command] (first (shortcuts descriptor command)))
  ([descriptor scopes command] (first (shortcuts descriptor scopes command))))

(defn editor-bindings [base descriptor clojure?]
  (reduce-kv (fn [bindings keys intent]
               (if (nil? intent) (dissoc bindings keys)
                   (assoc bindings keys (if (symbol? intent) (requiring-resolve intent) intent))))
             base (merge (get-in descriptor [:scopes :editor]) (when clojure? (get-in descriptor [:scopes :clojure])))))

(defn binding-target [value]
  (cond
    (var? value) (symbol (str (ns-name (:ns (meta value)))) (str (:name (meta value))))
    (fn? value) (symbol (repl/demunge (.getName (class value))))
    :else value))

(defn editor-keys [easel]
  (let [editor (or (get-in easel [:applets (get-in easel [:help :previous-focus]) :state :editor])
                   (some #(get-in % [:state :editor]) (vals (:applets easel))))
        clojure? (if editor (:easel/clojure? editor) true)
        base (or (:easel/base-key-bindings editor)
                 @(requiring-resolve
                   (if clojure? 'com.phronemophobic.clobber.modes.clojure.ui/clojure-key-bindings
                       'com.phronemophobic.clobber.modes.text.ui/key-bindings)))]
    (editor-bindings base (current easel) clojure?)))

(defn editor-shortcuts [bindings action]
  (vec (for [[keys target] (sort-by key bindings)
             :when (= (binding-target target) (binding-target action))] keys)))

(defn binding-label [target]
  (let [target (binding-target target)]
    (-> (name target) (str/replace #"^(editor-|init-)" "")
        (str/replace "-" " ") str/capitalize)))

(defn editor-prefix? [easel focus prefix]
  (boolean
    (some (fn [[keys _]]
            (let [chords (parse-keys keys)]
              (and (> (count chords) (count prefix))
                   (= prefix (vec (take (count prefix) chords))))))
          (get-in easel [:applets focus :state :editor :key-bindings]))))
