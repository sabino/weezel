(ns com.phronemophobic.easel.commands
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [com.phronemophobic.easel.theme :as theme]))

(def builtin (delay (theme/read-data (io/resource "com/phronemophobic/easel/commands.edn"))))

(defn validate [descriptor]
  (when-not (and (qualified-keyword? (:id descriptor)) (string? (:name descriptor))
                 (vector? (:intents descriptor))
                 (every? #(and (vector? %) (keyword? (first %))) (:intents descriptor))
                 (or (boolean? (:enabled? descriptor true)) (qualified-symbol? (:enabled? descriptor))))
    (throw (ex-info "A command needs an id, name, and intent vectors" {:command (:id descriptor)})))
  descriptor)

(defn prepare! [entries]
  (doseq [entry entries]
    (if (symbol? entry)
      (when-not (ifn? @(requiring-resolve entry))
        (throw (ex-info "A command provider must be a function" {:symbol entry})))
      (do (validate entry)
          (when (symbol? (:enabled? entry))
            (when-not (ifn? @(requiring-resolve (:enabled? entry)))
              (throw (ex-info "A command predicate must be a function" {:command (:id entry)})))))))
  entries)

(defn applet-present? [environment] (some? (:applet-id environment)))
(defn multiple-panes? [{:keys [easel]}]
  (> (count (filter #(empty? (:panes %)) (tree-seq :panes :panes (:root-pane easel)))) 1))
(defn child-pane? [{:keys [pane-id]}]
  (and pane-id (not= :com.phronemophobic.easel/root-pane pane-id)))
(defn dock-present? [{:keys [easel]}]
  (some #(= :dock (:role %))
        (tree-seq :children :children (:root ((requiring-resolve 'com.phronemophobic.easel.composition/current) easel)))))
(defn repl-configured? [{:keys [easel]}]
  (some #(= :repl (:id %)) (get-in ((requiring-resolve 'com.phronemophobic.easel.composition/current) easel) [:dock :items])))
(defn cancellable? [{:keys [easel]}]
  (boolean (or (:commands easel) (:help easel) (:pane-menu easel)
               (get-in easel [:root-pane :com.phronemophobic.easel/pane-resize]) (:key-prefix easel))))

(defn enabled? [command environment]
  (let [predicate (:enabled? command true)]
    (and command (boolean (if (symbol? predicate) ((requiring-resolve predicate) environment) predicate)))))

(defn intents [command environment]
  (when (enabled? command environment)
    (let [bindings {'?pane-id (:pane-id environment) '?applet-id (:applet-id environment)
                    '?root-pane-id :com.phronemophobic.easel/root-pane '?$easel (:easel-path environment '(keypath :easel))}]
      (walk/postwalk #(if (symbol? %) (get bindings % %) %) (:intents command)))))

(defn registry [composition environment]
  (reduce (fn [commands entry]
            (if (symbol? entry)
              (into commands (map (juxt :id identity)) (map validate ((requiring-resolve entry) environment)))
              (assoc commands (:id entry) entry))) @builtin (:commands composition)))

(defn matching [commands query category]
  (let [words (remove str/blank? (str/split (str/lower-case (str query)) #"\s+"))]
    (->> commands
         (filter #(not= false (:palette? %)))
         (filter #(or (nil? category) (= category (:category %))))
         (filter (fn [command]
                   (let [haystack (str/lower-case (str (:name command) " " (:description command) " "
                                                       (:keywords command) " " (:id command)))]
                     (every? #(str/includes? haystack %) words))))
         (sort-by #(vector (boolean (:disabled? %)) (:order % 50) (:name %)))
         vec)))
