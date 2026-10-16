(ns com.phronemophobic.easel.commands
  (:require [clojure.java.io :as io]
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
