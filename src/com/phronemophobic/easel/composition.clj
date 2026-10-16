(ns com.phronemophobic.easel.composition
  (:require [clojure.java.io :as io]
            [com.phronemophobic.easel.commands :as commands]
            [com.phronemophobic.easel.splitpane :as splitpane]
            [com.phronemophobic.easel.theme :as theme]))

(def builtin-resources
  {:easel/minimal "com/phronemophobic/easel/compositions/minimal.edn"
   :easel/studio "com/phronemophobic/easel/compositions/studio.edn"})

(defn builtin [id]
  (when-let [resource (builtin-resources id)] (theme/read-data (io/resource resource))))

(defn nodes [composition] (tree-seq #(seq (:children %)) :children (:root composition)))

(defn dimension? [value]
  (or (and (number? value) (Double/isFinite (double value)) (<= 0 value 100000))
      (qualified-keyword? value)
      (and (map? value) (qualified-keyword? (:token value))
           (number? (:offset value 0)) (Double/isFinite (double (:offset value 0))))))

(defn validate [descriptor]
  (when-not (and (= 1 (:composition/version descriptor)) (qualified-keyword? (:id descriptor))
                 (string? (:name descriptor))
                 (or (qualified-keyword? (:extends descriptor)) (map? (:root descriptor))))
    (throw (ex-info "Invalid composition descriptor" {:id (:id descriptor)})))
  (when (:root descriptor)
    (let [all (nodes descriptor) ids (map :id all)]
      (when-not (and (every? keyword? ids) (= (count ids) (count (set ids)))
                     (= 1 (count (filter #(= :workspace (:role %)) all)))
                     (<= (count (filter #(= :dock (:role %)) all)) 1))
        (throw (ex-info "Composition nodes need unique ids and one workspace region" {})))
      (doseq [node all]
        (when-not (or (and (#{:row :column :stack} (:layout node)) (vector? (:children node)) (not (:view node)))
                       (and (qualified-symbol? (:view node)) (not (:children node)))
                       (and (:spacer? node) (not (:view node)) (not (:children node))))
          (throw (ex-info "A composition node is a layout, a view var, or a spacer" {:node (:id node)})))
        (doseq [key [:width :height :gap :padding] :when (contains? node key)]
          (when-not (dimension? (get node key))
            (throw (ex-info "Invalid composition dimension" {:node (:id node) :key key}))))
        (doseq [key [:grow :max-width-ratio :max-height-ratio] :when (contains? node key)]
          (let [value (get node key)]
            (when-not (and (number? value) (Double/isFinite (double value)) (<= 0 value)
                           (or (= key :grow) (<= value 1)))
              (throw (ex-info "Invalid composition size ratio" {:node (:id node) :key key})))))
        (when (and (:background node) (not (qualified-keyword? (:background node))))
          (throw (ex-info "A composition background needs a color token" {:node (:id node)})))
        (when-let [predicate (:when node)]
          (when-not (or (boolean? predicate) (qualified-symbol? predicate)
                         (and (map? predicate) (qualified-symbol? (:predicate predicate)) (vector? (:args predicate []))))
            (throw (ex-info "Composition predicates are booleans or qualified function references" {:node (:id node)})))))))
  (when-let [header (get-in descriptor [:panes :header])]
    (when-not (and (map? header)
                   (if (:extends descriptor)
                     (and (or (not (contains? header :view)) (qualified-symbol? (:view header)))
                          (or (not (contains? header :height)) (dimension? (:height header))))
                     (and (qualified-symbol? (:view header)) (dimension? (:height header)))))
      (throw (ex-info "A pane header needs a view var and height" {}))))
  (when-let [keymap (:keymap descriptor)]
    (when-not (qualified-keyword? keymap) (throw (ex-info "A composition keymap needs a qualified id" {}))))
  (when-not (and (vector? (:commands descriptor []))
                 (every? #(or (qualified-symbol? %) (map? %)) (:commands descriptor)))
    (throw (ex-info "Composition commands are descriptors or provider vars" {})))
  (doseq [entry (:commands descriptor) :when (map? entry)] (commands/validate entry))
  (when-let [actions (get-in descriptor [:panes :actions])]
    (when-not (and (vector? actions) (every? qualified-keyword? actions))
      (throw (ex-info "Pane actions need command ids" {}))))
  (when-let [dock (:dock descriptor)]
    (let [items (:items dock []) ids (map :id items)]
      (when-not (and (dimension? (:header-height dock 40)) (vector? items)
                     (= (count ids) (count (set ids)))
                     (every? #(and (keyword? (:id %)) (string? (:label %))
                                    (or (and (qualified-keyword? (:tool %)) (nil? (:entry %)))
                                        (and (qualified-symbol? (:entry %)) (nil? (:tool %))))
                                    (or (nil? (:args %)) (vector? (:args %)))) items))
        (throw (ex-info "A dock needs uniquely named tool or entry descriptors" {})))))
  descriptor)

(defn resolve-composition
  ([descriptor] (resolve-composition descriptor builtin))
  ([descriptor lookup]
   (letfn [(inherit [value seen]
             (validate value)
             (when (seen (:id value)) (throw (ex-info "Composition inheritance cycle" {:id (:id value)})))
             (if-let [id (:extends value)]
               (let [parent (or (lookup id) (throw (ex-info "Parent composition is missing" {:id id})))
                     merged (theme/merge-data (inherit parent (conj seen (:id value))) value)]
                 (cond-> merged (:root value) (assoc :root (:root value))))
               value))]
     (let [result (inherit descriptor #{})]
       (when-not (:root result) (throw (ex-info "A resolved composition needs a root" {})))
       (validate (dissoc result :extends))
       (validate result)))))

(def default-composition (delay (resolve-composition (builtin :easel/minimal))))
(defn current [easel] (or (:composition easel) @default-composition))

(defn dimension [appearance value]
  (let [context {::theme/value appearance}
        result (cond (number? value) value
                     (keyword? value) (theme/token context value)
                     (map? value) (+ (or (theme/token context (:token value)) 0) (:offset value 0))
                     :else 0)]
    (when-not (and (number? result) (Double/isFinite (double result)))
      (throw (ex-info "A composition size token must resolve to a finite number" {:value value})))
    (max 0 result)))

(defn min-width? [{:keys [viewport]} minimum] (>= (first viewport) minimum))
(defn workspace-visible? [{:keys [easel]}] (not (or (:help easel) (:manager-id easel))))
(defn help-visible? [{:keys [easel]}] (boolean (:help easel)))
(defn manager-visible? [{:keys [easel]}] (and (not (:help easel)) (some? (:manager-id easel))))
(defn commands-visible? [{:keys [easel]}] (boolean (:commands easel)))
(defn dock-visible? [{:keys [easel]}]
  (and (:dock easel) (not (get-in easel [:dock :hidden?])) (workspace-visible? {:easel easel})))

(defn visible? [node environment]
  (let [predicate (:when node true)]
    (boolean (cond (boolean? predicate) predicate
                   (symbol? predicate) ((requiring-resolve predicate) environment)
                   :else (apply (requiring-resolve (:predicate predicate)) environment (:args predicate))))))

(defn prepare!
  ([descriptor] (prepare! descriptor (theme/current)))
  ([descriptor appearance]
   (commands/prepare! (:commands descriptor))
   (doseq [node (nodes descriptor) key [:width :height :gap :padding] :when (contains? node key)]
     (dimension appearance (get node key)))
   (doseq [node (nodes descriptor) :when (:background node)]
     (when-not (theme/token {::theme/value appearance} (:background node))
       (throw (ex-info "A composition background token is missing" {:node (:id node)}))))
   (dimension appearance (get-in descriptor [:panes :header :height] 0))
   (dimension appearance (get-in descriptor [:dock :header-height] 40))
   (doseq [node (nodes descriptor)
           symbol (remove nil? [(:view node) (when (symbol? (:when node)) (:when node))
                                (when (map? (:when node)) (get-in node [:when :predicate]))])]
     (when-not (ifn? @(requiring-resolve symbol))
       (throw (ex-info "A composition reference must name a function" {:symbol symbol}))))
   (when-let [symbol (get-in descriptor [:panes :header :view])]
     (when-not (ifn? @(requiring-resolve symbol))
       (throw (ex-info "A pane header must name a function" {:symbol symbol}))))
   descriptor))

(defn layout [descriptor size environment]
  (let [appearance (:appearance environment (theme/current))
        environment (assoc environment :viewport size)]
    (letfn [(extent [node key maximum]
              (when (contains? node key)
                (min maximum (dimension appearance (get node key))
                     (* maximum (get node (if (= key :width) :max-width-ratio :max-height-ratio) 1)))))
            (place [node x y width height]
              (when (visible? node environment)
                (let [frame {:id (:id node) :node node :role (:role node) :x x :y y :width width :height height}
                      padding (dimension appearance (:padding node 0))
                      px (min padding (/ width 2)) py (min padding (/ height 2))
                      inner-width (max 0 (- width (* 2 px))) inner-height (max 0 (- height (* 2 py)))
                      children (vec (filter #(visible? % environment) (:children node)))
                      column? (= :column (:layout node))
                      axis (if column? :height :width) available (if column? inner-height inner-width)
                      gap (if (> (count children) 1)
                            (min (dimension appearance (:gap node 0)) (/ available (dec (count children)))) 0)
                      sizes (when (#{:row :column} (:layout node))
                              (splitpane/child-sizes
                               (mapv (fn [child]
                                       (cond-> {(if column? :flex.grow/height :flex.grow/width) (:grow child 1)}
                                         (contains? child axis) (assoc axis (extent child axis available)))) children)
                               (if column? :column :row) (max 0 (- available (* gap (max 0 (dec (count children))))))))]
                  (into [frame]
                    (mapcat identity)
                    (if (= :stack (:layout node))
                      (for [child children
                            :let [cw (or (extent child :width inner-width) inner-width)
                                  ch (or (extent child :height inner-height) inner-height)
                                  [ax ay] (:align child [:start :start])
                                  offset (fn [alignment extra] (case alignment :end extra :center (/ extra 2) 0))]]
                        (place child (+ x px (offset ax (- inner-width cw)))
                               (+ y py (offset ay (- inner-height ch))) cw ch))
                      (loop [remaining children sizes sizes offset 0 result []]
                        (if-let [child (first remaining)]
                          (let [primary (first sizes)
                                cross (or (extent child (if column? :width :height) (if column? inner-width inner-height))
                                          (if column? inner-width inner-height))]
                            (recur (next remaining) (next sizes) (+ offset primary gap)
                                   (conj result (place child (+ x px (if column? 0 offset))
                                                       (+ y py (if column? offset 0))
                                                       (if column? cross primary) (if column? primary cross)))))
                          result)))))))]
      (vec (place (:root descriptor) 0 0 (max 0 (first size)) (max 0 (second size)))))))

(defn region [frames role] (some #(when (= role (:role %)) %) frames))
(defn provides? [frames capability]
  (boolean (some #(and (pos? (:width %)) (pos? (:height %)) (contains? (get-in % [:node :provides] #{}) capability)) frames)))
