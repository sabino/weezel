(ns com.phronemophobic.easel.workspace
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.sketchbook :as sketchbook]
            [com.phronemophobic.easel.theme :as theme]))

(def builtin-resources
  {:easel/sketchbook "com/phronemophobic/easel/workspaces/sketchbook.edn"
   :easel/data-lab "com/phronemophobic/easel/workspaces/data_lab.edn"
   :easel/notes "com/phronemophobic/easel/workspaces/notes.edn"})

(def entries
  {:easel/clobber 'com.phronemophobic.easel.clobber/clobber-applet
   :easel/documentation 'com.phronemophobic.easel.documentation/documentation-applet
   :easel/repl 'com.phronemophobic.easel.repl/repl-applet
   :easel/inspector 'com.phronemophobic.easel.inspector/inspector-applet})

(defn builtin [id]
  (when-let [resource (get builtin-resources id)] (theme/read-data (io/resource resource))))

(defn validate [descriptor]
  (when-not (and (= 1 (:workspace/version descriptor)) (qualified-keyword? (:id descriptor))
                 (string? (:name descriptor)) (vector? (:applets descriptor)) (map? (:layout descriptor)))
    (throw (ex-info "Invalid workspace descriptor" {})))
  (let [slots (map :slot (:applets descriptor))
        layout-slots (keep :slot (tree-seq :panes :panes (:layout descriptor)))]
    (when-not (and (every? keyword? slots) (= (count slots) (count (set slots)))
                   (every? (set slots) layout-slots) (= (count layout-slots) (count (set layout-slots))))
      (throw (ex-info "Workspace slots must be unique and reference declared applets" {}))))
  descriptor)

(defn- make-factory [descriptor session]
  (let [{:keys [tool args session-source? session-docs? label]} descriptor]
    (fn [handler]
      (let [a (case tool
                :easel/orbit (sketchbook/orbit-applet handler (:source* session) (:function session))
                :easel/scene-inspector (sketchbook/scene-inspector-applet handler (:source* session) (:function session))
                (if-let [entry (get entries tool)]
                  (let [args (cond session-source? [{:string (:source session) :ns (:ns session) :label label
                                                    :initial-value (:result session)}]
                                   session-docs? [{:compact? true :target {:kind :var :symbol
                                                                        (symbol (str (:ns session)) "points")}}]
                                   (= tool :easel/repl) [(or (:ns session) (first args) 'user)]
                                   :else args)]
                    (apply (requiring-resolve entry) handler args))
                  ((requiring-resolve 'com.phronemophobic.easel.packages/make-applet) handler tool args)))]
        (cond-> (assoc a :package/id tool :package/args args)
          label (assoc :label label)
          (= tool :easel/clobber) (assoc :icon "file-text"))))))

(defn layout [template ids]
  (walk/postwalk
   (fn [value]
     (if (map? value)
       (cond-> (assoc value :id (random-uuid))
         (:slot value) (assoc :applet-id (get ids (:slot value))))
       value))
   template))

(defn open! [handler preset-id workspace-id]
  (let [descriptor (validate (or (builtin preset-id)
                                 ((requiring-resolve 'com.phronemophobic.easel.packages/workspace-data) preset-id)))
        session (when-let [ns-sym (get-in descriptor [:session :namespace])] (sketchbook/session ns-sym))
        _ (when-let [scene (get-in descriptor [:session :scene])]
            (sketchbook/scene-value (atom scene))
            (reset! (:source* session) scene))
        ids (into {} (map (fn [a] [(:slot a) (random-uuid)])) (:applets descriptor))
        dock (into {} (keep #(when-let [slot (:dock %)] [slot (get ids (:slot %))])) (:applets descriptor))]
    (doseq [a (:applets descriptor)]
      (handler :com.phronemophobic.easel/add-applet
               {:id (get ids (:slot a)) :dock-slot (:dock a) :make-applet (make-factory a session)}))
    (handler :update :easel
             (fn [easel]
               (let [root (assoc (layout (:layout descriptor) ids) :id :com.phronemophobic.easel/root-pane
                                 :width (first (:size easel)) :height (second (:size easel)))
                     easel (-> easel
                               (assoc :root-pane root :workspace-id workspace-id :manager-id nil
                                      :eval-ns (or (:ns session) 'user)
                                      :dock {:selected (or (when (:repl dock) :repl) (first (keys dock))) :applets dock})
                               (assoc-in [:workspaces :by-id workspace-id]
                                         {:id workspace-id :name (:name descriptor) :preset preset-id
                                          :root-pane root :dock {:selected :repl :applets dock}}))]
                 ((requiring-resolve 'com.phronemophobic.easel/resize-window)
                  easel (:viewport easel [1536 1024]) (:content-scale easel [1 1])))))
    (handler :set '[(keypath :membrane.component/context) (keypath :focus)]
             (or (get ids :editor) (get ids :notes)))))

(defn initialize! [handler preset-id]
  (let [custom (sort-by :id (filter #(and (= :workspace (:kind %)) (not (:bundled? %)))
                                   (vals ((requiring-resolve 'com.phronemophobic.easel.packages/catalog)))))
        by-id (into {1 {:id 1 :name "Sketchbook" :preset :easel/sketchbook}
                     2 {:id 2 :name "Data lab" :preset :easel/data-lab}
                     3 {:id 3 :name "Notes" :preset :easel/notes}}
                    (map-indexed (fn [index p] (let [id (+ 4 index)] [id {:id id :name (:name p) :preset (:id p)}])) custom))
        id (or (some (fn [[id w]] (when (= preset-id (:preset w)) id)) by-id) 1)]
    (handler :update :easel assoc :workspaces {:last-id (+ 3 (count custom)) :by-id by-id})
    (open! handler preset-id id)))

(defn snapshot [easel id name]
  (let [visible (vec (distinct (concat (keep :applet-id (tree-seq :panes :panes (:root-pane easel)))
                                       (vals (get-in easel [:dock :applets])))))
        slots (zipmap visible (map #(keyword (str "applet-" %)) (range)))
        ns-sym (or (:eval-ns easel) 'user)
        scene* (some-> (find-ns ns-sym) (ns-resolve 'scene*) deref)
        descriptors (mapv (fn [applet-id]
                            (let [a (get (:applets easel) applet-id)
                                  tool (:package/id a)
                                  editor (get-in a [:state :editor])
                                  args (cond editor [{:string (str (:rope editor)) :ns (some-> (:eval-ns editor) ns-name)
                                                     :label (:label a)}]
                                             (= tool :easel/repl) [(get-in a [:state :ns])]
                                             (= tool :easel/documentation) [(select-keys (:state a) [:compact? :target])]
                                             :else (:package/args a))]
                              (when-not tool (throw (ex-info "This applet has no package descriptor for a saved preset" {:applet (:label a)})))
                              (cond-> {:slot (slots applet-id) :tool tool :args args :label (:label a)}
                                (:dock-slot a) (assoc :dock (:dock-slot a))))) visible)
        layout (letfn [(save-pane [pane]
                         (cond-> (select-keys pane [:direction :flex.grow/width :flex.grow/height])
                           (:applet-id pane) (assoc :slot (slots (:applet-id pane)))
                           (seq (:panes pane)) (assoc :panes (mapv save-pane (:panes pane)))))]
                 (save-pane (:root-pane easel)))]
    (cond-> {:workspace/version 1 :id id :name name :applets descriptors :layout layout}
      (and scene* (instance? clojure.lang.IDeref scene*))
      (assoc :session {:namespace ns-sym :scene (select-keys @scene* [:radius :count :selected])}))))

(defn save! [handler]
  (let [easel (handler :get :easel)
        index (inc (get-in easel [:workspaces :last-id] 0))
        id (keyword "user" (str "workspace-" (random-uuid)))
        name (str (get-in easel [:workspaces :by-id (:workspace-id easel) :name] "Workspace") " " index)
        descriptor (validate (snapshot easel id name))
        file (settings/file "workspaces" (str (clojure.core/name id) ".edn"))]
    (settings/write-data! file descriptor)
    ((requiring-resolve 'com.phronemophobic.easel.packages/import!) (.getPath file))
    (settings/update-settings! assoc :workspace id)
    (handler :update :easel #(-> % (assoc-in [:workspaces :last-id] index)
                                  (assoc-in [:workspaces :by-id index]
                                            {:id index :name name :preset id :root-pane (:root-pane easel)
                                             :dock (:dock easel) :eval-ns (:eval-ns easel)})))
    name))
