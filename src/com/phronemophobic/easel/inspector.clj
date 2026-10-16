(ns com.phronemophobic.easel.inspector
  (:require [clojure.core.protocols :as protocols]
            [clojure.datafy :as datafy]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.viscous :as viscous]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(def page-size 50)

(defn preview [obj]
  (let [output (StringBuilder.)
        overflow (ex-info "Preview truncated" {})
        append! (fn [value offset length]
                  (let [keep (min length (- 240 (.length output)))]
                    (if (string? value)
                      (.append output ^CharSequence value (int offset) (int (+ offset keep)))
                      (.append output ^chars value (int offset) (int keep)))
                    (when (< keep length) (throw overflow))))
        writer (proxy [java.io.Writer] []
                 (write
                   ([value]
                    (let [value (if (number? value) (str (char value)) value)]
                      (append! value 0 (count value))))
                   ([value offset length] (append! value offset length)))
                 (flush []) (close []))]
    (try
      (binding [*out* writer *print-length* 12 *print-level* 4] (pr obj))
      (.toString output)
      (catch Throwable e
        (if (identical? overflow e)
          (str output "…")
          (str "<" (some-> obj class .getName) " could not be printed>"))))))

(defn page [obj offset]
  (try
    (let [entries (cond
                    (map? obj) (seq obj)
                    (or (sequential? obj) (set? obj)) (map-indexed vector obj)
                    :else nil)
          chunk (vec (take (inc page-size) (drop offset entries)))]
      {:entries (vec (take page-size chunk))
       :more? (> (count chunk) page-size)})
    (catch Throwable e {:entries [] :more? false :error (str e)})))

(defn initial-state [obj]
  {:obj (viscous/wrap obj) :parents [] :offset 0 :query ""})

(defn display-value [obj]
  (if (instance? Throwable obj)
    (let [data (Throwable->map obj)]
      (merge (array-map :cause (:cause data) :data (:data data)) (dissoc data :cause :data)))
    obj))

(defn descend [state key value]
  (let [obj (display-value @(:obj state))]
    (-> (initial-state (protocols/nav obj key value))
        (assoc :parents (conj (:parents state) (select-keys state [:obj :offset :query]))))))

(defeffect ::navigate [$state key value]
  (let [state (dispatch! :get $state)]
    (applet/task! dispatch! $state #(descend state key @value))))

(defeffect ::transform [$state f]
  (let [state (dispatch! :get $state)]
    (applet/task! dispatch! $state
                  (fn []
                    (let [obj @(:obj state) result (f obj)]
                      (if (identical? obj result)
                        {:message "This value is already in that form."}
                        (-> (initial-state result)
                            (assoc :parents (conj (:parents state) (select-keys state [:obj :offset :query]))))))))))

(defui entry-view [{:keys [state entry width]}]
  (let [[k v] entry value (viscous/wrap v)
        key-width (min 180 (max 70 (* width 0.3)))
        value-width (max 1 (- width key-width 12))]
    (ui/padding 0 4
      (ui/flex-layout
        [(eui/button {:text (preview k) :width key-width :height 30 :size :small :variant :text
                      :on-click #(vector [::navigate $state k value])})
         (ui/on :mouse-down (fn [_] [[::dnd/drag-start {::dnd/obj {:x value}}]])
           (eui/code {:text (preview v) :width value-width}))] {:width width :gap 12 :align :start}))))

(defui inspector-view [{:keys [state size]}]
  (let [[w h] size width (max 1 (- w 24))
        obj @(:obj state) displayed (display-value obj)
        collection? (or (map? displayed) (sequential? displayed) (set? displayed))
        parents (:parents state) offset (:offset state 0) query (:query state "")
        {:keys [entries more? error]} (page displayed offset)
        matching (filter (fn [[k v]]
                           (str/includes? (str/lower-case (str (preview k) " " (preview v)))
                                          (str/lower-case query))) entries)
        toolbar (eui/wrap-row width
                  [(applet/button {:text "Back" :size :small :disabled? (empty? parents)
                                   :on-click #(when (seq parents)
                                                [[:set $state (assoc (peek parents) :parents (pop parents))]])})
                   (applet/button {:text "Datafy" :size :small
                                   :on-click #(vector [::transform $state datafy/datafy])})
                   (applet/button {:text "Meta" :size :small :disabled? (nil? (meta obj))
                                   :on-click #(vector [::transform $state meta])})
                   (when (instance? clojure.lang.IDeref obj)
                     (applet/button {:text "Deref" :size :small
                                     :disabled? (and (instance? clojure.lang.IPending obj) (not (realized? obj)))
                                     :on-click #(vector [::transform $state deref])}))
                   (applet/button {:text "Copy preview" :size :small
                                   :on-click #(vector [:clipboard-copy (preview obj)])})])
        header (ui/vertical-layout
                toolbar (ui/spacer 0 8)
                (eui/text {:text (str (or (some-> obj class .getName) "nil") " · depth " (count parents))
                            :width width :size 12 :color :color/secondary})
                (when collection?
                  (ui/padding 0 8 (eui/field {:text query :$text [$state '(keypath :query)]
                                             :width width :placeholder "Filter this page…"})))
                (when-let [message (or (:error state) error)]
                  (eui/text {:text message :width width :color :color/error}))
                (when-let [message (:message state)]
                  (eui/text {:text message :width width :size 12 :color :color/secondary})))
        footer (when collection?
                 (eui/row width
                   [(applet/button {:text "Previous" :size :small :disabled? (zero? offset)
                                    :on-click #(vector [:set [$state '(keypath :offset)] (max 0 (- offset page-size))])})
                    (eui/text {:text (str offset "–" (+ offset (count entries))) :size 12 :color :color/secondary})
                    (applet/button {:text "Next" :size :small :disabled? (not more?)
                                    :on-click #(when more? [[:set [$state '(keypath :offset)] (+ offset page-size)]])})]))
        body (if collection?
               (if (seq matching)
                 (apply ui/vertical-layout
                   (for [[k _ :as entry] matching]
                     (entry-view {:state state :entry entry :width width
                                  :extra (get extra [::entry k]) :$extra [$extra (list 'keypath [::entry k])]})))
                 (eui/text {:text (if (seq entries) "No matches on this page." "Empty collection.")
                             :color :color/secondary}))
               (ui/vertical-layout
                (eui/code {:text (preview obj) :width width})
                (when (and obj (not (or (string? obj) (number? obj) (boolean? obj) (keyword? obj) (symbol? obj))))
                  (ui/padding 0 16 (eui/text {:text "Datafy exposes structured data for objects that support it."
                                             :width width :size 12 :color :color/secondary})))))]
    (dnd/on-drop
     (fn [_ object] (when (contains? object :x) [[:set $state (initial-state @(:x object))]]))
     (ui/fixed-bounds [w h]
       (ui/padding 12 8
         (ui/vertical-layout
          header (ui/spacer 0 8)
          (basic/scrollview {:scroll-bounds [width (max 0 (- h (ui/height header) (ui/height footer) 32))]
                             :$body nil :body body})
          (ui/spacer 0 8) footer))))))

(defn inspector-applet
  "Inspect a value through bounded pages, metadata, datafy, and navigation."
  [_handler obj]
  (applet/tool-applet "Data Inspector" #'inspector-view (initial-state obj)))
