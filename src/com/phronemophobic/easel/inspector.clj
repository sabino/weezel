(ns com.phronemophobic.easel.inspector
  (:require [clojure.core.protocols :as protocols]
            [clojure.datafy :as datafy]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.viscous :as viscous]
            [com.phronemophobic.membrandt :as ant]
            [membrane.alpha.component.drag-and-drop :as dnd]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]))

(def page-size 50)

(defn preview [obj]
  (let [s (binding [*print-length* 12 *print-level* 4]
            (pr-str obj))]
    (if (> (count s) 240)
      (str (subs s 0 240) "…")
      s)))

(defn page [obj offset]
  (let [entries (cond
                  (map? obj) (seq obj)
                  (or (sequential? obj) (set? obj)) (map-indexed vector obj)
                  :else nil)
        chunk (vec (take (inc page-size) (drop offset entries)))]
    {:entries (vec (take page-size chunk))
     :more? (> (count chunk) page-size)}))

(defn initial-state [obj]
  {:obj (viscous/wrap obj) :parents [] :offset 0 :query ""})

(defn descend [state key value]
  (let [obj @(:obj state)]
    (-> (initial-state (protocols/nav obj key value))
        (assoc :parents (conj (:parents state) (dissoc state :parents))))))

(defeffect ::navigate [$state key value]
  (applet/task! dispatch! $state
                #(descend (dispatch! :get $state) key @value)))

(defeffect ::transform [$state f]
  (applet/task! dispatch! $state
                #(let [state (dispatch! :get $state)]
                   (-> (initial-state (f @(:obj state)))
                       (assoc :parents (conj (:parents state) (dissoc state :parents)))))))

(defui inspector-view [{:keys [state size]}]
  (dnd/on-drop
   (fn [_ object]
     (when (contains? object :x)
       [[:set $state (initial-state @(:x object))]]))
   (let [obj @(:obj state)
        parents (:parents state)
        offset (:offset state 0)
        query (:query state "")
        {:keys [entries more?]} (page obj offset)
        matching (filter (fn [[k v]]
                           (str/includes? (str/lower-case (str (preview k) " " (preview v)))
                                          (str/lower-case query)))
                         entries)]
    (ui/vertical-layout
     (ui/horizontal-layout
      (ant/button {:text "Back" :size :small :disabled? (empty? parents)
                   :on-click #(when (seq parents)
                                [[:set $state (assoc (peek parents) :parents (pop parents))]])})
      (ant/button {:text "Datafy" :size :small
                   :on-click #(vector [::transform $state datafy/datafy])})
      (ant/button {:text "Meta" :size :small
                   :on-click #(vector [::transform $state meta])})
      (when (and (instance? clojure.lang.IDeref obj)
                 (or (not (instance? clojure.lang.IPending obj)) (realized? obj)))
        (ant/button {:text "Deref" :size :small
                     :on-click #(vector [::transform $state deref])}))
      (ant/button {:text "Copy" :size :small
                   :on-click #(vector [:clipboard-copy (preview obj)])}))
     (ui/label (str (some-> obj class .getName) " | depth " (count parents)))
     (ant/text-input {:text query :$text [$state '(keypath :query)] :size :small
                      :placeholder "Filter page"})
     (when-let [error (:error state)] (ui/label error))
     (basic/scrollview
      {:scroll-bounds [(max 0 (first size)) (max 0 (- (second size) 120))]
       :$body nil
       :body
       (if (or (map? obj) (sequential? obj) (set? obj))
         (apply ui/vertical-layout
                (for [[k v] matching]
                  (let [value (viscous/wrap v)]
                    (ui/horizontal-layout
                     (ant/button {:text (preview k) :size :small
                                  :on-click #(vector [::navigate $state k value])})
                     (ui/on :mouse-down
                            (fn [_] [[::dnd/drag-start {::dnd/obj {:x value}}]])
                            (ui/label (preview v)))))))
         (viscous/inspector {:obj (:obj state)
                             :width (max 1 (quot (first size) 8))
                             :height 10
                             :show-context? true}))})
     (ui/horizontal-layout
      (ant/button {:text "Previous" :size :small :disabled? (zero? offset)
                   :on-click #(vector [:set [$state '(keypath :offset)] (max 0 (- offset page-size))])})
      (ui/label (str " " offset "–" (+ offset (count entries)) " "))
      (ant/button {:text "Next" :size :small :disabled? (not more?)
                   :on-click #(when more? [[:set [$state '(keypath :offset)] (+ offset page-size)]])}))))))

(defn inspector-applet [_handler obj]
  (applet/tool-applet "Data Inspector" #'inspector-view (initial-state obj)))
