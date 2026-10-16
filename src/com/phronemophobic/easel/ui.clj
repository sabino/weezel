(ns com.phronemophobic.easel.ui
  "Small native components which consume semantic theme tokens."
  (:require [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.membrandt :as ant]
            [com.phronemophobic.membrandt.icon :as icons]
            [com.phronemophobic.membrandt.icon.impl.common :as icon-common]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui]]
            [membrane.skia :as skia]
            [membrane.skia.paragraph :as para]
            [membrane.ui :as ui]))

(defn text-style
  ([context] (text-style context {}))
  ([context {:keys [role color size weight]}]
   (cond-> #:text-style{:font-families (theme/token context (if (= role :code) :font/code :font/ui))
                       :font-size (or size (theme/token context (if (= role :code) :font/code-size :font/size)))
                       :height 1.5 :height-override true
                       :color (if (vector? color) color (theme/token context (or color :color/text)))}
     weight (assoc :text-style/font-style {:font-style/weight weight}))))

(defui text [{:keys [text role width color size weight]}]
  (para/paragraph (str text) width
                  {:paragraph-style/text-style (text-style context {:role role :color color :size size :weight weight})}))

(defn label
  ([s] (label s {}))
  ([s opts]
   (para/paragraph (str s) (:width opts)
                   {:paragraph-style/text-style (text-style nil opts)})))

(defn adapt-colors
  "Adapt a native component's palette without changing its event handlers."
  [view palette]
  (let [children (ui/children view)
        view (cond
               (and (seq children) (satisfies? ui/IMakeNode view))
               (ui/make-node view (mapv #(adapt-colors % palette) children))
               (and (= 1 (count children)) (map? view) (:body view))
               (update view :body #(adapt-colors % palette))
               (and (= 1 (count children)) (map? view) (:elem view))
               (update view :elem #(adapt-colors % palette))
               (and (seq children) (map? view) (contains? view :context))
               (mapv #(adapt-colors % palette) children)
               :else view)
        replace-color #(get palette % %)
        style #(cond-> % (:text-style/color %) (update :text-style/color replace-color))]
    (cond-> view
      (and (map? view) (:color view)) (update :color replace-color)
      (instance? membrane.skia.paragraph.Paragraph view)
      (-> (update-in [:paragraph-style :paragraph-style/text-style] style)
          (update :paragraph
                  (fn [content]
                    (if (sequential? content)
                      (mapv #(if (and (map? %) (:style %)) (update % :style style) %) content)
                      content)))))))

(defn surface [context width height & [color radius]]
  (ui/with-color (theme/token context (or color :color/surface))
    (ui/with-style ::ui/style-fill (ui/rounded-rectangle (max 0 width) (max 0 height) (or radius 0)))))

(defn border [context width height & [color radius]]
  (ui/with-color (theme/token context (or color :color/border))
    (ui/with-style ::ui/style-stroke (ui/rounded-rectangle (max 0 width) (max 0 height) (or radius 0)))))

(defn rule [context width]
  (surface context width 1 :color/border))

(defn row [width children & [gap]]
  (ui/flex-layout (remove nil? children) {:width width :gap (or gap 8) :align :center}))

(defn wrap-row [width elements & [gap]]
  (let [gap (or gap 8)
        {:keys [rows row]} (reduce (fn [{:keys [row row-width] :as result} element]
                                    (let [w (ui/width element)]
                                      (if (and (seq row) (> (+ row-width gap w) (max 1 width)))
                                        (-> result (update :rows conj row) (assoc :row [element] :row-width w))
                                        (-> result (update :row conj element) (assoc :row-width (+ row-width gap w))))))
                                  {:rows [] :row [] :row-width (- gap)} (remove nil? elements))]
    (ui/flex-layout (map #(ui/flex-layout % {:gap gap :align :center}) (cond-> rows (seq row) (conj row)))
                    {:direction :column :gap gap})))

(defui icon [{:keys [name size color]}]
  (let [hex (get-in (theme/current context) [:tokens (or color :color/text)])
        svg (try (icons/svg-str name "outlined")
                 (catch IllegalArgumentException _ (icons/svg-str "appstore" "outlined")))]
    (skia/svg (icon-common/use-colors svg hex hex) [(or size 18) (or size 18)])))

(defui button [{:keys [text on-click size width height variant selected? disabled? hover? icon]}]
  (let [{:keys [padding radius] base-height :height} (theme/component context :button)
        variant (or variant :default)
        foreground (cond disabled? :color/muted (= variant :primary) :color/surface
                         (or selected? (= variant :link)) :color/accent :else :color/text)
        body (row nil [(when icon (com.phronemophobic.easel.ui/icon {:name icon :size 16 :color foreground}))
                        (com.phronemophobic.easel.ui/text {:text text :color foreground})] 6)
        w (max 0 (or width (+ (ui/width body) (* 2 (or padding 12)))))
        h (max 0 (or height (- (or base-height 32) (if (= size :small) 6 0))))
        background (cond selected? :color/selection (= variant :primary) :color/accent
                         (and hover? (not disabled?)) :color/hover
                         (#{:text :link} variant) nil :else :color/surface)
        body (ui/fixed-bounds [w h]
               [(when background (surface context w h background radius))
                (when (and (not (#{:text :link} variant)) (not= variant :primary))
                  (border context w h :color/border radius))
                (ui/translate (if width (or padding 12) (/ (- w (ui/width body)) 2))
                              (/ (- h (ui/height body)) 2) body)])]
    (basic/on-hover
     {:hover? hover? :$body nil
      :body (ui/on :mouse-down (fn [_] (when (and on-click (not disabled?)) (on-click))) body)})))

(defui field [{:keys [text placeholder width height disabled? on-submit on-escape on-key focus-id role variant
                     cursor select-cursor mpos down-pos last-click
                     ^:membrane.component/contextual focus]
              :or {text "" cursor 0}}]
  (let [{:keys [padding radius] field-height :height} (theme/component context :field)
        w (max 1 (or width 240)) h (max 1 (or height field-height 36))
        px (if (= variant :bare) 0 (or padding 12))
        py (/ (- h (* 1.5 (theme/token context (if (= role :code) :font/code-size :font/size)))) 2)
        focused? (and (= focus (or focus-id $text)) (not disabled?))
        paragraph (para/paragraph text nil {:paragraph-style/text-style (text-style context {:role role})})
        clamped-cursor (max 0 (min cursor (count text)))
        cursor-rect (first (para/get-rects-for-range (assoc paragraph :paragraph (str text " "))
                                                    clamped-cursor (inc clamped-cursor) :max :tight))
        offset (if focused? (max 0 (- (:x cursor-rect 0) (- w (* 2 px) 4))) 0)
        input {:text text :$text $text :cursor cursor :$cursor $cursor
               :select-cursor select-cursor :$select-cursor $select-cursor
               :mpos mpos :$mpos $mpos :down-pos down-pos :$down-pos $down-pos
               :last-click last-click :$last-click $last-click :focused? focused?}
        content [(when (and focused? select-cursor)
                   (for [{:keys [x y width height]} (para/get-rects-for-range paragraph
                                                        (min clamped-cursor select-cursor) (max clamped-cursor select-cursor) :max :tight)]
                     (ui/translate x y (surface context width height :color/selection))))
                 (if (and (empty? text) placeholder)
                   (com.phronemophobic.easel.ui/text {:text placeholder :role role :color :color/muted})
                   paragraph)
                 (when (and focused? (nil? select-cursor) cursor-rect)
                   (ui/translate (:x cursor-rect) (:y cursor-rect)
                                 (surface context 1 (:height cursor-rect) :color/accent)))]
        body (ui/fixed-bounds [w h]
               [(when-not (= variant :bare) (surface context w h (if disabled? :color/header :color/surface) radius))
                (when-not (= variant :bare) (border context w h (if focused? :color/accent :color/border) radius))
                (ui/translate px py
                              (ui/scissor-view [0 0] [(max 0 (- w (* 2 px))) (max 0 (- h (* 2 py)))]
                                               (ui/translate (- offset) 0 content)))])
        body (if disabled? (ui/no-events body)
                 (->> body
                      (ant/wrap-mouse-events input (- px offset) py paragraph)
                      (ant/wrap-keyboard-events input)
                      (ant/wrap-clipboard-events input)))]
    (ui/on ::ant/request-focus (fn [] [[:set $focus (or focus-id $text)]])
      (ui/wrap-on :key-press
                  (fn [handler key]
                    (or (when (and focused? on-key) (seq (on-key key)))
                    (cond (and focused? (= :enter key)) (when on-submit (on-submit))
                          (and focused? (= :escape key) on-escape) (on-escape)
                          :else (handler key)))) body))))

(defui section [{:keys [title body width]}]
  (ui/vertical-layout
   (text {:text title :weight :semi-bold})
   (ui/spacer 0 10)
   body
   (ui/spacer 0 18)))
