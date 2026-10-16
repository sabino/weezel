(ns com.phronemophobic.easel.browser
  (:require
   [com.phronemophobic.easel.model :as model]
   [com.phronemophobic.easel.applet :as applet]
   [com.phronemophobic.easel.events :as events]
   [com.phronemophobic.easel.settings :as settings]
   [com.phronemophobic.easel.ui :as eui]
   [membrane.skia :as skia]
   [membrane.ui :as ui]
   [membrane.component :refer [defeffect defui]]
   [clojure.java.io :as io]
   [com.phronemophobic.gen3 :as gen3]
   [com.phronemophobic.cef :as cef]
   [com.phronemophobic.cef.env :as cef-env]
   [com.phronemophobic.cef.browser :as b]
   #_[com.phronemophobic.replog :as replog])
  (:import com.sun.jna.Pointer
           com.sun.jna.Function
           com.phronemophobic.membrane.Skia
           com.phronemophobic.gen3.structs.cef_browser_host_t
           ;;com.phronemophobic.cljcef.CefBrowser
           ))

;; (replog/load-log (into []
;;                        (filter #(= (ns-name *ns*)
;;                                    (::replog/ns %)))
;;                        (replog/get-main-log)))

(defui browser-bar [{:keys [width url ready?]}]
  (ui/padding 4 2
    (ui/flex-layout
     [(eui/button {:text "←" :height 36 :disabled? (not ready?) :on-click #(vector [::browser-back])})
      (eui/button {:text "→" :height 36 :disabled? (not ready?) :on-click #(vector [::browser-forward])})
      (eui/button {:text "↻" :height 36 :disabled? (not ready?) :on-click #(vector [::browser-refresh])})
      (eui/field {:text url :width (max 1 (- width 190)) :disabled? (not ready?)
                  :on-submit #(vector [::load-url url])})
      (eui/button {:text "Go" :height 36 :disabled? (not ready?) :on-click #(vector [::load-url url])})]
     {:gap 4})))

(defeffect ::more! [$num]
  (dispatch! :update $num inc))

(defeffect ::browser-back [{:keys [browser]}]
  (gen3/call browser :go_back))

(defeffect ::browser-refresh [{:keys [browser]}]
  (gen3/call browser :reload_ignore_cache))

(defeffect ::show-inspector []
  (dispatch!
   :com.phronemophobic.easel/add-applet
   {:make-applet
    (fn [handler]
      ((requiring-resolve 'com.phronemophobic.easel.browser/browslet)
       handler
       "http://localhost:8888/"))}))

(defeffect ::browser-forward [{:keys [browser]}]
  (gen3/call browser :go_forward))

(defeffect ::load-url [{:keys [browser url]}]
  (let [frame (gen3/call browser :get_main_frame)]
    (gen3/call frame :load_url url)))


(def skialib @#'skia/membraneskialib)

(skia/defc skia_bgra8888_draw skialib Void/TYPE [skia-resource buffer width height row-bytes])
(defn skia-bgra8888-draw [resource buffer width height row-bytes]
  (skia_bgra8888_draw resource buffer (int width) (int height) (int row-bytes)))

(skia/defc skia_direct_bgra8888_buffer skialib Pointer [buf width height row-bytes])
(defn skia-direct-bgra8888-buffer [buf width height row-bytes]
  (skia_direct_bgra8888_buffer buf (int width) (int height) (int row-bytes)))

(skia/defc skia_cleanup skialib Void/TYPE [skia-resource])

(skia/defc skia_draw_surface skialib Void/TYPE [destination source])


(skia/defc skia_browser_buffer skialib Pointer [width height])
(defn skia-browser-buffer [width height]
  (skia_browser_buffer (int width) (int height)))

(skia/defc skia_browser_update skialib Void/TYPE [skia-resource dirty-rects-count dirty-rects buffer width height])
(defn skia-browser-update [resource dirty-rects-count dirty-rects buffer width height]
  (skia_browser_update resource (int dirty-rects-count) dirty-rects buffer (int width) (int height)))

(skia/defc skia_browser_draw skialib Void/TYPE [skia-resource buffer width height])
(defn skia-browser-draw [resource buffer width height]
  (skia_browser_draw resource buffer (int width) (int height)))


(defn skia-draw [dispatch! $browser-info content-scale paint-type nrects rects buffer width height]
  (when (zero? paint-type)
    (let [browser-info (dispatch! :get $browser-info)]
      (when-let [draw-lock (:draw-lock browser-info)]
        (locking draw-lock
          (if (:resource browser-info)
            (if (and (= width (:width browser-info))
                     (= height (:height browser-info))
                     (= content-scale (:content-scale browser-info)))
              (when (pos? (.intValue nrects))
                (skia-browser-update (:resource browser-info) (.intValue nrects) rects buffer width height)
                (dispatch! :update $browser-info update :browser-id (fnil inc 0)))
              (do
                (dispatch! :update
                           $browser-info
                           dissoc
                           :resource)
                (skia_cleanup (:resource browser-info))
                (skia-draw dispatch! $browser-info content-scale paint-type nrects rects buffer width height)))
            (let [resource (skia-browser-buffer width height)
                  browser-info {:resource resource
                                :content-scale content-scale
                                :width width
                                :height height}]
              (skia-browser-draw resource buffer width height)
              (dispatch! :update $browser-info update :browser-id (fnil inc 0))

              (dispatch! :update $browser-info merge browser-info)
              (dispatch! :repaint!)))))))
  ;; always return nil. don't leak cache
  nil)

(defrecord Browser [browser browser-id focused? content-scale width height resource draw-lock]
  ui/IOrigin
  (-origin [_]
    [0 0])


  ui/IMouseMove
  (-mouse-move [elem pos]
    (when browser
      (gen3/call (gen3/call browser :get_host)
                 :send_mouse_move_event
                 (gen3/map->mouse-event
                  {:x (first pos)
                   :y (second pos)
                   :modifiers gen3/EVENTFLAG_LEFT_MOUSE_BUTTON})
                 0)

      #_(.sendMouseMoveEvent (.getHost ^CefBrowser browser)
                           (cef/map->mouse-event
                            {:x (first pos)
                             :y (second pos)})
                           0)))
  
  ui/IMouseEvent
  (-mouse-event [elem pos button mouse-down? mods]
    (when browser
      (gen3/call (gen3/call browser :get_host)
                 :send_mouse_click_event
                 (gen3/map->mouse-event
                  {:x (first pos)
                   :y (second pos)
                   :modifiers gen3/EVENTFLAG_LEFT_MOUSE_BUTTON})
                 button
                 (if mouse-down?
                   0
                   1)
                 1)
      #_(.sendMouseClickEvent (.getHost ^CefBrowser browser)
                            (cef/map->mouse-event
                             {:x (first pos)
                              :y (second pos)})
                            button
                            (if mouse-down?
                              0
                              1)
                            1)
      ))

  ui/IScroll
  (-scroll [elem delta mpos]
    (when browser
      (gen3/call (gen3/call browser :get_host)
                 :send_mouse_wheel_event
                 (gen3/map->mouse-event
                  {:x (first mpos)
                   :y (second mpos)})
                 (first delta)
                 (second delta))
      #_(.sendMouseWheelEvent (.getHost ^CefBrowser browser)
                            (cef/map->mouse-event
                             {:x (first mpos)
                              :y (second mpos)})
                            (first delta)
                            (second delta))
      ))

  ui/IHasKeyPress
  (has-key-press [this]
    focused?)
  ui/IKeyPress
  (-key-press [elem k]
    (when (and browser focused?)
      (let [c (if (keyword? k)
                (if (= k :enter)
                  \return
                  nil
                  ;;(char (get skia/keycodes k))
                  )
                (.charAt k 0))]
        (when c
          (gen3/call (gen3/call browser :get_host)
                     :send_key_event
                     (gen3/map->key-event
                      {:type 3
                       :modifiers 0
                       :character c
                       :unmodified-character c}))
          #_(.sendKeyEvent (.getHost browser)
                         (cef/map->key-event
                          {:type 3
                           :modifiers 0
                           :character c
                           :unmodified-character c}))))))

  ui/IHasKeyEvent
  (has-key-event [this]
    focused?)
  ui/IKeyEvent
  (-key-event [elem key code action mods]
    (when (and focused? browser)
      (when (#{:press :release :repeat}
             action)
        (let [key-event {:type (case action
                                 :press 1
                                 :release 2
                                 :repeat 1
                                 )
                         :modifiers mods
                         :native-key-code code
                         :character (char key)
                         :unmodified-character (char key)}]
          (gen3/call (gen3/call browser :get_host)
                     :send_key_event
                     (gen3/map->key-event key-event))
          #_(.sendKeyEvent (.getHost ^CefBrowser browser)
                         (cef/map->key-event
                          key-event)))))
    )
  

  ui/IBounds
  (-bounds [this]
    (if content-scale
      [(/ width content-scale) (/ height content-scale)]
      [width height]))

  skia/IDraw
  (draw [this]
    (when draw-lock
      (locking draw-lock
        (when resource
          (skia/save-canvas
           (when (not= 1 content-scale)
             (let [scale (float (/ 1 content-scale))]
               (Skia/skia_set_scale skia/*skia-resource* scale scale)))
           (skia_draw_surface skia/*skia-resource* resource)))))))

(defn browser-ui [this $context context]
  (let [focus (:focus context)
        focus? (= focus (:id this))
        browser-info (:browser-info this)
        ui-state (:ui-state this)

        view
        (assoc
         (->Browser (:browser browser-info)
                    (:browser-id browser-info)
                    focus?
                    (:content-scale browser-info)
                    (:width browser-info)
                    (:height browser-info)
                    (:resource browser-info)
                    (:draw-lock browser-info))
         :id2 (:id this))

        view (if focus?
               (ui/wrap-on
                :mouse-down
                (fn [handler mpos]
                  (cons
                   [:set [$context (list 'keypath :focus)] (:id this)]
                   (handler mpos)))
                view)
               (ui/on
                :mouse-down
                (fn [_]
                  [[:set [$context (list 'keypath :focus)]
                    (:id this)]])
                (ui/no-events view)))]
    (ui/vertical-layout
     (ui/on
      ::browser-forward
      (fn []
        [[::browser-forward {:browser (:browser browser-info)}]])
      ::browser-refresh
      (fn []
        [[::browser-refresh {:browser (:browser browser-info)}]])
      ::load-url
      (fn [url]
        [[::load-url {:browser (:browser browser-info)
                      :url url}]])
      ::browser-back
      (fn []
        [[::browser-back {:browser (:browser browser-info)}]])
      (browser-bar
       (assoc ui-state
              :width (:width browser-info)
              :ready? (some? (:browser browser-info))
              :context context
              :$context $context)))
     view)))



(defn runtime-path []
  (io/file (or (System/getenv "EASEL_CEF_ROOT") (.getPath (settings/file "runtimes" "cef")))))

(defn install-runtime! []
  (let [target (runtime-path)]
    (.mkdirs target)
    (cef-env/download-and-extract-framework target)))

(declare browslet)

(defrecord Browslet [dispatch! initial-url]
  model/IApplet
  (-start [this $ref [width height] [sx sy]]
    (let [resources (atom {:active? true})
          draw-lock (Object.)
          $info [$ref '(keypath :browser-info)]
          $url [$ref '(keypath :ui-state) '(keypath :url)]
          scale (if (= sx sy) sx 1)
          dispatch-main #(dispatch! :dispatch-main %)
          start! (fn []
                   (future
                     (try
                       (when (:active? @resources)
                         (cef/prepare-environment! (runtime-path))
                         (b/create-browser
                          [width (max 1 (- height 40)) scale] initial-url dispatch-main
                          {:cef-path (runtime-path)
                           :cache-path (doto (settings/file "cache" "browser") (.mkdirs))
                           :on-after-created
                           (fn [browser]
                             (locking resources
                               (if (:active? @resources)
                                 (do (swap! resources assoc :browser browser)
                                     (dispatch! :update $info assoc :browser browser)
                                     (gen3/call (gen3/call browser :get_host) :set_focus (int 1)))
                                 (b/close browser))))
                           :life-span-handler/on-before-popup
                           (fn [{:keys [target-url]}]
                             (when (:active? @resources)
                               (dispatch! :com.phronemophobic.easel/add-applet
                                          {:make-applet #(browslet % target-url)})))
                           :load-handler/on-load-start
                           (fn [_ frame _]
                             (when (and (:active? @resources) (= 1 (gen3/call frame :is_main)))
                               (dispatch! :set $url (gen3/call frame :get_url))))
                           :on-paint+content-scale
                           (fn [_ scale paint-type nrects rects buffer width height]
                             (locking resources
                               (when (:active? @resources)
                                 (skia-draw dispatch! $info scale paint-type nrects rects buffer width height)
                                 (dispatch! :repaint!))))}))
                       (catch Throwable e
                         (events/report! e)
                         (dispatch! :update $ref assoc ::applet/error e)))))]
      (assoc this :$ref $ref :size [width height] :resources resources
             :ui-state {:url initial-url :$url $url :extra {}
                        :$extra [$ref '(keypath :ui-state) '(keypath :extra)]}
             :browser-info {:draw-lock draw-lock :width width :height (max 1 (- height 40))}
             ::model/queue [start!])))
  (-stop [this]
    (when-let [resources (:resources this)]
      (locking resources
        (when (:active? @resources)
          (swap! resources assoc :active? false)
          (when-let [browser (:browser @resources)] (b/close browser))
          (when-let [resource (get-in this [:browser-info :resource])]
            (dispatch! :dispatch-main #(locking (get-in this [:browser-info :draw-lock]) (skia_cleanup resource)))))))
    nil)
  model/IUI
  (-ui [this $context context] (browser-ui this $context context))
  model/IResizable
  (-resize [this [width height :as size] [sx sy]]
    (when-let [browser (get-in this [:browser-info :browser])]
      (b/resize browser [width (max 1 (- height 40))] (if (= sx sy) sx 1)))
    (assoc this :size size)))


(defn browslet [handler url]
  (-> (->Browslet handler url)
      (assoc :label "web")))

(comment
  (cef/download-and-extract-framework
   (doto (io/file ".cef")
     (.mkdirs))
   )
  (cef/download-and-prepare-environment!
   (doto (io/file ".cef")
     (.mkdirs)))

  (-> (b/list-browsers)
      first
      (gen3/call :get_host)
      (gen3/call :set_focus (int 1)))
  ,)
