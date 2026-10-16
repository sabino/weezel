(ns com.phronemophobic.easel.term
  (:require
   [com.phronemophobic.easel.model :as model]
   [clojure.core.async :as async]
   [membrane.skia :as skia]
   [asciinema.vt :as vt]
   [asciinema.vt.screen :as screen]
   [com.phronemophobic.easel.applet :as applet]
   [com.phronemophobic.easel.events :as events]
   [com.phronemophobic.easel.theme :as theme]
   [com.phronemophobic.easel.ui :as eui]
   [com.phronemophobic.membrane.term :as term]
   [membrane.ui :as ui]
   [clojure.java.io :as io])
  (:import [com.pty4j WinSize PtyProcessBuilder]))


(def term-events #'term/term-events)
(def term-view @#'term/term-view)
(def repaint! @#'skia/glfw-post-empty-event)

(def load-font
  (memoize (fn [family size]
  (if (skia/font-exists? (ui/font family size))
    (#'term/load-terminal-font skia/toolkit
                               family size)
    (#'term/load-terminal-font skia/toolkit
                               (skia/logical-font->font-family :monospace)
                               size)))))

(defn terminal-font [context]
  (load-font (first (theme/token context :font/code)) (theme/token context :font/code-size)))

(defn term-ui [this $context context]
  (let [focus (:focus context)
        focus? (= focus (:id this))
        font (terminal-font context)
        scheme (merge term/default-color-scheme
                      {:background (theme/token context :color/surface)
                       :foreground (theme/token context :color/text)
                       :cursor (theme/token context :color/accent)
                       :cursor-text (theme/token context :color/surface)})
        view
        (ui/on
         :mouse-down
         (fn [_]
           [[:set [$context (list 'keypath :focus)]
             (:id this)]])
         (term-view scheme
                    font
                    (:vt this)))
        view (if focus?
               (ui/wrap-on
                :key-event
                (fn [handler key scancode action mods]
                  (when (not= 39 key)
                    (handler key scancode action mods)))
                ;; todo send events through cmd-ch
                (if-let [pty (:pty this)]
                  (term-events pty view)
                  view))
               view)
        view (ui/padding 10 10 view)]
    view))

(defn debounce-chan
  "Coalesces values during each interval and closes out when in closes."
  [in out ms]
  (async/go
    (try
      (loop []
        (when-some [initial (async/<! in)]
          (let [timer (async/timeout ms)
                continue? (loop [value initial]
                            (let [[next-value port] (async/alts! [timer in] :priority true)]
                              (cond
                                (= port timer) (async/>! out value)
                                (nil? next-value) (do (async/>! out value) false)
                                :else (recur next-value))))]
            (when continue? (recur)))))
      (finally (async/close! out)))))

(defn repaint-chan []
  (let [debounced (async/chan (async/sliding-buffer 1))
        in (async/chan (async/sliding-buffer 1))]
    (debounce-chan in debounced 30)
    (async/thread
      (loop []
        (when-some [_ (async/<!! debounced)]
          (repaint!)
          (recur))))
    in))

(defn terminal-size [[w h]]
  (let [font (terminal-font nil)]
    [(int (max 1 (quot (max 0 (- w 20)) (:membrane.term/cell-width font))))
     (int (max 1 (quot (max 0 (- h 20)) (:membrane.term/cell-height font))))]))

(defn resize-screen [screen cols rows]
  (let [drop-rows (max 0 (- (inc (get-in screen [:cursor :y])) rows))
        resize-lines (fn [lines drop-rows]
                       (let [blank (screen/blank-line cols)]
                         (into [] (take rows)
                               (concat (map #(vec (take cols (concat % blank))) (drop drop-rows lines))
                                       (repeat blank)))))
        cursor (fn [cursor dropped]
                 (-> cursor
                     (update :x #(max 0 (min (dec cols) %)))
                     (update :y #(max 0 (min (dec rows) (- % dropped))))))]
    (cond-> (-> screen
                (assoc :width cols :height rows :top-margin 0 :bottom-margin (dec rows)
                       :next-print-wraps false
                       :lines (resize-lines (:lines screen) drop-rows)
                       :tabs (into (sorted-set)
                                   (concat (filter #(< % cols) (:tabs screen))
                                           (filter #(>= % (:width screen)) (screen/default-tabs cols)))))
                (update :cursor cursor drop-rows)
                (update-in [:saved :cursor] cursor drop-rows))
      (:other-buffer-lines screen)
      (assoc :other-buffer-lines (resize-lines (:other-buffer-lines screen) 0))
      (:other-buffer-saved screen)
      (update-in [:other-buffer-saved :cursor] cursor 0))))

(defn feed-text [vt text]
  (let [text (str (:pending-surrogate vt) text)
        pending? (and (seq text) (Character/isHighSurrogate (.charAt text (dec (count text)))))
        complete (if pending? (subs text 0 (dec (count text))) text)]
    (-> (reduce (fn [vt codepoint]
                  (try (vt/feed-one vt codepoint)
                       (catch IllegalArgumentException _ vt)))
                vt (.toArray (.codePoints complete)))
        (assoc :pending-surrogate (when pending? (subs text (dec (count text))))))))

(defn stop-terminal! [active? resources cmd-ch]
  (reset! active? false)
  (async/close! cmd-ch)
  (when-let [{:keys [pty repaint-ch]}
             (locking resources
               (let [owned @resources] (reset! resources nil) owned))]
    (async/close! repaint-ch)
    (.destroy ^Process pty)))

(defn start-terminal! [dispatch! $ref size command env active? resources cmd-ch]
  (when @active?
    (async/thread
      (try
        (let [pty (locking resources
                    (when @active?
                      (let [[cols rows] (terminal-size (or (:size (dispatch! :get $ref)) size))
                            pty (.start (doto (PtyProcessBuilder. (into-array String command))
                                          (.setInitialColumns cols)
                                          (.setInitialRows rows)
                                          (.setEnvironment (merge (into {} (System/getenv))
                                                                  {"TERM" "xterm-256color"} env))))]
                        (reset! resources {:pty pty :repaint-ch (repaint-chan)})
                        pty)))]
          (when pty
            (async/thread
              (try
                (loop []
                  (when-some [{:keys [cols rows]} (async/<!! cmd-ch)]
                    (when @active?
                      (.setWinSize pty (WinSize. cols rows))
                      (recur))))
                (catch Exception e (when @active? (events/report! e)))))
            (dispatch! :update $ref assoc :pty pty)
            (with-open [reader (io/reader (.getInputStream pty) :encoding "UTF-8")]
              (let [buffer (char-array 2048)]
                (loop []
                  (let [n (.read reader buffer)]
                    (when (and @active? (pos? n))
                      (let [text (String. buffer 0 n)]
                        (dispatch! :update [$ref '(keypath :vt)] feed-text text)
                        (when-let [repaint-ch (:repaint-ch @resources)] (async/offer! repaint-ch true)))
                      (recur))))))
            (dispatch! :update $ref assoc :exited? true :pty nil)))
        (catch Throwable e
          (when @active?
            (events/report! e)
            (dispatch! :update $ref assoc ::applet/error e)))
        (finally (stop-terminal! active? resources cmd-ch))))))

(defrecord Termlet [dispatch!]
  model/IApplet
  (-start [this $ref size _content-scale]
    (let [[cols rows] (terminal-size size)
          cmd-ch (async/chan (async/sliding-buffer 1))
          active? (atom true)
          resources (atom nil)
          command (or (:command this) [(or (System/getenv "SHELL") "/bin/bash") "-l"])]
      (assoc this :size size :cmd-ch cmd-ch :active? active? :resources resources
             :vt (vt/make-vt cols rows)
             ::model/queue
             [#(start-terminal! dispatch! $ref size command (:env this) active? resources cmd-ch)])))
  (-stop [this]
    (when-let [active? (:active? this)]
      (stop-terminal! active? (:resources this) (:cmd-ch this))))
  model/IUI
  (-ui [this $context context]
    (let [view (term-ui this $context context)]
      (if (:exited? this)
        (ui/vertical-layout (eui/label "Process exited") (ui/no-events view))
        view)))
  model/IResizable
  (-resize [this size _content-scale]
    (let [[cols rows] (terminal-size size)
          current (assoc this :size size)]
      (if (= [cols rows] [(-> this :vt :screen :width) (-> this :vt :screen :height)])
        current
        (do
          (when @(:active? this) (async/offer! (:cmd-ch this) {:cols cols :rows rows}))
          (update-in current [:vt :screen] resize-screen cols rows))))))

(defn termlet
  ([handler] (termlet handler {}))
  ([handler options]
   (assoc (->Termlet handler) :label "term" :command (:command options) :env (:env options))))
