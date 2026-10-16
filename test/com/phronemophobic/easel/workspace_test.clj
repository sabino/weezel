(ns com.phronemophobic.easel.workspace-test
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async]
            [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.packages-test :as fixtures]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.package-ui :as manager]
            [com.phronemophobic.easel.repl :as repl]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.shell :as shell]
            [com.phronemophobic.easel.sketchbook :as sketchbook]
            [com.phronemophobic.easel.splitpane :as splitpane]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.term :as term]
            [com.phronemophobic.easel.workspace :as workspace]
            [membrane.component :as component]
            [membrane.skia :as skia]
            [membrane.ui :as ui]))

(defn await-value [f]
  (loop [n 500]
    (or (f) (when (pos? n) (Thread/sleep 10) (recur (dec n))))))

(deftest pane-menu-renders-and-splits-an-empty-workspace
  (let [directory (fixtures/temporary-directory) previous @easel/app-state]
    (try
      (binding [settings/*directory* directory]
        (reset! easel/app-state {:easel (assoc (easel/make-easel) :$ref '(keypath :easel))})
        (swap! easel/app-state update :easel easel/resize-window [900 700] [1 1])
        (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)
              nodes #(tree-seq (fn [node] (seq (ui/children node))) ui/children (app))
              click (fn [text]
                      (let [button (some #(when (and (= text (:text %)) (:on-click %)) %) (nodes))]
                        (is (some? button) text)
                        (when button (run! #(apply easel/handler %) (ui/mouse-down button [5 5])))))]
          (click "⋯")
          (is (every? #(every? number? (ui/bounds %)) (nodes)))
          (let [close (some #(when (= "Close applet" (:text %)) %) (nodes))]
            (is (true? (:disabled? close)))
            (is (empty? (ui/mouse-down close [5 5]))))
          (click "Split right")
          (is (every? #(every? number? (ui/bounds %)) (nodes)))
          (is (= 2 (count (splitpane/flatten-pane (get-in @easel/app-state [:easel :root-pane])))))
          (is (nil? (get-in @easel/app-state [:easel :pane-menu])))
          (easel/handler ::easel/toggle-pane-resize {:$easel '(keypath :easel) :pane-id ::easel/root-pane})
          (is (every? #(every? number? (ui/bounds %)) (nodes)))
          (ui/mouse-up (app) [200 200])
          (easel/handler ::easel/toggle-pane-resize {:$easel '(keypath :easel) :pane-id ::easel/root-pane})))
      (finally (reset! easel/app-state previous)
               (fixtures/delete-directory! directory)))))

(deftest command-line-entrypoint-waits-for-window-close
  (let [directory (fixtures/temporary-directory)
        startup (io/file directory "startup.clj")
        output (io/file directory "output.txt")]
    (try
      (spit startup
            "(require '[com.phronemophobic.easel :as easel]
                      '[clojure.core.async :as async])
             (alter-var-root #'easel/run
               (constantly
                 (fn []
                   {::easel/closed
                    (async/thread
                      (Thread/sleep 500)
                      (println \"window-closed\")
                      (flush))})))")
      (let [process (-> (ProcessBuilder.
                         ^java.util.List
                         [(str (io/file (System/getProperty "java.home") "bin" "java"))
                          "-cp" (System/getProperty "java.class.path")
                          "clojure.main" "-i" (.getPath startup)
                          "-m" "com.phronemophobic.easel"])
                        (.redirectErrorStream true)
                        (.redirectOutput output)
                        (.start))]
        (try
          (is (.waitFor process 30 java.util.concurrent.TimeUnit/SECONDS))
          (when (.isAlive process) (.destroyForcibly process) (.waitFor process))
          (is (zero? (.exitValue process)) (slurp output))
          (is (re-find #"(?m)^window-closed$" (slurp output)) (slurp output))
          (finally
            (when (.isAlive process) (.destroyForcibly process) (.waitFor process)))))
      (finally (fixtures/delete-directory! directory)))))

(deftest successive-camera-drags-preserve-orientation
  (let [state (atom {:state {} :scene {:radius 96 :count 24} :width 400 :height 240 :mode :3d})
        app (component/make-app #'sketchbook/interactive-plot state)]
    (dotimes [_ 2]
      (ui/mouse-down (app) [20 20])
      (ui/mouse-move (app) [50 40])
      (ui/mouse-up (app) [50 40]))
    (is (< (abs (- 0.9 (get-in @state [:state :yaw]))) 0.000001))
    (is (< (abs (- 0.75 (get-in @state [:state :tilt]))) 0.000001))
    (is (nil? (get-in @state [:state :camera-drag])))))

(deftest responsive-workspace-geometry-stays-contained
  (doseq [width [0 120 600 900 1536] height [0 80 400 1024] dock? [true false]]
    (let [g (shell/geometry [width height] @theme/default-theme dock?)
          layout (splitpane/layout-workspace
                  (merge (:layout (workspace/builtin :easel/sketchbook))
                         {:width (:content-width g) :height (:workspace-height g)}) 8)
          leaves (splitpane/flatten-pane layout)]
      (is (every? #(not (neg? %)) (filter number? (vals g))))
      (is (every? #(and (<= 0 (:x %) (:content-width g))
                        (<= 0 (:y %) (:workspace-height g))
                        (<= (+ (:x %) (:width %)) (:content-width g))
                        (<= (+ (:y %) (:height %)) (:workspace-height g))) leaves)))))

(deftest switching-workspaces-preserves-live-panes-and-docks
  (let [directory (fixtures/temporary-directory) previous @easel/app-state]
    (try
      (binding [settings/*directory* directory]
        (reset! easel/app-state nil)
        (easel/initialize! {:preset :easel/sketchbook})
        (is (await-value #(not @easel/queue-scheduled?)))
        (let [initial (:easel @easel/app-state) initial-root (:root-pane initial)
              initial-dock (:dock initial)
              scene* (:source* (sketchbook/session))
              watch-count (count (.getWatches ^clojure.lang.IRef scene*))]
          (is (= 2 watch-count))
          (is (= 5 (count (:applets initial))))
          (is (= [1536 1024] (ui/bounds ((component/make-app #'easel/easel-view easel/app-state easel/handler)))))
          (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)
                focus (get-in @easel/app-state [:membrane.component/context :focus])]
            (ui/key-event (app) 88 0 :press ui/ALT-MASK)
            (ui/key-event (app) 88 0 :release ui/ALT-MASK)
            (ui/key-press (app) "Documentation")
            (is (= "Documentation" (get-in @easel/app-state [:easel :commands :query])))
            (ui/key-event (app) 256 0 :press 0)
            (is (= focus (get-in @easel/app-state [:membrane.component/context :focus]))))
          (easel/handler ::easel/open-workspace 2)
          (is (await-value #(not @easel/queue-scheduled?)))
          (is (= 2 (get-in @easel/app-state [:easel :workspace-id])))
          (is (not= initial-dock (get-in @easel/app-state [:easel :dock])))
          (easel/handler ::easel/open-workspace 1)
          (is (= initial-root (get-in @easel/app-state [:easel :root-pane])))
          (is (= initial-dock (get-in @easel/app-state [:easel :dock])))
          (swap! scene* assoc :radius 120)
          (let [scenes (keep #(get-in % [:state :scene]) (vals (get-in @easel/app-state [:easel :applets])))]
            (is (= [120 120] (mapv :radius scenes))))
          (let [id (get-in initial-dock [:applets :repl])
                path [:easel :applets (list 'keypath id) :state]]
            (easel/handler :update path assoc :source "(swap! scene* assoc :radius 140)")
            (easel/handler ::repl/evaluate path)
            (is (await-value #(seq (:entries (easel/handler :get path)))))
            (is (= 140 (:radius @scene*))))
          (let [points-var (:function (sketchbook/session)) original @points-var]
            (try
              (alter-var-root points-var (constantly (fn [scene] [[(:radius scene) 0]])))
              (is (= [[[140 0]] [[140 0]]]
                     (mapv #(get-in % [:state :scene :points])
                           (filter #(get-in % [:state :source*]) (vals (get-in @easel/app-state [:easel :applets]))))))
              (finally (alter-var-root points-var (constantly original)))))
          (let [name (workspace/save! easel/handler)
                saved (get (packages/catalog) (:workspace (settings/read-settings)))]
            (is (= name (:name saved)))
            (is (= 140 (get-in saved [:data :session :scene :radius])))
            (is (= 5 (count (get-in saved [:data :applets])))))
          (easel/handler ::easel/open-tools)
          (is (await-value #(not @easel/queue-scheduled?)))
          (easel/handler ::manager/select :easel/studio-dark)
          (doseq [size [[900 800] [1536 1024]]]
            (swap! easel/app-state update :easel easel/resize-window size [1 1])
            (let [nodes (tree-seq #(seq (ui/children %)) ui/children
                                  ((component/make-app #'easel/easel-view easel/app-state easel/handler)))]
              (is (some #(= "Preview" (:text %)) nodes))))
          (easel/remove-all-widgets!)
          (is (await-value #(not @easel/queue-scheduled?)))
          (is (empty? (.getWatches ^clojure.lang.IRef scene*))))
        (doseq [id [:easel/sketchbook :easel/data-lab :easel/notes]]
          (is (= id (:id (workspace/validate (workspace/builtin id)))))))
      (finally (easel/remove-all-widgets!)
               (await-value #(not @easel/queue-scheduled?))
               (reset! easel/app-state previous)
               (fixtures/delete-directory! directory)))))

(deftest closing-the-last-window-stops-applet-resources
  (let [directory (fixtures/temporary-directory) previous @easel/app-state
        terminal (atom nil)]
    (try
      (binding [settings/*directory* directory]
        (reset! easel/app-state nil)
        (with-redefs [easel/repaint! (fn [])
                      skia/run-sync
                      (fn [view options]
                        ((::skia/on-main options))
                        (is (= [1536 1024] (ui/bounds (view {:container-size [1536 1024] :content-scale [1 1]}))))
                        (easel/handler ::easel/add-applet
                                       {:make-applet #(term/termlet % {:command ["/bin/sleep" "10"]})})
                        (reset! terminal (some #(when (:resources %) %) (vals (:applets (:easel @easel/app-state)))))
                        (is (await-value #(some-> @terminal :resources deref :pty)))
                        nil)]
          (let [running (easel/run {:preset :easel/sketchbook})
                [_ channel] (async/alts!! [(::easel/closed running) (async/timeout 10000)])]
            (is (= (::easel/closed running) channel))
            (is (await-value #(not @easel/queue-scheduled?)))
            (is (empty? (get-in @easel/app-state [:easel :applets])))
            (is (nil? @(:resources @terminal)))
            (is (zero? @easel/live-windows))
            (is (false? @easel/window-ready?)))))
      (finally (easel/remove-all-widgets!)
               (await-value #(not @easel/queue-scheduled?))
               (reset! easel/app-state previous)
               (fixtures/delete-directory! directory)))))
