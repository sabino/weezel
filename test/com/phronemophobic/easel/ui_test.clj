(ns com.phronemophobic.easel.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel.diff :as diff]
            [com.phronemophobic.easel.documentation :as documentation]
            [com.phronemophobic.easel.exceptions :as exceptions]
            [com.phronemophobic.easel.flowstorm :as flowstorm]
            [com.phronemophobic.easel.git :as git]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.logging :as logging]
            [com.phronemophobic.easel.tap-watcher :as tap]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.shell :as shell]
            [com.phronemophobic.easel.commands :as commands]
            [com.phronemophobic.easel.events :as events]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.viscous :as viscous]
            [membrane.component :as component]
            [membrane.skia]
            [membrane.ui :as ui]))

(deftest populated-tool-views
  (doseq [size [[640 480] [160 100] [0 0]]
          [view state] [[#'inspector/inspector-view (inspector/initial-state {:a [1 2] :b nil})]
                        [#'inspector/inspector-view (inspector/initial-state nil)]
                        [#'diff/diff-view {:patch "@@ -1 +1 @@\n-old\n+new\n"}]
                        [#'diff/diff-view {:compare? true :before "" :after ""}]
                        [#'logging/logging-view {:entries [{:id 1 :level :warn :message "test"}]}]
                        [#'exceptions/exception-view {:entries [{:id 1 :throwable (ex-info "test" {:x 1})}]}]
                        [#'git/git-view {:directory "." :repository "." :branch "main"
                                          :files [{:index " " :worktree "M" :path "test.clj"}]
                                          :history [{:hash "abc" :subject "test" :author "Test"}]}]
                        [#'flowstorm/flowstorm-view {:runtime @flowstorm/runtime-state}]]]
    (testing (str view " " size)
      (let [app (component/make-app view {:state state :size size})
            bounds (ui/bounds (app))]
        (is (every? number? bounds)))))
  (let [app (component/make-app #'tap/tap-view
                                {:size [640 480] :tap-vals [{:id 0 :summary "nil"
                                                           :obj (viscous/wrap nil)}]})]
    (is (every? number? (ui/bounds (app))))))

(defrecord BrokenBounds []
  ui/IBounds
  (-bounds [_] (throw (IllegalStateException. "broken bounds"))))

(deftest broken-views-and-handlers-are-contained
  (let [this (assoc (applet/tool-applet "Broken" (constantly (->BrokenBounds)) {})
                    :size [300 200] :render-error* (atom nil))
        before (count (events/recent :exception))]
    (dotimes [_ 3]
      (is (every? number? (ui/bounds (applet/safe-ui this nil {})))))
    (is (= (inc before) (count (events/recent :exception)))))
  (let [this (assoc (applet/tool-applet "Bad handler"
                                       (fn [_] (ui/on :mouse-down (fn [_] (throw (AssertionError. "bad click")))
                                                      (ui/rectangle 100 100))) {})
                    :size [100 100] :render-error* (atom nil))]
    (is (nil? (ui/mouse-down (applet/safe-ui this nil {}) [1 1])))
    (is (= "bad click" (some-> (events/recent :exception) peek :throwable .getMessage)))))

(deftest editing-workspace-names
  (let [state (atom {:workspaces {:by-id {1 {:id 1 :name "one"}}} :width 150})
        base (component/default-handler state)
        selections (atom [])
        dispatch (fn [effect & args]
                   (if (= effect ::easel/select-workspace)
                     (swap! selections conj args)
                     (apply base effect args)))
        app (component/make-app #'easel/workspace-view state
                                 (fn [intents] (run! #(apply dispatch %) intents)))]
    (ui/mouse-down (app) [74 31])
    (ui/mouse-down (app) [74 31])
    (ui/key-press (app) "z")
    (ui/key-press (app) :enter)
    (is (= "zone" (get-in @state [:workspaces :by-id 1 :name])))
    (is (= 1 (count @selections)))
    (is (nil? (get-in @state [:membrane.component/context :focus])))))

(defn view-nodes [view]
  (tree-seq #(seq (ui/children %)) ui/children view))

(defn button-frames
  ([view] (button-frames view [0 0]))
  ([view [x y]]
   (let [[ox oy] (ui/origin view)
         x (+ x ox) y (+ y oy)
         button (when (instance? com.phronemophobic.easel.ui.Button view)
                  {:text (:text view) :hover? (:hover? view) :disabled? (:disabled? view)
                   :selected? (:selected? view)
                   :x x :y y :size (ui/bounds view)})
         [dx dy] (cond
                   (instance? membrane.ui.Padding view) [(:left view) (:top view)]
                   (instance? membrane.ui.ScrollView view) (:offset view)
                   :else [0 0])]
     (into (if button [button] [])
           (mapcat #(button-frames % [(+ x dx) (+ y dy)])) (ui/children view)))))

(defn move-pointer [app position]
  (let [view (app)]
    (ui/mouse-move view position)
    (ui/mouse-move-global view position)))

(deftest workspace-navigation-controls-have-independent-hover-state
  (let [ids [:first-user :terminal :inspector :git :documentation :second-user]
        labels ["user" "term" "Data Inspector" "Git" "Documentation" "user"]
        easel {:applets (into {} (map (fn [id label] [id {:id id :label label}]) ids labels))
               :root-pane {:panes (mapv #(hash-map :applet-id %) ids)}
               :workspace-id 1
               :workspaces {:by-id {1 {:name "Workspace"} 2 {:name "Data lab"} 3 {:name "Workspace"}}}}
        manager (assoc easel :manager-id :manager
                       :applets {:manager {:state {:page :discover
                                                   :registry {:test/a {:id :test/a :name "Shared name" :kind :toolset}
                                                              :test/b {:id :test/b :name "Shared name" :kind :toolset}}}}})]
    (doseq [[view args]
            [[#'shell/sidebar {:easel easel :width 180 :height 1000}]
             [#'shell/sidebar {:easel manager :width 180 :height 1000}]
             [#'shell/workspace-tabs {:easel easel :width 1000 :height 42}]
             [#'shell/dock {:easel (assoc easel :dock {:selected :values})
                           :width 1000 :height 240 :dock-body (ui/spacer 1000 190)}]]]
      (testing (str view)
        (let [state (atom (assoc args :membrane.component/context {:focus :inspector}))
              app (component/make-app view state)
              frames #(button-frames (app))
              selected #(mapv (juxt :text :x :y) (filter :selected? (frames)))
              initial-selection (selected)]
          (is (< 1 (count (frames))))
          (dotimes [_ 2]
            (doseq [{:keys [text x y size]} (frames)]
              (move-pointer app [(+ x 4) (+ y (/ (second size) 2))])
              (is (= [[text x y]] (mapv (juxt :text :x :y) (filter :hover? (frames)))) text)
              (is (= initial-selection (selected)) "Hover must not change selection")))
          (move-pointer app [-20 -20])
          (is (not-any? :hover? (frames)))
          (is (= (:easel args) (:easel @state))))))))

(deftest sidebar-rows-keep-their-applet-targets-after-reordering
  (let [ids [:first :second :third]
        state (atom {:easel {:applets (into {} (map #(vector % {:id % :label "user"}) ids))
                             :root-pane {:panes (mapv #(hash-map :applet-id %) ids)}}
                     :width 180 :height 800})
        calls (atom []) base (component/default-handler state)
        app (component/make-app #'shell/sidebar state
              (fn [intents]
                (doseq [[effect & args :as intent] intents]
                  (if (= effect ::easel/focus-applet)
                    (swap! calls conj (first args))
                    (base [intent])))))
        check-rows (fn [expected]
                     (reset! calls [])
                     (doseq [{:keys [x y size]} (filter #(= "user" (:text %)) (button-frames (app)))]
                       (let [pos [(+ x 4) (+ y (/ (second size) 2))]]
                         (move-pointer app pos)
                         (is (= [[x y]] (mapv (juxt :x :y) (filter :hover? (button-frames (app))))))
                         (ui/mouse-down (app) pos)))
                     (is (= expected @calls)))]
    (check-rows ids)
    (swap! state update-in [:easel :root-pane :panes] #(vec (reverse %)))
    (check-rows (vec (reverse ids)))
    (swap! state update-in [:easel :root-pane :panes] subvec 1)
    (check-rows [:second :first])))

(deftest documentation-controls-have-independent-hover-state
  (doseq [[view args]
          [[#'documentation/target-view
            {:state (assoc (documentation/initial-state) :target {:kind :guide :id :start} :ready? true)
             :width 800}]
           [#'documentation/target-view
            {:state (assoc (documentation/initial-state) :target {:kind :namespaces}
                           :namespaces '[clojure.core com.phronemophobic.easel.model]) :width 800}]
           [#'documentation/namespace-view
            {:state (documentation/initial-state) :target {:kind :namespace :symbol 'com.phronemophobic.easel.model}
             :width 800}]
           [#'documentation/examples-view
            {:state {:ready? true} :target {:kind :guide :id :start} :width 800
             :examples [{:source "1"} {:source "2"}]}]]]
    (testing (str view)
      (let [state (atom args)
            app (component/make-app view state)
            active #(mapv (juxt :text :x :y) (filter :hover? (button-frames (app))))]
        (doseq [{:keys [text x y size disabled?]} (button-frames (app)) :when (not disabled?)]
          (move-pointer app [(+ x 4) (+ y (/ (second size) 2))])
          (is (= [[text x y]] (active)) text))
        (move-pointer app [-20 -20])
        (is (empty? (active)))
        (is (= (:state args) (:state @state)))))))

(deftest large-diffs-render-in-pages
  (let [patch (str "@@ -1,10000 +1,10000 @@\n"
                   (apply str (map #(str " line-" % "\n") (range 10000))))
        state (atom {:state (:state (diff/diff-applet nil {:patch patch})) :size [640 480]})
        app (component/make-app #'diff/diff-view state)
        texts #(keep (fn [node] (or (:text node) (:paragraph node))) (view-nodes (app)))]
    (is (< (count (texts)) 1000))
    (is (some #(and (string? %) (.contains ^String % "line-0")) (texts)))
    (is (not-any? #(and (string? %) (.contains ^String % "line-9999")) (texts)))
    (swap! state assoc-in [:state :offset] 10000)
    (is (some #(and (string? %) (.contains ^String % "line-9999")) (texts)))))

(deftest git-actions-respect-the-loaded-directory
  (let [make-view (fn [directory]
                    ((component/make-app #'git/git-view
                                          {:size [640 480]
                                           :state {:directory directory :loaded-directory "loaded"
                                                   :repository "loaded" :files [{:index "?" :worktree "?" :path "file"}]}})))
        stage-button (fn [view] (some #(when (and (= "Stage" (:text %)) (:on-click %)) %) (view-nodes view)))]
    (is (nil? (seq (ui/mouse-down (stage-button (make-view "changed")) [5 5]))))
    (is (seq (ui/mouse-down (stage-button (make-view "loaded")) [5 5])))))

(deftest command-search-uses-description-and-category
  (let [items [{:id :test/a :name "Editor" :description "Structural Clojure editing" :category :applet}
               {:id :test/b :name "Clojure" :category :guide}
               {:id :test/c :name "Hidden" :palette? false}]]
    (is (= [:test/a] (mapv :id (commands/matching items "clojure structural" :applet))))
    (is (= [:test/b :test/a] (mapv :id (commands/matching items "" nil))))))

(deftest command-palette-can-reach-every-result-and-skips-disabled-actions
  (let [items (mapv (fn [i] {:id (keyword "test" (str i)) :name (format "Command %02d" i)
                             :disabled? (= i 2) :intents [[:test/run i]]}) (range 20))
        state (atom {:commands items :query "" :width 640 :height 400
                     :membrane.component/context {:focus ::shell/command-query}})
        base (component/default-handler state) calls (atom [])
        app (component/make-app #'shell/command-palette state
                                 (fn [intents]
                                   (doseq [[effect & args :as intent] intents]
                                     (if (= effect :test/run) (swap! calls conj (first args)) (base [intent])))))]
    (dotimes [_ 18] (ui/key-press (app) :down))
    (is (= 18 (get-in @state [:selection :index])))
    (ui/key-press (app) :enter)
    (is (= [19] @calls))
    (ui/key-press (app) :down)
    (is (= 0 (get-in @state [:selection :index])))
    (swap! state assoc :query "Command 02")
    (ui/key-press (app) :enter)
    (is (= [19] @calls))
    (swap! state assoc :query "no such command")
    (ui/key-press (app) :down)
    (is (every? number? (ui/bounds (app))))
    (ui/key-press (app) :enter)
    (is (= [19] @calls))))
