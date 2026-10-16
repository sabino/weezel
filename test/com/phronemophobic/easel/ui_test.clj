(ns com.phronemophobic.easel.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel.diff :as diff]
            [com.phronemophobic.easel.exceptions :as exceptions]
            [com.phronemophobic.easel.flowstorm :as flowstorm]
            [com.phronemophobic.easel.git :as git]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.logging :as logging]
            [com.phronemophobic.easel.tap-watcher :as tap]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.model :as model]
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

(deftest large-diffs-render-in-pages
  (let [patch (str "@@ -1,10000 +1,10000 @@\n"
                   (apply str (map #(str " line-" % "\n") (range 10000))))
        state (atom {:state (:state (diff/diff-applet nil {:patch patch})) :size [640 480]})
        app (component/make-app #'diff/diff-view state)
        texts #(keep :text (view-nodes (app)))]
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
