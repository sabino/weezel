(ns com.phronemophobic.easel.ui-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel.diff :as diff]
            [com.phronemophobic.easel.exceptions :as exceptions]
            [com.phronemophobic.easel.flowstorm :as flowstorm]
            [com.phronemophobic.easel.git :as git]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.logging :as logging]
            [com.phronemophobic.easel.tap-watcher :as tap]
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
