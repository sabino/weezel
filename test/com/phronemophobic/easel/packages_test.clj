(ns com.phronemophobic.easel.packages-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.model :as model]
            [com.phronemophobic.easel.packages :as packages]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.theme :as theme]
            [membrane.component :as component]
            [membrane.ui :as ui]))

(defn temporary-directory []
  (.toFile (java.nio.file.Files/createTempDirectory "easel-packages-" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn delete-directory! [directory]
  (doseq [file (reverse (file-seq directory))] (io/delete-file file true)))

(deftest package-plans-validate-before-installing
  (let [a {:package/version 1 :id :test/a :name "A" :version "1" :kind :toolset :requires [:test/b]}
        b (assoc a :id :test/b :requires [])]
    (is (= [:test/b :test/a] (mapv :id (packages/plan {:test/a a :test/b b} [:test/a]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"missing" (packages/plan {:test/a a} [:test/a])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cycle" (packages/plan {:test/a a :test/b (assoc b :requires [:test/a])} [:test/a]))))
  (doseq [coord [{:mvn/version "LATEST"} {:git/url "https://example.com/library" :git/sha "main"} {:local/root "/no/such/library"}]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (packages/validate {:package/version 1 :id :test/lib :name "Library" :version "1" :kind :library
                                     :deps {'test/library coord}})))))

(deftest local-packages-install-and-run-on-the-live-classpath
  (let [directory (temporary-directory)
        root (io/file directory "plugin")
        ns-name (str "easel_test_plugin_" (str/replace (str (random-uuid)) "-" ""))
        lib (symbol "easel-test" ns-name)
        entry (symbol ns-name "make-applet")
        tag (symbol ns-name "value")
        descriptor {:package/version 1 :id :test/plugin :name "Local plugin" :version "1" :kind :applet
                    :entry entry :deps {lib {:local/root (.getPath root)}}}]
    (try
      (.mkdirs (io/file root "src"))
      (spit (io/file root "deps.edn") "{:paths [\"src\"]}")
      (spit (io/file root "src" (str ns-name ".clj"))
            (str "(ns " ns-name " (:require [com.phronemophobic.easel.applet :as a]))\n"
                 "(defn read-value [x] {:value x})\n"
                 "(defn make-applet [handler] (a/tool-applet \"Installed plugin\" identity {}))"))
      (spit (io/file root "src" "data_readers.clj") (pr-str {tag (symbol ns-name "read-value")}))
      (.mkdirs (io/file root "src" ns-name))
      (spit (io/file root "src" ns-name "helper.clj")
            (str "(ns " ns-name ".helper) (defn value [] 42)"))
      (binding [settings/*directory* (io/file directory "config")]
        (let [state (atom {:install {}}) handler (component/default-handler state)
              result (promise) config settings/*directory*
              ;; Native startup has none of clojure.main's REPL thread bindings.
              thread (Thread.
                      (fn []
                        (binding [settings/*directory* config]
                          (deliver result @(applet/task! handler '(keypath :install)
                                                        #(packages/install! {:test/plugin descriptor} [:test/plugin]))))))]
          (.start thread)
          (is (not= ::timeout (deref result 120000 ::timeout)))
          (is (nil? (get-in @state [:install :error])))
          (is (= [:test/plugin] (get-in @state [:install :installed])))
          (is (false? (get-in @state [:install :restart?]))))
        (packages/with-runtime
          #(do (require (symbol ns-name))
               (is (= {:value 42} (read-string (str "#" tag " 42"))))))
        (let [result (promise)]
          (easel/handler ::easel/dispatch-fn
                         #(future (deliver result ((requiring-resolve (symbol (str ns-name ".helper") "value"))))))
          (is (= 42 (deref result 5000 ::timeout))))
        (let [result (promise)
              view (easel/runtime-view
                    (ui/on :mouse-down (fn [_] (deliver result (read-string (str "#" tag " 43"))) nil)
                           (ui/rectangle 100 100)))]
          (ui/mouse-down view [10 10])
          (is (= {:value 43} (deref result 1000 ::timeout))))
        (let [applet (packages/make-applet (fn [& _]) :test/plugin nil)]
          (is (= "Installed plugin" (:label applet)))
          (is (satisfies? model/IApplet applet)))
        (packages/pin! :test/plugin true)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"pinned"
                             (packages/install! {:test/plugin (assoc descriptor :version "2")} [:test/plugin])))
        (packages/pin! :test/plugin false)
        (let [toolset {:package/version 1 :id :test/tools :name "Tools" :kind :toolset :version "1" :requires [:test/plugin]}]
          (packages/install! {:test/plugin descriptor :test/tools toolset} [:test/tools])
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"needs" (packages/remove! :test/plugin)))
          (packages/remove! :test/tools))
        (packages/remove! :test/plugin)
        (is (nil? (get-in (settings/read-settings) [:packages :test/plugin]))))
      (finally (swap! packages/runtime-data-readers dissoc tag)
               (doseq [suffix ["" ".helper"]] (remove-ns (symbol (str ns-name suffix))))
               (delete-directory! directory)))))

(deftest raw-theme-imports-remain-outside-the-project
  (let [directory (temporary-directory) file (io/file directory "theme.edn")]
    (try
      (binding [settings/*directory* (io/file directory "config")]
        (settings/write-data! file {:theme/version 1 :id :test/ink :name "Ink" :extends :easel/studio-light
                                    :tokens {:color/accent "#224466"}})
        (packages/import! (.getPath file))
        (is (= "#224466" (get-in (packages/theme-data :test/ink) [:tokens :color/accent])))
        (is (= "Ink" (:name (theme/resolve-theme (packages/theme-data :test/ink) packages/theme-data)))))
      (finally (delete-directory! directory)))))
