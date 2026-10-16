(ns com.phronemophobic.easel.help-test
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.help :as help]
            [com.phronemophobic.easel.packages-test :as fixtures]
            [com.phronemophobic.easel.settings :as settings]
            [membrane.component :as component]
            [membrane.skia :as skia]
            [membrane.ui :as ui]))

(defn await-applets []
  (loop [n 500]
    (or (not @easel/queue-scheduled?)
        (when (pos? n) (Thread/sleep 10) (recur (dec n))))))

(defn isolated-workspace [f]
  (let [directory (fixtures/temporary-directory) previous @easel/app-state]
    (try
      (binding [settings/*directory* directory]
        (reset! easel/app-state nil)
        (f))
      (finally
        (easel/remove-all-widgets!)
        (await-applets)
        (reset! easel/app-state previous)
        (fixtures/delete-directory! directory)))))

(defn nodes [view] (tree-seq #(seq (ui/children %)) ui/children view))

(defn check-view [app]
  (is (every? #(every? number? (ui/bounds %)) (nodes (app)))))

(defn click [app text]
  (let [button (some #(when (and (= text (:text %)) (:on-click %)) %) (nodes (app)))]
    (is (some? button) text)
    (when button (run! #(apply easel/handler %) (ui/mouse-down button [5 5])))))

(deftest help-pages-keep-their-bounds-after-switching
  (doseq [width [820 300]
          page [:start :emacs :start]]
    (let [view (help/introduction-page {:width width :page page})]
      (is (= (ui/-bounds view) (ui/bounds view))))))

(deftest default-launch-does-not-restore-demo-contents
  (isolated-workspace
   (fn []
     (settings/write-data! (settings/file "settings.edn") {:workspace :easel/sketchbook})
     (with-redefs [skia/run-sync
                   (fn [view _]
                     (let [easel (:easel @easel/app-state)]
                       (is (empty? (:applets easel)))
                       (is (nil? (:dock easel)))
                       (is (= :easel/empty (get-in easel [:workspaces :by-id (:workspace-id easel) :preset])))
                       (is (= ["Workspace"] (mapv :name (vals (get-in easel [:workspaces :by-id])))))
                       (is (true? (get-in easel [:help :first-run?])))
                       (doseq [node (nodes (view {:container-size [1536 1024] :content-scale [1 1]}))]
                         (ui/bounds node))))]
       (let [running (easel/run)
             [value channel] (async/alts!! [(::easel/closed running) (async/timeout 10000)])]
         (is (= (::easel/closed running) channel))
         (is (nil? value))))
     (is (nil? (:onboarding/version (settings/read-settings)))))))

(deftest first-run-help-can-be-dismissed-and-reopened
  (isolated-workspace
   (fn []
     (easel/initialize! {:preset :easel/empty :onboarding? true})
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)]
       (doseq [[page title] help/pages]
         (click app title)
         (is (= page (get-in @easel/app-state [:easel :help :page])))
         (is (= [0 0] (get-in @easel/app-state [:easel :help :offset])))
         (doseq [size [[1536 1024] [600 400] [200 150] [0 0]]]
           (swap! easel/app-state update :easel easel/resize-window size [1 1])
           (check-view app))
         (swap! easel/app-state update :easel easel/resize-window [1536 1024] [1 1]))
       (click app "Start with an empty workspace")
       (is (empty? (get-in @easel/app-state [:easel :applets])))
       (is (nil? (get-in @easel/app-state [:easel :help])))
       (is (= help/version (:onboarding/version (settings/read-settings))))
       (reset! easel/app-state nil)
       (easel/initialize! {:preset :easel/empty :onboarding? true})
       (is (nil? (get-in @easel/app-state [:easel :help])))
       (ui/key-event (app) 290 0 :press 0)
       (is (= :start (get-in @easel/app-state [:easel :help :page])))
       (ui/key-event (app) 256 0 :press 0)
       (is (nil? (get-in @easel/app-state [:easel :help])))
       (ui/key-event (app) 290 0 :press 0)
       (is (some? (get-in @easel/app-state [:easel :help])))
       (ui/key-event (app) 71 0 :press ui/CONTROL-MASK)
       (is (nil? (get-in @easel/app-state [:easel :help])))
       (ui/key-event (app) 75 0 :press ui/CONTROL-MASK)
       (is (some? (get-in @easel/app-state [:easel :commands])))
       (ui/key-event (app) 71 0 :press ui/CONTROL-MASK)
       (is (nil? (get-in @easel/app-state [:easel :commands])))
       (easel/handler ::easel/toggle-pane-resize {:$easel '(keypath :easel) :pane-id ::easel/root-pane})
       (ui/key-event (app) 256 0 :press 0)
       (is (not (get-in @easel/app-state [:easel :root-pane ::easel/pane-resize])))))))

(deftest help-actions-open-only-the-requested-tools
  (isolated-workspace
   (fn []
     (easel/initialize! {:preset :easel/empty :onboarding? true})
     (let [app (component/make-app #'easel/easel-view easel/app-state easel/handler)]
       (click app "Open a blank editor")
       (is (await-applets))
       (is (= 1 (count (get-in @easel/app-state [:easel :applets]))))
       (let [editor (first (vals (get-in @easel/app-state [:easel :applets])))]
         (is (= "untitled.clj" (:label editor)))
         (is (= "" (str (get-in editor [:state :editor :rope]))))
         (is (= 'user (some-> (get-in editor [:state :editor :eval-ns]) ns-name)))
         (is (= (:id editor) (get-in @easel/app-state [:membrane.component/context :focus])))
         (ui/clipboard-paste (app) "(+ 1 2)")
         (ui/key-event (app) 88 0 :press ui/CONTROL-MASK)
         (ui/key-event (app) 69 0 :press ui/CONTROL-MASK)
         (is (loop [n 500]
               (or (seq (get-in @easel/app-state [:easel :applets (:id editor) :state :editor :line-val]))
                   (when (pos? n) (Thread/sleep 10) (recur (dec n))))))
         (let [editor (get-in @easel/app-state [:easel :applets (:id editor) :state :editor])]
           (is (= 3 (-> (get (:line-val editor) (:rope editor)) vals first deref)))))
       (is (nil? (get-in @easel/app-state [:easel :help])))
       (is (nil? (get-in @easel/app-state [:easel :dock])))
       (easel/handler ::easel/toggle-dock)
       (is (await-applets))
       (is (= 2 (count (get-in @easel/app-state [:easel :applets]))))
       (is (= :repl (get-in @easel/app-state [:easel :dock :selected])))
       (is (not (get-in @easel/app-state [:easel :dock :hidden?])))
       (easel/handler ::easel/toggle-dock)
       (is (true? (get-in @easel/app-state [:easel :dock :hidden?])))
       (easel/handler ::easel/show-help)
       (click app "Open documentation")
       (is (await-applets))
       (is (= 3 (count (get-in @easel/app-state [:easel :applets]))))
       (is (some #(= {:kind :guide :id :start} (get-in % [:state :target]))
                 (vals (get-in @easel/app-state [:easel :applets]))))))))
