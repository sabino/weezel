(ns com.phronemophobic.easel.scroll-test
  (:require [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel :as easel]
            [com.phronemophobic.easel.clobber :as clobber]
            [com.phronemophobic.clobber.modes.clojure.ui :as cui]
            [membrane.basic-components :as basic]
            [membrane.component :as component]
            [membrane.skia :as skia]
            [membrane.ui :as ui]))

(deftest native-wheel-deltas-move-content-in-the-system-direction
  (let [state (atom {:offset [50 30] :scroll-bounds [100 100] :body (ui/spacer 1000 1000)})
        app (component/make-app #'basic/scrollview state)
        window {:ui (atom (app)) :mouse-position (atom [20 20])}
        repaints (atom 0)]
    (with-redefs [skia/repaint! (fn [_] (swap! repaints inc) (reset! (:ui window) (app)))]
      (easel/native-scroll window nil -30 -20)
      (is (= [110 70] (:offset @state)))
      (easel/native-scroll window nil 30 20)
      (is (= [50 30] (:offset @state)))
      (easel/native-scroll window nil 1000 1000)
      (is (= [0 0] (:offset @state)))
      (easel/native-scroll window nil -1000 -1000)
      (is (= [900 900] (:offset @state)))
      (is (= 4 @repaints)))))

(deftest editor-wheel-scrolling-preserves-the-cursor-and-accumulates-small-deltas
  (let [editor (-> (cui/make-editor {:source (apply str (repeat 100 "(+ 1 2)\n"))})
                   (clobber/resize-editor [640 200]))
        moved (reduce clobber/scroll-editor editor (repeat 30 [0 2]))
        end (clobber/scroll-editor moved [0 100000])
        back (clobber/scroll-editor end [0 -100000])]
    (is (pos? (get-in moved [:viewport :start-line])))
    (is (= (:cursor editor) (:cursor moved) (:cursor end) (:cursor back)))
    (is (= (:rope editor) (:rope moved)))
    (is (< (get-in moved [:viewport :start-line]) (get-in end [:viewport :start-line])))
    (is (= 0 (get-in back [:viewport :start-line])))
    (let [app (component/make-app #'clobber/code-editor {:editor moved :focused? true})]
      (is (every? number? (ui/bounds (app)))))))
