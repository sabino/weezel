(ns com.phronemophobic.easel.browser-test
  (:require [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel.browser :as browser]
            [com.phronemophobic.gen3 :as gen3]
            [membrane.ui :as ui]))

(deftest browser-distinguishes-hover-drag-and-mouse-buttons
  (let [events (atom [])
        view (assoc (browser/->Browser :browser 0 true 1 200 200 nil nil) :mouse-buttons (atom 0))]
    (with-redefs [gen3/map->mouse-event identity
                  gen3/call (fn [object method & args]
                              (if (= method :get_host) :host
                                  (swap! events conj [method args])))]
      (ui/mouse-move view [10 10])
      (is (zero? (-> @events peek second first :modifiers)))
      (ui/mouse-event view [10 10] 0 true 0)
      (ui/mouse-move view [20 20])
      (is (= gen3/EVENTFLAG_LEFT_MOUSE_BUTTON (-> @events peek second first :modifiers)))
      (ui/mouse-event view [20 20] 0 false 0)
      (ui/mouse-move view [30 30])
      (is (zero? (-> @events peek second first :modifiers)))
      (ui/mouse-event view [10 10] 1 true 0)
      (is (= 2 (-> @events peek second second)))
      (is (= gen3/EVENTFLAG_RIGHT_MOUSE_BUTTON (-> @events peek second first :modifiers)))
      (ui/mouse-event view [10 10] 1 false 0)
      (ui/mouse-event view [10 10] 2 true 0)
      (is (= 1 (-> @events peek second second)))
      (is (= gen3/EVENTFLAG_MIDDLE_MOUSE_BUTTON (-> @events peek second first :modifiers))))))

(deftest embedded-browser-keeps-the-native-wheel-direction
  (let [events (atom []) view (browser/->Browser :browser 0 true 1 200 200 nil nil)]
    (with-redefs [gen3/map->mouse-event identity
                  gen3/call (fn [_ method & args]
                              (if (= method :get_host) :host (swap! events conj [method args])))]
      (ui/scroll view [12 24] [10 20])
      (is (= [:send_mouse_wheel_event (list {:x 10 :y 20} -12 -24)] (peek @events)))
      (ui/scroll view [-12 -24] [10 20])
      (is (= [:send_mouse_wheel_event (list {:x 10 :y 20} 12 24)] (peek @events))))))
