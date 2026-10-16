(ns com.phronemophobic.easel.term-test
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is]]
            [asciinema.vt :as vt]
            [com.phronemophobic.easel.term :as term]
            [com.phronemophobic.easel.model :as model]
            [membrane.component :as component]))

(defn await-value [f]
  (loop [remaining 200]
    (or (f) (when (pos? remaining) (Thread/sleep 10) (recur (dec remaining))))))

(deftest closed-repaint-channels
  (let [in (async/chan 2) out (async/chan 2)
        worker (term/debounce-chan in out 500)]
    (async/>!! in :first)
    (async/>!! in false)
    (async/close! in)
    (let [[value port] (async/alts!! [out (async/timeout 1000)])]
      (is (= out port))
      (is (false? value)))
    (is (= [nil out] (async/alts!! [out (async/timeout 1000)])))
    (is (= [nil worker] (async/alts!! [worker (async/timeout 1000)])))))

(deftest terminal-resize-keeps-content
  (let [original (term/feed-text (vt/make-vt 12 4) "hello\r\nworld")
        resized (update original :screen term/resize-screen 20 8)]
    (is (= (take 5 (get-in original [:screen :lines 0]))
           (take 5 (get-in resized [:screen :lines 0]))))
    (is (= 8 (count (get-in resized [:screen :lines]))))
    (is (every? #(= 20 (count %)) (get-in resized [:screen :lines])))
    (is (= {:x 5 :y 1 :visible true} (get-in resized [:screen :cursor])))))

(deftest terminal-unicode-batches
  (let [smile "🙂"
        partial (term/feed-text (vt/make-vt 12 4) (subs smile 0 1))
        complete (term/feed-text partial (str (subs smile 1) "!"))]
    (is (= [128578 33] (mapv first (take 2 (get-in complete [:screen :lines 0])))))
    (is (nil? (:pending-surrogate complete)))))

(deftest terminal-process-lifecycle
  (with-redefs [term/repaint! (fn [])]
    (let [state (atom {}) handler (component/default-handler state)
          applet (model/-start (term/termlet handler {:command ["/bin/sh" "-c" "printf terminal-ready"]})
                               '(keypath :applet) [640 400] [1 1])]
      (reset! state {:applet applet})
      (try
        (run! #(%) (::model/queue applet))
        (is (await-value #(get-in @state [:applet :exited?])))
        (is (= "terminal-ready" (apply str (map (comp char first) (take 14 (get-in @state [:applet :vt :screen :lines 0]))))))
        (is (await-value #(nil? @(:resources applet))))
        (finally (model/-stop applet))))))

(deftest closing-terminal-during-startup
  (let [state (atom {}) handler (component/default-handler state)
        applet (model/-start (term/termlet handler {:command ["/bin/sleep" "10"]})
                             '(keypath :applet) [640 400] [1 1])]
    (reset! state {:applet applet})
    (model/-stop applet)
    (run! #(%) (::model/queue applet))
    (is (nil? @(:resources applet)))
    (is (false? @(:active? applet)))))

(deftest closing-a-running-terminal
  (with-redefs [term/repaint! (fn [])]
    (let [state (atom {}) handler (component/default-handler state)
          applet (model/-start (term/termlet handler {:command ["/bin/sh" "-c" "printf ready; exec sleep 15"]})
                               '(keypath :applet) [640 400] [1 1])]
      (reset! state {:applet applet})
      (try
        (run! #(%) (::model/queue applet))
        (is (await-value #(get-in @state [:applet :pty])))
        (let [pty (get-in @state [:applet :pty])]
          (model/-stop applet)
          (is (await-value #(not (.isAlive ^Process pty)))))
        (is (nil? @(:resources applet)))
        (finally (model/-stop applet))))))
