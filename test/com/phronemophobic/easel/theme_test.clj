(ns com.phronemophobic.easel.theme-test
  (:require [clojure.test :refer [deftest is]]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.ui :as eui]
            [membrane.component :as component]
            [membrane.ui :as ui]))

(deftest inherited-themes-and-extension-tokens
  (let [descriptor {:theme/version 1 :id :test/theme :name "Test" :extends :easel/studio-light
                    :tokens {:color/accent "#123456" :test/link :color/accent}
                    :applets {:test/applet {:padding 7}}}
        resolved (theme/resolve-theme descriptor)
        context {::theme/value resolved}]
    (is (= "#123456" (get-in resolved [:tokens :test/link])))
    (is (= [(/ 18 255.0) (/ 52 255.0) (/ 86 255.0)] (theme/token context :color/accent)))
    (is (= {:padding 7} (theme/applet-style context :test/applet)))
    (is (= 44 (:header-height (theme/component context :pane))))
    (is (= 14 (theme/token context :font/size)))))

(deftest invalid-themes-do-not-resolve
  (doseq [overrides [{:theme/version 2} {:tokens {:color/accent "red"}}
                    {:tokens {:metric/topbar -1}} {:tokens {:metric/topbar ##NaN}}
                    {:tokens {:font/ui []}}
                    {:tokens {:test/a :test/b :test/b :test/a}}
                    {:tokens {:test/a :test/missing}}
                    {:components {:button {:height "large"}}}
                    {:components {:field {:height :color/accent}}}
                    {:components {:button {:padding :metric/missing}}}
                    {:extends :test/missing}]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (theme/resolve-theme (merge {:theme/version 1 :id :test/theme :name "Test"
                                               :extends :easel/base :tokens {}} overrides)))))
  (let [a {:theme/version 1 :id :test/a :name "A" :tokens {} :extends :test/b}
        b (assoc a :id :test/b :extends :test/a)]
    (is (thrown? clojure.lang.ExceptionInfo (theme/resolve-theme a {:test/a a :test/b b})))))

(deftest controls-share-theme-and-input-state
  (let [state (atom {:text "" :size [300 40]})
        app (component/make-app #'eui/field state)]
    (ui/mouse-down (app) [14 18])
    (ui/key-press (app) "hello")
    (is (= "hello" (:text @state)))
    (ui/key-press (app) :enter)
    (is (= "hello" (:text @state)))
    (is (= [240 36] (ui/bounds (app)))))
  (let [view (eui/button {:text "Disabled" :disabled? true :on-click #(vector [:test])})]
    (is (empty? (ui/mouse-down view [5 5])))))

(defn luminance [rgb]
  (reduce + (map * [0.2126 0.7152 0.0722]
                 (map #(if (<= % 0.04045) (/ % 12.92) (Math/pow (/ (+ % 0.055) 1.055) 2.4)) rgb))))

(deftest code-remains-readable-in-both-themes
  (doseq [id [:easel/studio-light :easel/studio-dark]
          token [:color/code-symbol :color/code-comment :color/code-string :color/code-keyword :color/code-definition]]
    (let [context {::theme/value (theme/resolve-theme (theme/builtin id))}
          a (luminance (theme/token context token)) b (luminance (theme/token context :color/surface))]
      (is (>= (/ (+ (max a b) 0.05) (+ (min a b) 0.05)) 4.5) (str id " " token)))))

(deftest source-highlighting-preserves-incomplete-and-unicode-code
  (doseq [source ["(defn hello [x] ; comment\n {:résultat (str \"λ🙂\" x) :ok true :n 42})"
                  "#\"a\\\"b\" \\space 'x ::keyword" "(str \"unfinished" "#_ (danger!)" ""
                  (str "\"" (apply str (repeat 20000 "x")) "\"")]]
    (is (= source (apply str (map :text (eui/code-spans source))))))
  (let [spans (eui/code-spans "(defn hi [] {:message \"hello\"}) ; note")
        color #(some (fn [span] (when (= % (:text span)) (:token span))) spans)]
    (is (= :color/code-definition (color "defn")))
    (is (= :color/code-keyword (color ":message")))
    (is (= :color/code-string (color "\"hello\"")))
    (is (= :color/code-comment (color "; note")))))
