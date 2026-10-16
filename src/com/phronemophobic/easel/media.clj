(ns com.phronemophobic.easel.media
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.viscous :as viscous]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui])
  (:import [javax.imageio ImageIO]
           [javax.sound.sampled AudioSystem Clip]))

(defn describe [path]
  (let [file (.getCanonicalFile (io/file path))]
    (when-not (.isFile file) (throw (ex-info "Choose an existing media file" {})))
    (when (> (.length file) (* 64 1024 1024)) (throw (ex-info "Media preview supports files up to 64 MiB" {})))
    (with-open [input (ImageIO/createImageInputStream file)]
      (let [readers (ImageIO/getImageReaders input)]
        (if (.hasNext readers)
          (let [reader (.next readers)]
            (try (.setInput reader input)
                 {:kind :image :file (.getPath file) :dimensions [(.getWidth reader 0) (.getHeight reader 0)]}
                 (finally (.dispose reader))))
          (with-open [audio (AudioSystem/getAudioInputStream file)]
            {:kind :audio :file (.getPath file) :format (str (.getFormat audio))}))))))

(defn- close-clip! [resources]
  (when-let [^Clip clip (:clip @resources)] (.stop clip) (.close clip))
  (swap! resources dissoc :clip))

(defeffect ::load [$state]
  (let [{:keys [path resources busy?]} (dispatch! :get $state) resources @resources]
   (when-not busy?
    (applet/task!
     dispatch! $state
     #(let [media (describe path)
            clip (when (= :audio (:kind media))
                   (let [clip (AudioSystem/getClip)]
                     (try (with-open [input (AudioSystem/getAudioInputStream (io/file (:file media)))] (.open clip input)) clip
                          (catch Throwable e (.close clip) (throw e)))))]
        (locking resources
          (close-clip! resources)
          (if (:active? @resources)
            (do (swap! resources assoc :clip clip) {:media media})
            (do (when clip (.close ^Clip clip)) {}))))))))

(defeffect ::play [$state play?]
  (when-let [resources (some-> (dispatch! :get $state) :resources deref)]
    (locking resources
      (when-let [^Clip clip (:clip @resources)]
        (if play? (do (.setFramePosition clip 0) (.start clip)) (.stop clip))))))

(defui media-view [{:keys [state size]}]
  (let [[w h] size {:keys [kind file dimensions format]} (:media state)
        bounds [(max 1 (- w 40)) (max 1 (- h 80))]]
    (ui/padding 20 12
      (ui/vertical-layout
       (eui/row nil [(eui/field {:text (:path state "") :$text [$state '(keypath :path)]
                                :placeholder "Image or audio file…" :width (max 80 (- w 140))
                                :on-submit #(vector [::load $state])})
                    (eui/button {:text "Open" :disabled? (:busy? state) :on-click #(vector [::load $state])})])
       (ui/spacer 0 12)
       (when-let [error (:error state)] (eui/text {:text error :width (first bounds) :color :color/error}))
       (case kind
         :image (let [scale (min 1 (/ (first bounds) (first dimensions)) (/ (second bounds) (second dimensions)))]
                  (ui/image file (mapv #(* scale %) dimensions)))
         :audio (ui/vertical-layout
                 (eui/text {:text format :width (first bounds) :size 12 :color :color/secondary})
                 (eui/row nil [(eui/button {:text "Play" :on-click #(vector [::play $state true])})
                              (eui/button {:text "Stop" :on-click #(vector [::play $state false])})]))
         (eui/text {:text "Images and local audio in your workspace." :width (first bounds) :color :color/secondary}))))))

(defn media-applet [handler]
  (let [resources (atom {:active? true})]
    (assoc (applet/tool-applet "Media" #'media-view {:path "" :resources (viscous/wrap resources)}
                              (fn [_] (fn [] (locking resources (close-clip! resources) (swap! resources assoc :active? false)))))
           :icon "picture")))
