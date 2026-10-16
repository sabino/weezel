(ns com.phronemophobic.easel.dewey
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [com.phronemophobic.easel.applet :as applet]
            [com.phronemophobic.easel.inspector :as inspector]
            [com.phronemophobic.easel.settings :as settings]
            [com.phronemophobic.easel.theme :as theme]
            [com.phronemophobic.easel.ui :as eui]
            [com.phronemophobic.viscous :as viscous]
            [membrane.basic-components :as basic]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui])
  (:import [java.net URI]
           [java.util.zip GZIPInputStream]
           [java.nio.file Files StandardCopyOption]))

(defn- connection [url]
  (doto (.openConnection (.toURL (URI. url)))
    (.setConnectTimeout 10000) (.setReadTimeout 30000)
    (.setRequestProperty "User-Agent" "Easel")))

(defn read-index [file]
  (with-open [input (io/input-stream file)
              stream (if (str/ends-with? (str file) ".gz") (GZIPInputStream. input) input)]
    (let [data (theme/read-data stream (* 64 1024 1024))]
      (when-not (and (map? data) (<= (count data) 100000) (every? map? (vals data)))
        (throw (ex-info "Expected a Dewey deps-libs.edn index" {})))
      data)))

(defn download-index! []
  (let [release (with-open [input (.getInputStream (connection "https://api.github.com/repos/phronmophobic/dewey/releases/latest"))]
                  (json/read (io/reader input) :key-fn keyword))
        asset (some #(when (= "deps-libs.edn.gz" (:name %)) %) (:assets release))
        _ (when-not asset (throw (ex-info "This Dewey release has no deps-libs.edn.gz asset" {})))
        target (settings/file "cache" "dewey.edn.gz")]
    (.mkdirs (.getParentFile target))
    (let [temporary (Files/createTempFile (.toPath (.getParentFile target)) ".dewey-" ".gz"
                                          (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (with-open [input (.getInputStream (connection (:browser_download_url asset)))
                    output (io/output-stream (.toFile temporary))]
          (let [buffer (byte-array 8192)]
            (loop [total 0]
              (let [n (.read input buffer)]
                (when (pos? n)
                  (when (> (+ total n) (* 16 1024 1024)) (throw (ex-info "Dewey download exceeded 16 MiB" {})))
                  (.write output buffer 0 n)
                  (recur (+ total n)))))))
        (let [index (read-index (.toFile temporary))]
          (Files/move temporary (.toPath target) (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))
          {:index (viscous/wrap index) :count (count index) :release (:tag_name release)})
        (finally (Files/deleteIfExists temporary))))))

(defn search [index query]
  (let [query (str/lower-case query)]
    (->> index
         (filter (fn [[lib info]] (str/includes? (str/lower-case (str lib " " (:description info) " " (:topics info))) query)))
         (sort-by (fn [[lib info]] [(- (or (:stars info) 0)) (str lib)]))
         (take 100))))

(defeffect ::load [$state download?]
  (let [source (:source (dispatch! :get $state))]
    (applet/task! dispatch! $state
                  #(if download? (download-index!)
                       (let [data (read-index (if (str/blank? source) (settings/file "cache" "dewey.edn.gz") source))]
                         {:index (viscous/wrap data) :count (count data) :release "Local"})))))

(defui dewey-view [{:keys [state size]}]
  (let [[w h] size width (max 1 (- w 40)) results (search (some-> (:index state) deref) (:query state ""))]
    (ui/padding 20 20
      (ui/vertical-layout
       (eui/text {:text "Dewey" :size 24 :weight :semi-bold})
       (ui/spacer 0 8)
       (eui/text {:text (if (:index state) (str (:count state) " libraries · " (:release state)) "Clojure libraries as data.")
                   :color :color/secondary})
       (ui/spacer 0 16)
       (eui/row nil [(eui/button {:text "Download latest index" :disabled? (:busy? state) :on-click #(vector [::load $state true])})
                    (eui/button {:text "Load local index" :disabled? (:busy? state) :on-click #(vector [::load $state false])})])
       (ui/spacer 0 12)
       (eui/field {:text (:source state "") :$text [$state '(keypath :source)] :width width :placeholder "Optional deps-libs.edn or .gz path"})
       (ui/spacer 0 12)
       (eui/field {:text (:query state "") :$text [$state '(keypath :query)] :width width :placeholder "Search libraries…"})
       (ui/spacer 0 12)
       (when (:busy? state) (eui/text {:text "Loading index…" :color :color/secondary}))
       (when-let [error (:error state)] (eui/text {:text error :width width :color :color/error}))
       (basic/scrollview
        {:scroll-bounds [width (max 1 (- h 270))] :$body nil
         :body (apply ui/vertical-layout
                 (for [[lib info] results]
                   (ui/vertical-layout
                    (eui/button {:text (str lib "    ☆ " (:stars info 0)) :width width :variant :text
                                 :on-click #(vector [:com.phronemophobic.easel/add-applet
                                                      {:make-applet (fn [handler] (inspector/inspector-applet handler info))}])})
                    (eui/text {:text (:description info "") :width width :size 12 :color :color/secondary})
                    (ui/spacer 0 12))))})))))

(defn dewey-applet [handler]
  (assoc (applet/tool-applet "Dewey" #'dewey-view {:query "" :source ""}) :icon "search"))
