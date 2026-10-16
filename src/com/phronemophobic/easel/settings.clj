(ns com.phronemophobic.easel.settings
  (:require [clojure.java.io :as io]
            [com.phronemophobic.easel.theme :as theme])
  (:import [java.nio.file Files StandardCopyOption]))

(def ^:dynamic *directory*
  (io/file (or (System/getenv "XDG_CONFIG_HOME")
               (str (System/getProperty "user.home") "/.config")) "easel"))

(defn file [& path] (apply io/file *directory* path))

(defn read-settings []
  (let [source (file "settings.edn")]
    (if (.isFile source)
      (let [value (theme/read-data source)]
        (when-not (map? value) (throw (ex-info "Settings must be an EDN map" {})))
        value)
      {})))

(defn write-data! [target value]
  (let [target (.getAbsoluteFile (io/file target))
        parent (.toPath (.getParentFile target))]
    (Files/createDirectories parent (make-array java.nio.file.attribute.FileAttribute 0))
    (let [temporary (Files/createTempFile parent ".easel-" ".edn" (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (spit (.toFile temporary) (pr-str value))
        (try
          (Files/move temporary (.toPath target)
                      (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
          (catch java.nio.file.AtomicMoveNotSupportedException _
            (Files/move temporary (.toPath target)
                        (into-array StandardCopyOption [StandardCopyOption/REPLACE_EXISTING]))))
        (finally (Files/deleteIfExists temporary)))))
  value)

(defn update-settings! [f & args]
  (locking *directory*
    (write-data! (file "settings.edn") (apply f (read-settings) args))))
