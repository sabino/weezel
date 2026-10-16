(ns com.phronemophobic.easel.git-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.phronemophobic.easel.diff :as diff]
            [com.phronemophobic.easel.git :as git]
            [com.phronemophobic.easel.process :as process])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defmacro with-repository [[repository] & body]
  `(let [~repository (.toFile (Files/createTempDirectory "easel-git-test-" (make-array FileAttribute 0)))]
     (try
       (git/git ~repository "init" "-q")
       (git/git ~repository "config" "user.name" "Easel Test")
       (git/git ~repository "config" "user.email" "easel@example.invalid")
       ~@body
       (finally
         (doseq [file# (reverse (file-seq ~repository))] (.delete file#))))))

(deftest git-roundtrip
  (with-repository [repository]
    (let [filename "-a [literal]\nλ.txt"
          file (io/file repository filename)]
      (spit file "first\n")
      (is (empty? (git/history repository)))
      (is (seq (:branch (git/refresh repository))))
      (is (= filename (:path (first (git/status repository)))))
      (git/stage! repository (first (git/status repository)))
      (is (= "A" (:index (first (git/status repository)))))
      (git/unstage! repository (first (git/status repository)))
      (is (.isFile file))
      (is (= "?" (:index (first (git/status repository)))))
      (git/stage! repository (first (git/status repository)))
      (git/commit! repository "Add file.")
      (is (empty? (git/status repository)))
      (is (= "Add file." (:subject (first (git/history repository)))))
      (spit file "second\n")
      (is (str/includes? (git/file-patch repository (first (git/status repository)) false) "+second"))
      (git/stage! repository (first (git/status repository)))
      (is (str/includes? (git/file-patch repository (first (git/status repository)) true) "+second"))
      (git/unstage! repository (first (git/status repository)))
      (is (= " " (:index (first (git/status repository)))))
      (git/git repository "checkout" "--detach" "-q")
      (is (str/starts-with? (:branch (git/refresh repository)) "detached at ")))))

(deftest renamed-paths
  (with-repository [repository]
    (spit (io/file repository "old.txt") "hello\n")
    (git/stage! repository {:path "old.txt"})
    (git/commit! repository "Initial.")
    (git/git repository "mv" "old.txt" "new name.txt")
    (let [entry (first (git/status repository))]
      (is (= "R" (:index entry)))
      (is (= "old.txt" (:original-path entry)))
      (is (= "new name.txt" (:path entry)))
      (git/unstage! repository entry)
      (is (.isFile (io/file repository "new name.txt")))
      (is (= #{"old.txt" "new name.txt"} (into #{} (map :path) (git/status repository)))))))

(deftest diff-hunks
  (let [patch (diff/text-diff "a\nb\nc\n" "a\nB\nc\nd\n")
        lines (diff/patch-lines patch)]
    (is (str/includes? patch "-b"))
    (is (str/includes? patch "+B"))
    (is (= [2 4] (mapv :new-line (filter #(= :add (:kind %)) lines))))
    (is (= [2] (mapv :old-line (filter #(= :remove (:kind %)) lines)))))
  (is (empty? (diff/text-diff "same" "same")))
  (is (str/includes? (diff/text-diff "" "new") "+new"))
  (let [lines (diff/patch-lines (diff/text-diff "--old\n" "++new\n"))]
    (is (= ["+++new"] (mapv :text (filter #(= :add (:kind %)) lines))))))

(deftest git-errors
  (is (thrown? clojure.lang.ExceptionInfo (git/repository-root "/dev/null")))
  (with-repository [repository]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"commit message"
                          (git/commit! repository "  ")))))

(deftest command-timeout
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"timed out"
                        (process/run ["/bin/sleep" "2"] {:timeout-ms 20}))))

(deftest bounded-command-output
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"display limit"
                        (process/run ["/bin/sh" "-c" "printf '%1000s' x"] {:max-output-chars 100})))
  (is (= {:exit 7 :out "hello" :err "error"}
         (process/run ["/bin/sh" "-c" "printf hello; printf error >&2; exit 7"])))
  (is (= "env-value" (:out (process/run ["/bin/sh" "-c" "printf '%s' \"$EASEL_TEST\""]
                                         {:env {"EASEL_TEST" "env-value"}})))))

(deftest timed-out-command-stops-its-child
  (let [pid-file (java.io.File/createTempFile "easel-child-" ".pid")]
    (try
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"timed out"
                            (process/run ["/bin/sh" "-c" "sleep 15 & child=$!; printf '%s' \"$child\" > \"$1\"; wait"
                                          "easel-test" (str pid-file)] {:timeout-ms 250})))
      (let [pid (Long/parseLong (slurp pid-file))]
        (is (loop [attempts 200]
              (let [handle (java.lang.ProcessHandle/of pid)]
                (or (.isEmpty handle)
                    (not (.isAlive ^java.lang.ProcessHandle (.get handle)))
                    (when (pos? attempts) (Thread/sleep 10) (recur (dec attempts))))))))
      (finally (.delete pid-file)))))
