(ns fress.jvm-read-check
  "Half of the bidirectional JVM cross-write gate (see bin/jvm-xcheck): a
  real clojure.data.fressian reader reads every file under
  /tmp/fress-jvm-xcheck/jolt-written/ (written by THIS port's own writer,
  via fress.jolt-write-fixtures.clj, run under jolt), and compares against
  fress.fixtures/fixtures index-by-index. Run via `clojure -M`, NOT jolt.
  Prints a FAILURES line and exits non-zero on any mismatch."
  (:require [fress.fixtures :as fx]
            [clojure.data.fressian :as fress])
  (:import [java.nio.file Files Paths]))

(def dir "/tmp/fress-jvm-xcheck/jolt-written")

(def fails (atom 0))
(dotimes [i (count fx/fixtures)]
  (let [expected (nth fx/fixtures i)
        bytes (Files/readAllBytes (Paths/get (str dir "/" i ".bin") (make-array String 0)))
        rdr (fress/create-reader (java.io.ByteArrayInputStream. bytes) :checksum? false)
        got (fress/read-object rdr)]
    (if (fx/deep-eq? expected got)
      (println i "OK")
      (do (swap! fails inc)
          (println i "FAIL expected:" (pr-str expected) "got:" (pr-str got) "class:" (class got))))))
(if (zero? @fails)
  (println "ALL JOLT-WRITTEN FIXTURES READ CORRECTLY BY THE JVM")
  (do (println @fails "FAILURES")
      (System/exit 1)))
