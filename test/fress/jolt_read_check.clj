(ns fress.jolt-read-check
  "Half of the bidirectional JVM cross-write gate (see bin/jvm-xcheck):
  read every file under /tmp/fress-jvm-xcheck/jvm-written/ (written by a
  real clojure.data.fressian writer, via fress.jvm-write-fixtures.clj) with
  THIS port's own reader, and compare against fress.fixtures/fixtures
  index-by-index. Prints a FAILURES line and exits non-zero on any mismatch."
  (:require [fress.fixtures :as fx]
            [fress.reader :as r]))

(def dir "/tmp/fress-jvm-xcheck/jvm-written")

(def fails (atom 0))
(dotimes [i (count fx/fixtures)]
  (let [expected (nth fx/fixtures i)
        bytes (java.nio.file.Files/readAllBytes
                (java.nio.file.Paths/get (str dir "/" i ".bin") (make-array String 0)))
        rdr (r/reader bytes)
        got (r/readObject rdr)]
    (if (fx/deep-eq? expected got)
      (println i "OK")
      (do (swap! fails inc)
          (println i "FAIL expected:" (pr-str expected) "got:" (pr-str got))))))
(if (zero? @fails)
  (println "ALL JVM-WRITTEN FIXTURES READ CORRECTLY BY JOLT")
  (do (println @fails "FAILURES")
      (System/exit 1)))
