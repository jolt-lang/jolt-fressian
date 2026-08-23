(ns fress.jvm-write-fixtures
  "Half of the bidirectional JVM cross-write gate (see bin/jvm-xcheck): a
  real clojure.data.fressian writer writes every value in
  fress.fixtures/fixtures, one file per value, under
  /tmp/fress-jvm-xcheck/jvm-written/. fress.jolt-read-check.clj (run under
  jolt) then reads them back. Run via `clojure -M`, NOT jolt."
  (:require [fress.fixtures :as fx]
            [clojure.data.fressian :as fress])
  (:import [org.fressian.impl BytesOutputStream]
           [java.io File FileOutputStream]))

(def dir "/tmp/fress-jvm-xcheck/jvm-written")

(.mkdirs (File. dir))
(dotimes [i (count fx/fixtures)]
  (let [bos (BytesOutputStream.)
        w (fress/create-writer bos)]
    (fress/write-object w (nth fx/fixtures i))
    (with-open [fos (FileOutputStream. (str dir "/" i ".bin"))]
      (.write fos (.toByteArray bos)))))
(println "JVM wrote" (count fx/fixtures) "fixtures to" dir)
