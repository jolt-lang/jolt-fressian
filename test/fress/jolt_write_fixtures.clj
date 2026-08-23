(ns fress.jolt-write-fixtures
  "Half of the bidirectional JVM cross-write gate (see bin/jvm-xcheck):
  write every value in fress.fixtures/fixtures using THIS port's own
  writer, one file per value, under /tmp/fress-jvm-xcheck/jolt-written/.
  fress.jvm-read-check.clj then reads them back with a real
  clojure.data.fressian reader.

  `jolt run <file> <args>` does not invoke -main with those args the way
  `clj -M -m ns args` does — it just loads the file — so this runs directly
  at load time against a fixed, well-known directory rather than taking one
  as a CLI argument."
  (:require [fress.fixtures :as fx]
            [fress.writer :as w]
            [fress.impl.buffer :as buf]))

(def dir "/tmp/fress-jvm-xcheck/jolt-written")

(.mkdirs (java.io.File. dir))
(dotimes [i (count fx/fixtures)]
  (let [out (buf/byte-stream)
        wtr (w/writer out)]
    (w/writeObject wtr (nth fx/fixtures i))
    (with-open [fos (java.io.FileOutputStream. (str dir "/" i ".bin"))]
      (.write fos (buf/toByteArray out)))))
(println "jolt wrote" (count fx/fixtures) "fixtures to" dir)
