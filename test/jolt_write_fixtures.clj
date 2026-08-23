(ns jolt-write-fixtures
  (:require [fress.writer :as w]
            [fress.impl.buffer :as buf]
            [fress.impl.bigdec :as bd]))

(def big-string (apply str (repeat 100000 "y")))
(def big-bytes (byte-array (map byte (range -128 127))))
(def chunked-bytes (byte-array (repeat 70000 (byte 7))))

(def fixtures
  [nil true false 0 1 -1 42 -42 1000 -1000 100000 -100000
   10000000000 -10000000000 9223372036854775807 -9223372036854775808
   1.5 0.0 1.0 -0.0
   "" "a" "hello world" big-string
   :foo :ns/foo 'bar
   [1 2 3] (list 1 2 3) #{1 2 3} {:a 1 :b 2}
   \a
   123456789012345678901234567890
   -123456789012345678901234567890
   {:nested {:a [1 2 {:b #{3 4}}]}}
   (java.util.UUID/fromString "12345678-1234-5678-1234-567812345678")
   #"abc.*def"
   (java.net.URI. "http://example.com/path?q=1")
   (java.util.Date. 1700000000000)
   big-bytes
   chunked-bytes
   [1 1 1 1 1 "repeat" "repeat" "repeat" :k :k :k]
   (int-array [1 2 3]) (long-array [1 2 3]) (float-array [1.5 2.5])
   (double-array [1.5 2.5]) (boolean-array [true false])
   (bd/bigdec 150 2)])

(.mkdirs (java.io.File. "/tmp/fress-xcheck/jolt-written"))
(dotimes [i (count fixtures)]
  (let [out (buf/byte-stream)
        wtr (w/writer out)]
    (w/writeObject wtr (nth fixtures i))
    (with-open [fos (java.io.FileOutputStream. (str "/tmp/fress-xcheck/jolt-written/" i ".bin"))]
      (.write fos (buf/toByteArray out)))))

(println "wrote" (count fixtures) "fixtures")
