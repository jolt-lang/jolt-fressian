(ns jvm-xcheck-read
  (:require [fress.reader :as r]
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

(defn arr? [x] (and x (.isArray ^Class (class x))))

(defn deep-eq? [a b]
  (cond
    (and (arr? a) (arr? b))
    (and (= (alength a) (alength b))
         (every? true? (map deep-eq? (seq a) (seq b))))

    (and (instance? java.util.regex.Pattern a) (instance? java.util.regex.Pattern b))
    (= (.pattern ^java.util.regex.Pattern a) (.pattern ^java.util.regex.Pattern b))

    (and (float? a) (float? b))
    (== a b)

    :else (= a b)))

(def fails (atom 0))
(dotimes [i (count fixtures)]
  (let [expected (nth fixtures i)
        bytes (java.nio.file.Files/readAllBytes
                (java.nio.file.Paths/get (str "/tmp/fress-xcheck/jvm-written/" i ".bin") (make-array String 0)))
        rdr (r/reader bytes)
        got (r/readObject rdr)]
    (if (deep-eq? expected got)
      (println i "OK" (pr-str (if (arr? expected) (class expected) expected)))
      (do (swap! fails inc)
          (println i "FAIL expected:" (pr-str expected) "got:" (pr-str got))))))

(println (if (zero? @fails) "ALL JVM-WRITTEN FIXTURES READ CORRECTLY BY JOLT" (str @fails " FAILURES")))
