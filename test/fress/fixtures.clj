(ns fress.fixtures
  "The fixture battery for the bidirectional JVM cross-write gate (see
  test/fress/jolt_write_fixtures.clj, jolt_read_check.clj,
  jvm_write_fixtures.clj, jvm_read_check.clj) and for the in-process
  round-trip suite (test/fress/roundtrip_test.clj).

  A single shared source of values loadable by BOTH jolt and a real JVM
  Clojure, so the two directions of the cross-write gate are provably
  comparing against the identical fixture list rather than two hand-kept
  copies that could silently drift apart.

  Does NOT include a fress.impl.bigdec/Bigdec value: writing one from the
  JVM side would need a custom clojure.data.fressian write-handler
  registered for that class (fressian's own default handlers don't know
  about it), which isn't worth the complexity here — bigdec's cross-write
  compatibility (jolt's Bigdec <-> a real java.math.BigDecimal) is already
  verified directly in fress.writer/fress.reader's own commit history and
  by test/fress/roundtrip_test.clj's in-process bigdec case.")

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
   [1 1 1 1 1 "repeat" "repeat" "repeat" :k :k :k] ; exercises the priority cache
   (int-array [1 2 3]) (long-array [1 2 3]) (float-array [1.5 2.5])
   (double-array [1.5 2.5]) (boolean-array [true false])])

(defn arr? [x] (and x (.isArray ^Class (class x))))

(defn deep-eq?
  "Value equality across the fixture set, including the two cases where
  plain `=` isn't the right comparison: arrays (compared element-wise,
  since Java array equality is by reference) and java.util.regex.Pattern
  (compared by source, since Pattern has no value equality at all)."
  [a b]
  (cond
    (and (arr? a) (arr? b))
    (and (= (alength a) (alength b))
         (every? true? (map deep-eq? (seq a) (seq b))))

    (and (instance? java.util.regex.Pattern a) (instance? java.util.regex.Pattern b))
    (= (.pattern ^java.util.regex.Pattern a) (.pattern ^java.util.regex.Pattern b))

    :else (= a b)))
