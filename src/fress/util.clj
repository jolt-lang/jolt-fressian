(ns fress.util
  "Ported from upstream's fress.util.

  Upstream is mostly TextEncoder/TextDecoder plumbing, five near-identical
  multimethods coercing various inputs into a specific JS typed-array type,
  IEquiv extensions on Int8Array/Uint8Array, and an isBigEndian runtime probe
  — all needed only because JS typed arrays are views over a raw
  ArrayBuffer with no inherent element-type-vs-signedness distinction, and
  because JS has no signed-byte-array-that-auto-UTF8-encodes-a-string
  primitive. jolt's own array constructors already do everything those
  multimethods exist for: `byte-array` already UTF-8-encodes a string
  argument, and `int-array`/`float-array`/`double-array`/`long-array`/
  `boolean-array`/`object-array` already coerce a size or a collection —
  so reader/writer call those directly rather than through a
  fress.util-owned indirection layer. This file keeps only what has no
  ready-made jolt equivalent: debug logging, range constants, the `expected`
  error helper, and the two single-byte sign-conversion helpers.

  This port is also always big-endian by construction (see
  fress.impl.raw-output/raw-input), so upstream's isBigEndian probe has
  nothing to do here at all."
  (:require [fress.impl.bigint :as bn]))

(def ^:dynamic *debug* false)

(defn dbg [& args]
  (when *debug*
    (apply println args)))

(defn valid-pointer? [ptr]
  (and (number? ptr)
       (<= 0 ptr)
       (integer? ptr)))

(def ^:const u8_MAX_VALUE 255)

(def ^:const i16_MIN_VALUE -32767)
(def ^:const i16_MAX_VALUE 32767)
(def ^:const u16_MAX_VALUE 65535)

(def ^:const i32_MIN_VALUE -2147483648)
(def ^:const i32_MAX_VALUE 2147483647)
(def ^:const u32_MAX_VALUE 4294967295)

(def ^:const f32_MIN_VALUE 1.4E-45)
(def ^:const f32_MIN_NORMAL 1.17549435E-38)
(def ^:const f32_MAX_VALUE 3.4028235E38)

;; jolt has native arbitrary-precision integers, so these are just literals —
;; no hex-string parse-and-sign-fix (bn/hex->signed-bigint) needed to get an
;; exact value the way upstream needs for values a JS float64 can't hold.
(def i64_MIN_VALUE -9223372036854775808)
(def i64_MAX_VALUE 9223372036854775807)

(def i128_MIN_VALUE -170141183460469231731687303715884105728)
(def i128_MAX_VALUE 170141183460469231731687303715884105727)

(def u64_MAX_VALUE 18446744073709551615)
(def u128_MAX_VALUE 340282366920938463463374607431768211455)

(def ^:const MAX_SAFE_INTEGER 0x1fffffffffffff)

(defn expected
  ([tag code index]
   (throw (ex-info (str "Expected " tag " with code: " code " prior to index: " index) {})))
  ([tag code index o]
   (throw (ex-info (str "Expected " tag " with code: " code " prior to index: " index
                        ", got " (type o) " " (pr-str o) " instead")
                    {:value o}))))

(defn time->inst [^long time] (java.util.Date. time))

(defn i8->u8
  "Reinterpret a signed byte's bit pattern as its unsigned value, 0..255."
  ^long [i8]
  (bit-and i8 0xFF))

(defn u8->i8
  "Narrow an unsigned byte value, 0..255, to its signed two's-complement
  byte value, -128..127 — the same narrowing clojure.core/byte-array's aset
  already does, exposed here as its own function since callers sometimes
  need it outside an array slot."
  ^long [u8]
  (let [ba (byte-array 1)]
    (aset ba 0 u8)
    (aget ba 0)))

(def bigint? bn/bigint?)

(def bigint bn/bigint)
