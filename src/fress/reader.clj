(ns fress.reader
  "Ported from upstream's fress.reader.

  Differences from upstream, beyond the mechanical JS-isms -> jolt
  translation already established elsewhere in this port:

  - internalReadString/readUTF8 use jolt's real `(String. bytes \"UTF-8\")`
    instead of upstream's hand-rolled byte-by-byte UTF-8 decoder (needed
    upstream only because it wanted to avoid depending on TextDecoder
    being present in every JS host).

  - internalReadInt's 32/40/48-bit packed cases need no goog.math.Long at
    all — same reasoning as fress.impl.raw-input: jolt's bit-shift-left/
    bit-or on a `long` wrap exactly like raw 64-bit two's-complement
    arithmetic, which is exactly what combining a few high 'packing' bits
    with a lower N-bit unsigned value needs.

  - The priority/struct caches (which need TRUE mutation, not a persistent
    data structure — readAndCacheObject- reserves a slot with a sentinel
    value *before* recursively reading the object, specifically so a
    circular reference back to that same slot resolves correctly) use a
    real java.util.ArrayList directly, exactly like upstream's own
    ArrayList usage, rather than reinventing a growable vector.

  - FressianReader is a deftype with explicit ^:unsynchronized-mutable
    fields, not a defrecord: upstream's defrecord + set! on priorityCache/
    structCache only works because cljs records compile to plain mutable
    JS objects; real Clojure (and jolt) needs deftype + an explicit mutable
    annotation for that (same fix as fress.impl.adler32).

  - readUUID/readIntArray/readLongArray/etc. read straight into
    java.util.UUID/int-array/long-array/float-array/double-array/
    object-array — no BigInt64Array-vs-safe-integer dance for longs (a jolt
    long already holds any 64-bit value exactly) and no hex-string UUID
    reconstruction (java.util.UUID has a (long, long) constructor that
    takes the msb/lsb straight, matching how the JVM's own default UUID
    read handler does it, and how fress.writer's writeUUID now writes them).

  - The WASM-only STR (0xF2) code (codes.clj: 'WASM read-only') is dropped:
    jolt-fressian's own writer never emits it, and there is no WASM memory
    concept here for it to read from."
  (:require [fress.impl.raw-input :as rawIn]
            [fress.impl.codes :as codes]
            [fress.impl.ranges :as ranges]
            [fress.impl.table :as table]
            [fress.impl.bigint :as bn]
            [fress.impl.bigdec :as bd]
            [fress.util :as util]))

(def ^:dynamic *keywordize-keys* false) ;; this can be lossy!

(defrecord StructType [tag fields])
(defrecord TaggedObject [tag value])

(defprotocol IFressianReader
  (read- [this code])
  (readNextCode [this])
  (readBoolean [this])
  (readInt [this])
  (readDouble [this])
  (readFloat [this])
  (readInt32 [this])
  (readObject [this])
  (readCount- [this])
  (readObjects- [this length])
  (readClosedList [this])
  (readOpenList [this])
  (readAndCacheObject- [this cache])
  (lookupCache [this cache index])
  (validateFooter [this] [this calculatedLength magicFromStream])
  (handleStruct- [this tag fields])
  (getHandler- [this tag])
  (getPriorityCache- [this])
  (getStructCache- [this])
  (resetCaches [this]))

(defn internalReadString [rdr length]
  (String. ^bytes (rawIn/readFully (.-raw-in rdr) length) "UTF-8"))

(defn readUTF8
  [rdr]
  (let [length (readCount- rdr)]
    (String. ^bytes (rawIn/readFully (.-raw-in rdr) length) "UTF-8")))

(defn internalReadDouble [rdr code]
  (cond
    (== code codes/DOUBLE) (rawIn/readRawDouble (.-raw-in rdr))
    (== code codes/DOUBLE_0) 0.0
    (== code codes/DOUBLE_1) 1.0
    :else
    (let [o (read- rdr code)]
      (if (number? o)
        o
        (util/expected "double" code (rawIn/getBytesRead (.-raw-in rdr)) o)))))

(defn internalReadInt
  [rdr code]
  (cond
    (== code 0xFF) -1

    (<= 0x00 code 0x3F)
    (bit-and code 0xFF)

    (<= 0x40 code 0x5F)
    (bit-or (bit-shift-left (- code codes/INT_PACKED_2_ZERO) 8) (rawIn/readRawInt8 (.-raw-in rdr)))

    (<= 0x60 code 0x6F)
    (bit-or (bit-shift-left (- code codes/INT_PACKED_3_ZERO) 16) (rawIn/readRawInt16 (.-raw-in rdr)))

    (<= 0x70 code 0x73)
    (bit-or (bit-shift-left (- code codes/INT_PACKED_4_ZERO) 24) (rawIn/readRawInt24 (.-raw-in rdr)))

    (<= 0x74 code 0x77)
    (bit-or (bit-shift-left (- code codes/INT_PACKED_5_ZERO) 32) (rawIn/readRawInt32 (.-raw-in rdr)))

    (<= 0x78 code 0x7B)
    (bit-or (bit-shift-left (- code codes/INT_PACKED_6_ZERO) 40) (rawIn/readRawInt40 (.-raw-in rdr)))

    (<= 0x7C code 0x7F)
    (bit-or (bit-shift-left (- code codes/INT_PACKED_7_ZERO) 48) (rawIn/readRawInt48 (.-raw-in rdr)))

    (== code codes/INT)
    (rawIn/readRawInt64 (.-raw-in rdr))

    :else
    (let [o (read- rdr code)]
      (if (number? o) o
        (util/expected "i64" code (rawIn/getBytesRead (.-raw-in rdr)) o)))))

(defn internalReadList [rdr length]
  (vec (readObjects- rdr length)))

(defn internalReadBytes
  "called on codes/BYTES"
  [rdr length]
  (rawIn/readFully (.-raw-in rdr) length))

(defn internalReadChunkedBytes
  "called on codes/BYTES_CHUNK"
  [rdr]
  (loop [chunks [] code codes/BYTES_CHUNK]
    (if (== code codes/BYTES_CHUNK)
      (let [cnt (readCount- rdr)]
        (recur (conj chunks (internalReadBytes rdr cnt)) (readNextCode rdr)))
      (do
        (when-not (== code codes/BYTES)
          (throw (ex-info (str "conclusion of chunked bytes " code) {})))
        (let [chunks (conj chunks (internalReadBytes rdr (readCount- rdr)))
              total (reduce + (map alength chunks))
              result (byte-array total)]
          (loop [cs chunks pos 0]
            (when (seq cs)
              (let [c (first cs)]
                (System/arraycopy c 0 result pos (alength c))
                (recur (rest cs) (+ pos (alength c))))))
          result)))))

(defn internalReadChunkedString [rdr length]
  (let [sb (StringBuilder. ^String (internalReadString rdr length))]
    (loop []
      (let [code (readNextCode rdr)]
        (cond
          (<= codes/STRING_PACKED_LENGTH_START code (+ codes/STRING_PACKED_LENGTH_START 7))
          (.append sb ^String (internalReadString rdr (- code codes/STRING_PACKED_LENGTH_START)))

          (== code codes/STRING)
          (.append sb ^String (internalReadString rdr (readCount- rdr)))

          (== code codes/STRING_CHUNK)
          (do (.append sb ^String (internalReadString rdr (readCount- rdr)))
              (recur))

          :else
          (util/expected "chunked string" code (rawIn/getBytesRead (.-raw-in rdr))))))
    (.toString sb)))

(defn internalRead [rdr code]
  (cond
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; primitives

    (== code codes/UTF8)
    (readUTF8 rdr)

    (== code codes/ERROR)
    (readObject rdr)

    (== code codes/TRUE)
    true

    (== code codes/FALSE)
    false

    (== code codes/NULL)
    nil

    (or (== code 0xFF) (<= 0x00 code 0x7F) (== code codes/INT))
    (internalReadInt rdr code)

    (or (== code codes/DOUBLE) (== code codes/DOUBLE_0) (== code codes/DOUBLE_1))
    (internalReadDouble rdr code)

    (== code codes/FLOAT)
    (rawIn/readRawFloat (.-raw-in rdr))

    (<= codes/BYTES_PACKED_LENGTH_START code (dec codes/BYTES_PACKED_LENGTH_END))
    (internalReadBytes rdr (- code codes/BYTES_PACKED_LENGTH_START))

    (== code codes/BYTES)
    (internalReadBytes rdr (readCount- rdr))

    (== code codes/BYTES_CHUNK)
    (internalReadChunkedBytes rdr)

    (<= codes/STRING_PACKED_LENGTH_START code (dec codes/STRING_PACKED_LENGTH_END))
    (internalReadString rdr (- code codes/STRING_PACKED_LENGTH_START))

    (== code codes/STRING)
    (internalReadString rdr (readCount- rdr))

    (== code codes/STRING_CHUNK)
    (internalReadChunkedString rdr (readCount- rdr))

    (<= codes/LIST_PACKED_LENGTH_START code (dec codes/LIST_PACKED_LENGTH_END))
    (internalReadList rdr (- code codes/LIST_PACKED_LENGTH_START))

    (== code codes/LIST)
    (internalReadList rdr (readCount- rdr))

    (== code codes/BEGIN_CLOSED_LIST)
    (let [handler (getHandler- rdr "list")]
      (handler (readClosedList rdr)))

    (== code codes/BEGIN_OPEN_LIST)
    (let [handler (getHandler- rdr "list")]
      (handler (readOpenList rdr)))

    (== code codes/FOOTER)
    (let [calculated-length (dec (rawIn/getBytesRead (.-raw-in rdr)))
          magic (+ (bit-shift-left codes/FOOTER 24) (rawIn/readRawInt24 (.-raw-in rdr)))]
      (validateFooter rdr calculated-length magic)
      (readObject rdr))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; extended types

    (== code codes/MAP) (handleStruct- rdr "map" 1)
    (== code codes/SET) (handleStruct- rdr "set" 1)
    (== code codes/_UUID) (handleStruct- rdr "uuid" 2)
    (== code codes/REGEX) (handleStruct- rdr "regex" 1)
    (== code codes/URI) (handleStruct- rdr "uri" 1)
    (== code codes/BIGINT) (handleStruct- rdr "bigint" 1)
    (== code codes/BIGDEC) (handleStruct- rdr "bigdec" 2)
    (== code codes/INST) (handleStruct- rdr "inst" 1)
    (== code codes/SYM) (handleStruct- rdr "sym" 2)
    (== code codes/KEY) (handleStruct- rdr "key" 2)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; typed arrays

    (== code codes/INT_ARRAY) (handleStruct- rdr "int[]" 2)
    (== code codes/LONG_ARRAY) (handleStruct- rdr "long[]" 2)
    (== code codes/FLOAT_ARRAY) (handleStruct- rdr "float[]" 2)
    (== code codes/DOUBLE_ARRAY) (handleStruct- rdr "double[]" 2)
    (== code codes/BOOLEAN_ARRAY) (handleStruct- rdr "boolean[]" 2)
    (== code codes/OBJECT_ARRAY) (handleStruct- rdr "Object[]" 2)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (== code codes/PUT_PRIORITY_CACHE)
    (readAndCacheObject- rdr (getPriorityCache- rdr))

    (== code codes/GET_PRIORITY_CACHE)
    (lookupCache rdr (getPriorityCache- rdr) (readInt32 rdr))

    (<= codes/PRIORITY_CACHE_PACKED_START code (dec codes/PRIORITY_CACHE_PACKED_END))
    (lookupCache rdr (getPriorityCache- rdr) (- code codes/PRIORITY_CACHE_PACKED_START))

    (<= codes/STRUCT_CACHE_PACKED_START code (dec codes/STRUCT_CACHE_PACKED_END))
    (let [struct-type (lookupCache rdr (getStructCache- rdr) (- code codes/STRUCT_CACHE_PACKED_START))]
      (handleStruct- rdr (.-tag ^StructType struct-type) (.-fields ^StructType struct-type)))

    (== code codes/STRUCTTYPE)
    (let [tag (readObject rdr)
          n-fields (readInt32 rdr)]
      (.add ^java.util.ArrayList (getStructCache- rdr) (StructType. tag n-fields))
      (handleStruct- rdr tag n-fields))

    (== code codes/STRUCT)
    (let [struct-type (lookupCache rdr (getStructCache- rdr) (readInt32 rdr))]
      (handleStruct- rdr (.-tag ^StructType struct-type) (.-fields ^StructType struct-type)))

    (== code codes/RESET_CACHES)
    (do (resetCaches rdr) (readObject rdr))

    :else
    (throw (ex-info (str "unmatched code: " code) {:code code}))))

(def UNDER_CONSTRUCTION (Object.))

(deftype FressianReader [in raw-in lookup
                         ^:unsynchronized-mutable priorityCache
                         ^:unsynchronized-mutable structCache]
  IFressianReader
  (readNextCode [_this] (rawIn/readRawByte raw-in))
  (readInt [this] (internalReadInt this (readNextCode this)))
  (readInt32 [this]
    (let [i (readInt this)]
      (when (or (< i util/i32_MIN_VALUE) (< util/i32_MAX_VALUE i))
        (throw (ex-info (str "value " i " out of range for i32") {})))
      i))
  (readCount- [this] (readInt32 this))
  (read- [this code] (internalRead this code))
  (readObject [this] (read- this (readNextCode this)))
  (readFloat [this]
    (let [code (readNextCode this)]
      (if (== code codes/FLOAT)
        (rawIn/readRawFloat raw-in)
        (let [o (read- this code)]
          (if (number? o) o (util/expected "float" code (rawIn/getBytesRead raw-in) o))))))
  (readDouble [this] (internalReadDouble this (readNextCode this)))
  (readBoolean [this]
    (let [code (readNextCode this)]
      (cond
        (== code codes/TRUE) true
        (== code codes/FALSE) false
        :else
        (let [o (read- this code)]
          (if (boolean? o) o (util/expected "boolean" code (rawIn/getBytesRead raw-in) o))))))

  (getStructCache- [this]
    (or structCache
        (let [c (java.util.ArrayList.)]
          (set! (.-structCache this) c)
          c)))
  (getPriorityCache- [this]
    (or priorityCache
        (let [c (java.util.ArrayList.)]
          (set! (.-priorityCache this) c)
          c)))
  (resetCaches [_this]
    (when priorityCache (.clear ^java.util.ArrayList priorityCache))
    (when structCache (.clear ^java.util.ArrayList structCache)))
  (getHandler- [this tag]
    (let [handler (lookup this tag)]
      (if (nil? handler)
        (throw (ex-info (str "no read handler for tag: " (pr-str tag)) {}))
        handler)))
  (handleStruct- [this tag fields]
    (let [handler (lookup this tag)]
      (if (nil? handler)
        (TaggedObject. tag (readObjects- this fields))
        (handler this tag fields))))
  (readObjects- [this length]
    (let [objects (object-array length)]
      (dotimes [i length] (aset objects i (readObject this)))
      objects))
  (readClosedList [this]
    (loop [objects []]
      (let [code (readNextCode this)]
        (if (== code codes/END_COLLECTION)
          objects
          (recur (conj objects (read- this code)))))))
  (readOpenList [this]
    (loop [objects []]
      (let [code (try (readNextCode this) (catch Exception _ codes/END_COLLECTION))]
        (if (== code codes/END_COLLECTION)
          objects
          (recur (conj objects (read- this code)))))))
  (readAndCacheObject- [this cache]
    (let [^java.util.ArrayList cache cache
          index (.size cache)
          _ (.add cache UNDER_CONSTRUCTION)
          o (readObject this)]
      (.set cache index o)
      o))
  (lookupCache [_this cache index]
    (let [^java.util.ArrayList cache cache]
      (if (< index (.size cache))
        (let [o (.get cache index)]
          (if (identical? o UNDER_CONSTRUCTION)
            (throw (ex-info "Unable to resolve circular reference in cache" {}))
            o))
        (throw (ex-info (str "Requested object beyond end of cache at " index) {})))))
  (validateFooter [this]
    (let [calculated-length (rawIn/getBytesRead raw-in)
          magic-from-stream (rawIn/readRawInt32 raw-in)]
      (validateFooter this calculated-length magic-from-stream)))
  (validateFooter [this calculated-length magic-from-stream]
    (if-not (== magic-from-stream codes/FOOTER_MAGIC)
      (throw (ex-info (str "Invalid footer magic, expected " codes/FOOTER_MAGIC " got " magic-from-stream) {}))
      (let [length-from-stream (rawIn/readRawInt32 raw-in)]
        (if-not (== length-from-stream calculated-length)
          (throw (ex-info (str "Invalid footer length, expected " calculated-length " got " length-from-stream) {}))
          (do
            (rawIn/validateChecksum raw-in)
            (resetCaches this)))))))

(defn readSet [rdr _ _]
  (into #{} (readObject rdr)))

(def ^:private array-map-cutoff
  "clojure.data.fressian's map read handler builds a PersistentArrayMap when
  the flat key/value list is shorter than 16 -- fewer than 8 entries -- and a
  PersistentHashMap at or above it. Matched exactly so a map read off the wire
  has the same type, and so the same iteration order, as it does on the JVM."
  16)

(defn- build-map [kvs]
  (if (< (count kvs) array-map-cutoff)
    (apply array-map kvs)
    (apply hash-map kvs)))

(defn readMap [rdr _ _]
  (let [kvs (readObject rdr)]
    (build-map
      (if-not *keywordize-keys*
        kvs
        (map-indexed (fn [i x] (if (and (even? i) (string? x)) (keyword x) x)) kvs)))))

(defn readIntArray [rdr _ _]
  (let [length (readInt rdr)
        arr (int-array length)]
    (dotimes [i length] (aset arr i (readInt rdr)))
    arr))

(defn readLongArray [rdr _ _]
  (let [length (readInt rdr)
        arr (long-array length)]
    (dotimes [i length] (aset arr i (readInt rdr)))
    arr))

(defn readFloatArray [rdr _ _]
  (let [length (readInt rdr)
        arr (float-array length)]
    (dotimes [i length] (aset arr i (readFloat rdr)))
    arr))

(defn readDoubleArray [rdr _ _]
  (let [length (readInt rdr)
        arr (double-array length)]
    (dotimes [i length] (aset arr i (readDouble rdr)))
    arr))

(defn readObjectArray [rdr _ _]
  (let [length (readInt rdr)
        arr (object-array length)]
    (dotimes [i length] (aset arr i (readObject rdr)))
    arr))

(defn readBooleanArray [rdr _ _]
  (let [length (readInt rdr)
        arr (boolean-array length)]
    (dotimes [i length] (aset arr i (readBoolean rdr)))
    arr))

(defn- bytes->long
  "MSB-first signed 64-bit reconstruction from 8 already-read bytes at off —
  same wraparound-is-correct reasoning as fress.impl.raw-input/read-unsigned-msb."
  [^bytes bs off]
  (loop [i 0 acc 0]
    (if (== i 8)
      acc
      (recur (inc i) (+ (bit-shift-left acc 8) (bit-and (aget bs (+ off i)) 0xFF))))))

(defn readUUID [rdr _ _]
  (let [^bytes bytes (readObject rdr)]
    (when-not (== (alength bytes) 16)
      (throw (ex-info (str "invalid UUID buffer size:" (alength bytes)) {})))
    (java.util.UUID. (bytes->long bytes 0) (bytes->long bytes 8))))

(defn readRegex [rdr _ _]
  (re-pattern (readObject rdr)))

(defn readUri [rdr _ _]
  (java.net.URI. (readObject rdr)))

(defn readInst [rdr _ _]
  (java.util.Date. (long (readInt rdr))))

(defn readKeyword [rdr _ _]
  (keyword (readObject rdr) (readObject rdr)))

(defn readSymbol [rdr _ _]
  (symbol (readObject rdr) (readObject rdr)))

(defn readRecord [rdr tag component-count name->map-ctor]
  (let [rname (readObject rdr)
        rmap (readObject rdr)]
    (if-let [rcons (get name->map-ctor (name rname))]
      (rcons rmap)
      (TaggedObject. "record" [rname rmap]))))

(defn readBigInt
  [rdr _ _]
  (bn/bytes->bigint (readObject rdr)))

(defn readBigDecimal
  "BIGDEC (0xC7) carries two fields: the unscaled value as bytes (a bigint) and
   an integer scale. Value = unscaled x 10^-scale. Mirrors JVM Fressian's default
   BigDecimal read; returns a fress.impl.bigdec/Bigdec."
  [rdr _ _]
  (let [unscaled (bn/bytes->bigint (readObject rdr))
        scale (readObject rdr)]
    (bd/->Bigdec unscaled scale)))

(defn readChar
  [rdr _ _]
  (char (readObject rdr)))

(def default-read-handlers
  (table/from-array
    ["list" (fn [objectArray] (vec objectArray))
     "utf8" (fn [rdr _ _] (readUTF8 rdr))
     "set" readSet
     "map" readMap
     "int[]" readIntArray
     "long[]" readLongArray
     "float[]" readFloatArray
     "double[]" readDoubleArray
     "boolean[]" readBooleanArray
     "Object[]" readObjectArray
     "uuid" readUUID
     "regex" readRegex
     "uri" readUri
     "inst" readInst
     "key" readKeyword
     "sym" readSymbol
     "char" readChar
     "bigint" readBigInt
     "bigdec" readBigDecimal]))

(defn build-lookup
  [user-handlers name->map-ctor]
  (let [handlers (if (empty? user-handlers)
                   default-read-handlers
                   (table/add-handlers (table/from-table default-read-handlers) user-handlers))]
    (fn lookup [_rdr tag]
      (if (= "record" tag)
        (or (table/?get handlers "record")
            (fn [rdr tag field-count] (readRecord rdr tag field-count name->map-ctor)))
        (table/?get handlers tag)))))

(defn valid-handler-key?
  [k]
  (if (coll? k)
    (every? string? k)
    (string? k)))

(defn valid-user-handlers? [uh]
  (and (map? uh)
       (every? fn? (vals uh))
       (every? valid-handler-key? (keys uh))))

(defn valid-name->map-ctor? [m]
  (and (map? m)
       (every? string? (keys m))
       (every? fn? (vals m))))

(defn reader
  [in & {:keys [handlers checksum? offset name->map-ctor]
         :or {handlers nil, checksum? false}}]
  (when handlers
    (assert (valid-user-handlers? handlers)))
  (when name->map-ctor
    (assert (valid-name->map-ctor? name->map-ctor)))
  (when offset
    (assert (util/valid-pointer? offset) "fress.reader/reader given invalid pointer as offset"))
  (let [offset (or offset 0)
        lookup (build-lookup handlers name->map-ctor)
        raw-in (rawIn/raw-input in offset checksum?)]
    (FressianReader. in raw-in lookup nil nil)))
