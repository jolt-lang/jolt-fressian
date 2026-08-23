(ns fress.writer
  "Ported from upstream's fress.writer.

  Differences from upstream, beyond the mechanical JS-typed-array -> jolt
  byte-array translation already established in fress.impl.*:

  - writeNumber always writes a non-integer number as DOUBLE, full stop —
    neither upstream's approach nor the originally-intended jolt approach
    survived contact with jolt as it exists today. Upstream guesses
    float-vs-double from the VALUE's magnitude (`(<= f32_MIN_VALUE n
    f32_MAX_VALUE)`), which is lossy (a double like 1.1 that happens to
    fall inside float32's range gets written as a FLOAT, losing precision).
    The plan here was to do better using jolt's `float?`, which correctly
    reports true for a value made via `(float x)` — except it turns out
    `float?` (and `double?`) report true for EVERY floating-point number
    under jolt regardless of how it was made, so there is currently no way
    at all, from portable Clojure code, to tell 1.5 and (float 1.5) apart.
    Given that, always-DOUBLE is the only non-lossy choice available:
    writeFloat is still exposed and used correctly for jolt-fressian's own
    internal float-array element writes (writeFloatArray), just never
    reached through generic writeNumber dispatch. Revisit if jolt ever
    grows a real, distinguishable float type.

  - writeBigInt64/writeBigInt64Array (upstream's 'BigInt that still happens
    to fit in a packed int' dispatch branch) don't exist here at all: a jolt
    long already holds any 64-bit value natively and Fressian's own
    packed-int/BIGINT boundary already lines up with jolt's own long/BigInt
    promotion boundary (see fress.impl.bigint's docstring) — plain
    writeLongArray covers every long, no special BigInt handling needed.

  - writeUUID writes the UUID's two 64-bit halves directly
    (getMostSignificantBits/getLeastSignificantBits -> writeRawInt64 x2)
    instead of upstream's lowercase-hex-regex parse — the same 16 bytes,
    without the string round-trip, and matching how the JVM's own default
    UUID write handler does it.

  - The handler table's collection dispatch is seeded with Clojure's
    INTERFACES (IPersistentMap/IPersistentVector/IPersistentSet/ISeq)
    rather than enumerating concrete classes the way upstream has to
    (PersistentHashMap, PersistentArrayMap, ObjMap, ChunkedSeq, ...) — jolt
    has many concrete implementations behind each of those interfaces
    (array-map, hash-map, sorted-map, ...), and matching on the interface
    is both fewer entries and correct for all of them, including ones
    neither upstream nor this file explicitly names.

  - Primitive array types ([B, [I, [F, [D, [J, [Z, and object arrays) are
    keyed in the handler table by their class NAME (a string) rather than
    the Class object itself: jolt's Class objects for array types have a
    real equals/hashCode bug — `(= (Class/forName \"[B\") (class
    (byte-array 3)))` is false even though both print as `[B` and hash
    equal — so a Class-object key registered at table-build time never
    matches the (different) Class object obtained from an actual value
    later. `.getName` strings compare correctly, so array dispatch in
    build-handler-lookup checks `.isArray` first and looks up by name
    string in that case."
  (:require [clojure.string :as string]
            [fress.impl.codes :as codes]
            [fress.impl.ranges :as ranges]
            [fress.impl.raw-output :as rawOut]
            [fress.impl.hopmap :as hop]
            [fress.impl.table :as table]
            [fress.impl.bigint :as bn]
            [fress.impl.bigdec :as bd]
            [fress.util :as util]))

(def ^:dynamic *stringify-keys* false)

(defprotocol IFressianWriter
  (writeNull [this])
  (writeBoolean [this b])
  (writeInt [this i])
  (writeDouble [this d])
  (writeFloat [this f])
  (writeString [this s])
  (writeList [this o])
  (writeBytes [this bs] [this bs offset length])
  (writeFooter [this])
  (clearCaches [this])
  (resetCaches [this])
  (getPriorityCache [this])
  (getStructCache [this])
  (writeTag [this tag componentCount])
  (writeCount [this n])
  (shouldSkipCache- [this o])
  (doWrite- [this tag o handler cache?])
  (writeAs [this tag o] [this tag o cache?])
  (writeObject [this o] [this o cache?])
  (writeCode [this code])
  (beginOpenList [this])
  (beginClosedList [this])
  (endList [this]))

(defn- magnitude [n] (if (neg? n) (- n) n))

(defn- bit-length
  "Bits needed to represent the magnitude of n, matching
  `(.toString (Math/abs n) 2).length` for any sign — including n = 0, whose
  binary string is \"0\" (length 1), not \"\" (length 0)."
  [n]
  (let [m (magnitude n)]
    (if (zero? m)
      1
      (loop [m m c 0]
        (if (zero? m) c (recur (quot m 2) (inc c)))))))

(defn- bit-switch
  "Bits NOT needed to represent n in a 64-bit word — upstream's own name and
  contract, used purely to pick a packed-int width."
  [n]
  (- 64 (bit-length n)))

(defn internalWriteInt [wtr n]
  (let [s (bit-switch n)
        raw (.-raw-out wtr)]
    (cond
      (<= 58 s 64)
      (do
        (when (< n -1)
          (rawOut/writeRawByte raw (+ codes/INT_PACKED_2_ZERO (bit-shift-right n 8))))
        (rawOut/writeRawByte raw n))

      (<= 52 s 57)
      (do
        (rawOut/writeRawByte raw (+ codes/INT_PACKED_2_ZERO (bit-shift-right n 8)))
        (rawOut/writeRawByte raw n))

      (<= 45 s 51)
      (do
        (rawOut/writeRawByte raw (+ codes/INT_PACKED_3_ZERO (bit-shift-right n 16)))
        (rawOut/writeRawInt16 raw n))

      (<= 39 s 44)
      (do
        (rawOut/writeRawByte raw (+ codes/INT_PACKED_4_ZERO (bit-shift-right n 24)))
        (rawOut/writeRawInt24 raw n))

      (<= 31 s 38)
      (do
        (rawOut/writeRawByte raw (+ codes/INT_PACKED_5_ZERO (bit-shift-right n 32)))
        (rawOut/writeRawInt32 raw n))

      (<= 23 s 30)
      (do
        (rawOut/writeRawByte raw (+ codes/INT_PACKED_6_ZERO (bit-shift-right n 40)))
        (rawOut/writeRawInt40 raw n))

      (<= 15 s 22)
      (do
        (rawOut/writeRawByte raw (+ codes/INT_PACKED_7_ZERO (bit-shift-right n 48)))
        (rawOut/writeRawInt48 raw n))

      :else
      (do
        (writeCode wtr codes/INT)
        (rawOut/writeRawInt64 raw n)))))

(defn internalWriteFooter [wrt length]
  (let [raw-out (.-raw-out wrt)]
    (rawOut/writeRawInt32 raw-out codes/FOOTER_MAGIC)
    (rawOut/writeRawInt32 raw-out length)
    (rawOut/writeRawInt32 raw-out (rawOut/getChecksum raw-out))))

(defn defaultWriteString
  "Encode the whole string to UTF-8 up front (jolt's byte-array already does
  this — no chunked-encode-into-a-scratch-buffer needed the way upstream's
  JS side needs), then frame it in <=65535-byte pieces exactly like
  writeBytes does for raw bytes — a final piece under 8 bytes packs into the
  tag byte; a final piece at or over that uses STRING+count; any non-final
  piece uses STRING_CHUNK+count."
  [this s]
  (let [bytes (byte-array (str s))
        total (alength bytes)]
    (loop [off 0]
      (let [remaining (- total off)
            chunk-len (min remaining ranges/BYTE_CHUNK_SIZE)
            final? (= (+ off chunk-len) total)]
        (cond
          (and final? (< chunk-len ranges/STRING_PACKED_LENGTH_END))
          (writeCode this (+ codes/STRING_PACKED_LENGTH_START chunk-len))

          final?
          (do (writeCode this codes/STRING) (writeCount this chunk-len))

          :else
          (do (writeCode this codes/STRING_CHUNK) (writeInt this chunk-len)))
        (rawOut/writeRawBytes (.-raw-out this) bytes off chunk-len)
        (when-not final?
          (recur (+ off chunk-len)))))))

(deftype FressianWriter [out raw-out ^:unsynchronized-mutable priorityCache
                         ^:unsynchronized-mutable structCache lookup]
  IFressianWriter
  (writeCode [_this code] (rawOut/writeRawByte raw-out code))

  (writeCount [this n] (writeInt this n))

  (writeNull [this] (writeCode this codes/NULL))

  (writeBoolean [this b]
    (if (nil? b)
      (writeNull this)
      (writeCode this (if b codes/TRUE codes/FALSE)))
    this)

  (writeInt [this i]
    (if (nil? i)
      (writeNull this)
      (internalWriteInt this i))
    this)

  (writeFloat [this f]
    (writeCode this codes/FLOAT)
    (rawOut/writeRawFloat raw-out f)
    this)

  (writeDouble [this d]
    (cond
      (== d 0.0) (writeCode this codes/DOUBLE_0)
      (== d 1.0) (writeCode this codes/DOUBLE_1)
      :else (do (writeCode this codes/DOUBLE)
                 (rawOut/writeRawDouble raw-out d)))
    this)

  (writeBytes [this bytes]
    (if (nil? bytes)
      (writeNull this)
      (writeBytes this bytes 0 (alength bytes)))
    this)

  (writeBytes [this bytes offset length]
    (if (< length ranges/BYTES_PACKED_LENGTH_END)
      (do
        (rawOut/writeRawByte raw-out (+ codes/BYTES_PACKED_LENGTH_START length))
        (rawOut/writeRawBytes raw-out bytes offset length))
      (loop [len length
             off offset]
        (if (< ranges/BYTE_CHUNK_SIZE len)
          (do
            (writeCode this codes/BYTES_CHUNK)
            (writeCount this ranges/BYTE_CHUNK_SIZE)
            (rawOut/writeRawBytes raw-out bytes off ranges/BYTE_CHUNK_SIZE)
            (recur (- len ranges/BYTE_CHUNK_SIZE) (+ off ranges/BYTE_CHUNK_SIZE)))
          (do
            (writeCode this codes/BYTES)
            (writeCount this len)
            (rawOut/writeRawBytes raw-out bytes off len)))))
    this)

  (writeString [this s] (defaultWriteString this s))

  (writeObject [this o] (writeAs this nil o))
  (writeObject [this o cache?] (writeAs this nil o cache?))

  (writeAs [this tag o] (writeAs this tag o false))
  (writeAs [this tag o cache?]
    (if-let [handler (lookup tag o)]
      (doWrite- this tag o handler cache?)
      (throw (ex-info (str "no handler for tag :" (pr-str tag) ", type: " (pr-str (type o))) {}))))

  (getPriorityCache [this]
    (or priorityCache (let [c (hop/hopmap 16)] (set! (.-priorityCache this) c) c)))

  (getStructCache [this]
    (or structCache (let [c (hop/hopmap 16)] (set! (.-structCache this) c) c)))

  (clearCaches [_this]
    (when (and priorityCache (not (hop/isEmpty priorityCache)))
      (hop/clear priorityCache))
    (when (and structCache (not (hop/isEmpty structCache)))
      (hop/clear structCache)))

  (resetCaches [this]
    (writeCode this codes/RESET_CACHES)
    (clearCaches this)
    this)

  (shouldSkipCache- [_this o]
    (or (nil? o)
        (boolean? o)
        (and (string? o) (zero? (count o)))
        (and (number? o) (or (== 0.0 o) (== 1.0 o)))))

  (doWrite- [this tag o handler cache?]
    (if (or (not cache?) (shouldSkipCache- this o))
      (handler this o)
      (let [index (hop/oldIndex (getPriorityCache this) o)]
        (if (== index -1)
          (do ;;newly interned, write PUT + object
            (writeCode this codes/PUT_PRIORITY_CACHE)
            (doWrite- this tag o handler false))
          ;;already cached
          (if (< index ranges/PRIORITY_CACHE_PACKED_END)
            (writeCode this (+ codes/PRIORITY_CACHE_PACKED_START index))
            (do ;;past cache packing
              (writeCode this codes/GET_PRIORITY_CACHE)
              (writeInt this index)))))))

  (writeList [this lst]
    (if (nil? lst)
      (writeNull this)
      (let [length (count lst)]
        (if (< length ranges/LIST_PACKED_LENGTH_END)
          (rawOut/writeRawByte raw-out (+ length codes/LIST_PACKED_LENGTH_START))
          (do
            (writeCode this codes/LIST)
            (writeCount this length)))
        (doseq [item lst]
          (writeObject this item))))
    this)

  (beginOpenList [this]
    (if-not (zero? (rawOut/getBytesWritten raw-out))
      (throw (ex-info "openList must be called from the top level, outside any footer context." {}))
      (writeCode this codes/BEGIN_OPEN_LIST))
    this)

  (beginClosedList [this]
    (writeCode this codes/BEGIN_CLOSED_LIST)
    this)

  (endList [this]
    (writeCode this codes/END_COLLECTION)
    this)

  (writeTag [this tag component-count]
    (if-let [shortcut-code (get codes/tag->code tag)]
      (writeCode this shortcut-code)
      (let [index (hop/oldIndex (getStructCache this) tag)]
        (cond
          (== index -1)
          (do
            (writeCode this codes/STRUCTTYPE)
            ;; cannot control how keys are written on JVM so leaving as default
            (defaultWriteString this tag)
            (writeInt this component-count))

          (< index ranges/STRUCT_CACHE_PACKED_END)
          (writeCode this (+ codes/STRUCT_CACHE_PACKED_START index))

          :else
          (do
            (writeCode this codes/STRUCT) ;<= when cache length exceeds packing
            (writeInt this index)))))
    this)

  (writeFooter [this]
    (internalWriteFooter this (rawOut/getBytesWritten raw-out))
    (clearCaches this)
    this))

(defn writeNumber [this n]
  (if (integer? n) (writeInt this n) (writeDouble this n))
  this)

(defn fullname [kw]
  (if-not (qualified-keyword? kw)
    (name kw)
    (str (namespace kw) "/" (name kw))))

(defn writeMap [wrt m]
  (writeTag wrt "map" 1)
  (if-not *stringify-keys*
    (writeList wrt (mapcat identity (seq m)))
    (writeList wrt (mapcat (fn [[k v :as entry]]
                             (if (keyword? k)
                               [(fullname k) v]
                               entry))
                           (seq m)))))

(defn- writeNamed [tag wtr s]
  (writeTag wtr tag 2)
  (writeObject wtr (namespace s) true)
  (writeObject wtr (name s) true))

(defn writeSet [wtr s]
  (writeTag wtr "set" 1)
  (writeList wtr (into [] s)))

(defn writeInst [wtr ^java.util.Date date]
  (writeTag wtr "inst" 1)
  (writeInt wtr (.getTime date)))

(defn writeUri [wtr ^java.net.URI u]
  (writeTag wtr "uri" 1)
  (writeString wtr (.toString u)))

(defn writeRegex [wtr ^java.util.regex.Pattern re]
  (writeTag wtr "regex" 1)
  (writeString wtr (.pattern re)))

(defn writeUUID [wtr ^java.util.UUID u]
  (writeTag wtr "uuid" 1)
  (writeBytes wtr
    (let [ba (byte-array 16)
          msb (.getMostSignificantBits u)
          lsb (.getLeastSignificantBits u)]
      (dotimes [i 8]
        (aset ba i (bit-and (bit-shift-right msb (* 8 (- 7 i))) 0xFF))
        (aset ba (+ i 8) (bit-and (bit-shift-right lsb (* 8 (- 7 i))) 0xFF)))
      ba)))

(defn writeByteArray [wrt bytes]
  (writeBytes wrt bytes))

(defn writeIntArray [wtr a]
  (writeTag wtr "int[]" 2)
  (let [length (alength a)]
    (writeInt wtr length)
    (dotimes [i length] (writeInt wtr (aget a i)))))

(defn writeFloatArray [wtr a]
  (writeTag wtr "float[]" 2)
  (let [length (alength a)]
    (writeInt wtr length)
    (dotimes [i length] (writeFloat wtr (aget a i)))))

(defn writeDoubleArray [wtr a]
  (writeTag wtr "double[]" 2)
  (let [length (alength a)]
    (writeInt wtr length)
    (dotimes [i length] (writeDouble wtr (aget a i)))))

(defn writeBooleanArray [wtr a]
  (writeTag wtr "boolean[]" 2)
  (writeInt wtr (alength a))
  (dotimes [i (alength a)] (writeBoolean wtr (aget a i))))

(defn writeLongArray [wtr a]
  (writeTag wtr "long[]" 2)
  (writeInt wtr (alength a))
  (dotimes [i (alength a)] (writeInt wtr (aget a i))))

(defn writeObjectArray [wtr a]
  (writeTag wtr "Object[]" 2)
  (writeInt wtr (alength a))
  (dotimes [i (alength a)] (writeObject wtr (aget a i))))

(defn class-sym
  "Record types need a string so the name can survive munging. Is converted to
   symbol before serializing."
  [rec rec->tag]
  (let [name (get rec->tag (type rec))]
    (if (string? name)
      (symbol name)
      (throw (ex-info "writing records requires corresponding entry in record->name" {:type (type rec)})))))

(defn writeRecord [w rec rec->tag]
  (writeTag w "record" 2)
  (writeObject w (class-sym rec rec->tag) true)
  (writeTag w "map" 1)
  (beginClosedList w)
  (doseq [[field value] rec]
    (writeObject w field true)
    (writeObject w value))
  (endList w))

(defn write-bigint
  [wrt n]
  (writeTag wrt "bigint" 1)
  (writeBytes wrt (bn/bigint->bytes n)))

(defn write-bigdecimal
  "BIGDEC = unscaled (bigint, as bytes) + integer scale. Mirrors JVM Fressian's
   default BigDecimal write; JVM-readable back to a java.math.BigDecimal."
  [wrt x]
  (writeTag wrt "bigdec" 2)
  (writeBytes wrt (bn/bigint->bytes (bd/->unscaled x)))
  (writeInt wrt (bd/->scale x)))

(defn writeChar
  [wrt ^Character ch]
  (writeTag wrt "char" 1)
  (writeInt wrt (int ch)))

(def default-write-handlers
  (table/from-array
    [Long writeNumber
     clojure.lang.BigInt write-bigint
     Double writeNumber
     String writeString
     Boolean writeBoolean
     Character writeChar
     java.util.Date writeInst
     java.util.regex.Pattern writeRegex
     "[B" writeByteArray
     "[I" writeIntArray
     "[F" writeFloatArray
     "[D" writeDoubleArray
     "[J" writeLongArray
     "[Z" writeBooleanArray
     "[Ljava.lang.Object;" writeObjectArray
     bd/Bigdec write-bigdecimal
     java.net.URI writeUri
     ;; writeNull is 1-arg (writeNull is also called directly, unlike every
     ;; other handler here); the handler table always invokes as (handler
     ;; this o), so it needs a 2-arg adapter. Upstream's cljs handler table
     ;; gets away with wiring writeNull in directly because JS silently
     ;; drops a function call's extra argument; jolt, like the JVM, enforces
     ;; arity strictly.
     nil (fn [wtr _o] (writeNull wtr))
     java.util.UUID writeUUID
     clojure.lang.Keyword #(writeNamed "key" %1 %2)
     clojure.lang.Symbol #(writeNamed "sym" %1 %2)
     clojure.lang.IPersistentMap writeMap
     clojure.lang.IPersistentSet writeSet
     clojure.lang.IPersistentVector writeList
     clojure.lang.ISeq writeList
     clojure.lang.MapEntry writeList
     "boolean[]" writeBooleanArray
     "long[]" writeLongArray
     "Object[]" writeObjectArray
     "char" writeChar]))

(defn build-inheritance-lookup [handlers]
  (let [classes (filter class? (table/entries handlers))]
    (fn [o]
      (loop [classes classes]
        (when (seq classes)
          (let [c (first classes)]
            (if (instance? c o)
              (table/?get handlers c)
              (recur (rest classes)))))))))

(defn- type-key
  "Handler-table lookup key for obj's type — the class itself, except for an
  array type, which is looked up by class NAME (see the array-dispatch note
  in this namespace's docstring)."
  [obj]
  (let [c (class obj)]
    (if (and c (.isArray ^Class c))
      (.getName ^Class c)
      c)))

(defn build-handler-lookup
  [user-handlers rec->tag]
  (let [handlers (if (empty? user-handlers)
                   default-write-handlers
                   (table/add-handlers (table/from-table default-write-handlers) user-handlers))
        inh-lookup (build-inheritance-lookup handlers)]
    (fn [tag obj]
      (if (some? tag)
        (table/?get handlers tag)
        (if (record? obj)
          (or (table/?get handlers (type-key obj))
              (if-let [custom-writer (table/?get handlers "record")]
                (fn [wrt rec] (custom-writer wrt rec rec->tag))
                (fn [wrt rec] (writeRecord wrt rec rec->tag))))
          (or (table/?get handlers (type-key obj))
              (inh-lookup obj)))))))

(defn valid-handler-key?
  "singular or coll of constructors and string tags"
  [k]
  (if (coll? k)
    (every? #(or (class? %) (string? %)) k)
    (or (class? k) (string? k))))

(defn valid-user-handlers?
  [uh]
  (and (map? uh)
       (every? fn? (vals uh))
       (every? valid-handler-key? (keys uh))))

(defn valid-record->name?
  "each key should be a record class"
  [m]
  (and (map? m)
       (every? class? (keys m))
       (every? string? (vals m))))

(defn normalize-handlers
  "Normalize type->tag->writer (a la data.fressian) to flat type->writer"
  [user-handlers]
  (reduce-kv (fn [acc type tag-writer-map]
               (if (map? tag-writer-map)
                 (assoc acc type (val (first tag-writer-map)))
                 (assoc acc type tag-writer-map)))
             {}
             user-handlers))

(defn writer
  "Create a writer that combines userHandlers with the normal type handlers
   built into Fressian."
  [out & {:keys [handlers record->name checksum? offset]}]
  (let [handlers (some-> handlers normalize-handlers)]
    (when handlers (assert (valid-user-handlers? handlers) "invalid write handler shape"))
    (when record->name (assert (valid-record->name? record->name)))
    (let [lookup-fn (build-handler-lookup handlers record->name)
          checksum? (if (some? checksum?) checksum? true)
          raw-out (rawOut/raw-output out {:offset (or offset 0) :checksum? checksum?})]
      (FressianWriter. out raw-out nil nil lookup-fn))))
