(ns fress.impl.raw-output
  "Ported from upstream's fress.impl.raw-output.

  Upstream's `>>>` macro is NOT a real unsigned shift — it's
  `Math.floor(a / 2^b)`, which upstream uses because it is exactly equal to a
  true arithmetic (sign-extending) right shift for ANY integer, positive or
  negative (floor-division by a power of two rounds toward -infinity, which is
  what two's-complement arithmetic shift does bit-for-bit) — unlike JS's real
  `>>>`/`<<` operators, which only behave correctly within 32 bits. That
  identity means it translates directly to plain `bit-shift-right`, which jolt
  (backed by Chez's native arbitrary-precision integers, not float64) computes
  exactly, at any width. `<<` is a straight `bit-shift-left`.

  Upstream reinterprets a float32/float64's bits by aliasing a typed array's
  backing ArrayBuffer through Int8Array/Float32Array/Float64Array views —
  jolt's byte-array has no such aliasing, so this port bit-puns through
  jolt.ffi (write as the float type, read back the same address as the
  matching-width int type) instead. Since every multi-byte value here is
  written/read one byte at a time in explicit big-endian order (see
  write-msb-bytes!), there is no separate little/big-endian branch to carry
  over — this port is big-endian by construction, with no isBigEndian check
  needed at all."
  (:require [fress.impl.adler32 :as adler]
            [fress.impl.buffer :as buf]
            [jolt.ffi :as ffi]))

(defprotocol IRawOutput
  (getByte [this index])
  (getBytesWritten [this])
  (writeRawByte [this b])
  (writeRawBytes [this bytes] [this bs off len])
  (writeRawInt16 [this i])
  (writeRawInt24 [this i])
  (writeRawInt32 [this i])
  (writeRawInt40 [this i])
  (writeRawInt48 [this i])
  (writeRawInt64 [this i])
  (writeRawFloat [this f])
  (writeRawDouble [this d])
  (getChecksum [this])
  (reset [this]))

(defn- double->bits
  "IEEE-754 bit pattern of a double, as a signed 64-bit long — bit-punned via
  jolt.ffi (write the double, read the same address back as :int64) rather
  than any new jolt-core primitive."
  ^long [d]
  (let [p (ffi/alloc 8)]
    (try
      (ffi/write p :double d 0)
      (ffi/read p :int64 0)
      (finally (ffi/free p)))))

(defn- float->bits
  "IEEE-754 bit pattern of a float, as a signed 32-bit int."
  ^long [f]
  (let [p (ffi/alloc 4)]
    (try
      (ffi/write p :float (float f) 0)
      (ffi/read p :int32 0)
      (finally (ffi/free p)))))

(defn- write-msb-bytes!
  "Write the low nbytes of v, most-significant byte first."
  [rawout v nbytes]
  (loop [shift (* 8 (dec nbytes))]
    (when (>= shift 0)
      (writeRawByte rawout (bit-and (bit-shift-right v shift) 0xFF))
      (recur (- shift 8)))))

(deftype RawOutput [out checksum]
  IRawOutput
  (getChecksum [_this] (if (nil? checksum) 0 @checksum))

  (reset [_this]
    (buf/reset out)
    (when checksum (adler/reset checksum)))

  (getByte [_this index] (buf/getByte out index))

  (getBytesWritten [_this] (buf/getBytesWritten out))

  (writeRawByte [_this b]
    (buf/writeByte out b)
    (when checksum (adler/update! checksum b)))

  (writeRawBytes [this bytes] (writeRawBytes this bytes 0 (alength bytes)))

  (writeRawBytes [_this bytes offset length]
    (buf/writeBytes out bytes offset length)
    (when checksum (adler/update! checksum bytes offset length)))

  (writeRawInt16 [this i] (write-msb-bytes! this i 2))
  (writeRawInt24 [this i] (write-msb-bytes! this i 3))
  (writeRawInt32 [this i] (write-msb-bytes! this i 4))
  (writeRawInt40 [this i] (write-msb-bytes! this i 5))
  (writeRawInt48 [this i] (write-msb-bytes! this i 6))
  (writeRawInt64 [this i] (write-msb-bytes! this i 8))

  (writeRawFloat [this f] (write-msb-bytes! this (float->bits f) 4))
  (writeRawDouble [this d] (write-msb-bytes! this (double->bits d) 8)))

(defn raw-output
  ([] (raw-output nil))
  ([out] (raw-output out {}))
  ([out {:keys [offset checksum?]}]
   (let [out (buf/writable-buffer out offset)
         checksum (when checksum? (adler/adler32))]
     (RawOutput. out checksum))))
