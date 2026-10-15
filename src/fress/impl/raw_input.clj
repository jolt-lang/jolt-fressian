(ns fress.impl.raw-input
  "Ported from upstream's fress.impl.raw-input.

  Upstream reconstructs 32/40/48/64-bit values via goog.math.Long because a JS
  `number` can't exactly hold anything past 2^53 — jolt (Chez-backed) has
  native arbitrary-precision integers, so this port just accumulates the bytes
  directly with no Long-emulation layer at all: read-unsigned-msb builds the
  N-byte value the same way any width does.

  readRawInt64 needs no separate sign-correction step, despite upstream
  getting a signed result there 'for free' from goog.math.Long always being a
  signed 64-bit container (readRawInt32/40/48 stay unsigned, matching
  upstream's explicit `.and L_U32_MAX_VALUE` masking): jolt's bit-shift-left,
  like real Clojure's, does not auto-promote a `long` on overflow —
  `(bit-shift-left 72057594037927935 8)` wraps to -256, matching raw 64-bit
  two's-complement arithmetic bit-for-bit — so accumulating all 8 bytes via
  plain bit-shift-left/+ already lands on the correctly-signed result the
  moment the top bit is shifted in. (A separate 'subtract 2^64 if the top bit
  is set' step would in fact be wrong here: `(bit-shift-left 1 63)` itself
  wraps to Long/MIN_VALUE, not +2^63, for the same reason — bitwise ops are
  fixed-width `long` ops in real Clojure, not bignum ops, and jolt matches
  that exactly, including the shift-amount-mod-64 JVM quirk.)

  Float/double decode bit-puns through jolt.ffi the same way fress.impl.raw-output
  bit-puns on the write side — see that namespace's docstring."
  (:require [fress.impl.adler32 :as adler]
            [fress.impl.buffer :as buf]
            [jolt.ffi :as ffi]))

(defprotocol IRawInput
  (readRawByte [this])
  (readRawInt8 [this])
  (readRawInt16 [this])
  (readRawInt24 [this])
  (readRawInt32 [this])
  (readRawInt40 [this])
  (readRawInt48 [this])
  (readRawInt64 [this])
  (readRawFloat [this])
  (readRawDouble [this])
  (readFully [this length] "signed byte-array copy")
  (getBytesRead [this])
  (reset [this])
  (close [this] "throw EOF on any further reads, even if room")
  (validateChecksum [this]))

(defn- bits->double
  ^double [bits]
  (let [p (ffi/alloc 8)]
    (try
      (ffi/write p :int64 bits 0)
      (ffi/read p :double 0)
      (finally (ffi/free p)))))

(defn- bits->float
  ^double [bits]
  (let [p (ffi/alloc 4)]
    (try
      (ffi/write p :int32 bits 0)
      (ffi/read p :float 0)
      (finally (ffi/free p)))))

(defn- read-unsigned-msb
  "Read nbytes, most-significant byte first, accumulating via plain
  bit-shift-left/+. For nbytes <= 6 this is the correct unsigned value
  (upstream's own contract for readRawInt32/40/48); for nbytes = 8 the natural
  64-bit long wraparound lands on the correctly-SIGNED value instead — see the
  namespace docstring."
  [rawin nbytes]
  (loop [i 0 acc 0]
    (if (== i nbytes)
      acc
      (recur (inc i) (+ (bit-shift-left acc 8) (readRawByte rawin))))))

(defrecord RawInput [in checksum]
  IRawInput
  (getBytesRead [_this] (buf/getBytesRead in))

  (readRawByte [_this]
    (let [b (buf/readUnsignedByte in)]
      (when checksum (adler/update! checksum b))
      b))

  (readFully [_this length]
    (let [bytes (buf/readSignedBytes in length)]
      (when checksum (adler/update! checksum bytes 0 length))
      bytes))

  (reset [_this]
    (buf/reset in)
    (when checksum (adler/reset checksum)))

  (close [_this] nil)

  (readRawInt8 [this] (readRawByte this))
  (readRawInt16 [this] (read-unsigned-msb this 2))
  (readRawInt24 [this] (read-unsigned-msb this 3))
  (readRawInt32 [this] (read-unsigned-msb this 4))
  (readRawInt40 [this] (read-unsigned-msb this 5))
  (readRawInt48 [this] (read-unsigned-msb this 6))
  (readRawInt64 [this] (read-unsigned-msb this 8))

  (readRawFloat [this] (bits->float (read-unsigned-msb this 4)))
  (readRawDouble [this] (bits->double (read-unsigned-msb this 8)))

  (validateChecksum [this]
    (if (nil? checksum)
      (readRawInt32 this)
      (let [calculated @checksum
            received (readRawInt32 this)]
        (when (not= calculated received)
          (throw (ex-info (str "Invalid footer checksum, expected " calculated " got " received)
                           {:expected calculated :got received})))))))

(defn raw-input
  ([in] (raw-input in 0))
  ([in start-index] (raw-input in start-index true))
  ([in start-index validate-adler?]
   (let [in (buf/readable-buffer in start-index)]
     (RawInput. in (when validate-adler? (adler/adler32))))))
