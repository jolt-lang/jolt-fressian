(ns fress.impl.buffer
  "Byte-buffer plumbing for the reader/writer, ported from upstream's
  fress.impl.buffer (which is built entirely on JS typed arrays over an
  ArrayBuffer) onto jolt byte-arrays.

  Two things upstream's JS version needs that this port doesn't:
    - A 'BufferReader' with pointer-arithmetic-free zero-copy slices: jolt (like
      the JVM) has no zero-copy view over a byte-array, so readSignedBytes /
      getBytes return a fresh copy — the same tradeoff jolt's own
      java.nio.ByteBuffer/slice already makes, and equally harmless here since
      every caller either decodes the copy into a string/number or hands it
      straight back to the app.
    - BytesOutputStream's growth strategy: upstream grows a plain JS array via
      .push (JS arrays are dynamic). jolt byte-arrays are fixed-size, so growth
      here re-allocates and bulk-copies (System/arraycopy) on overflow, doubling
      capacity — the same algorithm the real org.fressian.impl.BytesOutputStream
      uses on the JVM.")

(defprotocol IBuffer
  (getByte [this index])
  (getBytes [this off length])
  (reset [this]))

(defprotocol IBufferReader
  (getBytesRead [this])
  (notifyBytesRead [this count])
  (readUnsignedByte [this])
  (readSignedByte [this])
  (readUnsignedBytes [this length] "unsigned byte values 0..255, as an int-array copy")
  (readSignedBytes [this length] "signed byte-array copy"))

(defprotocol IBufferWriter
  (getFreeCapacity [this] "remaining free bytes to write")
  (room? [this length])
  (getBytesWritten [this])
  (writeByte [this byte])
  (writeBytes [this bytes] [this bytes offset length])
  (notifyBytesWritten [this count]))

(defprotocol IStreamingWriter
  (toByteArray [this] "byte-array copy of current buffer contents. does not close.")
  (flushTo [this out] [this out offset]
    "write bytes into an externally provided byte-array at the given offset"))

(deftype BufferReader [^bytes backing ^long backing-offset ^:unsynchronized-mutable ^long bytesRead]
  IBuffer
  (reset [this] (set! (.-bytesRead this) 0))
  (getByte [_this index] (aget backing (+ backing-offset index)))
  (getBytes [_this off length]
    (let [start (+ backing-offset off)]
      (java.util.Arrays/copyOfRange backing start (+ start length))))
  IBufferReader
  (getBytesRead [_this] bytesRead)
  (notifyBytesRead [this n] (set! (.-bytesRead this) (+ bytesRead n)))
  (readUnsignedByte [this]
    (let [i (+ backing-offset bytesRead)]
      (when (>= i (alength backing)) (throw (ex-info "EOF" {})))
      (let [b (bit-and (aget backing i) 0xFF)]
        (notifyBytesRead this 1)
        b)))
  (readSignedByte [this]
    (let [i (+ backing-offset bytesRead)]
      (when (>= i (alength backing)) (throw (ex-info "EOF" {})))
      (let [b (aget backing i)]
        (notifyBytesRead this 1)
        b)))
  (readSignedBytes [this length]
    (let [start (+ backing-offset bytesRead)
          out (java.util.Arrays/copyOfRange backing start (+ start length))]
      (notifyBytesRead this length)
      out))
  (readUnsignedBytes [this length]
    (let [start (+ backing-offset bytesRead)
          out (int-array length)]
      (dotimes [i length] (aset out i (bit-and (aget backing (+ start i)) 0xFF)))
      (notifyBytesRead this length)
      out)))

(deftype BytesOutputStream [^:unsynchronized-mutable ^bytes arr ^:unsynchronized-mutable ^long bytesWritten]
  clojure.lang.IDeref
  (deref [this] (toByteArray this))
  IBuffer
  (reset [this] (set! (.-bytesWritten this) 0))
  IStreamingWriter
  (flushTo [this buf] (flushTo this buf 0))
  (flushTo [_this buf ptr] (System/arraycopy arr 0 buf ptr bytesWritten))
  (toByteArray [_this]
    (if (== bytesWritten (alength arr))
      arr
      (java.util.Arrays/copyOf arr bytesWritten)))
  IBufferWriter
  (room? [_this _length] true)
  (getBytesWritten [_this] bytesWritten)
  (notifyBytesWritten [this n] (set! (.-bytesWritten this) (+ bytesWritten n)))
  (writeByte [this byte]
    (when (== bytesWritten (alength arr))
      (let [bigger (byte-array (max 8 (* 2 (alength arr))))]
        (System/arraycopy arr 0 bigger 0 bytesWritten)
        (set! (.-arr this) bigger)))
    (aset arr bytesWritten byte)
    (notifyBytesWritten this 1))
  (writeBytes [this bytes] (writeBytes this bytes 0 (alength bytes)))
  (writeBytes [this bytes offset length]
    (let [needed (+ bytesWritten length)]
      (when (> needed (alength arr))
        (let [bigger (byte-array (max needed (* 2 (alength arr))))]
          (System/arraycopy arr 0 bigger 0 bytesWritten)
          (set! (.-arr this) bigger)))
      (System/arraycopy bytes offset arr bytesWritten length)
      (notifyBytesWritten this length))))

(defn byte-stream [] (BytesOutputStream. (byte-array 32) 0))

(defn with-capacity [n] (BytesOutputStream. (byte-array (max 1 (int n))) 0))

(deftype BufferWriter [^bytes backing ^long backing-offset ^:unsynchronized-mutable ^long bytesWritten]
  IBuffer
  (reset [this] (set! (.-bytesWritten this) 0))
  (getByte [_this index] (aget backing (+ backing-offset index)))
  (getBytes [_this offset length]
    (let [start (+ backing-offset offset)]
      (java.util.Arrays/copyOfRange backing start (+ start length))))
  IBufferWriter
  (getFreeCapacity [_this] (- (alength backing) backing-offset bytesWritten))
  (room? [this length] (<= length (getFreeCapacity this)))
  (getBytesWritten [_this] bytesWritten)
  (notifyBytesWritten [this n] (set! (.-bytesWritten this) (+ bytesWritten n)))
  (writeByte [this byte]
    (if (room? this 1)
      (do (aset backing (+ backing-offset bytesWritten) byte)
          (notifyBytesWritten this 1)
          this)
      (throw (ex-info "BufferWriter out of room" {}))))
  (writeBytes [this bytes] (writeBytes this bytes 0 (alength bytes)))
  (writeBytes [this bytes offset length]
    (if (room? this length)
      (do (System/arraycopy bytes offset backing (+ backing-offset bytesWritten) length)
          (notifyBytesWritten this length)
          this)
      (throw (ex-info "BufferWriter out of room" {})))))

(defn readable-buffer
  "Build a BufferReader over a collection of bytes."
  ([backing] (readable-buffer backing 0))
  ([backing backing-offset]
   (cond
     (bytes? backing)
     (BufferReader. backing (long (or backing-offset 0)) 0)

     (satisfies? IBufferReader backing)
     backing

     (vector? backing)
     (readable-buffer (byte-array backing) backing-offset)

     (instance? BytesOutputStream backing)
     (readable-buffer (toByteArray backing) backing-offset)

     (instance? BufferWriter backing)
     (readable-buffer (.-backing ^BufferWriter backing) backing-offset)

     :else
     (throw
       (ex-info
         (str "invalid input type " (type backing) " passed to readable-buffer.\n"
              "Input must be a byte-array, vector, or IBufferWriter instance")
         {:backing backing})))))

(defn writable-buffer
  "Build a BufferWriter over a byte-array. If nil, returns a BytesOutputStream."
  ([] (writable-buffer nil nil))
  ([backing] (writable-buffer backing 0))
  ([backing backing-offset]
   (cond
     (nil? backing)
     (byte-stream)

     (satisfies? IBufferWriter backing)
     backing

     (bytes? backing)
     (BufferWriter. backing (long (or backing-offset 0)) 0)

     :else
     (throw
       (ex-info
         (str "invalid input type " (type backing) " passed to writable-buffer.\n"
              "Input must be a byte-array or nil")
         {:backing backing})))))
