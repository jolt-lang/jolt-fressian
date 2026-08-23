(ns fress.impl.adler32
  "Portable Adler-32 (RFC 1950), ported byte-for-byte from upstream's
  fress.impl.adler32. Fressian's own footer/checksum framing needs this; jolt
  has no java.util.zip.Adler32 shim, but none is needed since this algorithm
  is entirely self-contained arithmetic over a byte array.

  Upstream implements this as a cljs defrecord mutated via set! on its field —
  valid in ClojureScript, where defrecord fields compile to plain JS
  properties, but not real Clojure: only deftype fields explicitly marked
  mutable support set!. Ported here as a deftype accordingly.")

(defprotocol Adler32Protocol
  (update! [this byte] [this bytes offset length])
  (reset [this]))

(def ^:const adler32-base 65521)

(deftype Adler32 [^:unsynchronized-mutable value]
  clojure.lang.IDeref
  (deref [_this] (bit-and value 0xffffffff))
  Adler32Protocol
  (update! [this byte]
    (let [s1 (+ (bit-and value 0xffff) (bit-and byte 0xff))
          s2 (+ (bit-and (bit-shift-right value 16) 0xffff) s1)]
      (set! (.-value this)
        (bit-or (bit-shift-left (mod s2 adler32-base) 16)
                (mod s1 adler32-base)))))
  (update! [this bs off len]
    (doseq [i (range off (+ off len))]
      (update! this (aget bs i))))
  (reset [this] (set! (.-value this) 1)))

(defn adler32 [] (Adler32. 1))
