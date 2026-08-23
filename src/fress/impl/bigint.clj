(ns fress.impl.bigint
  "Ported from upstream's fress.impl.bigint.

  Upstream's version is almost entirely hex/binary-string gymnastics to get a
  two's-complement fixed-width byte representation out of js/BigInt — needed
  because JS BigInt is a pure arbitrary-precision *magnitude + sign* value
  with no relationship to a byte pattern at all, and because JS `number`
  can't hold a 64-bit value exactly, so upstream also needs a BigInt for
  every packed int that doesn't fit in 53 bits even when it fits in 64
  (see writer.cljs's bn/bit-switch / bn/>> dispatch for 'BigInt that still
  fits in a packed int' — a distinction that doesn't exist here at all: a
  jolt long holds any 64-bit value natively, and only overflow past 64 bits
  promotes to clojure.lang.BigInt, which is exactly the boundary at which
  Fressian's own wire format switches from packed-int to the BIGINT (0xC6)
  tag).

  What's actually needed here is just signed two's-complement byte-array <->
  bigint conversion (Fressian's own encoding for its BIGINT/BIGDEC types),
  which falls out of `mod`'s floor-division convention directly: for a
  nonnegative modulus, `(mod bn modulus)` is already the nonnegative
  two's-complement bit pattern of bn mod that modulus, for ANY sign of bn —
  no bit-complement/flip-string dance required.")

(defn bigint?
  "True for a genuine jolt bignum.

  KNOWN JOLT GAP: unlike real Clojure, where `5N`/`(bigint 5)` always report
  `clojure.lang.BigInt` regardless of magnitude, jolt currently normalizes a
  small bigint down to a plain Long, losing the 'explicitly constructed as a
  bigint' type tag (confirmed against real Clojure 1.12: `(class 5N)` =>
  clojure.lang.BigInt there, java.lang.Long under jolt). So this predicate —
  and therefore the writer's int/BIGINT dispatch — only recognizes bigints
  that overflow 64 bits, not a small value the caller explicitly tagged as
  one. Revisit once jolt preserves that type identity."
  [n]
  (instance? clojure.lang.BigInt n))

(defn bigint
  [n]
  (clojure.core/bigint n))

(defn abs
  [bn]
  (if (< bn 0) (- bn) bn))

(defn- pow256 [k] (reduce *' 1 (repeat k 256)))

(defn bytes->bigint
  "Interpret a signed byte-array (two's-complement, most-significant byte
  first) — Fressian's own on-wire bigint representation — as a bigint."
  [bs]
  (let [n (alength bs)]
    (if (zero? n)
      0
      (let [modulus (pow256 n)
            half (quot modulus 2)
            u (loop [i 0 acc 0]
                (if (== i n)
                  acc
                  (recur (inc i) (+' (*' acc 256) (bit-and (aget bs i) 0xFF)))))]
        (if (>= u half) (-' u modulus) u)))))

(defn- min-twos-complement-len
  "Smallest byte count k >= 1 such that bn fits as a signed k-byte
  two's-complement integer."
  [bn]
  (loop [k 1]
    (let [half (quot (pow256 k) 2)]
      (if (and (<= (- half) bn) (< bn half))
        k
        (recur (inc k))))))

(defn bigint->bytes
  "Minimal-length signed two's-complement byte-array (most-significant byte
  first) for a signed integer — matching java.math.BigInteger/toByteArray's
  contract, which is what the real JVM Fressian writer produces on the wire."
  [bn]
  (let [bn (bigint bn)
        k (min-twos-complement-len bn)
        modulus (pow256 k)
        u (mod bn modulus)
        out (byte-array k)]
    (loop [i (dec k) v u]
      (when (>= i 0)
        (aset out i (mod v 256))
        (recur (dec i) (quot v 256))))
    out))
