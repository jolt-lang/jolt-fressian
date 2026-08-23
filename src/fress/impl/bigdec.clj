(ns fress.impl.bigdec
  "A minimal, dependency-free arbitrary-precision decimal, mirroring
  java.math.BigDecimal: an unscaled bigint value and an integer `scale`,
  where the numeric value = unscaled x 10^(-scale).

  This is the canonical value produced by the Fressian BIGDEC (0xC7) reader
  and consumed by the BIGDEC writer — the exact counterpart of JVM
  Fressian's default BigDecimal support.

  jolt has a real, working bigdec/BigDecimal (arbitrary-precision, printed
  with the `M` suffix like real Clojure) — but its shim doesn't expose
  `.unscaledValue`/`.scale`, or a (BigInteger, scale) constructor, which is
  exactly the decomposition Fressian's wire format needs. So this stays a
  small standalone record — like upstream's own custom deftype was, for the
  same reason (ClojureScript has no BigDecimal at all) — rather than jolt's
  native bigdec. Equality/hash fall out of defrecord's structural equality
  for free: unlike upstream, which has to compare js/BigInt values by their
  string representation because cljs `=`/`==` are unreliable on js/BigInt,
  jolt's bigint already has correct value equality and hashing."
  (:require [fress.impl.bigint :as bn]))

(defn- render
  "Render (unscaled x 10^-scale) as a canonical decimal string."
  [unscaled scale]
  (let [negative? (< unscaled 0)
        digits    (str (bn/abs unscaled))
        body (cond
               (< scale 0) (str digits (apply str (repeat (- scale) \0)))
               (== scale 0) digits
               :else
               (let [digits (if (<= (count digits) scale)
                              ;; pad so there is at least one integer digit
                              (str (apply str (repeat (- (inc scale) (count digits)) \0)) digits)
                              digits)
                     n (count digits)]
                 (str (subs digits 0 (- n scale)) "." (subs digits (- n scale)))))]
    (str (when negative? "-") body)))

(defrecord Bigdec [unscaled scale]
  Object
  (toString [_this] (render unscaled scale)))

(defn bigdec
  "Construct a Bigdec from an unscaled bigint and an integer scale."
  [unscaled scale]
  (->Bigdec (bn/bigint unscaled) scale))

(defn bigdec?
  [x]
  (instance? Bigdec x))

(defn ->unscaled [bd] (.-unscaled ^Bigdec bd))
(defn ->scale    [bd] (.-scale ^Bigdec bd))

(defn ->str [bd] (render (.-unscaled ^Bigdec bd) (.-scale ^Bigdec bd)))
