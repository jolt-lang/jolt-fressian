(ns fress.impl.hopmap
  "Ported from upstream's fress.impl.hopmap — an open-addressing hash table
  (a 'hopscotch'-style scheme: each bucket has a PRIMARY slot pair used only
  when a hash lands there directly, and a SECONDARY/collision slot pair used
  when a different bucket's probe sequence passes through during linear
  probing) used for the reader/writer's priority cache.

  Ported mechanically: `<<` -> bit-shift-left, upstream's atom-based bkt
  mutation inside a loop -> a loop-rebound local (no behavioral difference,
  just avoids an unneeded atom on a purely single-threaded, sequential
  counter), upstream's deftype fields (implicitly mutable in cljs) -> jolt
  deftype fields with explicit ^:unsynchronized-mutable, upstream's
  in-place `keys` array grow-via-.-length assignment (a plain JS array
  feature with no jolt equivalent) -> allocate a new, bigger object-array and
  copy the old contents in.

  _findSlot's collision-probe loop is NOT carried over as upstream wrote it.
  Upstream's loop terminates only when `bkt` wraps back around to 0 — it
  computes the probed slot's hash into `idx` but never actually checks it,
  unlike `_get`/`_intern`'s own probe loops, which do. Reproduced faithfully
  at first, this loses entries for real: inserting 10 keys into a hopmap
  that starts small enough to resize partway through already drops two of
  them (confirmed empirically — keys inserted immediately before a resize
  they themselves trigger come back not-found afterward), because
  mid-resize re-insertion can walk onto an already-occupied bucket and
  _findSlot hands out that slot anyway instead of continuing to probe,
  silently overwriting the entry already there. _findSlot here instead
  probes exactly like _get/_intern do — advance bkt until the probed
  bucket's own hash is actually zero — which is what 'find an empty slot'
  has to mean for this to work at all.")

(defprotocol IHopMap
  (clear [this])
  (oldIndex [this k])
  (isEmpty [this])
  (intern [this k])
  (resize [this])
  (findSlot [this hash]))

(defn _hash [k]
  (let [h (hash k)]
    (if (zero? h) ; reserve 0 for no-entry
      42
      h)))

(defn _get
  "@param k, non-null
   @return the integer associated with k, or -1 if not present"
  [this k]
  (let [hopidx (.-hopidx this)
        keys (.-keys this)
        hash (_hash k)
        mask (dec (.-cap this))
        bkt0 (bit-and hash mask)
        bhash0 (aget hopidx (bit-shift-left bkt0 2))]
    (if (zero? bhash0)
      -1
      (let [key-index0 (aget hopidx (inc (bit-shift-left bkt0 2)))
            bkey0 (aget keys key-index0)]
        (if (and (= hash bhash0) (= k bkey0))
          key-index0
          (loop [bkt (bit-and (inc bkt0) mask)]
            (let [slot (+ (bit-shift-left bkt 2) 2)
                  bhash (aget hopidx slot)]
              (if (zero? bhash)
                -1
                (let [key-index (aget hopidx (inc slot))
                      bkey (aget keys key-index)]
                  (if (and (= hash bhash) (= bkey k))
                    key-index
                    (recur (bit-and (inc bkt) mask))))))))))))

(defn _clear [this]
  (set! (.-count this) 0)
  (dotimes [i (.-cap this)]
    (aset (.-keys this) i nil))
  (let [cap2 (bit-shift-left (.-cap this) 2)]
    (dotimes [i cap2]
      (aset (.-hopidx this) i 0))))

(defn- intern-at! [this slot hash k]
  (let [i (.-count this)]
    (aset (.-hopidx this) slot hash)
    (aset (.-hopidx this) (inc slot) i)
    (aset (.-keys this) i k)
    (set! (.-count this) (inc (.-count this)))
    (when (== (.-count this) (.-cap this))
      (resize this))
    i))

(defn _oldIndex
  "Puts k in the map if it was not already present.
   Returns -1 if k was freshly added
   Returns k's index if k was already in the map.
   @param k, non-null
   @return the integer associated with k or -1"
  [this k]
  (let [countBefore (.-count this)
        index (intern this k)]
    (if (== countBefore (.-count this))
      index ; already present
      -1)))

(defn _intern
  "Puts k in the map (if not present) and assigns and returns the index associated with it
   assigns ints monotonically from 0
   @param k, non-null
   @return the integer associated with k"
  [this k]
  (let [hopidx (.-hopidx this)
        keys (.-keys this)
        hash (_hash k)
        mask (dec (.-cap this))
        bkt0 (bit-and hash mask)
        bhash0 (aget hopidx (bit-shift-left bkt0 2))]
    (if (zero? bhash0)
      (intern-at! this (bit-shift-left bkt0 2) hash k)
      (let [key-index0 (aget hopidx (inc (bit-shift-left bkt0 2)))]
        (if (and (== hash bhash0) (= k (aget keys key-index0)))
          key-index0
          (loop [bkt (bit-and (inc bkt0) mask)]
            (let [slot (+ (bit-shift-left bkt 2) 2)
                  bhash (aget hopidx slot)]
              (cond
                (zero? bhash) (intern-at! this slot hash k)
                (== hash bhash)
                (let [key-index (aget hopidx (inc slot))]
                  (if (= k (aget keys key-index))
                    key-index
                    (recur (bit-and (inc bkt) mask))))
                :else (recur (bit-and (inc bkt) mask))))))))))

(defn _findSlot
  [this hash]
  (let [hopidx (.-hopidx this)
        mask (dec (.-cap this))
        bkt0 (bit-and hash mask)
        bhash0 (aget hopidx (bit-shift-left bkt0 2))]
    (if (zero? bhash0)
      (bit-shift-left bkt0 2)
      (loop [bkt (bit-and (inc bkt0) mask)]
        (let [slot (+ (bit-shift-left bkt 2) 2)
              bhash (aget hopidx slot)]
          (if (zero? bhash)
            slot
            (recur (bit-and (inc bkt) mask))))))))

(defn _resize
  [this]
  (let [oldhops (.-hopidx this)
        old-keys (.-keys this)
        new-cap (bit-shift-left (.-cap this) 1)
        new-keys (object-array new-cap)]
    (System/arraycopy old-keys 0 new-keys 0 (alength old-keys))
    (set! (.-hopidx this) (int-array (* 2 (alength oldhops))))
    (set! (.-cap this) new-cap)
    (set! (.-keys this) new-keys)
    (loop [slot 0]
      (when (< slot (alength oldhops))
        (let [item (aget oldhops slot)
              new-slot (findSlot this item)]
          (aset (.-hopidx this) new-slot item)
          (aset (.-hopidx this) (inc new-slot) (aget oldhops (inc slot)))
          (recur (+ 2 slot)))))))

(deftype InterleavedIndexHopMap
  [^:unsynchronized-mutable ^long cap
   ^:unsynchronized-mutable hopidx
   ^:unsynchronized-mutable keys
   ^:unsynchronized-mutable ^long count]
  clojure.lang.ILookup
  (valAt [this k] (_get this k))
  (valAt [this k not-found] (let [v (_get this k)] (if (== v -1) not-found v)))
  IHopMap
  (isEmpty [_this] (zero? count))
  (clear [this] (_clear this))
  (oldIndex [this k] (_oldIndex this k))
  (intern [this k] (_intern this k))
  (resize [this] (_resize this))
  (findSlot [this h] (_findSlot this h)))

(defn hopmap
  ([] (hopmap 1024))
  ([capacity]
   (let [cap (loop [c 1] (if (< c capacity) (recur (bit-shift-left c 1)) c))
         hopidx (int-array (bit-shift-left cap 2)) ;; [hash, idx of key, collision hash, collision idx, ...]
         keys (object-array cap)]
     (InterleavedIndexHopMap. cap hopidx keys 0))))
