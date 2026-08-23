(ns fress.impl.table
  "Ported from upstream's fress.impl.table — a flat key/value lookup table
  for tag/type -> handler-fn dispatch, kept as a linear scan rather than a
  real hashmap (small, fixed at setup time, and this is exactly what
  upstream's own doc comment says it's for: 'faster than cljs hashmaps and
  drags in much less code').

  Upstream backs this with a plain, in-place-mutated JS array (.indexOf,
  .push) and extends bare `Object` with methods literally named `set`/`keys`
  — meaningless to port 1:1 since only `?get`/`add-handlers` are ever
  actually called from reader.cljs/writer.cljs (`set`/`keys` are unused
  outside this file), and shadowing clojure.core/set and clojure.core/keys
  at the top level would be an unforced, confusing mistake. So this keeps
  only the two methods that are actually part of the API — `?get` and
  `add-handlers`, dot-invokable exactly like upstream, via a protocol — and
  backs the table with an immutable persistent vector held in a mutable
  field rather than a mutated array: `from-table`'s copy-on-write contract
  (a derived table's handlers can't affect the table it was built from) then
  falls out for free from the vector's own immutability, instead of needing
  upstream's explicit `.slice`.")

(defprotocol IHandlerTable
  (?get [this k])
  (add-handlers [this handlers]))

(defn- index-of [v x]
  (loop [i 0]
    (cond
      (>= i (count v)) -1
      (= (nth v i) x) i
      :else (recur (inc i)))))

(defn- table-set! [table k v]
  (let [a (.-a table)
        i (index-of a k)]
    (if (> i -1)
      (set! (.-a table) (assoc a (inc i) v))
      (set! (.-a table) (conj a k v)))
    table))

(defn- add-handler [table [k handler]]
  (if (coll? k)
    ;; allow multiple keys pointing to same handler
    (reduce (fn [t k] (table-set! t k handler)) table k)
    (table-set! table k handler)))

(deftype HandlerTable [^:unsynchronized-mutable a]
  IHandlerTable
  (?get [_this k]
    (let [i (index-of a k)]
      (when (> i -1) (nth a (inc i)))))
  (add-handlers [this handlers] (reduce add-handler this handlers)))

(defn from-array [arr] (HandlerTable. (vec arr)))
(defn from-table [t] (HandlerTable. (.-a ^HandlerTable t)))
