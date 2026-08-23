# jolt-fressian

[Fressian](https://github.com/Datomic/fressian) — the binary serialization
format used by Datomic and `clojure.data.fressian` — implemented for the
[jolt](https://github.com/jolt-lang/jolt) runtime.

Based on [pkpkpk/fress](https://github.com/pkpkpk/fress), but ported
from scratch onto jolt's own byte-array/`jolt.ffi` primitives rather than
JavaScript typed arrays — this is a from-scratch Fressian implementation, not
a wrapper around `org.fressian`/`clojure.data.fressian` (those JVM classes
don't exist under jolt).

## Usage

```clojure
(require '[fress.api :as fress])

(fress/write {:a 1 :b [1 2 3]})       ;=> byte-array
(fress/read (fress/write "hello"))    ;=> "hello"

;; multiple objects on one stream
(let [out (fress/byte-stream)
      w   (fress/create-writer out)]
  (fress/write-object w 1)
  (fress/write-object w 2)
  (let [rdr (fress/create-reader @out)]
    (fress/read-batch rdr)))          ;=> [1 2]
```

## Type mapping

| Clojure                                    | Fressian wire type        |
|---------------------------------------------|---------------------------|
| `nil`, `true`/`false`                       | null / boolean            |
| any integer up to 64 bits                   | packed int (1–8 bytes)    |
| a `clojure.lang.BigInt` beyond 64 bits       | `bigint`                  |
| any non-integer number                      | `double`                  |
| `string`                                    | packed/chunked `string`   |
| `byte-array`                                | packed/chunked `bytes`    |
| `int-array`/`long-array`/`float-array`/`double-array`/`boolean-array`/`object-array` | typed arrays |
| `keyword`, `symbol`                         | `key` / `sym`             |
| vector, list, seq                           | `list`                    |
| map (any `IPersistentMap`)                  | `map`                     |
| set (any `IPersistentSet`)                  | `set`                     |
| `char`                                      | `char`                    |
| `java.util.UUID`                            | `uuid`                    |
| `java.util.regex.Pattern`                   | `regex`                   |
| `java.net.URI`                              | `uri`                     |
| `java.util.Date`                            | `inst`                    |
| `fress.impl.bigdec/Bigdec` (jolt has no real `BigDecimal` decomposition) | `bigdec` |
| a `defrecord`                                | `record` (needs `:record->name`/`:name->map-ctor`) |

Known jolt limitations this port works around or can't close (documented
in-code where they bite): a small bigint (`(bigint 0)`) loses its `BigInt`
type tag under jolt, so it can't be told apart from a plain `long` and is
written as a packed int rather than a `bigint`; `float?`/`double?` both
report `true` for every floating-point value, so there is currently no
portable way to write an explicit `float` through generic dispatch (it's
always written as `double`); a `defrecord` type referenced by its bare name
doesn't `=` the `Class` of an actual instance of it, so a `:record->name`
map's keys need `(class (ctor ...))`, not the bare type name.

## Test

```
git clone --recurse-submodules https://github.com/jolt-lang/jolt-fressian
cd jolt-fressian
jolt -M:test
```

`jolt -M:test` runs the in-process `clojure.test` regression suite
(`test/fress/golden_test.clj`), including byte-exact golden vectors
captured from a real JVM Fressian writer.

### JVM cross-write gate

```
bin/jvm-xcheck
```

Writes a battery of values with jolt's writer and reads them back with a
real `clojure.data.fressian` reader, and the reverse — the actual
conformance oracle for wire compatibility, which round-tripping within jolt
alone can't prove. Needs `clojure` and `java` on `PATH`; skips cleanly if
either is missing.

## License

EPL-1.0, same as upstream `pkpkpk/fress` — see [epl-v10.html](epl-v10.html).
