(ns fress.api
  "Ported from upstream's fress.api — the public entry point.

  Upstream is a .cljc with a :clj branch (a thin delegate to the real
  org.fressian/clojure.data.fressian JVM library — not reusable here, since
  those classes don't exist under jolt) and a :cljs branch (calls straight
  into fress.reader/fress.writer, which is the actual from-scratch
  implementation). This ports the :cljs branch only.

  write-utf8 (the 0xBF UTF8 private-extension code) is implemented via the
  'utf8' struct tag rather than the raw code upstream's writeRawUTF8 can
  use directly: the raw 0xBF byte is, per codes.clj's own comment, private
  to fress clients and unrecognized by the real JVM Fressian spec, so the
  tag form is the one that's actually portable to a genuine JVM reader —
  matching what upstream's own :clj branch already had to do for the same
  reason (registering a \"utf8\" read handler rather than touching Codes)."
  (:refer-clojure :exclude [read])
  (:require [fress.reader :as r]
            [fress.writer :as w]
            [fress.impl.buffer :as buf]
            [fress.impl.raw-output :as rawOut]))

(defn- fressian-reader? [in] (instance? fress.reader.FressianReader in))
(defn- fressian-writer? [in] (instance? fress.writer.FressianWriter in))

(defn create-reader
  "Create a fressian reader targeting in.
   - :handlers is just a map of tag->fn merged with default read handlers
   - :checksum? {boolean} :: maintain a checksum for each byte read, validated
     when footer received. throws when fails. If no footer, has no effect
   - :name->map-ctor map of record names to map->Record constructors at runtime
       {\"string-name\" map->some-record}
   - :offset an integer byte offset into `in` to start reading from"
  [in & opts]
  (apply r/reader in opts))

(defn read-object
  "Read a single object from a fressian reader."
  [rdr]
  (assert (fressian-reader? rdr))
  (r/readObject rdr))

(defn tagged-object?
  "Returns true if o is a tagged object, which will occur when
   the reader does not recognized a specific type.  Use tag
   and tagged-value to access the contents of a tagged-object."
  [o]
  (instance? fress.reader.TaggedObject o))

(defn tag
  "Returns the tag if object is a tagged-object, else nil."
  [o]
  (:tag o))

(defn tagged-value
  "Returns the value (an Object array) wrapped by obj, or nil
   if obj is not a tagged object."
  [o]
  (:value o))

(defn create-writer
  "Create a fressian writer targeting out.
    - :handlers is just a map of {type {\"tag\" write-fn}} merged with default
      write handlers
    - :record->name map of record class to string-name (the string version
      of the record's fully resolved symbol)"
  [out & opts]
  (apply w/writer out opts))

(defn write-object
  "Write a single object to a fressian writer."
  ([writer o]
   (assert (fressian-writer? writer))
   (w/writeObject writer o))
  ([writer o cache?]
   (assert (fressian-writer? writer))
   (w/writeObject writer o cache?)))

(defn write-utf8
  "write a string as raw utf-8 bytes, via the \"utf8\" struct tag rather
  than fress's private 0xBF code (see namespace docstring)."
  ([writer s] (write-utf8 writer s false))
  ([writer s _cache?]
   (assert (fressian-writer? writer))
   (assert (string? s))
   (let [bytes (byte-array s)
         length (alength bytes)]
     (w/writeTag writer "utf8" 2)
     (w/writeCount writer length)
     (rawOut/writeRawBytes (.-raw-out ^fress.writer.FressianWriter writer) bytes 0 length))))

(defn write-tag
  "for use in custom write handlers"
  [writer tag field-count]
  (assert (string? tag))
  (assert (and (number? field-count) (<= 1 field-count)))
  (w/writeTag writer tag field-count))

(defn write-footer
  "use to seal off a writer with a final byte count & checksum for
   verification by a reader. Induces EOF"
  [writer]
  (assert (fressian-writer? writer))
  (w/writeFooter writer))

(defn reset-caches
  "write a signal to the reader to forget established cache codes"
  [writer]
  (assert (fressian-writer? writer))
  (w/resetCaches writer))

(defn begin-closed-list
  "Begin writing a fressianed list.  To end the list, call end-list.
   Used to write sequential data whose size is not known in advance."
  [writer]
  (assert (fressian-writer? writer))
  (w/beginClosedList writer))

(defn end-list
  "Ends a list begun with begin-closed-list."
  [writer]
  (assert (fressian-writer? writer))
  (w/endList writer))

(defn begin-open-list
  "Writes fressian code to begin an open list.  An
   open list can be terminated either by a call to end-list,
   or by simply closing the stream.  Used to write sequential
   data whose size is not known in advance, in contexts where
   stream failure can safely be interpreted as end of list."
  [writer]
  (assert (fressian-writer? writer))
  (w/beginOpenList writer))

(defn field-caching-writer
  "Returns a record writer that caches values for keys
   matching cache-pred, which is typically specified
   as a set, e.g. (field-caching-writer #{:color})"
  [cache-pred]
  (fn [wtr rec record->name]
    (w/writeTag wtr "record" 2)
    (w/writeObject wtr (w/class-sym rec record->name) true)
    (w/writeTag wtr "map" 1)
    (w/beginClosedList wtr)
    (doseq [[field value] rec]
      (w/writeObject wtr field true)
      (w/writeObject wtr value (boolean (cache-pred field))))
    (w/endList wtr)))

(defn byte-stream [] (buf/byte-stream))

(defn flush-to
  ([stream out] (flush-to stream out 0))
  ([stream out offset]
   (assert (instance? fress.impl.buffer.BytesOutputStream stream))
   (buf/flushTo stream out offset)))

(defn read
  "Convenience method for reading a single fressian object.
   Takes same options as create-reader"
  [readable & options]
  (r/readObject (apply create-reader readable options)))

(defn read-batch
  "Read a fressian reader fully (until eof), returning a (possibly empty)
   vector of results."
  [fin]
  (assert (fressian-reader? fin))
  (let [sentinel (Object.)]
    (loop [objects (transient [])]
      (let [obj (try (r/readObject fin) (catch Exception _e sentinel))]
        (if (identical? obj sentinel)
          (persistent! objects)
          (recur (conj! objects obj)))))))

(defn read-all
  "like read-batch but accepts readables in addition to FressianReaders"
  [in & options]
  (if (fressian-reader? in)
    (read-batch in)
    (read-batch (apply create-reader in options))))

(defn write
  "Convenience method for writing a single object.  Returns a
   byte-array. Options are the same as for create-reader,
   with one additional option :footer? {bool}, if specified will
   write a fressian footer after writing the object."
  [obj & options]
  (let [{:keys [footer?]} (when options (apply hash-map options))
        bos (buf/byte-stream)
        writer (apply create-writer bos options)]
    (w/writeObject writer obj)
    (when footer?
      (w/writeFooter writer))
    (buf/toByteArray bos)))
