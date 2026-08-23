(ns fress.impl.ranges
  "Packed-int range boundaries. Upstream computes these via js/parseInt on hex
  strings because a JS number can't hold e.g. 0xFFFFFFFFFFFFF000 exactly; jolt
  has native arbitrary-precision integers, so the signed 64-bit values are
  written directly instead.")

(def ^:const PACKED_1_START 1)
(def ^:const PACKED_1_END 64)
(def ^:const PACKED_2_START -4096)              ; -0x1000
(def ^:const PACKED_2_END 4096)                 ;  0x1000
(def ^:const PACKED_3_START -524288)            ; -0x80000
(def ^:const PACKED_3_END 524288)               ;  0x80000
(def ^:const PACKED_4_START -33554432)          ; -0x2000000
(def ^:const PACKED_4_END 33554432)             ;  0x2000000
(def ^:const PACKED_5_START -8589934592)        ; -0x200000000
(def ^:const PACKED_5_END 8589934592)           ;  0x200000000
(def ^:const PACKED_6_START -2199023255552)     ; -0x20000000000
(def ^:const PACKED_6_END 2199023255552)        ;  0x20000000000
(def ^:const PACKED_7_START -562949953421312)   ; -0x2000000000000
(def ^:const PACKED_7_END 562949953421312)      ;  0x2000000000000

(def ^:const PRIORITY_CACHE_PACKED_END 32)
(def ^:const STRUCT_CACHE_PACKED_END 16)
(def ^:const BYTES_PACKED_LENGTH_END 8)
(def ^:const STRING_PACKED_LENGTH_END 8)
(def ^:const LIST_PACKED_LENGTH_END 8)

(def ^:const BYTE_CHUNK_SIZE 65535)
