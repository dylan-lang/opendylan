Module:   internal
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Definition of the Dylan-extensions module.

(define-module dylan-extensions
  (use internal
    export: 
      (
       ignore ignorable
       false-or one-of
       power-of-two-ceiling
       singleton-object
       range-from range-by
       <subclass> subclass-class
       <union> union-type1 union-type2
       <limited-integer> limited-integer-min limited-integer-max
       <big-integer>
       <double-integer>
       <byte-character>
       <abstract-integer>
       <machine-word>
       $machine-word-size
       $maximum-signed-machine-word $minimum-signed-machine-word
       $maximum-unsigned-machine-word $minimum-unsigned-machine-word
       as-unsigned
       integer-as-raw raw-as-integer
       abstract-integer-as-raw raw-as-abstract-integer

       decode-single-float encode-single-float
       decode-double-float encode-double-float
       debug-name debug-name-setter subclass
       <hashed-collection>
       tally tally-setter mask mask-setter elements 
       elements-setter 
       void-element gc-state gc-state-setter
       current-gc-state rehash! rehash-using! grow!
       index-for-key merge-hash-ids merge-hash-states 
       includes-key?

       <string-table> <hash-state>
       collection-hash sequence-hash 
       values-hash
       string-hash
       case-insensitive-string-hash
       case-insensitive-equal
       remove-all-keys!

       as-keyword keyword?

       slot-offset slot-element slot-element-setter
       )))

;; These two modules are used by native implementations of various libraries
;; (e.g., Streams, File-System) and it's simpler to have these modules exit
;; in the emulator than create separate module.dylan (or library.dylan) files
;; for each of those libraries.

(define-module dylan-primitives
  (use internal))

(define-module dylan-direct-c-ffi
  (use internal))

;; eof
