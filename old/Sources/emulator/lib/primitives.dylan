;; From CL:

(import-cl-functions

  ((clos allocate-instance)         as: allocate)
  ((clos dylan-make)                as: make)
  ((clos dylan-initialize)          as: initialize)
  ((clos dylan-singleton)           as: singleton)
  ((clos dylan-check-type)          as: check-type)
  ((dylan dylan-instance?)          as: instance?)
  ((dylan dylan-slot-initialized?)  as: slot-initialized?)

  ((clos dylan-slot-offset)             as: slot-offset)
  ((clos fast-standard-instance-access) as: slot-element)
  ((clos dylan-slot-element-setter)     as: slot-element-setter)

  ((clos dylan-object-class)        as: object-class)
  ((dylan dylan-subclass?)          as: subclass?)
  ((dylan dylan-subclass?)          as: subtype?)
  ((clos dylan-direct-superclasses) as: direct-superclasses)
  ((clos dylan-direct-subclasses)   as: direct-subclasses)

  (class-of as: subclass)
  (class-of as: subtype)

  ((clos dylan-function-arguments)        as: function-arguments)
  ((clos dylan-sorted-applicable-methods) as: sorted-applicable-methods)

  ((dylan dylan-function-name)            as: function-debug-name)
  ((clos class-name)                      as: class-debug-name)

  generic-function-methods
  method-specializers
  ((dylan dylan-find-method)              as: find-method)
  add-method

  ((dylan dylan-feature-present?) as: feature-present?)
  ((dylan add-feature))
  ((dylan remove-feature))

  ((dylan dylan-keyword?) as: keyword?)

)

(import-cl-functions
  (cons as: pair)
  list 
  (car as: head)
  (cdr as: tail)
  ((dylan dylan-head-setter) as: head-setter)
  ((dylan dylan-tail-setter) as: tail-setter)
)

;; Chapter 14. Numbers

(import-cl-classes

  (number                   as: <number>)
  (complex                  as: <complex>)
  (real                     as: <real>)
  (rational                 as: <rational>)
  (integer                  as: <integer>)
 
  (float                    as: <float>)
  (single-float             as: <single-float>)
  (double-float             as: <double-float>)
  (double-float             as: <extended-float>)

)

(import-cl-functions

  ((dylan dylan-oddp)   as: odd?)
  ((dylan dylan-evenp)  as: even?)
  ((dylan dylan-zerop)  as: zero?)
  ((dylan dylan-plusp)  as: positive?)
  ((dylan dylan-minusp) as: negative?)

  (- as: negative)

  +
  -
  *
  /

  floor
  ceiling
  round
  truncate

  (floor    as: floor/)
  (ceiling  as: ceiling/)
  (round    as: round/)
  (truncate as: truncate/)

  ;; modulo    in Dylan
  ;; remainder in Dylan

  abs 
  log expt (expt as: \^)
  sqrt 

  logior
  logxor
  logand
  lognot
  ((dylan dylan-logbit?) as: logbit?)
  ash

  lcm gcd

)

(import-cl-functions

  ((dylan dylan-format) as: format)
  ((dylan dylan-break) as: break)
  ((dylan dylan-cerror) as: cerror)
  ((dylan dylan-print) as: print)
  ((dylan dylan-standard-output) as: standard-output)
  ((dylan dylan-standard-input) as: standard-input)

  (apply as: apply/list)

  identity
  values

  ;; Arrays:

  (make-array        as: make/array)
  (aref              as: aref)
  ((dylan dylan-aref-setter) as: aref-setter)
  (array-dimensions  as: dimensions)
  (array-dimension   as: dimension)
  (array-rank        as: rank)
  
  ;; Sequences:
  
  (length             as: size/sequence)
  (elt                as: element/sequence*integer)
  ((system set-elt)   as: set-element/sequence*integer)
  (map                as: map-as/sequence)
  (map-into           as: map-into/sequence)
  ((dylan dylan-any?)         as: any?/sequence)
  ((dylan dylan-every?)       as: every?/sequence)
  ((dylan dylan-remove)       as: remove/sequence)
  (remove             as: remove/sequence)
  (delete             as: remove!/sequence)

  (make-array         as: make/vector)
  ((system *%in-vector) as: vector)
  (vector-push-extend as: push/stretchy-vector)
  (vector-pop         as: pop/stretchy-vector)
  (adjust-array       as: adjust/stretchy-vector)
  (make-string        as: make/string)
  (make-list          as: make/list)
  (mapc               as: do/list)

  ;; Tables:

  (make-hash-table    as: make/table)
  ((dylan dylan-gethash)      as: element/table*object)
  ((dylan dylan-sethash)      as: set-element/table*object)
  ((dylan dylan-remhash)      as: remove-key/table*object)
  ((dylan dylan-maphash)      as: do/function*table)
  (hash-table-count   as: size/table)

  ;; Coercion:

  (char-int as: as/integer*character)
  (int-char as: as/character*integer)
  (char-upcase   as: as-uppercase/character)
  (char-downcase as: as-lowercase/character)

  (symbol-name as: as/string*symbol)
  ((dylan dylan-intern-symbol)  as: as/symbol*string)
  (symbol-name as: as/string*keyword)
  ((dylan dylan-intern-keyword) as: as/keyword*string)

  (round                         as: as/integer*number)
  ((dylan dylan-as-float)        as: as/float*number)
  ((dylan dylan-as-single-float) as: as/single-float*number)
  ((dylan dylan-as-double-float) as: as/double-float*number)
  (rationalize                   as: as/rational*number)
  (complex                       as: as/complex*number)

  ((dylan dylan-restart-query) as: restart-query)
  ((dylan dylan-return-query)  as: return-query)

  ;; Misc:

  translate-cl-value-name
  translate-cl-class-name

  ((generic-env generic-load) as: load)

  ((dylan dylan-default-handler) as: default-handler)
  ((dylan dylan-signal)          as: signal)
  ((dylan dylan-error)           as: error)

  abort)

(import-cl-functions
  ((dylan dylan-eq)    as: id?)
  ((dylan dylan-null)  as: null?)
  ((dylan dylan=)      as: =/number*number)
  ((dylan dylan<)      as: </number*number)
  ((dylan dylan-equal) as: =/object*object))

(import-cl-classes
  (standard-object          as: <object>)
  (class                    as: <class>)

  ((dylan dylan-boolean)    as: <boolean>)

  (stream                   as: <stream>)

  (character                as: <character>)
  (symbol                   as: <symbol>)
  (symbol                   as: <keyword>)

  (list                     as: <list>)
  (cons                     as: <pair>)
  (null                     as: <empty-list>)

  (array                    as: <array>)
  ((clos abstract-vector)   as: <vector>)
  ((clos abstract-simple-vector) as: <simple-vector>)
  (vector                   as: <simple-object-vector>)
  ((clos abstract-string)   as: <string>)
  (string                   as: <byte-string>)

  (function                 as: <method>)
  (function                 as: <function>)
  (standard-generic-function as: <generic-function>)

  ((clos cl-sequence)                     as: <cl-sequence>)
  ((clos collection)                      as: <collection>)
  ((clos explicit-key-collection)         as: <explicit-key-collection>)
  ((clos mutable-collection)              as: <mutable-collection>)
  ((clos sequence)                        as: <sequence>)
  ((clos mutable-explicit-key-collection) as: <mutable-explicit-key-collection>)
  ((clos mutable-sequence)                as: <mutable-sequence>)

  (condition         as: <condition>)
  (warning           as: <warning>)
  (serious-condition as: <serious-condition>)
  (error             as: <error>)
  (simple-error      as: lisp/<simple-error>)
  (simple-warning    as: lisp/<simple-warning>)
  (type-error        as: <type-error>)

  ((dylan dylan-restart)        as: <restart>)
  ((dylan dylan-simple-restart) as: <simple-restart>))

(import-cl-functions
  (simple-condition-format-string as: condition-format-string)
  (simple-condition-format-arguments as: condition-format-arguments)

  type-error-expected-type
  (type-error-datum as: type-error-value))

;; Extras:

(import-cl-functions
  ((system call-system-showing-output)
      as: call-system-showing-output-internal)
  (mark-and-sweep as: mark-and-sweep)
  (clean-down     as: clean-down)
  ((system reader-error) as: lisp/reader-error)
)

(import-cl-functions
  ((dylan dylan-eval) as: eval))

(import-cl-functions
  ((dylan dylan-probe-file) as: probe-file))

(import-cl-functions
  ((dylan dylan-decode-single-float) as: decode-single-float)
  ((dylan dylan-encode-single-float) as: encode-single-float)
  ((dylan dylan-decode-double-float) as: decode-double-float)
  ((dylan dylan-encode-double-float) as: encode-double-float))

(import-cl-values
  *standard-input*
  *standard-output*)

(import-cl-functions
  ((dylan dylan-ignore) as: ignore)
  ((dylan dylan-ignorable) as: ignorable))

;; eof 
