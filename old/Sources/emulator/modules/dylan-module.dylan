Module:   internal
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Definition of the Dylan module.

(define-module dylan
  (use internal
    import: (address-of current-gc-state))
  (use internal
    ;; rename: ((<lisp-object-table> => <object-table>))
    export: 
      (

       ;;; Evaluation.

       values
       ~
       apply

       ;;; Objects.

       <object>
       make
       initialize

       slot-initialized?

       ;;; Equality and Magnitute Comparisons.

       <boolean>

       ==
       ~==
       =
       ~=
       <
       >
       <=
       >=

       ;;; Functions.

       <function>
       <method>
       <generic-function>

       add-method
       generic-function-methods
       method-specializers
       function-arguments
       applicable-method?
       sorted-applicable-methods
       find-method

       ;;; Types and Classes.

       <type>
       <class>
       instance?
       subtype?
       as
       shallow-copy
       type-for-copy
       object-class
       all-superclasses
       direct-superclasses
       direct-subclasses

       <singleton>
       singleton

       type-union
       limited
       subclass
       
       ;;; Collections.

       ;; Iteration protocol.

       forward-iteration-protocol
       backward-iteration-protocol

       key-sequence
       current-key
       
       element
       element-setter

       ;; Classes.

       <collection>
       <mutable-collection>
       <stretchy-collection>

       size 
       size-setter
       empty?
       do
       map
       map-as
       map-into
       any?
       every?
       reduce
       reduce1
       member?
       find-key
       replace-elements!
       fill!

       <sequence>
       <mutable-sequence>
       <stretchy-sequence>

       add
       add!
       add-new
       add-new!
       remove
       remove!
       choose
       choose-by
       intersection
       union
       remove-duplicates
       remove-duplicates!
       copy-sequence
       concatenate-as
       concatenate
       replace-subsequence!
       reverse
       reverse!
       sort
       sort!
       first
       second
       third
       last
       first-setter
       second-setter
       third-setter
       last-setter
       subsequence-position

       <explicit-key-collection>
       <mutable-explicit-key-collection>

       <array>

       aref
       aref-setter
       dimensions
       
       rank
       row-major-index
       dimension

       <deque>
       
       push
       pop
       push-last
       pop-last
       add!
       remove!
       
       <list>
       <empty-list>
       <pair>

       pair
       head
       tail
       head-setter
       tail-setter
       list

       <range>

       range

       <string>
       <byte-string>
       <unicode-string>

       as-lowercase
       as-lowercase!
       as-uppercase
       as-uppercase!

       <table>
       <object-table>

       remove-key!

       table-protocol
       merge-hash-codes
       $permanent-hash-state
       object-hash

       <vector>
       <stretchy-vector>
       <simple-vector>
       <simple-object-vector>

       vector

       ;;; Conditions.

       <condition>
       <serious-condition>
       <error>
       <simple-error>
       <type-error>
       <warning>
       <simple-warning>
       <restart>
       <simple-restart>
       <abort>

       signal
       error
       cerror
       break
       check-type
       abort

       default-handler
       restart-query
       return-query

       do-handlers
       return-allowed?
       return-description

       condition-format-string
       condition-format-arguments

       type-error-value
       type-error-expected-type

       ;;; Numbers.

       <number>

       <complex>
       <real>
       <rational>

       <float>
       <single-float>
       <double-float>
       <extended-float>

       <integer>
       $maximum-integer $minimum-integer

       odd?
       even?
       zero?
       positive?
       negative?
       integral?

       +
       *
       -
       /
       
       negative

       floor
       ceiling
       round
       truncate

       floor/
       ceiling/
       round/
       truncate/
       
       modulo
       remainder
       
       \^

       min
       max

       abs

       logior
       logxor
       logand
       lognot
       logbit?
       ash
       lsh

       gcd lcm

       ;;; Keywords and Symbols.

       <symbol>
       <keyword>

       ;;; Characters.

       <character>

       as-uppercase
       as-lowercase

       ;;; Functional Operators.

       compose
       complement
       disjoin
       conjoin
       curry
       rcurry
       always

       ;;; Miscellaneous.

       identity

       ;;; Extensions.

       *error-presentation-hook*
       *warning-presentation-hook*

       ;;; Hacks!!!!!!!!! 

       ;; For prefix:

       not
       id?
       /=
       expt

       ;; For backward compatibility:

       =hash

       subclass?

       initial-state
       next-state
       current-element
       current-element-setter
       copy-state
       
       final-state
       previous-state
     
       class-for-copy

       ;; Because hygiene doesn't work:

       %define-module
       %define-module-alias
       list*

       ;; Because the translator needs them:

       keyword?
       as-keyword

       ;;; Defining forms

       define-module

       )

      )
)

;; eof
