;; A quick cosmetic hack at the type system:

#|

(define-class <type> (<object>))

; (define-generic-function subtype? (type1 type2))
; (define-generic-function instance? (object type))

(define subtype? subclass?)

(define <real-singleton> <list>)

(define-method make ((s <real-singleton>) #rest args)
  (singleton-object s))

(define-method instance? ((x <object>) (s <real-singleton>))
  (== x (singleton-object s)))

(define-method subtype? ((s1 <real-singleton>) (s1 <real-singleton>))
  (== (singleton-object s1) (singleton-object s2)))

(define-method subtype? ((s <real-singleton>) (c <class>))
  (subtype? (object-class (singleton-object s)) c))

(define-method subtype? ((c <class>) (s <real-singleton>))
  #f)

|#

(define singleton-object second)

;; Stub type system implementations
;; Unused class definitions, so that code can refer to the type and slots

(define <type> <object>)

(define-class <limited-type> (<type>))

(define-class <limited-integer> (<limited-type>)
   (limited-integer-min
      required-init-keyword: min:)
   (limited-integer-max
      required-init-keyword: max:))

(define-method limited ((c <class>) #rest keys)
  c)

(define-class <union> (<type>)
   (union-type1 type: <type>
      required-init-keyword: type1:)
   (union-type2 type: <type>
      required-init-keyword: type2:))

(define-method type-union (first-type #rest more-types)
  (reduce binary-type-union first-type more-types))

(define-method binary-type-union (t1 t2)
  <object>)

;; hack to support (deprecated) union on types
;; Only valid types are classes and lists (could be singletons!)

(define-method union ((t1 <class>) (t2 <class>) #key)
  (type-union t1 t2))

(define-method union ((t1 <class>) (t2 <list>) #key)
  (type-union t1 t2))

(define-method union ((t1 <list>) (t2 <class>) #key)
  (type-union t1 t2))

;; eof
