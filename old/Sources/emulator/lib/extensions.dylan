;; Miscellaneous extensions, exported in various ways

;;; type conveniences

(define false-or
  (method ((type <type>))
    (type-union (singleton #f) type)))

(define one-of
  (method (first #rest rest)
    (reduce type-union (singleton first) (map singleton rest))))

(define <byte-character> <character>)

(define-class <subclass> (<limited-type>)
  (subclass-class type: <class>
     required-init-keyword: class:))

;; eof
