(define-method as-uppercase ((c <character>))
  (as-uppercase/character c))

(define-method as-lowercase ((c <character>))
  (as-lowercase/character c))

(define-method as ((class (singleton <integer>)) (c <character>))
  (as/integer*character c))

(define-method as ((class (singleton <character>)) (i <integer>))
  (as/character*integer i))

(define-method < ((c1 <character>) (c2 <character>))
  (< (as <integer> c1) (as <integer> c2)))

;; eof
