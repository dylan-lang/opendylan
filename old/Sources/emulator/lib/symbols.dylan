
(define-method as ((class (singleton <string>)) (s <keyword>))
  (as/string*keyword s))

(define-method as ((class (singleton <keyword>)) (s <string>))
  (as/keyword*string (as-uppercase s)))

(define-method as ((class (singleton <symbol>)) (s <string>))
  (as/symbol*string (as-uppercase s)))

(define-method as ((class (singleton <string>)) (s <symbol>))
  (as/string*symbol s))

;; Hack!!!

(define-method as-keyword ((s <symbol>))
  (as/keyword*string (as <string> s)))

(define-method as-keyword ((s <string>))
  (as/keyword*string (as-uppercase s)))

(define-method = ((x <symbol>) (y <symbol>))
  (bind ((x-string (as <string> x))
         (y-string (as <string> y)))
    (and (= (size x-string) (size y-string))
         (= x-string y-string))))

;; eof
