(define-method modulo ((x <real>) (y <real>))
  (bind ((quo mod (floor/ x y)))
    mod))

(define-method remainder ((x <real>) (y <real>))
  (bind ((quo rem (truncate/ x y)))
    rem))

(define-method = ((x <number>) (y <number>))
  (=/number*number x y))

(define-method < ((x <number>) (y <number>))
  (</number*number x y))

(define-method as ((class (singleton <integer>)) (x <number>))
  (as/integer*number x))

(define-method as ((class (singleton <real>)) (x <number>))
  (as <float> x))

;; In our product, <integer>s are converted to <single-float>s only
;; if the conversion shouldn't lose any precision.  This code mimics
;; that behavior.
(define-method as ((class (singleton <float>)) (x <number>))
  (if (or (</number*number x (- 0 (- (ash 1 24) 2)))
	  (</number*number (- (ash 1 24) 2) x))
      (as/double-float*number x)
      (as/single-float*number x)))

(define-method as ((class (singleton <single-float>)) (x <number>))
  (as/single-float*number x))

(define-method as ((class (singleton <double-float>)) (x <number>))
  (as/double-float*number x))

(define-method as ((class (singleton <rational>)) (x <number>))
  (as/rational*number x))
#|
(define-method as ((class (singleton <ratio>)) (x <number>))
  (as/rational*number x))

(define-method as ((class (singleton <complex>)) (x <number>))
  (as/complex*number x))
|#

;; Kludge.

(define-method integral? ((x <integer>))
  #t)

(define-method integral? ((x <real>))
  (bind ((quo rem (truncate x)))
    (zero? rem)))

;; eof
