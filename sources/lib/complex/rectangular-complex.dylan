module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-class <rectangular-complex> (<complex>)
  (real-part required-init-keyword: real-part: type: <real>)
  (imag-part required-init-keyword: imag-part: type: <real>))

(define-method contagious-class ((x <integer>) (y <rectangular-complex>))
  <rectangular-complex>)
(define-method contagious-class ((x <float>) (y <rectangular-complex>))
  <rectangular-complex>)
(define-method contagious-class ((x <rectangular-complex>) (y <integer>))
  <rectangular-complex>)
(define-method contagious-class ((x <rectangular-complex>) (y <ratio>))
  <rectangular-complex>)
(define-method contagious-class ((x <rectangular-complex>) (y <float>))
  <rectangular-complex>)
(define-method contagious-class
    ((x <rectangular-complex>) (y <rectangular-complex>))
  <rectangular-complex>)
(define-method contagious-class ((x <ratio>) (y <rectangular-complex>))
  <rectangular-complex>)

(define-method make
    ((class (singleton <complex>))
     #rest all-keys #key real (imag: imaginary) magnitude angle)
  (if real
      (if imaginary
	  (if magnitude
	      (error "Illegal arguments to make <complex> ~A" all-keys)
	      (if angle
		  (error "Illegal arguments to make <complex> ~A" all-keys)
		  (make-rectangular real imaginary)))
	  (if magnitude
	      (error "Illegal arguments to make <complex> ~A" all-keys)
	      (if angle
		  (error "Illegal arguments to make <complex> ~A" all-keys)
		  (make-rectangular real 0))))
      (if imaginary
	  (if magnitude
	      (error "Illegal arguments to make <complex> ~A" all-keys)
	      (if angle
		  (error "Illegal arguments to make <complex> ~A" all-keys)
		  (make-rectangular 0 imaginary)))
	  (if magnitude
	      (if angle
		  (make-polar magnitude angle)
		  (make-polar magnitude 0))
	      (if angle
		  (make-polar 0 angle)
		  (make-rectangular 0 0))))))

(define-method canonicalize ((number <rectangular-complex>))
  (if (= (imag-part number) 0)
      (shallow-copy (real-part number))
      number))

(define-method as ((class (singleton <rectangular-complex>)) (real <real>))
  (make-rectangular real 0))

(define-method id-hash ((object <rectangular-complex>))
  (values (merge-hash-ids
           (id-hash (real-part object)) (id-hash (imag-part object))
           ordered: #T)
          $permanent-hash-state))

(define-method make-rectangular ((real-part <real>) (imag-part <real>))
  (make <rectangular-complex> real-part: real-part imag-part: imag-part))

(define-method make-rectangular* ((real-part <real>) (imag-part <real>))
  (canonicalize (make-rectangular real-part imag-part)))

(define $i (make-rectangular 0 1)) 

(define-method make-polar ((magnitude <real>) (angle <real>))
  (make <rectangular-complex>
	real-part: (* magnitude (sin angle))
	imag-part: (* magnitude (cos angle))))

(define-method angle ((complex <rectangular-complex>))
  (atan2 (real-part complex) (imag-part complex)))

(define-method integral? ((number <rectangular-complex>))
  (and (integral? (real-part number)) (integral? (imag-part number))))

(define-method =
    ((number-1 <rectangular-complex>) (number-2 <rectangular-complex>))
  (and (= (real-part number-1) (real-part number-2))
       (= (imag-part number-1) (imag-part number-2))))

(define-method <
    ((number-1 <rectangular-complex>) (number-2 <rectangular-complex>))
  (bind ((distance-1 (+ (* (real-part number-1) (real-part number-1))
                        (* (imag-part number-1) (imag-part number-1))))
         (distance-2 (+ (* (real-part number-2) (real-part number-2))
                        (* (imag-part number-2) (imag-part number-2)))))
    (< distance-1 distance-2)))

(define-method +
    ((number-1 <rectangular-complex>) (number-2 <rectangular-complex>))
  (make-rectangular* (+ (real-part number-1) (real-part number-2))
		     (+ (imag-part number-1) (imag-part number-2))))

(define-method *
    ((number-1 <rectangular-complex>) (number-2 <rectangular-complex>))
  (make-rectangular* (- (* (real-part number-1) (real-part number-2))
			(* (imag-part number-1) (imag-part number-2)))
		     (+ (* (imag-part number-1) (real-part number-2))
			(* (real-part number-1) (imag-part number-2)))))

(define-method -
    ((number-1 <rectangular-complex>) (number-2 <rectangular-complex>))
  (make-rectangular* (- (real-part number-1) (real-part number-2))
		     (- (imag-part number-1) (imag-part number-2))))

(define-method / ((x <rectangular-complex>) (y <rectangular-complex>))
  (bind (((rx <real>) (real-part x))
	 ((ix <real>) (imag-part x))
	 ((ry <real>) (real-part y))
	 ((iy <real>) (imag-part y))
	 ((denominator <float>) (+ (* ry ry) (* iy iy))))
    (make-rectangular* (/ (+ (* rx ry) (* ix iy)) denominator)
		       (/ (- (* ix ry) (* rx iy)) denominator))))

(define-method negative ((number <rectangular-complex>))
  (make-rectangular (- (real-part number)) (- (imag-part number))))

(define-method abs ((number <rectangular-complex>))
  (sqrt (+ (* (real-part number) (real-part number))
	   (* (imag-part number) (imag-part number)))))

(define-method phase ((number <rectangular-complex>))
  (atan2 (imag-part number) (real-part number)))

;;; !@#$ <COMPLEX> IRRATIONALS NOT NECESSARILY WORKING
;;; !@#$ NEED TO COMPARE AGAINST A DEPENDABLE IMPLEMENTATION

(define-method sin ((number <rectangular-complex>))
  (bind (((real <real>) (real-part number))
	 ((imag <real>) (imag-part number)))
    (make-rectangular (* (sin real) (cosh imag)) (* (cos real) (sinh imag)))))

(define-method cos ((number <rectangular-complex>))
  (bind (((real <real>) (real-part number))
	 ((imag <real>) (imag-part number)))
    (make-rectangular (* (cos real) (cosh imag))
		      (- (* (sin real) (sinh imag))))))

(define-method tan ((number <rectangular-complex>))
  (bind (((numerator <number>) (sin number))
	 ((denominator <number>) (cos number)))
    (if (zero? denominator)
	(error "~S undefined tangent." number)
	(/ numerator denominator))))

(define-method asin ((number <rectangular-complex>))
  (- (* $i (log (+ (* $i number) (sqrt (- 1 (* number number))))))))

(define-method acos ((number <rectangular-complex>))
  (- (/ $pi 2) (asin number)))

(define-method atan ((number <rectangular-complex>))
  (/ (- (log (+ 1 (* $i number))) (log (- 1 (* $i number)))) (* 2 $i)))

(define-method sinh ((number <rectangular-complex>))
  (/ (- (exp number) (exp (- number))) 2))

(define-method cosh ((number <rectangular-complex>))
  (/ (+ (exp number) (exp (- number))) 2))

(define-method tanh ((number <rectangular-complex>))
  (/ (- (exp number) (exp (- number))) (+ (exp number) (exp (- number)))))

(define-method asinh ((number <rectangular-complex>))
  (log (+ number (sqrt (+ 1 (* number number))))))

(define-method acosh ((number <rectangular-complex>))
  (log (+ number (* (+ number 1) (sqrt (/ (- number 1) (+ number 1)))))))

(define-method atanh ((number <rectangular-complex>))
  (log (* (+ number 1) (sqrt (/ (- 1 (* number number)))))))

(define-method log ((number <rectangular-complex>))
  (make-rectangular* (log (abs number)) (phase number)))

(define-method exp ((power <rectangular-complex>))
  (* (exp (real-part power)) (cis (imag-part power))))

(define-method expt ((base <real>) (power <rectangular-complex>))
  (if (negative? base)
      (/ (exp (* power (log (- base)))))
      (exp (* power (log base)))))

(define-method expt ((base <rectangular-complex>) (power <real>))
  (* (expt (abs base) power)
     (cis (* power (phase base)))))

(define-method expt ((base <rectangular-complex>) (power <rectangular-complex>))
  (exp (* power (log base))))

(define-method sqrt ((number <rectangular-complex>))
  (exp (/ (log number) 2)))
