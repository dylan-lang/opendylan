module:    ratio
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-class <ratio> (<rational>)
  (numerator   required-init-keyword: numerator:   type: <integer>)
  (denominator required-init-keyword: denominator: type: <integer>))

(define-method contagious-class ((x <ratio>) (y <ratio>)) <ratio>)
(define-method contagious-class ((x <integer>) (y <ratio>)) <ratio>)
(define-method contagious-class ((x <ratio>) (y <integer>)) <ratio>)
(define-method contagious-class ((x <float>) (y <ratio>)) (object-class x))
(define-method contagious-class ((x <ratio>) (y <float>)) (object-class y))

(define-method as ((class (singleton <rational>)) (ratio <ratio>)) ratio)
(define-method as ((class (singleton <rational>)) (float <single-float>))
  (as <ratio> float))

(define-method as ((class (singleton <integer>)) (ratio <ratio>))
  (truncate/ (numerator ratio) (denominator ratio)))

(define-method as ((class (singleton <single-float>)) (ratio <ratio>))
  (/ (as <float> (numerator ratio)) (as <float> (denominator ratio))))

(define-method as ((class (singleton <float>)) (ratio <ratio>))
  (as <single-float> ratio))

(define-method make-ratio ((numerator <integer>) (denominator <integer>))
  (make <ratio> numerator: numerator denominator: denominator))
(define-method make-ratio* ((numerator <integer>) (denominator <integer>))
  (canonicalize (make <ratio> numerator: numerator denominator: denominator)))

(define-method canonicalize ((number <ratio>))
  (bind (((the-gcd <integer>) (gcd (numerator number) (denominator number))))
    (unless (= the-gcd 1)
      (set! (numerator number) (truncate/ (numerator number) the-gcd))
      (set! (denominator number) (truncate/ (denominator number) the-gcd)))
    (when (negative? (denominator number))
      (set! (denominator number) (- (denominator number)))
      (set! (numerator number) (- (numerator number))))
    (if (= (denominator number) 1)
	(numerator number)
	number)))

(define-method as ((class (singleton <ratio>)) (integer <integer>))
  (make-ratio integer 1))

(define-method as ((class (singleton <ratio>)) (float <single-float>))
  (rationalize-with-epsilon
   float (as <single-float> (epsilon <single-float>))))

(define-method as ((class (singleton <ratio>)) (float <float>))
  (rationalize-with-epsilon float (epsilon (object-class float))))

(define-method id-hash ((object <ratio>))
  (values (merge-hash-ids
           (id-hash (numerator object)) (id-hash (denominator object))
           ordered: #T)
          $permanent-hash-state))

(define-method integral? ((number <ratio>)) #F)

(define-method = ((number-1 <ratio>) (number-2 <ratio>))
  (and (= (numerator number-1) (numerator number-2))
       (= (denominator number-1) (denominator number-2))))

(define-method < ((number-1 <ratio>) (number-2 <ratio>))
  (bind ((numerator-1 (* (numerator number-1) (denominator number-2)))
         (numerator-2 (* (numerator number-2) (denominator number-1))))
    (< numerator-1 numerator-2)))

(define-method + ((number-1 <ratio>) (number-2 <ratio>))
  (make-ratio* (+ (* (numerator number-1) (denominator number-2))
		  (* (numerator number-2) (denominator number-1)))
	       (* (denominator number-1) (denominator number-2))))

(define-method * ((number-1 <ratio>) (number-2 <ratio>))
  (make-ratio* (* (numerator number-1) (numerator number-2))
	       (* (denominator number-1) (denominator number-2))))

(define-method - ((number-1 <ratio>) (number-2 <ratio>))
  (make-ratio* (- (* (numerator number-1) (denominator number-2))
		  (* (numerator number-2) (denominator number-1)))
	       (* (denominator number-1) (denominator number-2))))

(define-method / ((number-1 <ratio>) (number-2 <ratio>))
  (make-ratio* (* (numerator number-1) (denominator number-2))
	       (* (denominator number-1) (numerator number-2))))

(define-method negative ((number <ratio>))
  (make-ratio* (- (numerator number)) (denominator number)))
