module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method angle ((real <real>)) 0)

(define-method phase ((number <real>)) (as (object-class number) 0))

(define-method expt ((base <real>) (power <real>))
  (expt (as <single-float> base) (as <single-float> power)))

(define-method sin ((number <real>))
  (%single-float (primitive-single-float-sin
		  (%single-float-data (as <single-float> number)))))
(define-method cos ((number <real>))
  (%single-float (primitive-single-float-cos
		  (%single-float-data (as <single-float> number)))))
(define-method cis ((number <real>))
  (make-rectangular (cos number) (sin number)))
(define-method tan ((number <real>))
  (%single-float (primitive-single-float-tan
		  (%single-float-data (as <single-float> number)))))
(define-method asin ((number <real>))
  (if (> (abs number) 1)
      (asin (make-rectangular number (as (object-class number) 0)))
      (%single-float (primitive-single-float-asin
		      (%single-float-data (as <single-float> number))))))
(define-method acos ((number <real>))
  (if (> (abs number) 1)
      (acos (make-rectangular number (as (object-class number) 0)))
      (%single-float (primitive-single-float-acos
		      (%single-float-data (as <single-float> number))))))
(define-method atan ((number <real>))
  (%single-float (primitive-single-float-atan
		  (%single-float-data (as <single-float> number)))))
(define-method atan2 ((numerator <real>) (denominator <real>))
  (if (and (zero? numerator) (zero? denominator))
      (if (negative? (sign (as <float> denominator)))
	  numerator
	  (if (negative? (sign (as <float> numerator)))
	      (- $pi)
	      $pi))
      (%single-float (primitive-single-float-atan2 
		      (%single-float-data (as <single-float> numerator))
		      (%single-float-data (as <single-float> denominator))))))
(define-method sinh ((number <real>))
  (%single-float (primitive-single-float-sinh
		  (%single-float-data (as <single-float> number)))))
(define-method cosh ((number <real>))
  (%single-float (primitive-single-float-cosh
		  (%single-float-data (as <single-float> number)))))
(define-method tanh ((number <real>))
  (%single-float (primitive-single-float-tanh
		  (%single-float-data (as <single-float> number)))))
(define-method asinh ((number <real>))
  (%single-float (primitive-single-float-asinh
		  (%single-float-data (as <single-float> number)))))
(define-method acosh ((number <real>))
  (%single-float (primitive-single-float-acosh
		  (%single-float-data (as <single-float> number)))))
(define-method atanh ((number <real>))
  (%single-float (primitive-single-float-atanh
		  (%single-float-data (as <single-float> number)))))

(define-method exp ((number <number>))
  (expt $e number))

(define-method log/ ((number <number>) (base <number>))
  (/ (log number) (log base)))

;; !@#$ INCOMPATIBLE WITH ONE DEFINED IN NUMBER.DYLAN

(define-method log ((number <real>))
  (if (negative? number)
      (log (make-rectangular number 0.0))
      (%single-float (primitive-single-float-log
		      (%single-float-data (as <single-float> number))))))

(define-method sqrt ((number <real>))
  (if (negative? number)
      (sqrt (make-rectangular number 0))
      (%single-float (primitive-single-float-sqrt
		      (%single-float-data (as <single-float> number))))))

