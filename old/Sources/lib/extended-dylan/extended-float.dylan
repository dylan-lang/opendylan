module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method expt ((base <single-float>) (power <single-float>))
  (%single-float (primitive-single-float-expt
		  (%single-float-data (as <single-float> base))
		  (%single-float-data (as <single-float> power)))))

(define-method rationalize ((x <real>))
  (rationalize-with-epsilon x (epsilon <single-float>)))

(define-method rationalize-with-epsilon ((x <real>) (eps <real>))
  ;; ORIGINALLY WRITTEN BY GOSPER
  (cond ((negative? x) (- (rationalize-with-epsilon (- x) eps)))
	((zero? x) 0)
	(else:
	 (bind (((y <real>) 0)
		((a <integer>) 0)
		((x-class <class>) (object-class x)))
	   (bind-methods
	     ((looper (xx onum oden num den)
		(if (or (zero? den)
			(> (abs
			    (/ (- x (/ (as x-class num) (as x-class den))) x))
			   eps))
		    (looper (set! y (/ (as x-class 1.0) (- xx (as x-class a))))
			    num den
			    (+ (* (set! a (truncate y)) num) onum)
			    (+ (* a den) oden))
		    (make-ratio* num den))))
	     (looper x 1 0 (set! a (truncate x)) 1))))))

;;;;;
;;;;; EXTENSIONS -- TRANSLATED FROM CMUCL
;;;;;

(define-method make-using-decoded-bits
    ((class (singleton <single-float>))
     (sign <integer>) (exponent <integer>) (significand <integer>))
  (%single-float
   (primitive-decoded-bits-as-single-float 
    (%integer-data sign) 
    (%integer-data exponent)
    (%integer-data significand))))

(define-method make-using-bits
    ((class (singleton <single-float>)) (bits <integer>))
  (%single-float (primitive-bits-as-single-float (%integer-data bits))))

(define-method sign-shift ((class (singleton <single-float>))) 31)

(define-method sign-byte ((class (singleton <single-float>)))
  (byte 1 (sign-shift <single-float>)))

(define-method exponent-byte ((class (singleton <single-float>))) (byte 8 23))

(define-method significand-byte
    ((class (singleton <single-float>))) (byte 23 0))

(define-method radix ((class (singleton <single-float>))) 2)

(define-method bias ((float (singleton <single-float>))) 126)

(define-method normal-exponent-max ((float (singleton <single-float>))) 254)

(define-method normal-exponent-min ((float (singleton <single-float>))) 1)

(define-method hidden-bit ((float (singleton <single-float>))) (ash 1 23))

(define-method trapping-nan-bit ((float (singleton <single-float>))) (ash 1 22))

(define-method digits ((float (singleton <single-float>)))
  (+ (byte-size (significand-byte <single-float>)) 1))

(define-method digits ((float <single-float>)) (digits <single-float>))

(define-method most-positive ((class (singleton <single-float>)))
  (make-using-decoded-bits
   class 0 (normal-exponent-max class) (ldb (significand-byte class) -1)))

(define-method most-negative ((class (singleton <single-float>)))
  (make-using-decoded-bits
   class 1 (normal-exponent-max class) (ldb (significand-byte class) -1)))

(define-method least-positive ((class (singleton <single-float>)))
  (make-using-decoded-bits class 0 0 1))

(define-method least-negative ((class (singleton <single-float>)))
  (make-using-decoded-bits class 1 0 1))

(define-method least-positive-normalized ((class (singleton <single-float>)))
  (make-using-decoded-bits class 0 (normal-exponent-min class) 0))

(define-method least-negative-normalized ((class (singleton <single-float>)))
  (make-using-decoded-bits class 1 (normal-exponent-min class) 0))

(define-method positive-infinity ((class (singleton <single-float>)))
  (make-using-decoded-bits class 0 (+ (normal-exponent-max class) 1) 0))

(define-method negative-infinity ((class (singleton <single-float>)))
  (make-using-decoded-bits class 1 (+ (normal-exponent-max class) 1) 0))

(define-method epsilon ((class (singleton <single-float>)))
  (make-using-decoded-bits class 0 (- (bias class) (- (digits class) 1)) 1))

(define-method negative-epsilon ((class (singleton <single-float>)))
  (make-using-decoded-bits class 0 (- (bias class) (digits class)) 1))

(define-method normalized? ((number <single-float>))
  (and (zero? (ldb (exponent-byte <single-float>) (bits number)))
       (not (zero? number))))

;;; !@#$ THE FOLLOWING FOUR HAVEN'T BEEN TESTED

(define-method saturated-exponent? ((number <single-float>))
  (= (ldb (exponent-byte <single-float>) (bits number))
     (ldb (exponent-byte <single-float>) -1)))

(define-method infinity? ((number <single-float>))
  (and (saturated-exponent? number)
       (zero? (ldb (significand-byte <single-float>) (bits number)))))

(define-method nan? ((number <single-float>))
  (and (saturated-exponent? number)
       (zero? (logand (ldb (significand-byte <single-float>) (bits number))
		      (trapping-nan-bit <single-float>)))))

(define-method trapping-nan? ((number <single-float>))
  (and (saturated-exponent? number)
       (not (zero? (logand (ldb (significand-byte <single-float>)
                                (bits number))
			   (trapping-nan-bit <single-float>))))))

(define-method precision ((number <float>))
  (cond ((zero? number) 0)
	((denormalized? number)
	 (bind ((ignore (exponent <integer>)
		 (denormalized-integer-decode number)))
	   (+ (digits number)
	      (- (digits number) 1) (bias (object-class number)) exponent)))
	(else: (digits number))))

(define-method denormalized? ((number <single-float>))
  (and (zero? (ldb (exponent-byte <single-float>) (bits number)))
       (not (zero? number))))

(define-method denormalized-integer-decode ((x <single-float>))
  (bind (((bits <integer>) (bits (abs x)))
	 ((sig <integer>) (ash (ldb (significand-byte <single-float>) bits) 1))
	 ((extra-bias <integer>) 0))
    (while (not (zero? (logand sig (hidden-bit <single-float>))))
      (set! sig (ash sig 1))
      (set! extra-bias (+ extra-bias 1)))
    (values sig
	    (- (- (bias <single-float>)) (digits <single-float>) extra-bias)
	    (sign x 1))))

;;;   INTEGER-DECODE Returns three values:
;;;   1) an integer representation of the significand.
;;;   2) the exponent for the power of 2 that the significand must be multiplied
;;;      by to get the actual value.  This differs from the DECODE-FLOAT
;;;      exponent by FLOAT-DIGITS, since the significand has been scaled to
;;;      have all its digits before the radix point.
;;;   3) -1 or 1 (i.e. the sign of the argument.)

(define-method integer-decode ((x <single-float>))
  (bind (((bits <integer>) (bits (abs x)))
	 ((exp <integer>) (ldb (exponent-byte <single-float>) bits))
	 ((sig <integer>) (ldb (significand-byte <single-float>) bits))
	 ((sign <integer>) (sign x 1))
	 ((biased <integer>)
	  (- exp (bias <single-float>) (digits <single-float>))))
    (unless (<= exp (normal-exponent-max <single-float>))
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((and (zero? exp) (zero? sig))
	   (values 0 biased sign))
	  ((< exp (normal-exponent-min <single-float>))
	   (denormalized-integer-decode x))
	  (else:
	   (values (logior sig (hidden-bit <single-float>)) biased sign)))))

(define-method denormalized-decode ((x <single-float>))
  (bind (((sig <integer>) (exp <integer>) (sign <integer>)
	  (denormalized-integer-decode x)))
    (values (make-using-bits
	     <single-float>
	     (dpb sig (significand-byte <single-float>)
		  (dpb (bias <single-float>) (exponent-byte <single-float>) 0)))
	    (+ exp (digits <single-float>))
	    (as <single-float> sign))))

;;;   DECODE Returns three values:
;;;   1) a floating-point number representing the significand.  This is always
;;;      between 0.5 (inclusive) and 1.0 (exclusive).
;;;   2) an integer representing the exponent.
;;;   3) -1.0 or 1.0 (i.e. the sign of the argument).

(define-method decode ((x <single-float>))
  (bind (((bits <integer>) (bits (abs x)))
	 ((exp <integer>) (ldb (exponent-byte <single-float>) bits))
	 ((sign <single-float>) (sign x 1.0))
	 ((biased <integer>) (- exp (bias <single-float>))))
    (unless (<= exp (normal-exponent-max <single-float>))
      (error "Can't decode NAN or infinity: ~S." x))
    (cond ((zero? x)
	   ;;; !@#$ CANT REAL 0.0f0 yet
	   (values 0.0 biased sign))
	  ((< exp (normal-exponent-min <single-float>))
	   (denormalized-decode x))
	  (else:
	   (values (make-using-bits
		    <single-float>
		    (dpb (bias <single-float>) (exponent-byte <single-float>)
			 bits))
		   biased sign)))))

;;; SCALE-FLOAT-MAYBE-UNDERFLOW  --  Internal
;;; Handle float scaling where the X is denormalized or the result is
;;; denormalized or underflows to 0.

(define-method scale-maybe-underflow ((x <single-float>) (exp <integer>))
  (bind (((sig <integer>) (old-exp <integer>) (integer-decode x))
	 ((digits <integer>) (digits x))
	 ((new-exp <integer>) (+ exp old-exp digits (bias <single-float>)))
	 ((the-sign <integer>) (sign x 1)))
    (cond
      ((< new-exp (normal-exponent-min <single-float>))
#|
       (when (current-float-trap :inexact)
	 (error 'floating-point-inexact :operation 'scale-float
		:operands (list x exp)))
       (when (current-float-trap :underflow)
	 (error 'floating-point-underflow :operation 'scale-float
		:operands (list x exp)))
|#
       (bind (((shift <integer>) (- new-exp 1)))
	 (if (< shift (- (- digits 1)))
	     (sign x 0.0)
	     (make-using-decoded-bits
	      <single-float> the-sign 0 (ash sig shift)))))
      (else:
       (make-using-decoded-bits the-sign new-exp sig)))))


;;; SCALE-MAYBE-OVERFLOW  --  Internal
;;; Called when scaling a float overflows, or the oringinal float was a NaN
;;; or infinity.  If overflow errors are trapped, then error, otherwise return
;;; the appropriate infinity.  If a NaN, signal or not as appropriate.

(define-method scale-maybe-overflow ((x <single-float>) (exp <integer>))
  (cond
   ((infinity? x)
    ;; Infinity is infinity, no matter how small...
    x)
   ((nan? x)
#|
    (when (and (trapping-nan? x)
	       (current-float-trap :invalid))
      (error 'floating-point-invalid :operation 'scale-float
	     :operands (list x exp)))
|#
    x)
   (else:
#|    
    (when (current-float-trap :overflow)
      (error 'floating-point-overflow :operation 'scale-float
	     :operands (list x exp)))
    (when (current-float-trap :inexact)
      (error 'floating-point-inexact :operation 'scale-float
	     :operands (list x exp)))
|#
    (* (sign x 1.0)
       (positive-infinity <single-float>)))))

;;; SCALE returns the value (* f (expt (float 2 f) ex)), but with no
;;; unnecessary loss of precision or overflow.

(define-method scale ((x <single-float>) (exp <integer>))
  (bind (((bits <integer>) (bits x))
	 ((old-exp <integer>) (ldb (exponent-byte <single-float>) bits))
	 ((new-exp <integer>) (+ old-exp exp)))
    (cond
     ((zero? x) x)
     ((or (< old-exp (normal-exponent-min <single-float>))
	  (< new-exp (normal-exponent-min <single-float>)))
      (scale-maybe-underflow x exp))
     ((or (> old-exp (normal-exponent-max <single-float>))
	  (> new-exp (normal-exponent-max <single-float>)))
      (scale-maybe-overflow x exp))
     (else:
      (make-using-bits
       <single-float>
       (dpb new-exp (exponent-byte <single-float>) bits))))))
