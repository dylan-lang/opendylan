module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; <SET>

(define-method print
    ((collection <set>) #key (stream *standard-output*) (verbose? #F))
  (format stream "[")
  (for ((first? = #T then #F)
        (state = (initial-state collection)
               then (next-state collection state))
        (while state))
    (unless first?
      (format stream " "))
    (print (current-element collection state) 
           stream: stream verbose?: verbose?))
  (format stream "]")
  collection)

;;;; <ASSOCIATION>

(define-method print
    ((object <association>) #key (stream *standard-output*) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "key: ~S value: ~S" (key object) (value object))))

;;;; <RECTANGULAR-COMPLEX>

(define-method print
    ((complex <rectangular-complex>) 
     #key (stream *standard-output*) (verbose? #F) (base 10))
  (format stream "#C(")
  (print (real-part complex) stream: stream verbose?: verbose? base: base)
  (print #\Space stream: stream)
  (print (imag-part complex) stream: stream verbose?: verbose? base: base)
  (print #\) stream: stream)
  complex)

;;;; <RATIO>

(define-method print 
    ((ratio <ratio>) #key (stream *standard-output*) (verbose? #F) (base 10))
  (print (numerator ratio) stream: stream verbose?: verbose? base: base)
  (print #\/ stream: stream)
  (print (denominator ratio) stream: stream verbose?: verbose? base: base)
  ratio)

;;;; <FLOAT>

;;; Float printing.
;;;
;;;  Written by Bill Maddox
;;;
;;;  Translated to Dylan by Jonathan Bachrach 26Dec92
;;;
;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or #F.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-#F value of FDIGITS has been
;;;                specified.  Field overflow is not considerd an error at this
;;;                level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or #F, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non #F, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non #F, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;
;;; WARNING: For efficiency, there is a single string object *digit-string*
;;; which is modified destructively and returned as the value of
;;; FLONUM-TO-STRING.  Thus the returned value is not valid across multiple 
;;; calls.
;;;
;;; NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!

(define $digits "0123456789")

(define-variable *digit-string* (make <stretchy-vector> size: 50 fill: #\?)) 

(define-method flonum-to-string ((x <float>) #key width fdigits scale fmin)
  (cond ((zero? x)
	 ;;zero is a special case which float-string cannot handle
	 (if fdigits
	     (bind ((s (make <string> size: (+ fdigits 1) fill: #\0)))
	       (set! (element s 0) #\.)
	       (values s (size s) #t (zero? fdigits) 0))
	     (values "." 1 #t #t 0)))
	(else:
	 (set! (size *digit-string*) 0)
	 (bind (((sig <integer>) (exp <integer>) (integer-decode x))
		((the-precision <integer>) (precision x))
		((the-digits <integer>) (digits x))
		((fudge <integer>) (- the-digits the-precision))
		((width <integer>) (if width (max width 1) #F)))
	   (float-string (ash sig (- fudge)) (+ exp fudge) the-precision width
			 fdigits scale fmin)))))

(define-method float-string
    ((fraction <integer>) (exponent <integer>) (precision <integer>)
     (width <object>) (fdigits <object>) (scale <object>) (fmin <object>))
  (bind (((r <integer>) fraction) ((s <integer>) 1) ((m- <integer>) 1)
	 ((m+ <integer>) 1) ((k <integer>) 0)
	 ((digits <integer>) 0) ((decpnt <integer>) 0)
	 (cutoff #F) (roundup #F) (u #F) (low #F) (high #F))
    ;; Represent fraction as r/s, error bounds as m+/s and m-/s.
    ;; Rational arithmetic avoids loss of precision in subsequent calculations.
    (cond ((> exponent 0)
	   (set! r (ash fraction exponent))
	   (set! m- (ash 1 exponent))	   
	   (set! m+ m-))                   
	  ((< exponent 0)
	   (set! s (ash 1 (- exponent)))))
    ;; adjust the error bounds m+ and m- for unequal gaps
    (when (= fraction (ash 1 precision))
      (set! m+ (ash m+ 1))
      (set! r (ash r 1))
      (set! s (ash s 1)))
    ;;scale value by requested amount, and update error bounds
    (when scale
      (if (negative? scale)
	  (bind ((scale-factor (expt 10 (- scale))))
	    (set! s (* s scale-factor)))
	  (bind ((scale-factor (expt 10 scale)))
	    (set! r (* r scale-factor))
	    (set! m+ (* m+ scale-factor))
	    (set! m- (* m- scale-factor)))))
    ;;scale r and s and compute initial k, the base 10 logarithm of r
    (until (>= r (ceiling/ s 10))
      (set! k (- k 1))
      (set! r (* r 10))
      (set! m- (* m- 10))
      (set! m+ (* m+ 10)))
    (bind-exit (return)
      (while #T
	(until (< (+ (ash r 1) m+) (ash s 1))
	  (set! s (* s 10))
	  (set! k (+ k 1)))
	;;determine number of fraction digits to generate
	(cond (fdigits
	       ;;use specified number of fraction digits
	       (set! cutoff (- fdigits))
	       ;;don't allow less than fmin fraction digits
	       (when (and fmin (> cutoff (- fmin)))
                 (set! cutoff (- fmin))))
	      (width
	       ;;use as many fraction digits as width will permit
	       ;;but force at least fmin digits even if width will be exceeded
	       (if (< k 0)
		   (set! cutoff (- 1 width))
		   (set! cutoff (+ (- k width) 1)))
	       (when (and fmin (> cutoff (- fmin)))
                 (set! cutoff (- fmin)))))
	;;If we decided to cut off digit generation before precision has
	;;been exhausted, rounding the last digit may cause a carry propagation.
	;;We can prevent this, preserving left-to-right digit generation, with
	;;a few magical adjustments to m- and m+.  Of course, correct rounding
	;;is also preserved.
	(when (or fdigits width)
	  (bind (((a <integer>) (- cutoff k))
		 ((y <integer>) s))
	    (if (>= a 0)
		(for ((i from 0 below a)) (set! y (* y 10)))
		(for ((i from 0 below (- a))) (set! y (ceiling/ y 10))))
	    (set! m- (max y m-))
	    (set! m+ (max y m+))
	    (when (= m+ y) (set! roundup #t))))
	(when (< (+ (ash r 1) m+) (ash s 1)) (return))))
    ;; zero-fill before fraction if no integer part
    (when (< k 0)
      (set! decpnt digits)
      (add! *digit-string* #\.)
      (for ((i from 0 below (- k)))
	(set! digits (+ digits 1)) (add! *digit-string* #\0)))
    ;; generate the significant digits
    (bind-exit (return)
      (while #T
	(set! k (- k 1))
	(when (= k -1)
	  (add! *digit-string* #\.)
	  (set! decpnt digits))
	(bind ((nu nr (truncate/ (* r 10) s))) (set! u nu) (set! r nr))
	(set! m- (* m- 10))
	(set! m+ (* m+ 10))
	(set! low (< (ash r 1) m-))
	(if roundup
	    (set! high (>= (ash r 1) (- (ash s 1) m+)))
	    (set! high (> (ash r 1) (- (ash s 1) m+))))
	;;stop when either precision is exhausted or we have printed as many
	;;fraction digits as permitted
	(when (or low high (and cutoff (<= k cutoff))) (return))
	(add! *digit-string* (element $digits u))
	(set! digits (+ digits 1))))
    ;; if cutoff occured before first digit, then no digits generated at all
    (when (or (not cutoff) (>= k cutoff))
      ;;last digit may need rounding
      (add! *digit-string*
	    (element $digits
		     (cond ((and low (not high)) u)
			   ((and high (not low)) (+ u 1))
			   (else: (if (<= (ash r 1) s) u (+ u 1))))))
      (set! digits (+ digits 1)))
    ;; zero-fill after integer part if no fraction
    (when (>= k 0)
      (for ((i from 0 below k))
	(set! digits (+ digits 1))
	(add! *digit-string* #\0))
      (add! *digit-string* #\.)
      (set! decpnt digits))
    ;; add trailing zeroes to pad fraction if fdigits specified
    (when fdigits
      (for ((i from 0 below (- fdigits (- digits decpnt))))
	(set! digits (+ digits 1))
	(add! *digit-string* #\0)))
    ;; all done
    (values *digit-string* (+ digits 1) (= decpnt 0) (= decpnt digits) decpnt)))

;;; SCALE-EXPONENT  --  Internal
;;;
;;; Given a non-negative floating point number, SCALE-EXPONENT returns a new
;;; floating point number Z in the range (0.1, 1.0] and and exponent E such
;;; that Z * 10^E is (approximately) equal to the original number.  There may
;;; be some loss of precision due the floating point representation.  The
;;; scaling is always done with long float arithmetic, which helps printing of
;;; lesser precisions as well as avoiding generic arithmetic.
;;;
;;; When computing our initial scale factor using EXPT, we pull out part of
;;; the computation to avoid over/under flow.  When denormalized, we must pull
;;; out a large factor, since there is more negative exponent range than
;;; positive range.

(define-method scale-exponent ((original-x <float>))
  (bind (((x <entended-float>) (as <extended-float> original-x))
	 ((sig <float>) (exponent <integer>) (decode x)))
    (if (= x 0.0l0)
	(values (as (object-class original-x) 0.0l0) 1)
	(bind ((ex (round (* exponent (log/ 2l0 10))))
	       (x (if (negative? ex)
		      (if (denormalized? x)
			  (* x 1.0l16 (expt 10.0l0 (- (- ex) 16)))
			  (* x 10.0l0 (expt 10.0l0 (- (- ex) 1))))
		      (/ x 10.0l0 (expt 10.0l0 (- ex 1))))))
	  (iterate div-loop ((d 1.0l0) (y x) (ex ex))
	    (if (< y 1.0l0)
		(iterate mul-loop ((m 1.0l0) (z y) (ex ex))
		  (if (>= z 0.1l0)
		      (values (as (object-class original-x) z) ex)
		      (bind ((next-m (* m 10.0l0)))
			(mul-loop next-m (* y next-m) (- ex 1)))))
		(bind ((next-d (* d 10.0l0)))
		  (div-loop next-d (/ x next-d) (+ ex 1)))))))))

;; !@#$ READ-DEFAULT-FLOAT-FORMAT -> should be in reader
(define-variable *read-default-float-format* <single-float>)

;;; PRINT-FLOAT-EXPONENT  --  Internal
;;; Print the appropriate exponent marker for X and the specified exponent.
;;;
;;; Entry point for the float printer as called by PRINT, PRIN1, PRINC,
;;; etc.  The argument is printed free-format, in either exponential or 
;;; non-exponential notation, depending on its magnitude.
;;;
;;; NOTE: When a number is to be printed in exponential format, it is scaled in
;;; floating point.  Since precision may be lost in this process, the
;;; guaranteed accuracy properties of FLONUM-TO-STRING are lost.  The
;;; difficulty is that FLONUM-TO-STRING performs extensive computations with
;;; integers of similar magnitude to that of the number being printed.  For
;;; large exponents, the bignums really get out of hand.  If bignum arithmetic
;;; becomes reasonably fast and the exponent range is not too large, then it
;;; might become attractive to handle exponential notation with the same
;;; accuracy as non-exponential notation, using the method described in the
;;; Steele and White paper.

(define-method print-float-exponent
    ((x <float>) (exp <integer>) (stream <stream>))
  ;; !@#$ PRINT-RADIX
  (bind ((*print-radix* #F))
    (if (instance? x *read-default-float-format*)
	(unless (= exp 0)
	  (if (positive? exp)
	      (format stream "e+~D" exp)
	      (format stream "e~D" exp)))
	(begin
	 (format stream "~A")
	 (select x instance?
	   ((<single-float>) #\f)
	   ((<double-float>) #\d)
	   ((<extended-float>) #\l))
	 (if (positive? exp)
	     (format stream "+~D" exp) 
	     (format stream "~D" exp))))))

;;; FLOAT-FORMAT-NAME  --  Internal
;;; Return the string name of X's float format.

(define-method float-format-name ((x <float>))
  ;; !@#$ (debug-name (object-class x))
  (select x instance?
    ((<single-float>) "SINGLE-FLOAT")
    ((<double-float>) "DOUBLE-FLOAT")
    ((<extended-float>) "EXTENDED-FLOAT")))

;;; OUTPUT-FLOAT-INFINITY  --  Internal
;;; Write out an infinity using #. notation, or flame out if
;;; *print-readably* is true and *read-eval* is false.

(define-method output-float-infinity ((x <float>) (stream <stream>))
  #|
  (cond (*read-eval*
	 (write-string "#." stream))
	(*print-readably*
	 (error "Unable to print infinities readably without #."))
	(t
	 (write-string "#<" stream)))
  |#
  (print "#<" stream: stream)
  (print (float-format-name x) stream: stream)
  (print (if (positive? x) "-POSITIVE-" "-NEGATIVE-") stream: stream)
  (print "INFINITY" stream: stream)
  ;; (unless *read-eval* ...)
  (print ">" stream: stream))

;;; OUTPUT-FLOAT-NAN  --  Internal
;;; Output a #< NaN or die trying.

(define-method output-float-nan ((x <float>) (stream <stream>))
  #|
  (when *print-readably*
    (error "Can't print NaN's readably."))
  |#
  (print "#<" stream: stream)
  (print (float-format-name x) stream: stream)
  (print (if (trapping-nan? x) " Trapping" " Quiet") stream: stream)
  (print " NaN>" stream: stream))

;;; OUTPUT-FLOAT  --  Internal
;;; Functioned called by OUTPUT-OBJECT to handle floats.

(define-method output-float ((x <float>) (stream <stream>))
  #+native:
  (cond
   ((infinity? x)
    (output-float-infinity x stream))
   ((nan? x)
    (output-float-nan x stream))
   (else:
    (bind (((x <float>)
	    (cond ((negative? (sign x 1.0))
		   (print #\- stream: stream)
		   (- x))
		  (else: x))))
      (cond
       ((zero? x)
	(print "0.0" stream: stream)
	(print-float-exponent x 0 stream))
       (else:
	(output-float-aux
	 x stream (as (object-class x) 1/1000)
	 (as (object-class x) 10000000)))))))
  #-native:
  (lisp:write (%single-float-data x))
  x)

(define-method output-float-aux
    ((x <float>) (stream <stream>) (e-min <float>) (e-max <float>))
  (if (and (>= x e-min) (< x e-max))
      ;;free format
      (bind ((str len lpoint tpoint (flonum-to-string x)))
	(when lpoint (print #\0 stream: stream))
	(print (as <string> str) stream: stream)
	(when tpoint (print #\0 stream: stream))
	(print-float-exponent x 0 stream))
      ;;exponential format 
      (bind ((f ex (scale-exponent x))
	     (str len lpoint tpoint
	      (flonum-to-string (* f 10.0)))) ;; !@#$ scale: 1
	(when lpoint (print #\0 stream: stream))
	(print (as <string> str) stream: stream)
	(when tpoint (print #\0 stream: stream))
	;; subtract out scale factor of 1 passed to flonum-to-string
	(print-float-exponent x (- ex 1) stream))))

(define-method print
    ((float <float>) #key (stream *standard-output*) (verbose? #F))
  (output-float float stream)
  float)
