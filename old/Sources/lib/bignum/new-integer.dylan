;;; Copyright (c) 1993, Jonathan Bachrach
;;; Copyright (c) 1993, Functional Objects, Inc.
;;; Copyright (c) 1993, IRCAM
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms,  with or without modifica-
;;; tion,  are not permitted  without the express prior written permission of the
;;; copyright holders.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY JONATHAN BACHRACH,  Functional Objects, Inc., 
;;; AND IRCAM ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO,  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PAR-
;;; TICULAR PURPOSE ARE  DISCLAIMED.   IN NO EVENT SHALL JONATHAN BACHRACH,   THE
;;; Functional Objects, Inc.,  OR  IRCAM BE LIABLE FOR ANY DIRECT,  INDIRECT,  IN-
;;; CIDENTAL, SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL DAMAGES  (INCLUDING,  BUT NOT 
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;   LOSS OF USE, DATA, 
;;; OR PROFITS;  OR BUSINESS INTERRUPTION)  HOWEVER CAUSED  AND  ON ANY THEORY OF 
;;; LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,  OR TORT (INCLUDING NEGLI-
;;; GENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  EVEN 
;;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-module "DYLAN")

(export '(lcm gcd binary-lcm binary-gcd prime?
	  logior logxor logand logeqv lognand lognor logandc1 logandc2 logorc1 logorc2
	  lognot logbit? ash 
	  ))

(define-method shallow-copy ((integer <integer>))
  (%integer (%integer-data integer)))

(define-method as ((class (singleton <integer>)) (ratio <ratio>))
  (truncate/ (numerator ratio) (denominator ratio)))

(define-method print
    ((integer <integer>) #key (stream *standard-output*) (verbose? #F) (base 10))
  (bind-methods ((sub-print ((integer <integer>))
		  (bind (((quotient <integer>) (remainder <integer>)
			  (truncate/ integer base)))
		    (unless (zero? quotient)
		       ;; Recurse until you have all the digits pushed on stack
		       (sub-print quotient))
		     ;; Then as each recursive call unwinds, turn the digit (in
		     ;; remainder) into a character and output the character.
		     (print (as <character>
				(if (and (> remainder 9) (> base 10))
				    (+ (as <integer> #\A)
				       (- remainder 10))
				    (+ (as <integer> #\0) remainder)))
			    stream: stream))))
    (if (negative? integer)
	(begin
	 (print #\- stream: stream)
	 (sub-print (- integer)))
	(sub-print integer))
    integer))

(define-method odd? ((integer <integer>))
  (= (logand integer 1) 1))

(define-method even? ((integer <integer>))
  (= (logand integer 1) 0))

(define-method integral? ((number <integer>)) #T)

(define-method numerator ((number <integer>)) number)

(define-method denominator ((number <integer>)) 1)

(define-method angle ((real <real>)) 0)

(define-method angle ((complex <rectangular-complex>))
  (atan2 (real-part complex) (imag-part complex)))

(define-method lcm (#rest integers)
  (reduce binary-lcm 1 integers))

(define-method gcd (#rest integers)
  (reduce binary-gcd 0 integers))

(define-method binary-lcm ((n <integer>) (m <integer>))
  (* (truncate/ (max n m) (gcd n m)) (min n m)))

(define-method binary-gcd ((integer-1 <integer>) (integer-2 <integer>))
  (cond ((= integer-1 0) integer-2)
	((= integer-2 0) integer-1)
	((= integer-1 integer-2) integer-1)
	(else:
	 (for (((k <integer>) 0 (+ k 1))
	       ((u <integer>) (abs integer-1) (ash u -1))
	       ((v <integer>) (abs integer-2) (ash v -1)))
	     ((odd? (logior u v))
	      (bind-exit (return)
		(for (((tmp <integer>)
		       (if (odd? u) (- v) (ash u -1)) (ash tmp -1))) ()
		  (when (odd? tmp)
		    (if (positive? tmp)
			(set! u tmp)
			(set! v (- tmp)))
		    (set! tmp (- u v))
		    (when (zero? tmp)
		      (return (ash u k)))))))))))

(define-method prime? ((x <integer>))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (even? x))
	   (not (zero? (remainder x 3)))
	   (for* (((q <integer>) 6 (truncate/ x d))
		  ((r <integer>) 1 (remainder x d))
		  ((inc <integer>) 2 (logxor inc 6));; 2,4,2,4,...
		  ((d <integer>) 5 (+ d inc)))
	       ((or (= r 0) (> d q)) (/= r 0))))))

(define-method binary/ ((number-1 <integer>) (number-2 <integer>))
  (make-ratio* number-1 number-2))

(define-method unary/ ((number <integer>))
  (make-ratio* 1 number))

(define-method unary- ((number <integer>))
  (- 0 number))

(define-method expt ((base <integer>) (power <integer>))
  (cond ((negative? power)
	 (/ (expt base (- power))))
	((= base 2)
	 (ash 1 power))
	(else:
	 (for (((next-n <integer>) (ash power -1) (ash power -1))
	       ((total <integer>) (if (odd? power) base 1)
		                  (if (odd? power) (* base total) total)))
	     ((zero? next-n) total)
	   (set! base (* base base))
	   (set! power next-n)))))

(define-method sqrt ((number <integer>))
  (if (negative? number)
      (sqrt (make-rectangular number 0))
      (bind (((lg <integer>) (length number))
	     ((lo <integer>) (ash 1 (ash (- lg 1) -1)))
	     ((hi <integer>) (+ lo (ash lo (if (odd? lg) -1 0)))))
	(until (<= (- hi 1) lo)
	  (bind (((mid <integer>) (ash (+ lo hi) -1)))
	    (if (<= (* mid mid) number) (set! lo mid) (set! hi mid))))
	lo)))

(define-method logbit? ((index <integer>) (integer <integer>))
  (not (zero? (logand (ash integer (- index)) 1))))

(define-method logior (#rest integers)
  (reduce binary-logior 0 integers))

(define-method logxor (#rest integers)
  (if (empty? integers) 0 (reduce1 binary-logxor integers)))

(define-method logand (#rest integers)
  (if (empty? integers) 0 (reduce1 binary-logand integers)))

(define-method logeqv (#rest integers)
  (if (empty? integers) 0 (reduce1 binary-logeqv integers)))

(define-method lognot ((integer <integer>))
  (logxor integer integer))

(define-method binary-logeqv ((x <integer>) (y <integer>))
  (lognot (logxor x y)))

(define-method lognand ((integer-1 <integer>) (integer-2 <integer>))
  (lognot (logand integer-1 integer-2)))

(define-method lognor ((integer-1 <integer>) (integer-2 <integer>))
  (lognot (logior integer-1 integer-2)))

(define-method logandc1 ((integer-1 <integer>) (integer-2 <integer>))
  (logand (lognot integer-1) integer-2))

(define-method logandc2 ((integer-1 <integer>) (integer-2 <integer>))
  (logand integer-1 (lognot integer-2)))

(define-method logorc1 ((integer-1 <integer>) (integer-2 <integer>))
  (logior (lognot integer-1) integer-2))

(define-method logorc2 ((integer-1 <integer>) (integer-2 <integer>))
  (logior integer-1 (lognot integer-2)))

;;;;;
;;;;; <small-integer>
;;;;;

(define-method precision ((class (singleton <integer>))) 31)

(define-method most-positive ((class (singleton <integer>)))
  (lognot (most-negative <integer>)))

(define-method most-negative ((class (singleton <integer>)))
  (ash 1 (- (precision <integer>) 1)))

(define-method length ((integer <integer>))
  (for ((shift (- 1 (precision <integer>)) (+ shift 1)))
       ((or (positive? shift)
	    (= (logand (ash integer shift) 1) 1)) (+ (- shift) 1))))

(define-method =hash ((integer <integer>)) integer)

(define-method binary= ((number-1 <integer>) (number-2 <integer>))
  (primitive-true?
   (primitive-integer-equals? (%integer-data number-1) (%integer-data number-2))))

(define-method binary< ((number-1 <integer>) (number-2 <integer>))
  (primitive-true?
   (primitive-integer-less-than?
    (%integer-data number-1) (%integer-data number-2))))

(define-method binary+ ((number-1 <integer>) (number-2 <integer>))
  (%integer
   (primitive-integer-plus (%integer-data number-1) (%integer-data number-2))))

(define-method binary* ((number-1 <integer>) (number-2 <integer>))
  (%integer
   (primitive-integer-times (%integer-data number-1) (%integer-data number-2))))

(define-method binary- ((number-1 <integer>) (number-2 <integer>))
  (%integer
   (primitive-integer-minus (%integer-data number-1) (%integer-data number-2))))

(define-method unary- ((number <integer>))
  (%integer (primitive-integer-negate (%integer-data number))))

(define $expt-maximum-integer-exponent 10000)

(define-method expt ((base <integer>) (power <integer>))
  (when (> (abs power) $expt-maximum-integer-exponent)
    (cerror "Continue with calculation."
	    "The absolute value of ~S exceeds ~S."
	    power '$expt-maximum-integer-exponent base power))
  (next-method))

(define-method binary-logior ((x <integer>) (y <integer>))
  (%integer (primitive-integer-or (%integer-data x) (%integer-data y))))

(define-method binary-logxor ((x <integer>) (y <integer>))
  (%integer (primitive-integer-xor (%integer-data x) (%integer-data y))))

(define-method binary-logand ((x <integer>) (y <integer>))
  (%integer (primitive-integer-and (%integer-data x) (%integer-data y))))

(define-method lognot ((integer <integer>))
  (%integer (primitive-integer-not (%integer-data integer))))

(define-method ash ((integer <integer>) (count <integer>))
  (if (binary< count 0)
      (%integer (primitive-integer-right-shift 
		 (%integer-data integer) 
		 (primitive-integer-negate (%integer-data count))))
      (%integer (primitive-integer-left-shift 
		 (%integer-data integer) (%integer-data count)))))

;;;;;
;;;;; BYTE's
;;;;;

(define-class <byte-specifier> (<object>)
  (byte-size     init-keyword: size:     type: <integer>)
  (byte-position init-keyword: position: type: <integer>))

(define-method byte ((size <integer>) (position <integer>))
  (make <byte-specifier> size: size position: position))

(define-method print
    ((byte-specifier <byte-specifier>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (byte-specifier stream)
      (format stream "SIZE: ~S POSITION: ~S"
	      (byte-size byte-specifier)
	      (byte-position byte-specifier))))

(define-method ldb ((byte-specifier <byte-specifier>) (integer <integer>))
  (logand (ash integer (- (byte-position byte-specifier)))
	  (- (ash 1 (byte-size byte-specifier)) 1)))

(define-method ldb-test ((byte-specifier <byte-specifier>) (integer <integer>))
  (not (zero? (ldb byte-specifier integer))))

(define-method mask-field
    ((byte-specifier <byte-specifier>) (integer <integer>))
  (logand integer (ash (- (ash 1 (byte-size byte-specifier)) 1)
		       (byte-position byte-specifier))))

(define-method dpb
    ((new-byte <integer>) (byte-specifier <byte-specifier>) (integer <integer>))
  (bind (((mask <integer>) (- (ash 1 (byte-size byte-specifier)) 1)))
    (logior (logand integer (lognot (ash mask (byte-position byte-specifier))))
	    (ash (logand new-byte mask) (byte-position byte-specifier)))))

(define-method deposit-field
    ((new-byte <integer>) (byte-specifier <byte-specifier>) (integer <integer>))
  (bind (((mask <integer>) (ash (ldb (byte (byte-size byte-specifier) 0) -1)
				(byte-position byte-specifier))))
    (logior (logand new-byte mask)
	    (logand integer (lognot mask)))))
