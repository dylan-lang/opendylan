module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method precision ((class (singleton <integer>))) 31)

(define-method most-positive ((class (singleton <integer>)))
  (lognot (most-negative <integer>)))

(define-method most-negative ((class (singleton <integer>)))
  (ash 1 (- (precision <integer>) 1)))

(define-method length ((integer <integer>))
  (for (((shift <integer>) from (- 1 (precision <integer>)))
	(until (or (positive? shift)
		   (= (logand (ash integer shift) 1) 1))))
    finally (+ (- shift) 1)))

(define-method logeqv ((x <integer>) (y <integer>))
  (%integer
   (primitive-integer-not 
    (primitive-integer-xor (%integer-data x) (%integer-data y)))))

(define-method lognand ((integer-1 <integer>) (integer-2 <integer>))
  (%integer
   (primitive-integer-not
    (primitive-integer-and
     (%integer-data integer-1) (%integer-data integer-2)))))

(define-method lognor ((integer-1 <integer>) (integer-2 <integer>))
  (%integer
   (primitive-integer-not
    (primitive-integer-or
     (%integer-data integer-1) (%integer-data integer-2)))))

(define-method logandc1 ((integer-1 <integer>) (integer-2 <integer>))
  (%integer
   (primitive-integer-and 
    (primitive-integer-not (%integer-data integer-1))
    (%integer-data integer-2))))

(define-method logandc2 ((integer-1 <integer>) (integer-2 <integer>))
  (%integer
   (primitive-integer-and 
    (%integer-data integer-1)
    (primitive-integer-not (%integer-data integer-2)))))

(define-method logorc1 ((integer-1 <integer>) (integer-2 <integer>))
  (%integer
   (primitive-integer-or 
    (primitive-integer-not (%integer-data integer-1))
    (%integer-data integer-2))))

(define-method logorc2 ((integer-1 <integer>) (integer-2 <integer>))
  (%integer
   (primitive-integer-or 
    (%integer-data integer-1)
    (primitive-integer-not (%integer-data integer-2)))))

(define-method prime? ((x <integer>))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (even? x))
	   (not (zero? (remainder x 3)))
	   (iterate grovel
	       (((q <integer>) 6) ((r <integer>) 1)
		((inc <integer>) 2) ((d <integer>) 5))
	     (if (or (= r 0) (> d q))
		 (/= r 0)
		 (bind ((next-d (+ d inc)))
		   (grovel (truncate/ x d) (remainder x d)
			   (logxor inc 6) next-d)))))))

(define-method expt ((base <integer>) (power <integer>))
  (cond ((negative? power)
	 (/ (expt base (- power))))
	((= base 2)
	 (ash 1 power))
	(else:
	 (for (((next-n <integer>) = (ash power -1) then (ash power -1))
	       ((total <integer>) = (if (odd? power) base 1)
		then (if (odd? power) (* base total) total))
	       (until (zero? next-n)))
	   (set! base (* base base))
	   (set! power next-n)
	   finally total))))

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
    ((new-byte <integer>) (byte-specifier <byte-specifier>)
     (integer <integer>))
  (bind (((mask <integer>) (- (ash 1 (byte-size byte-specifier)) 1)))
    (logior (logand integer (lognot (ash mask (byte-position byte-specifier))))
	    (ash (logand new-byte mask) (byte-position byte-specifier)))))

(define-method deposit-field
    ((new-byte <integer>) (byte-specifier <byte-specifier>)
     (integer <integer>))
  (bind (((mask <integer>) (ash (ldb (byte (byte-size byte-specifier) 0) -1)
				(byte-position byte-specifier))))
    (logior (logand new-byte mask)
	    (logand integer (lognot mask)))))
