module:    random
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; !@#$ UNTESTED

;;; !@#$ THIS IS FROM extended-integer.dylan
(define-method precision ((class (singleton <integer>))) 30)

(define $random-constant-a 8373)
(define $random-constant-c 101010101)
(define $random-upper-bound (- (most-positive <integer>) 1))
(define $random-max 54)
(define $integer-length (precision <integer>))

(define-variable *random-seed* 0)

(define-method random-1 ()
  (set! *random-seed*
	(modulo
         (+ (* *random-seed* $random-constant-a) $random-constant-c)
	 (+ $random-upper-bound 1))))

(define-class <random-state> (<object>)
  (random-state-j init-keyword: j: init-value: 24 type: <integer>)
  (random-state-k init-keyword: k: init-value: 0  type: <integer>)
  (seed           init-keyword: seed:
		  init-function:
                  (method ()
		    (bind (((vector <simple-object-vector>)
			    (make <vector> size: (+ $random-max 1))))
		      (for ((index from 0 below (+ $random-max 1)))
			(set! (element vector index) (random-1)))
		      vector))))

(define *random-state* (make <random-state>))

(define-method random-3 ((state <random-state>))
  (bind (((seed <simple-object-vector>) (seed state))
	 ((j <integer>) (random-state-j state))
	 ((k <integer>) (random-state-k state)))
    (set! (element seed k)
	  (bind (((a <integer>)
		  (- $random-upper-bound
		     (element seed (set! (random-state-j state)
					 (if (= j 0) $random-max (- j 1))))
		     (element seed (set! (random-state-k state)
					 (if (= k 0) $random-max (- k 1)))))))
	    (if (negative? a) (- a) (- $random-upper-bound a))))))

(define-method copy ((state <random-state>))
  (make <state>
	seed: (shallow-copy (seed state))
	j:    (random-state-j state)
	k:    (random-state-k state)))

(define $no-state (list "NO STATE"))

(define-method make ((class (singleton <random-state>)) #key (state $no-state))
  (cond ((id? state $no-state) (copy *random-state*))
	((instance? state <random-state>) (copy state))
	((id? state #T)
	 (set! *random-seed* (get-universal-time))
	 (next-method class))
	(else:
	 (error "Bad argument, ~A, for RANDOM-STATE" state))))

(define-method random ((number <integer>) #key (state *random-state*))
  (if (positive? number)
      (make <integer> data: (remainder (random-3 state) number))
      (error "Non-positive argument, ~A, to RANDOM" number)))
      
(define-method random ((number <float>) #key (state *random-state*))
  (if (positive? number)
      (bind (((length <integer>) (digits number)))
	(* number (/ (as <float> (random (ash 2 length) state))
		     (as <float> (ash 2 length)))))
      (error "Non-positive argument, ~A, to RANDOM" number)))
