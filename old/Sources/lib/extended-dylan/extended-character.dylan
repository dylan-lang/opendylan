module:    dylan-print
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method standard? ((character <character>))
  #T)

(define $delete-character (as <character> 127))

(define-method graphic? ((character <character>))
  (bind (((code <integer>) (as <integer> character)))
    (and (>= code (as <integer> #\Space))
	 (< code (as <integer> $delete-character)))))

(define-method digit? ((character <character>) #key (radix 10))
  (bind (((code <integer>) (as <integer> character)))
    (or (and (>= code (as <integer> #\0)) (<= code (as <integer> #\9))
	     (- code (as <integer> #\0)))
	(and (> radix 10) (< radix 36)
	     (or (and (>= code (as <integer> #\A))
		      (< (- code (as <integer> #\A)) (- radix 10))
		      (+ (- code (as <integer> #\A)) 10))
		 (and (>= code (as <integer> #\a))
		      (< (- code (as <integer> #\a)) (- radix 10))
		      (+ (- code (as <integer> #\a)) 10)))))))

(define-method alphanumeric? ((character <character>))
  (or (alpha? character) (digit? character)))

(define-method both-case? ((character <character>))
  (not (alpha? character)))
