module:    extended-library
language:  prefix-dylan
author:    Eliot Miranda and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;; LITERAL TABLES

(define-class <literal-table> (<equal-table>))

(define-method key=
    ((table <literal-table>) foo bar #values (equal <boolean>))
  (and (id? (object-class foo) (object-class bar))
       (= foo bar)))

;; statistics

(define-method number-probes
    ((table <table-e>) key #values (number-probes <integer>))
  (bind ((void (void-element table))
         (mask (mask table))
         (elements (elements table)))
    (iterate search 
         (((i <integer>) (logand mask (key-hash table key))) ((n <integer>) 0))
       (bind ((probe (element elements i)))
	 (if (id? void probe)
	     (error "Internal Error")
	     (if (key= table key probe)
	         n
	         (search (logand (+ i 2) mask) (+ n 1))))))))

(define-method compute-statistics ((table <table-e>))
  (bind (((probe-depth-total <single-float>) 0.0))
    (for ((key in (key-sequence table)))
      (set! probe-depth-total (+ probe-depth-total (number-probes table key))))
    (values 
     (if (= (size table) 0) 0 (/ probe-depth-total (size table)))
     (/ (size table) (as <single-float> (/ (size (elements table)) 2))))))

(define-method distribution-table ((table <table-e>))
  (bind ((stats (make <equal-table>)))
    (for ((key in (key-sequence table)))
      (bind ((class/hash
              (list (object-class key) 
                    (logand (key-hash table key) (mask table)))))
        (set! (element stats class/hash)
              (+ (element stats class/hash default: 0) 1))))
    stats))

