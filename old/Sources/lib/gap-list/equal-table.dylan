module:    internal
language:  prefix-dylan
author:    Eliot Miranda and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-class <equal-table> (<table>))

(define-method table-protocol ((table <equal-table>))
  (values = equal-hash))

;;;; EQUAL-HASH

(define-method equal-hash (object) 
  (values 57 $permanent-hash-state))

(define-method equal-hash ((object <character>))
  (object-hash object))

(define-method equal-hash ((object <boolean>))
  (object-hash object))

(define-method equal-hash ((object <number>)) 
  (object-hash object))

(define-method equal-hash ((object <type>)) 
  (object-hash object))

(define-method equal-hash ((object <function>)) 
  (object-hash object))

(define-method equal-hash ((object <condition>)) 
  (object-hash object))

(define-method equal-hash ((object <symbol>)) 
  (equal-hash (as <string> object)))

(define-method equal-hash ((collection <collection>))
  (equal-hash-with-depth collection 0))

(define $equal-hash-max-length 3)
(define $equal-hash-max-depth 3)

(define-method equal-hash-with-depth (object (depth <integer>))
  (equal-hash object))

(define-method equal-hash-with-depth
    ((collection <collection>) (depth <integer>))
  (if (= depth $equal-hash-max-depth)
      (values 0 $permanent-hash-state)
      (bind ((hash 2) (state $permanent-hash-state))
        ;; !@#$ NEW SYNTAX (element keyed-by key in collection)
	(for ((key in (key-sequence collection))
	      ((index <integer>) from 0 below $equal-hash-max-length))
	  (bind ((element (element collection key))
                 (element-hash element-state
		  (equal-hash-with-depth element (+ depth 1)))
		 (key-hash key-state
		  (equal-hash-with-depth key (+ depth 1)))
		 (key-element-hash key-element-state
		  (merge-hash-codes
		   element-hash element-state key-hash  key-state
                   ordered: #T))
		 (new-hash new-state
		  (merge-hash-codes
		   hash state key-element-hash key-element-state
                   ordered: #F)))
	    (set! hash new-hash)
	    (set! state new-state))
	  finally (values hash state)))))

(define-method equal-hash-with-depth 
    ((collection <sequence>) (depth <integer>))
  (if (= depth $equal-hash-max-depth)
      (values 0 $permanent-hash-state)
      (bind-methods 
	  ((next-hash-code (hash state index)
	     (bind ((element-hash element-state
		     (equal-hash-with-depth
		      (element collection index) (+ depth 1))))
	       (merge-hash-codes
		hash state element-hash element-state ordered: #T))))
        (bind ((size (size collection)))
          (case size 
            ((0) (values 21011959 $permanent-hash-state))
            ((1) (next-hash-code 1 $permanent-hash-state 0))
            ((2) (bind ((element-hash element-code
			 (next-hash-code 2 $permanent-hash-state 0)))
		   (next-hash-code element-hash element-code 1)))
            (else: 
	     (bind ((mid (truncate/ size 2))
		    (element-0-hash element-0-code
		     (next-hash-code size $permanent-hash-state 0))
		    (element-mid-hash element-mid-code
		     (next-hash-code element-0-hash element-0-code mid)))
	       (next-hash-code
                element-mid-hash element-mid-code (- size 1)))))))))

(define-method equal-hash-with-depth ((collection <list>) (depth <integer>))
  (if (iterate grovel ((ptr collection)) ;; !@#$ could use proper? here
	(or (id? ptr '()) 
            (bind ((tail (tail ptr)))
              (and (instance? tail <list>) (grovel tail)))))
      (next-method) ;; proper then use standard method
      (equal-hash-with-depth (head collection) (+ depth 1))))
