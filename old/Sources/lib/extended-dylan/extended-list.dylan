module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; !@#$ JB DUBIOUS -- NOT REALLY RETURNING <PAIR>
(define-method as ((class (singleton <pair>)) (collection <collection>))
  (as <list> collection))

(define-method =hash-with-depth ((pair <pair>) (depth <integer>))
  (if (= depth $=hash-max-depth)
      0
      (=hash-mash
       (=hash-with-depth (head pair) (+ depth 1))
       (=hash-with-depth (tail pair) (+ depth 1)))))

#|
(define-variable circularity-table (make <fast-table>))

(define-method circular? ((list <list>))
  (clear! circularity-table)
  (bind-methods
      ((circular? ((list <list>))
	 (if (empty? list)
	     #F
	     (bind ((head (first list)))
	       (if (element circularity-table head default: #F)
		   #T
		   (begin
		    (set! (element circularity-table head) #T)
		    (circular? (rest list))))))))
    (circular? list)))
|#

(define-method concatenate! ((list <list>) #more rest-lists)
  #|
  (when (circular? list)
    (error "CONCATENATE WITH CIRCULAR LIST"))
  |#
  ;; !@#$ NOT SUPER EFFICIENT BUT THEN THIS ISN'T NECESSARY
  (if (every? (method (x) (instance? x <list>)) rest-lists)
      (bind-methods
	  ((connect! ((result <list>) (ptr <list>) (next <list>))
	     (if (empty? result)
		 (values next next)
		 (begin
		  (set! (tail ptr) next)
		  (values result next))))
	   (connect-list! ((result <list>) (ptr <list>) (list <list>))
	     (iterate grovel
		 (((result <list>) result) ((ptr <list>) ptr)
		  ((list <list>) list))
	       (if (empty? list)
		   (values result ptr)
		   (bind ((result ptr (connect! result ptr list)))
		     (grovel result ptr (tail list)))))))
	(bind (((result <list>) (ptr <list>) (connect-list! '() '() list)))
	  (iterate grovel
	      (((result <list>) result) ((ptr <list>) ptr)
	       ((index <integer>) 0))
	    (if (= index (size rest-lists))
		result
		(bind ((result ptr
			 (connect-list! result ptr (element rest-lists index))))
		  (grovel result ptr (+ index 1))))))))
  (next-method))

(define-method concatenate ((list <list>) #more rest-lists)
  (if (every? (method (x) (instance? x <list>)) rest-lists)
      (bind-methods
	  ((add-item! ((result <list>) (ptr <list>) item)
	     (if (empty? result)
		 (bind ((ptr (pair item '())))
		   (values ptr ptr))
		 (begin
		  (set! (tail ptr) (pair item '()))
		  (values result (tail ptr)))))
	   (add-list! ((result <list>) (ptr <list>) (list <list>))
	     (iterate grovel
		 (((result <list>) result) ((ptr <list>) ptr)
		  ((list <list>) list))
	       (if (empty? list)
		   (values result ptr)
		   (bind ((result ptr (add-item! result ptr (head list))))
		     (grovel result ptr (tail list)))))))
	(bind (((result <list>) (ptr <list>) (add-list! '() '() list)))
	  (iterate grovel
	      (((result <list>) result) ((ptr <list>) ptr)
	       ((index <integer>) 0))
	    (if (= index (size rest-lists))
		result
		(bind ((result ptr
			 (add-list! result ptr (element rest-lists index))))
		  (grovel result ptr (+ index 1)))))))
      (next-method)))

(define-method find-pair ((object <object>) (list <list>) #key (test id?))
  (bind-exit (return)
    (for ((item in list))
      (when (test object (head item))
	(return item)))))

;;;;; EXTENSIONS

(define-method butlast ((list <list>))
  (reverse! (rest (reverse list))))

(define-method nthtail ((list <list>) (index <integer>))
  (for ((i from 0 below index))
    (set! list (tail list)))
  list)

#|
(define-method proper? ((null <empty-list>)) #T)
(define-method proper? ((object <object>)) #F)
(define-method proper? ((pair <pair>))
  (proper? (tail pair)))
|#

(define-method proper? ((list <list>) #values (proper? <boolean>))
  (iterate grovel ((ptr list))
    (if (instance? ptr <list>)
	(if (empty? ptr) #T (grovel (tail ptr)))
	#F)))

;;; <ASSOCIATION-LIST>

(define-method element
    ((list <list>) (key <keyword>) #key (default (unsupplied)))
  (bind-exit (return)
    (for ((items = list then (tail (tail items))))
      (when (empty? items)
	(if (unsupplied? default)
	    (error "ELEMENT outside of range: ~S ~S" list key)
	    (return default)))
      (when (id? (head items) key)
	(return (head (tail items)))))))

