module:    doubly-linked-list
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-class <deque-element> (<object>)
  prev
  next
  (data init-keyword: data:))

(define $empty-deque-element (make <deque-element>))
(set! (prev $empty-deque-element) $empty-deque-element)
(set! (next $empty-deque-element) $empty-deque-element)
(set! (data $empty-deque-element) #F)

(define-method initialize ((deque-element <deque-element>) #rest all-keys)
  (next-method)
  (set! (prev deque-element) $empty-deque-element)
  (set! (next deque-element) $empty-deque-element)
  deque-element)

;;;;; <doubly-linked-list>

(define-class <doubly-linked-list> (<deque>)
  (front type: <deque-element>)  ;; PRIVATE
  (back  type: <deque-element>)) ;; PRIVATE

(define-method initialize
    ((deque <doubly-linked-list>) #key (size 0) (fill #f))
  (next-method)
  (set! (front deque) $empty-deque-element)

  (set! (back deque) $empty-deque-element)
  (for ((count from 0 below size))
    (push deque fill))
  deque)

(define-method as
    ((class (singleton <doubly-linked-list>)) (object <collection>))
  (bind (((new-deque <doubly-linked-list>) (make <doubly-linked-list>)))
    (for ((element in object))
      (push-last new-deque element))
    new-deque))
    
;;; SEQUENCES

(define-method remove!
    ((deque <doubly-linked-list>) (value <object>)
     #key (test id?) (count (size deque)))
  (for (((deque-element <deque-element>)
	 = (front deque) then (next deque-element))
	(until (or (= count 0) (id? deque-element $empty-deque-element))))
    (when (test (data deque-element) value)
      (unless (or (id? (front deque) deque-element)
		  (id? (back deque) deque-element))
	(set! (next (prev deque-element)) (next deque-element))
	(set! (prev (next deque-element)) (prev deque-element)))
      (when (id? (front deque) deque-element)
	(set! (front deque) (next deque-element))
	(if (id? (front deque) $empty-deque-element)
	    (set! (back deque) $empty-deque-element)
	    (set! (prev (front deque)) $empty-deque-element)))
      (when (id? (back deque) deque-element)
	(set! (back deque) (prev deque-element))
	(if (id? (back deque) $empty-deque-element)
	    (set! (front deque) $empty-deque-element)
	    (set! (next (back deque)) $empty-deque-element)))
      (set! count (- count 1))))
  deque)

(define-method reverse! ((deque <doubly-linked-list>))
  (bind (((old-front <deque-element>) (front deque)))
    (set! (front deque) $empty-deque-element)
    (set! (back  deque) $empty-deque-element)
    (bind ((deque-element old-front))
      (until (id? deque-element $empty-deque-element)
	(bind (((last-deque-element <deque-element>) deque-element))
	  (set! deque-element (next last-deque-element))
	  (set! (prev last-deque-element) $empty-deque-element)
	  (set! (next last-deque-element) $empty-deque-element)
	  (push-deque-element deque last-deque-element)))
      deque)))

;;; DEQUE

(define-method push-deque-element
    ((deque <doubly-linked-list>) (new-deque-element <deque-element>))
  (bind (((old-front <deque-element>) (front deque)))
    (set! (front deque) new-deque-element)
    (set! (next new-deque-element) old-front)
    (unless (id? old-front $empty-deque-element)
      (set! (prev old-front) new-deque-element))
    (when (id? (back deque) $empty-deque-element)
      (set! (back deque) new-deque-element))
    deque))

(define-method push ((deque <doubly-linked-list>) (new-element <object>))
  (push-deque-element deque (make <deque-element> data: new-element)))

(define-method pop ((deque <doubly-linked-list>))
  (bind (((deque-element <deque-element>) (front deque)))
    (if (id? deque-element $empty-deque-element)
	(error "POP empty deque ~S" deque)
	(begin
	 (set! (front deque) (next deque-element))
	 (unless (id? (front deque) $empty-deque-element)
	   (set! (prev (front deque)) $empty-deque-element))
	 (when (id? (back deque) deque-element)
	   (set! (back deque) $empty-deque-element))
	 (data deque-element)))))
	
(define-method push-last-deque-element
    ((deque <doubly-linked-list>) (new-deque-element <deque-element>))
  (bind (((old-back <deque-element>) (back deque)))
    (set! (back deque) new-deque-element)
    (set! (prev new-deque-element) old-back)
    (unless (id? old-back $empty-deque-element)
      (set! (next old-back) new-deque-element))
    (when (id? (front deque) $empty-deque-element)
      (set! (front deque) new-deque-element))
    deque))

(define-method push-last ((deque <doubly-linked-list>) (new-element <object>))
  (push-last-deque-element
   deque (make <deque-element> data: new-element)))

(define-method pop-last ((deque <doubly-linked-list>))
  (bind (((deque-element <deque-element>) (back deque)))
    (if (id? deque-element $empty-deque-element)
	(error "POP-LAST empty deque ~S" deque)
	(begin
	 (set! (back deque) (prev deque-element))
	 (unless (id? (back deque) $empty-deque-element)
	   (set! (next (back deque)) $empty-deque-element))
	 (when (id? (front deque) deque-element)
	   (set! (front deque) $empty-deque-element))
	 (data deque-element)))))
	
;;;; ITERATION

(define-method initial-state ((deque <doubly-linked-list>))
  (bind (((deque-element <deque-element>) (front deque)))
    (if (id? deque-element $empty-deque-element) #F deque-element)))

(define-method final-state ((deque <doubly-linked-list>))
  (bind (((deque-element <deque-element>) (back deque)))
    (if (id? deque-element $empty-deque-element) #F deque-element)))

(define-method next-state 
    ((deque <doubly-linked-list>) (state <deque-element>))
  (bind (((deque-element <deque-element>) (next state)))
    (if (id? deque-element $empty-deque-element) #F deque-element)))

(define-method previous-state
    ((deque <doubly-linked-list>) (state <deque-element>))
  (bind (((deque-element <deque-element>) (prev state)))
    (if (id? deque-element $empty-deque-element) #F deque-element)))

(define-method copy-state
    ((deque <doubly-linked-list>) (state <deque-element>))
  state)

(define-method current-element
    ((deque <doubly-linked-list>) (state <deque-element>)) (data state))

(define-method current-element-setter
    ((new-value <object>) (deque <doubly-linked-list>) 
     (state <deque-element>))
  (set! (data state) new-value))
