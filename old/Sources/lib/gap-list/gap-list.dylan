module:    gap-list
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define $minimum-gap-list-data-size 4)

(define-class <gap-list> (<deque>)
  (data  type: <simple-object-vector>)
  (%size init-value: 0 type: <integer>)
  (gap-size init-value: 0 type: <integer>)
  (gap-start init-value: 0 type: <integer>))

(define-method initialize ((gap-list <gap-list>) #key (size 0) (fill #F))
  (next-method)
  (bind ((data-size (max size $minimum-gap-list-data-size)))
    (set! (data gap-list)
	  (make <simple-object-vector> size: data-size fill: fill))
    (set! (gap-size gap-list) (- data-size size))
    (set! (gap-start gap-list) size))
  gap-list)

(define-method as ((class (singleton <gap-list>)) (object <collection>))
  (bind (((new-deque <gap-list>) (make <gap-list>)))
    (for ((element in object))
      (push-last new-deque element))
    new-deque))
    
(define-method size ((collection <gap-list>))
  (- (size (data collection)) (gap-size collection)))

(define-method empty? ((collection <gap-list>))
  (= (size collection) 0))

;;; SEQUENCES

(define-method data-index ((deque <gap-list>) (index <integer>))
  (if (< index (gap-start deque)) index (+ index (gap-size deque))))

(define-method element
    ((collection <gap-list>) (index <integer>) #key (default (unsupplied)))
  (if (or (< index 0) (>= index (size collection)))
      (if (unsupplied? default)
	  (error "Element outside of range: ~S ~S" collection index)
	  default)
      (element (data collection) (data-index collection index))))

(define-method element-setter
    ((new-value <object>) (collection <gap-list>) (index <integer>))
  (if (or (< index 0) (>= index (size collection)))
      (error "Element outside of range: ~S ~S" collection index)
      (set! (element (data collection) (data-index collection index))
            new-value)))

(define-method remove! 
    ((sequence <gap-list>) (target <object>) 
     #key (test id?) (count (size sequence)))
  (move-gap! sequence (size sequence))
  (iterate grovel
     (((count <integer>) count)
      ((src-index <integer>) 0) ((dst-index <integer>) 0))
    (if (< src-index (size sequence))
	(bind ((item (element sequence src-index)))
	  (cond ((and (> count 0) (test item target))
		 (grovel (- count 1) (+ src-index 1) dst-index))
		(else:
		 (set! (element sequence dst-index) item)
		 (grovel count (+ src-index 1) (+ dst-index 1)))))
        (begin
         (set! (gap-size sequence)
	       (+ (gap-size sequence) (- (gap-start sequence) dst-index)))
         (set! (gap-start sequence) dst-index))))
  sequence)

(define-method reverse! ((deque <gap-list>))
  (set! (data deque) (reverse! (data deque)))
  (set! (gap-start deque)
        (- (size (data deque)) (gap-start deque) (gap-size deque)))
  deque)

;;; DEQUE

(define-method move-gap! ((gap-list <gap-list>) (new-start <integer>))
  ;; (format #T "MOVING GAP FROM ~D TO ~D~%" (gap-start gap-list) new-start)
  (if (<= new-start (gap-start gap-list))
      (for ((i from (+ (gap-start gap-list) (- (gap-size gap-list) 1))
	       to (+ new-start (gap-size gap-list))
	       by -1))
	(set! (element (data gap-list) i)
	      (element (data gap-list) (- i (gap-size gap-list)))))
      (for ((i from (gap-start gap-list) below new-start))
	(set! (element (data gap-list) i)
	      (element (data gap-list) (+ i (gap-size gap-list))))))
  (set! (gap-start gap-list) new-start))

(define-method insert-at! ((gap-list <gap-list>) (index <integer>) new-value)
  (when (= (gap-size gap-list) 0)
    (bind ((old-size (size (data gap-list)))
	   (new-size (* old-size 2)))
      (set! (data gap-list)
	    (replace-subsequence!
	     (make <simple-object-vector> size: new-size)
	     (data gap-list)))
      (set! (gap-start gap-list) old-size)
      (set! (gap-size gap-list) (- new-size old-size))))
  (move-gap! gap-list index)
  (set! (element (data gap-list) index) new-value)
  (set! (gap-start gap-list) (+ (gap-start gap-list) 1))
  (set! (gap-size gap-list) (- (gap-size gap-list) 1))
  gap-list)

(define-method delete-at! ((gap-list <gap-list>) (index <integer>))
  (move-gap! gap-list index)
  (set! (gap-size gap-list) (+ (gap-size gap-list) 1))
  gap-list)

(define-method push ((deque <gap-list>) (new-element <object>))
  (insert-at! deque 0 new-element))

(define-method pop ((deque <gap-list>))
  (if (empty? deque)
      (error "POP empty deque ~S" deque)
      (bind ((value (element deque 0)))
        (delete-at! deque 0)
        value)))
	
(define-method push-last ((deque <gap-list>) (new-element <object>))
  (insert-at! deque (size deque) new-element))

(define-method pop-last ((deque <gap-list>))
  (bind ((real-size (size deque)))
    (if (= real-size 0)
        (error "POP empty deque ~S" deque)
        (bind ((value (element deque (- real-size 1))))
          (delete-at! deque (- (size (data deque)) (gap-size deque) 1))
          value))))
	
;;;; ITERATION

(define-method current-element
    ((deque <gap-list>) (state <integer>)) 
  (element deque state))

(define-method current-element-setter
    ((new-value <object>) (deque <gap-list>) (state <integer>))
  (set! (element deque state) new-value))

(define-method initial-state ((sequence <gap-list>))
  (if (empty? sequence) #F 0))

(define-method final-state ((sequence <gap-list>))
  (if (empty? sequence) #F (- (size sequence) 1)))

(define-method next-state ((sequence <gap-list>) (state <integer>))
  (if (= state (- (size sequence) 1)) #F (+ state 1)))

(define-method previous-state ((sequence <gap-list>) (state <integer>))
  (if (= state 0) #F (- state 1)))

(define-method copy-state ((sequence <gap-list>) (state <integer>)) state)

(define-method print ((deque <gap-list>) #key stream verbose?)
  (format stream "[<deque> START: ~D SIZE: ~D DATA: ~S]" 
          (gap-start deque) (gap-size deque) (data deque))
  deque)
