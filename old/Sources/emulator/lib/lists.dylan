;; List creation:


(define-method make ((class (singleton <list>)) #key (size 0) (fill #f))
  (for ((i size (- i 1))
	(l '()  (pair fill l)))
       ((zero? i) l)))

(define-method make ((class (singleton <pair>)) #key)
  (pair #f #f))

(define-method make ((class (singleton <empty-list>)) #key)
  '())


;;; NEW ITERATION PROTOCOL

(define list-next-state
  (method ((collection <list>) (state <list>))
    (tail state)))

(define general-list-finished-state?
  (method ((collection <list>) state limit #values (_ <boolean>))
    (or (id? state '()) (not (instance? state <list>)))))


(define list-finished-state?
  (method ((collection <list>) (state <list>) limit #values (_ <boolean>))
    (id? state '())))

(define list-current-key
  (method ((collection <list>) (state <list>) #values (_ <integer>))
    (iterate search (((l <list>) collection) ((k <integer>) 0))
      (if (id? l state) k (search (tail l) (+ k 1))))))

(define list-current-element
  (method ((collection <list>) (state <list>) #values (_ <object>))
    (head state)))

(define list-current-element-setter
  (method (new-value (collection <list>) (state <list>) #values (_ <object>))
    (set! (head state) new-value)))

(define-method forward-iteration-protocol ((collection <list>))
  (values collection '() list-next-state general-list-finished-state?
	  list-current-key list-current-element list-current-element-setter
	  identity-copy-state))

;; List iteration protocol implementation:

(define-method initial-state ((l <list>)) 
  (if (null? l) #f l))

(define-method next-state ((l <list>) (state <list>))
  (bind ((next-state (tail state)))
    (if (null? next-state) #f next-state)))

(define-method current-element ((l <list>) (state <list>))
  (head state))

(define-method copy-state ((l <list>) (state <list>))
  state)

(define-method current-element-setter (value (l <list>) (state <list>))
  (set! (head state) value)
  value)

;; Coercion:

(define-method class-for-copy ((l <list>)) <list>)
(define-method class-for-copy ((l <pair>)) <list>)
(define-method class-for-copy ((l <empty-list>)) <list>)

(define-method as ((class (singleton <list>)) (l <list>))
  l)

(define-method as ((class (singleton <list>)) (c <collection>))
  (bind ((tail-handle (pair 'hack '()))
         (end tail-handle))
    (for ((state (initial-state c) (next-state c state)))
         ((not state) (tail tail-handle))
      (bind ((cell (pair (current-element c state) '())))
        (set! (tail end) cell)
        (set! end cell)))))

;; Sequence stuff:

(define-method add! ((l <list>) value) (pair value l))

;; Defaults:

;; removed!
#|
(define-method find-pair (value (l <list>) #key (test id?))
  (if (null? l) #f
    (bind ((elt (head l)))
      (if (and (instance? elt <pair>) (test value (head elt))) elt
	(find-pair value (tail l) test: test)))))
|#

;; Comparison:

(define-method = ((l1 <list>) (l2 <list>)) 
  #f)

(define-method = ((l1 <pair>) (l2 <pair>))
  (and (= (head l1) (head l2))
       (= (tail l1) (tail l2))))

(define-method = ((l1 <empty-list>) (l2 <empty-list>))
  #t)

(define-method = ((l <list>) (s <sequence>))
  (bind-exit (return)
    (for ((state (initial-state s) (next-state s state))
          (walk  l                 (tail walk)))
         (#f #f)
      (cond
        ((not state)
          (return (id? walk '())))
	((not (instance? walk <pair>))
          (return #f))
        ((not (= (head walk) (current-element s state)))
          (return #f))))))

(define-method = ((s <sequence>) (l <list>))
  (= l s))

;; Optimizations:

(define-method empty? ((s <empty-list>)) #t)
(define-method empty? ((s <pair>)) #f)

;; eof
