(define-class <deque> (<mutable-sequence> <stretchy-sequence>)
  deque-start-node
  deque-end-node)

;; Creation:

(define-method initialize ((dq <deque>) 
			   #key (size 0)
			        (fill #f))
  (bind ((start (cons-node! '() 'start '()))
         (end   (cons-node! start 'end '())))
    (set! (deque-start-node dq) start)
    (set! (deque-end-node dq)   end))
  (for ((i size (- i 1)))
       ((zero? i))
    (push dq fill))
  dq)

;; Specific protocol:

(define-generic-function push      (dq value))
(define-generic-function pop       (dq))
(define-generic-function push-last (dq value))
(define-generic-function pop-last  (dq))

;; Implementation:

(define-method push ((dq <deque>) value)
  (cons-node-after! (deque-start-node dq) value)
  dq)

(define-method pop ((dq <deque>))
  (bind ((start (deque-start-node dq)))
    (when (empty? dq)
      (error "pop: the deque ~s is empty" dq))
    (delete-node! (node-next start))))

(define-method push-last ((dq <deque>) value)
  (cons-node-before! (deque-end-node dq) value)
  dq)

(define-method pop-last ((dq <deque>))
  (bind ((end (deque-end-node dq)))
    (when (empty? dq)
      (error "pop-last: the deque ~s is empty" dq))
    (delete-node! (node-last end))))

(define-method add! ((dq <deque>) value) (push dq value))

;; Iteration:

(define-method empty? ((dq <deque>))
  (== (node-next (deque-start-node dq)) (deque-end-node dq)))

(define-method initial-state ((dq <deque>))
  (if (empty? dq) #f (node-next (deque-start-node dq))))

(define-method next-state ((dq <deque>) node)
  (bind ((next (node-next node)))
    (if (null-node? (node-next next)) #f next)))

(define-method current-element ((dq <deque>) node)
  (node-value node))

(define-method copy-state ((dq <deque>) node)
  node)

(define-method current-element-setter (value (dq <deque>) node)
  (set! (node-value node) value)
  value)

(define-method final-state ((dq <deque>))
  (if (empty? dq) #f (node-last (deque-end-node dq))))

(define-method previous-state ((dq <deque>) node)
  (bind ((last (node-last node)))
    (if (null-node? (node-last last)) #f last)))

;;; NEW ITERATION PROTOCOL

(define deque-next-state
    (method ((collection <deque>) (state <doubly-linked-list>) 
            #values (_ <doubly-linked-list>))
       (node-next state)))

(define deque-finished-state?
  (method ((collection <deque>) (state <doubly-linked-list>)
	   (limit <doubly-linked-list>) #values (_ <boolean>))
    (== state limit)))

(define deque-current-key
  (method ((collection <deque>) (state <doubly-linked-list>))
    0))

(define deque-current-element
  (method ((collection <deque>) (state <doubly-linked-list>))
    (node-value state)))

(define deque-current-element-setter
  (method (new-value (collection <deque>) (state <doubly-linked-list>))
    (set! (node-value state) new-value)))

(define-method forward-iteration-protocol ((collection <deque>))
  (values (node-next (deque-start-node collection)) (deque-end-node collection)
	  deque-next-state deque-finished-state? deque-current-key
	  deque-current-element deque-current-element-setter
	  identity-copy-state))

;; Coercion:

(define-method as ((class (singleton <deque>)) (v <deque>))
  v)

(define-method as ((class (singleton <deque>)) (c <collection>))
  (bind ((dq (make <deque>)))
    (for ((state (initial-state c) (next-state c state)))
         ((not state) dq)
      (push-last dq (current-element c state)))))

;; Collection:

(define-method size-setter ((n <integer>) (dq <deque>))
  ;; Well lazy!
  (bind ((sz (size dq)))
    (if (< n sz)
      (dotimes (i (- sz n))
        (pop-last dq))
      (dotimes (i (- n sz))
        (push-last dq #f)))))

(define-method remove! ((dq <deque>) value #key (test ==) (count #f))
  (bind ((end (deque-end-node dq)))
    (bind-methods ((remove!-internal (count node)
		     (if (or (== node end) (and count (<= count 0)))
		       dq
		       (bind ((next (node-next node)))
			 ;; gts,98mar30: (if (test value (node-value node))
			 (if (test (node-value node) value)  ;; gts,98mar30
			   (begin
			     (delete-node! node)
			     (remove!-internal (and count (- count 1))
					       next))
			 (remove!-internal count next))))))
      (remove!-internal count (node-next (deque-start-node dq))))))

;; Misc:

(define-method print ((dq <deque>) 
                      #key 
                      (stream (standard-output))
                      (verbose? #f))
  (format stream "#<deque: ~s>" (if (empty? dq) 'empty (as <list> dq)))
  dq)

;; eof
