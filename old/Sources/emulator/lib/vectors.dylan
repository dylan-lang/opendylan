;; (define-class <vector> (...)

;; Creation:

(define-method make ((class (singleton <vector>)) #key (size 0) (fill #f))
  (make <simple-object-vector> size: size fill: fill))

(define-method make ((class (singleton <simple-vector>)) #key (size 0) (fill #f))
  (make <simple-object-vector> size: size fill: fill))

(define-method make ((class (singleton <simple-object-vector>))
                     #key (size 0) (fill #f))
  (if (= size 0)
      #()
      (make/vector size initial-element: fill)))

;; Iteration:

(define-method initial-state ((v <simple-object-vector>))
  (if (zero? (size/sequence v)) #f 0))

(define-method next-state ((v <simple-object-vector>) (state <integer>))
  (bind ((next (+ state 1)))
    (if (< next (size/sequence v)) next #f)))

(define-method current-element ((v <simple-object-vector>) (state <integer>))
  (element/sequence*integer v state))

(define-method copy-state ((v <simple-object-vector>) (state <integer>))
  state)

(define-method current-key ((v <simple-object-vector>) (state <integer>))
  state)

(define-method current-element-setter (value (v <simple-object-vector>) (state <integer>))
  (set-element/sequence*integer v state value))

(define-method final-state ((v <simple-object-vector>))
  (if (zero? (size/sequence v)) #f (- (size/sequence v) 1)))

(define-method previous-state ((v <simple-object-vector>) (state <integer>))
  (bind ((prev (- state 1)))
    (if (< prev 0) #f prev)))

;;; NEW ITERATION PROTOCOL


(define vector-current-element (method ((v <simple-object-vector>) (state <integer>))
  (element/sequence*integer v state)))

(define vector-current-element-setter
  (method (value (v <simple-object-vector>) (state <integer>))
    (set-element/sequence*integer v state value)))

(define-method forward-iteration-protocol ((collection <simple-object-vector>))
  (values 0 (size collection) sequence-next-state sequence-finished-state?
	  sequence-current-key
	  vector-current-element vector-current-element-setter
	  identity-copy-state))


;; Coercion:

(define-method as ((class (singleton <vector>)) any)
  (as <simple-object-vector> any))

(define-method as ((class (singleton <simple-object-vector>)) (v <simple-object-vector>))
  v)

(define-method as ((class (singleton <simple-object-vector>)) (c <collection>))
  (bind ((len (size c)))
    (unless len
      (error "can't coerce unbounded collections to <vector>"))
    (bind ((new (make <simple-object-vector> size: len)))
      (for ((state (initial-state c) (next-state c state))
            (i     0                 (+ i 1)))
          ((not state) new)
        (set! (element new i) (current-element c state))))))

;; eof
