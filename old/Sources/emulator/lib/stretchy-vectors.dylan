;; Stretchy CL vectors via an indirection since the two cannot be 
;; distinguished by class using CLOS.

(define-class <stretchy-vector> (<vector>)
  stretchy-vector-data)

;; Creation:

(define-method initialize ((v <stretchy-vector>) #key (size 0) (fill #f))
  (if (> size 0)
      (set! (stretchy-vector-data v)
	    (make/vector size 
			 initial-element: fill
			 adjustable:      't
			 fill-pointer:    't))
      (set! (stretchy-vector-data v) #f))
  v)

;; Size:

(define-method size ((v <stretchy-vector>))
  (bind ((data (stretchy-vector-data v)))
    (if data (size data) 0)))

(define-method size-setter ((n <integer>) (v <stretchy-vector>))
  (bind ((data (stretchy-vector-data v)))
    (if data
	(adjust/stretchy-vector data n
				initial-element: #f
				fill-pointer: 't)
        
        (set! (stretchy-vector-data v)
	      (make/vector n
			   initial-element: #f
			   adjustable:      't
			   fill-pointer:    't)))))

;; Random access:

(define-method element
    ((v <stretchy-vector>) (index <integer>) #key (default (unsupplied)))
  ;; No need to check for data = #f, because the range check will fail
  (if (or (< index 0) (>= index (size v)))
      (if (unsupplied? default)
	  (error "Element outside of range: ~S ~S" v index)
	  default)
      (element (stretchy-vector-data v) index)))

(define-method element-setter
    ((new-value <object>) (v <stretchy-vector>) (index <integer>))
  ;; No need to check for data = #f, because the range check will fail
  (if (or (< index 0) (>= index (size v)))
      (error "element-setter outside of range: ~S ~S"
	     v index)
      (set! (element (stretchy-vector-data v) index) new-value)))

;; Iteration:

(define-method initial-state ((v <stretchy-vector>))
  (if (zero? (size v)) #f 0))

(define-method next-state ((v <stretchy-vector>) (state <integer>))
  (bind ((next (+ state 1)))
    (if (< next (size v)) next #f)))

(define-method current-element ((v <stretchy-vector>) (state <integer>))
  ;; No need to check for data = #f, because the iteration will not get here
  (element/sequence*integer (stretchy-vector-data v) state))

(define-method current-element-setter (value (v <stretchy-vector>) (state <integer>))
  ;; No need to check for data = #f, because the iteration will not get here
  (set-element/sequence*integer (stretchy-vector-data v) state value))

(define-method current-key ((v <stretchy-vector>) (state <integer>))
  state)

(define-method copy-state ((v <stretchy-vector>) (state <integer>))
  state)


;;; NEW ITERATION PROTOCOL

(define stretchy-vector-current-element
  (method ((v <stretchy-vector>) (state <integer>))
    (element/sequence*integer (stretchy-vector-data v) state)))

(define stretchy-vector-current-element-setter
  (method (value (v <stretchy-vector>) (state <integer>))
    (set-element/sequence*integer (stretchy-vector-data v) state value)))

(define-method forward-iteration-protocol ((collection <stretchy-vector>))
  (values 0 (size collection) sequence-next-state sequence-finished-state?
	  sequence-current-key
	  stretchy-vector-current-element stretchy-vector-current-element-setter
	  identity-copy-state))


;; Coercion:

(define-method as ((class (singleton <stretchy-vector>)) (v <stretchy-vector>))
  v)

(define-method as ((class (singleton <stretchy-vector>)) (c <collection>))
  (bind ((len (size c))
         (new (make <stretchy-vector> size: len)))
    (for ((state (initial-state c) (next-state c state))
          (i     0                 (+ i 1)))
         ((not state) new)
      (set! (element new i) (current-element c state)))))

;; Other:

(define ensure-stretchy-vector-data 
  (method ((v <stretchy-vector>))
    (bind ((data (stretchy-vector-data v)))
      (if data
          data
          (begin
           (set! (stretchy-vector-data v)
  	         (make/vector 4	;just a guess
			      initial-element: #f
		  	      adjustable:      't
			      fill-pointer:    0))
           (set! data (stretchy-vector-data v))
           data)))))

(define-method add! ((v <stretchy-vector>) elt)
  (bind ((data (ensure-stretchy-vector-data v)))
    (push/stretchy-vector elt data))
  v)

(define-method remove! 
    ((vector <stretchy-vector>) (target <object>)
     #rest all-keys #key (test id?) (count (size vector)))
  (bind ((data (ensure-stretchy-vector-data vector)))
    (apply remove! data target all-keys))
  #|
  (iterate grovel
     (((count <integer>) count)
      ((src-index <integer>) 0) ((dst-index <integer>) 0) )
    (if (< src-index (size vector))
	(bind ((item (element vector src-index)))
	  (cond ((and (> count 0) (test item target))
		 (grovel (- count 1) (+ src-index 1) dst-index))
		(else:
		 (set! (element vector dst-index) item)
		 (grovel count (+ src-index 1) (+ dst-index 1)))))
        (set! (size vector) dst-index)))
  |#
  vector)

(define-method print ((v <stretchy-vector>)
	              #key
                      (stream   (standard-output))
                      (verbose? #f))
  (bind ((data (stretchy-vector-data v)))
    (if data
	(format stream "#<stretchy-vector: ~s>" (as <list> data))
        (format stream "#<empty stretchy-vector>")))
  v)

;; Optimized sorting.

(define-method sort ((v <stretchy-vector>) #key (test <) (stable #f))
  (bind ((new (make <stretchy-vector>)))
    (when (stretchy-vector-data v)
      (set! (stretchy-vector-data new) 
	    (sort (stretchy-vector-data v) test: test stable: stable)))
    new))

(define-method sort! ((v <stretchy-vector>) #key (test <) (stable #f))
  (when (stretchy-vector-data v)
    (set! (stretchy-vector-data v) 
	  (sort! (stretchy-vector-data v) test: test stable: stable)))
  v)

;; eof
