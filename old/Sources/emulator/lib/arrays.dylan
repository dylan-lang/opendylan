;; Creation:

(define-method make ((class (singleton <array>)) 
		     #key (dimensions #f)
		          (fill       #f))
  (make/array (as <list> dimensions) initial-element: fill))

;; Protocol:

;; aref, aref-setter, dimensions are directly from CL

(define-method element ((a <array>) 
			(i <sequence>) 
			#key (default #f))
  (apply aref a (as <list> i)))

(define-method element-setter (value
				 (a <array>) 
                                 (i <sequence>))
  (apply aref-setter value a i))

(define-method size ((a <array>))
  (reduce * 1 (dimensions a)))

;; Iteration: 

(define-class <array-state> (<object>)
  (array-state-dimension-count required-init-keyword: dimension-count:)
  (array-state-counter         required-init-keyword: counter:))

(define-method initial-state ((a <array>))
  (if (zero? (size a)) #f
    (bind ((dims (dimensions a))
	   (axes (size dims)))
      (make <array-state>
	    dimension-count: axes
	    counter: (make <vector> size: axes fill: 0)))))

(define-method next-state ((a <array>) (state <array-state>))
  (bind ((dims (dimensions a))
	 (axes (array-state-dimension-count state)))
    (bind-method push-counter ((counter (array-state-counter state))
			       (index 0))
      (if (= index axes) #f
	(bind ((val (+ (element counter index) 1)))
	  (cond
	    ((< val (element dims index))
	      (set! (element counter index) val)
	      state)
	    (else:
	      (set! (element counter index) 0)
	      (push-counter counter (+ index 1)))))))))

(define-method current-element ((a <array>) (state <array-state>))
  (element a (array-state-counter state)))

(define-method copy-state ((a <array>) (state <array-state>))
  (make <array-state>
	dimension-count: (array-state-dimension-count state)
	counter: (shallow-copy (array-state-counter state))))

(define-method current-key ((a <array>) (state <array-state>))
  (shallow-copy (array-state-counter state)))

(define-method current-element-setter (value
                                         (a <array>) 
					 (state <array-state>))
  (set! (element a (array-state-counter state)) value))

;; Coercion:

(define-method as ((class (singleton <array>)) (a <array>))
  a)

(define-method as ((class (singleton <array>)) (c <sequence>))
  (as <vector> c))

;; Misc:

;; What the hell do we do here??

(define do-arrays-by-key
  (method (f colns early-return late-return)
    (bind ((keys (reduce1 (rcurry intersection test: =) (map key-sequence colns))))
      (for ((ks (initial-state keys) (next-state keys ks)))
	   ((or (not ks) 
                (bind ((key (current-element keys ks)))
                  (f key)))
            (if ks early-return late-return))))))

(define-method map-into ((targ <array>) proc (src <array>) #rest more)
  (bind ((all (pair src more)))
    (do-arrays-by-key
      (method (key)
        (set! (element targ key)
              (apply proc (map (rcurry element key) all)))
        #f)
      (pair targ all)
      'ignored
      targ)))
      
(define-method map-as ((class (singleton <array>))
		       proc 
		       (c <collection>)
		       #rest more)
  (cond
    ((and (instance? c <sequence>)
	  (every? (rcurry instance? <sequence>) more))
      (apply map-as <vector> proc x more))
    ((and (instance? c <array>)
          (every? (rcurry instance? <array>) more))
      (bind-methods
        ((merge-dims (d1 d2)
           (bind ((lower higher
                    (if (< (size d1) (size d2))
                      (values d1 d2)
                      (values d2 d1))))
             (bind ((low (map max lower higher))
                    (hi  (copy-sequence higher start: (size lower))))
               (concatenate low hi)))))
        (apply
          map-into
          (make <array>
                dimensions: (reduce
                              (method (acc col)
                                (merge-dims acc (dimensions col)))
                              (dimensions c)
                              more))
          proc
          c
          more)))
    (else:
      (next-method))))

;; Efficient fill! method for 2-d arrays

;; andrewa, 26 Mar 1997 - the default method takes forever
(define-method fill! ((a <array>) value #key (start 0) end)
  (ignore start end)
  (bind ((dims (dimensions a)))
    (if (= (size dims) 2)
       (for-each ((i (range from: 0 up-to: (element dims 0)))) ()
	 (for-each ((j (range from: 0 up-to: (element dims 1)))) ()
	   (set! (aref a i j) value)))
      (next-method)))
  a)

;; eof
