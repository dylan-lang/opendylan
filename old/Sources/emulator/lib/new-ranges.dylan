;;
;;; Ranges.
;;;
;;;   Ranges are immutable sequences but are so special as to be
;;;   unable to depend on the most of the defaults. This
;;;   implementation is pretty dodgy and probably doesn't deal
;;;   correctly with all open sequences.
;;;
;;

(define-class <range> (<sequence>)
  (range-ordering type: <function>)
  (range-metric   type: <function>)
  (range-from    type: <number>)
  (range-last     type: <number>)
  (range-by     type: <number>)
  (range-size     type: <integer>))

(define-method only-one-of? (#rest args)
  (<= (reduce (method (x y) (+ x (if y 1 0))) 0 args) 1))

(define-method initialize ((range <range>)
                           #key (from    0)
                                (to      #f)
                                (above   #f)
                                (below   #f)
                                (by      1)
                                (size    #f)
                                ; deprecated
                                (up-to   #f)
                                (through #f))
  (when (zero? by)
    (error "range: invalid step ~s" by))
  (unless (only-one-of? to above below up-to through)
    (error
     "range: only one of to:, above:, below:, up-to:, and through: allowed"))
  (when to
    (set! through to))
  (if (positive? by)
      (when below
        (set! up-to below))
      (when above
        (set! up-to above)))
  (bind
    ((order ; does x come before y in this sequence
       (if (positive? by)
	 (method (x y) 
	   (cond 
	     ((not x) #f)
	     ((not y) #t)
	     (else: (< x y))))
	 (method (x y) 
	   (cond 
	     ((not x) #f)
	     ((not y) #t)
	     (else: (> x y))))))
     (metric
       (if (positive? by) 
	 (method (x y) (- x y))
	 (method (x y) (- y x)))))
    (bind ((last #f))
      (when through
	(set! last (+ from (* (floor/ (metric through from) (abs by)) by))))
      (when (and up-to (order up-to last))
	(set! last (+ from (* (ceiling (- (/ (metric up-to from) (abs by)) 1))
			      by))))
      (when (and size (order (+ from (* (- size 1) by)) last))
	(set! last (+ from (* (- size 1) by))))
      (bind ((size  (if last 
			(+ (floor/ (metric last from) (abs by)) 1)
		      #f)))
	(when (and size (negative? size))
	  (set! size 0))
        (set! (range-ordering range) order)
        (set! (range-metric range)   metric)
        (set! (range-from range)    from)
        (set! (range-last range)     last)
        (set! (range-by range)     by)
        (set! (range-size range)     size)
        range))))

(define range
  (method (#rest args #key from by to above below size up-to through)
    (apply make <range> args)))

(define $the-null-range (make <range> from: 0 through: -1))

(define-method empty? ((r <range>))
  (bind ((size (range-size r)))
    (if size (zero? size) #f)))

(define-method member? ((value <number>) (r <range>) #key (test (void)))
  (cond
    ((empty? r) 
      #f)
    ((and (not (void? test)) 
          (not (or (id? test id?)
                   (id? test =))))
      (next-method))
    (else:
      (bind ((ordered? (range-ordering r))
	     (metric (range-metric r))
	     (first (range-from r))
	     (last (range-last r))
	     (step (range-by r)))
        (and (or (ordered? first value)
	         (= first value))
	     (or (ordered? value last)
	         (= last value))
	     (bind ((quo rem (round/ (metric value first) (abs step))))
	       (= value (+ first (* quo step)))))))))

(define-method size ((r <range>))
  (range-size r))

(define-method copy-sequence ((r <range>) #key (start 0) end)
  (unless end (set! end (- (size r) 1)))
  (range from: (element r start) through: (element r end) by: (range-by r)))

(define-method = ((r1 <range>) (r2 <range>))
  (cond
    ((and (empty? r1) (empty? r2)) #t)
    ((or (empty? r1) (empty? r2)) #f)
    (else:
      (and (id? (range-from r1) (range-from r2))
           (id? (range-by r1)  (range-by r2))
           (id? (range-last r1)  (range-last r2))))))

(define-method reverse ((r <range>))
  (if (empty? r) r
    (begin
      (unless (size r)
        (error "reverse: the unbounded range ~s cannot be reversed" r))
      (range from:    (range-last r)
             through: (range-from r)
             by:      (- (range-by r))))))

(define-method reverse! ((r <range>))
  (if (empty? r) r
    (begin
      (unless (size r)
        (error "reverse! the unbounded range ~s cannot be reversed" r))
      (bind ((tmp (range from: (range-last r)
                         through: (range-from r)
                         by: (- (range-by r)))))
        (set! (range-ordering r) (range-ordering tmp))
        (set! (range-metric r) (range-metric tmp))
        (set! (range-from r) (range-from tmp))
        (set! (range-last r) (range-last tmp))
        (set! (range-by r) (range-by tmp))
        r))))

(define sgn
  (method (x)
    (cond
      ((negative? x) -1)
      ((positive? x) 1)
      (else:         0))))

(define-method intersection ((r1 <range>) (r2 <range>) #key test)
  (cond
    ((empty? r1) r1)
    ((empty? r2) r2)
    (else:
      (bind ((start1 (range-from r1))
             (start2 (range-from r2))
             (step1 (range-by r1))
             (step2 (range-by r2))
             (end1 (range-last r1))
             (end2 (range-last r2))
             (ordered-wrt-1? (range-ordering r1))
             (ordered-wrt-2? (range-ordering r2)))
        (cond
          ;; (1) same start and step:
          ((and (= start1 start2) 
		(= step1 step2))
            (if (and (not end1) (not end2))
              (range from: start1 
                     by:   step1)
              (range from: start1
                     by:   step1
                     through: (if (ordered-wrt-1? end1 end2) end1 end2))))
          ;; (2) opposite directions:
          ((not (= (sgn step1) (sgn step2)))
            (cond
              ;; (1) <-1-|    |-2->
              ((ordered-wrt-1? start2 start1)
                $the-null-range)
              ;; (2) |-1-|    |-2-|
              ((and end1 end2)
                (if (ordered-wrt-1? end1 end2)
                  $the-null-range
                  (intersection r1 (reverse r2))))
              ;; (3) |-1-> |-2-|
              (end2
	        (intersection (range from:    start1
                                     by:      step1
                                     through: end2)
                              r2))
              ;; (4) |-1-| <-2-|
              (end1
                (intersection r1
                              (range from:    start2
                                     by:      step2
                                     through: start1)))
              ;; (4) |-1-> <-2-|
              (else:
                (intersection (range from:    start1
                                     by:      step1
                                     through: start2)
                              (range from:    start2
                                     by:      step2
                                     through: start1)))))
          ;; (3) same direction:
          (else: 
	   (if (ordered-wrt-1? start1 start2)
             (normalised-range-intersection r1 r2)
             (normalised-range-intersection r2 r1))))))))

(define range-extended 
  (method (f)
    (method (x y)
      (if (and (instance? x <integer>)
               (instance? y <integer>))
        (f x y)
        (bind ((rx (as <rational> x))
               (ry (as <rational> y))
               (lc (lcm (denominator rx) (denominator ry))))
          (/ (f (* rx lc) (* ry lc)) lc))))))

(define range-extended-lcm (range-extended lcm))
(define range-extended-gcd (range-extended gcd))

(define normalised-range-intersection
  (method (r1 r2)
    (bind ((first1 (range-from r1))
           (first2 (range-from r2))
           (step1  (range-by r1))
           (step2  (range-by r2)))
      (bind ((diff (- first2 first1))
             (hcf (range-extended-gcd step1 step2)))
        (bind ((mon rem (floor/ diff hcf)))
          (if (not (zero? rem)) $the-null-range
            ;; Look, it's less hassle than the CRT, OK?
            (bind ((start-index (find-key r2 
                                          (method (elt)
                                            (member? elt r1)))))
              (if (not start-index) $the-null-range
                (bind ((last1 (range-last r1))
                       (last2 (range-last r2))
                       (step (* (sgn step1) (range-extended-lcm step1 step2))))
                  (cond
                    ((and (not last1) (not last2))
                      (range from: (element r2 start-index)
                             by:   step))
                    ((or (not last1) (not last2))
		      (range from:    (element r2 start-index)
                             by:      step
                             through: (or last1 last2)))
                    (else:
                      (bind ((sup (if ((range-ordering r1) last1 last2)
                                    last1
                                    last2))
                             (start (element r2 start-index))
                             (end
			       (+ start
                                  (* (floor/ (- sup start) (abs step))
                                     (abs step)))))
                        (range from:    (element r2 start-index)
                               through: end
                               by:      step)))))))))))))

;; Iteration:

(define-method initial-state ((r <range>))
  (if (empty? r) #f 0))

(define-method next-state ((r <range>) (state <number>))
  (bind ((size (range-size r))
	 (step (range-by r))
	 (first (range-from r)))
    (if (or (not size) (< state (- size 1))) (+ state 1) #f)))

(define-method current-element ((r <range>) (state <number>))
  (+ (range-from r) (* state (range-by r))))

(define-method copy-state ((r <range>) (state <number>))
  state)

;;; NEW ITERATION PROTOCOL

(define range-next-state
  (method ((collection <range>) (state <integer>) #values (_ <integer>))
    (+ state (range-by collection))))

(define range-finished-state? 
  (method ((collection <range>) (state <integer>) limit #values (_ <boolean>))
    (if limit
	(> state (- limit 1))
	#F)))

(define range-current-element
  (method ((collection <range>) (state <integer>))
    (+ (range-from r) (* state (range-by r)))))

(define range-current-element-setter
  (method (new-value (collection <range>) (state <integer>))
    state))

(define-method forward-iteration-protocol ((collection <range>))
  (values (range-from collection) (range-size collection) range-next-state
	  range-finished-state? sequence-current-key sequence-current-key
	  sequence-current-element-setter identity-copy-state))

;; Coercion:

(define-method as ((class (singleton <range>)) (r <range>))
  r)

;; Misc:

(define-method print ((r <range>) 
	              #key
                      (stream   (standard-output))
                      (verbose? #f))
  (if (and (size r) (zero? (size r)))
    (format stream "#<the-empty-range>")
    (bind ((last (range-last r)))
      (if last
        (format stream
                "#<range: ~s through to ~s, by ~s>"
	        (range-from r)
	        last
	        (range-by r))
        (format stream
                "#<range: from ~s onwards, by ~s>"
	        (range-from r)
	        (range-by r)))
      r)))

;; eof
