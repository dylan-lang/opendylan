;;
;;; Sequences.
;;;
;;;   This file defines the sequence protocol generics and their
;;;   default implementations. Also, some specialised implementations
;;;   of the standard collection operations are defined to avoid
;;;   the alignment necessary over an arbitrary set of collections. 
;;;
;;;   The mutable version of <sequence> is dealt with here too.
;;;
;;

;
;; Specialised implementations of the collection protocols.
;;
;;   Alignment is unnecessary on all-sequence maps, dos etc and these
;;   can be more efficiently implemented in this case.
;;
;

;; --

;; <sequence> collection protocol defaults:

(define-method key-sequence ((c <sequence>))
  (bind ((seq-size (size c)))
    (if seq-size 
      (range from: 0 up-to: seq-size by: 1)
      (range from: 0 by: 1))))

(bind ((noob (list 'noob)))

  (define-method element ((c <sequence>) key #key (default noob))
    (for ((state (initial-state c) (next-state c state))
          (k     0                 (+ k 1)))
         ((or (not state) (= k key))
          (if state (current-element c state)
            (if (id? default noob) 
	      (error
	        "element: key ~s not found in ~s and no default was supplied"
		key c)
              default)))))

)

;; --

;; <strechy-sequence>

(define-class <stretchy-sequence> (<sequence> <stretchy-collection>))

;; --

;; <mutable-sequence> collection protocol defaults:

(define-method element-setter (new-value
                                 (c <mutable-sequence>) 
				 (key <integer>))
  (for ((state (initial-state c) (next-state c state))
        (k     0                 (+ k 1)))
       ((or (not state) (= k key))
        (if state
          (set! (current-element c state) new-value)
	  (error
	    "element: key ~s not found in ~s to update with ~s"
	    key c new-value)))))

;; --

;; <sequence> protocol:

(define-generic-function add                  (seq elt))
(define-generic-function add!                 (seq elt))
(define-generic-function add-new              (seq elt #key test))
(define-generic-function add-new!             (seq elt #key test))
(define-generic-function remove               (seq val #key test count))
(define-generic-function remove!              (seq val #key test count))
(define-generic-function choose               (pred seq))
(define-generic-function choose-by            (pred test-seq val-seq))
(define-generic-function intersection         (seq1 seq2 #key test))
(define-generic-function union                (seq1 seq2 #key test))
(define-generic-function remove-duplicates    (seq #key test))
(define-generic-function remove-duplicates!   (seq #key test))
(define-generic-function copy-sequence        (seq #key start end))
(define-generic-function concatenate-as       (class seq #rest more))
(define-generic-function concatenate          (seq #rest more))
(define-generic-function replace-subsequence! (mut-seq ins-seq #key start end))
(define-generic-function reverse              (seq))
(define-generic-function reverse!             (seq))
(define-generic-function sort                 (seq #key test stable))
(define-generic-function sort!                (seq #key test stable))

(define-generic-function first  (seq #rest default))
(define-generic-function second (seq #rest default))
(define-generic-function third  (seq #rest default))
(define-generic-function last   (seq #rest default))

(define-generic-function first-setter  (val seq))
(define-generic-function second-setter (val seq))
(define-generic-function third-setter  (val seq))
(define-generic-function last-setter   (val seq))

(define-generic-function subsequence-position (seq to-find #key test count))

;; --

;; <sequence> defaults:

(define-method add ((seq <sequence>) elt)
  (bind ((seq-size (size seq))
         (new      (make (class-for-copy seq) size: (+ seq-size 1))))
    (set! (element new 0) elt)
    (dotimes (i seq-size)
      (set! (element new (+ i 1)) (element seq i)))
    new))

(define-method add! ((seq <sequence>) elt)
  (add seq elt))

(define-method add-new ((seq <sequence>) elt #key (test id?))
  (if (member? elt seq test: test) seq
    (add seq elt)))

(define-method add-new! ((seq <sequence>) elt #key (test id?))
  (if (member? elt seq test: test) seq
    (add! seq elt)))

;; For shame...

(define-method remove ((seq <sequence>) elt #key (test id?) (count #f))
  (bind ((new-size 
	   (bind-method measure ((state (initial-state seq))
				 (seen 0)
				 (keep 0))
	     (cond
	       ((not state) 
		 keep)
	       ((and (or (not count) (< seen count))
		     (test (current-element seq state) elt))
		 (measure (next-state seq state)
			  (+ seen 1)
			  keep))
	       (else:
		 (measure (next-state seq state)
			  seen
			  (+ keep 1))))))
	 (new (make (class-for-copy seq) size: new-size)))
    (bind-method fill ((seq-state (initial-state seq))
		       (new-state (initial-state new))
		       (seen 0))
      (cond
        ((not seq-state) 
	  new)
	((and (or (not count) (< seen count))
	      (test (current-element seq seq-state) elt))
	  (fill (next-state seq seq-state)
		new-state
		(+ seen 1)))
	(else:
	  (set! (current-element new new-state)
		(current-element seq seq-state))
	  (fill (next-state seq seq-state)
		(next-state new new-state)
		seen))))))

(define-method remove! ((seq <sequence>) elt #key (test id?) (count #f))
  (if count
    (remove seq elt test: test count: count)
    (remove seq elt test: test)))

(define-method choose (take? (seq <sequence>))
  (bind ((new-size 
	   (bind-method measure ((state (initial-state seq))
				 (keep 0))
	     (cond
	       ((not state) 
		 keep)
	       ((take? (current-element seq state))
		 (measure (next-state seq state)
			  (+ keep 1)))
	       (else:
		 (measure (next-state seq state)
			  keep)))))
	 (new (make (class-for-copy seq) size: new-size)))
    (bind-method fill ((seq-state (initial-state seq))
		       (new-state (initial-state new)))
      (cond
        ((not seq-state) 
	  new)
	((take? (current-element seq seq-state))
	  (set! (current-element new new-state)
		(current-element seq seq-state))
	  (fill (next-state seq seq-state)
		(next-state new new-state)))
	(else:
	  (fill (next-state seq seq-state)
		new-state))))))

(define-method choose-by (take? (tst <sequence>) (val <sequence>))
  (bind ((new-size 
	   (bind-method measure ((tst-state (initial-state tst))
				 (val-state (initial-state val))
				 (keep 0))
	     (cond
	       ((or (not tst-state) (not val-state)) 
		 keep)
	       ((take? (current-element tst tst-state))
		 (measure (next-state tst tst-state)
			  (next-state val val-state)
			  (+ keep 1)))
	       (else:
		 (measure (next-state tst tst-state)
			  (next-state val val-state)
			  keep)))))
	 (new (make (class-for-copy val) size: new-size)))
    (bind-method fill ((tst-state (initial-state tst))
		       (val-state (initial-state val))
		       (new-state (initial-state new)))
      (cond
        ((or (not tst-state) (not val-state))
	  new)
	((take? (current-element tst tst-state))
	  (set! (current-element new new-state)
		(current-element val val-state))
	  (fill (next-state tst tst-state)
		(next-state val val-state)
		(next-state new new-state)))
	(else:
	  (fill (next-state tst tst-state)
		(next-state val val-state)
		new-state))))))

(define-method concatenate-as (class (seq <sequence>) #rest more)
  (if (empty? more) (as class seq)
    (bind ((len (+ (reduce (method (acc seq) (+ acc (size seq))) (size seq) more)))
           (new (make class size: len))
           (i 0))
      (for-each ((seq (pair seq more)))
                ()
        (for-each ((elt seq))
                  ()
          (set! (element new i) elt)
          (set! i (+ i 1))))
      new)))

(define-method concatenate ((seq <sequence>) #rest more)
  (apply concatenate-as (class-for-copy seq) seq more))

(define-method reverse ((seq <sequence>))
  (bind ((seq-size (size seq))
         (new-seq (make (class-for-copy seq) size: seq-size)))
    (dotimes (i seq-size)
      (set! (element new-seq i) 
            (element seq (- seq-size (+ i 1)))))
    new-seq))

(define-method reverse! ((seq <sequence>))
  (reverse seq))

(define-method reverse! ((seq <mutable-sequence>))
  (bind ((sz (size seq)))
    (dotimes (i (floor/ sz 2))
      (bind ((tmp (element seq i)))
        (set! (element seq i) (element seq (- sz i 1)))
        (set! (element seq (- sz i 1)) tmp)))
    seq))

(define-method intersection ((s1 <sequence>) (s2 <sequence>) #key (test id?))
  (bind ((result '()))
    (for-each ((elt s1)) ()
      (when (member? elt s2 test: test)
	(set! result (pair elt result))))
    (as (class-for-copy s1) result)))

(define-method union ((s1 <sequence>) (s2 <sequence>) #key (test id?))
  (bind ((s2-keep '()))
    (for-each ((elt s2)) ()
      (unless (member? elt s1 test: test)
        (set! s2-keep (pair elt s2-keep))))
    (concatenate-as (class-for-copy s1) s1 s2-keep)))

(define-method remove-duplicates ((seq <sequence>) #key (test id?))
  (bind ((keep '()))
    (for-each ((elt seq)) ()
      (unless (member? elt keep test: test)
        (set! keep (pair elt keep))))
    (as (class-for-copy seq) keep)))

(define-method remove-duplicates! ((seq <sequence>) #key (test id?))
  (remove-duplicates seq test: test))

(define-method copy-sequence ((seq <sequence>) #key (start 0) (end #f))
  (unless end (set! end (size seq)))
  (bind ((new (make (class-for-copy seq) size: (- end start))))
    (for ((seq-i start (+ seq-i 1))
	  (new-i 0     (+ new-i 1)))
	 ((= seq-i end) new)
      (set! (element new new-i) (element seq seq-i)))))

(define-method replace-subsequence! ((target <sequence>)
				     (source <sequence>)
				     #key (start 0) (end (size target)))
  ;; not particularly efficient, but it works
  ;; fix it when someone needs it
  (concatenate (copy-sequence target start: 0 end: start)
	       source
	       (copy-sequence target start: end)))

(define-method replace-subsequence! ((target <mutable-sequence>)
				     (source <sequence>)
				     #key (start 0) (end (size target)))
  (bind ((source-size (size source)))
     (when (== source-size (- end start))
	(for ((target-i start (+ target-i 1))
	      (source-i 0     (+ source-i 1)))
	     ((= target-i end) target)
	  (set! (element target target-i) (element source source-i))))
     ;; not particularly efficient, but it works
     ;; fix it when someone needs it
     (concatenate (copy-sequence target start: 0 end: start)
	          source
	          (copy-sequence target start: end))))

;; This looks like infinite recursion but will always bottom-out because 
;; the class-for-copy should always be a mutable sequence.

(define-method sort ((seq <sequence>) #key (test <) (stable #f))
  (sort! (shallow-copy seq) test: test stable: stable))

(define-method sort! ((seq <sequence>) #key (test <) (stable #f))
  (sort! (shallow-copy seq) test: test stable: stable))

;; A blast from the past! It's... bubble sort!!

(define-method sort! ((seq <mutable-sequence>) #key (test <) (stable #f))
  (bind ((len  (size seq))
         (flag #t))
    (if (zero? len) seq
      (for ((i (- len 1) (- i 1)))
	   ((or (= i 0) (not flag)) seq)
        (set! flag #f)
	(for ((j 0 (+ j 1)))
	     ((= j i))
	  (bind ((a (element seq j))
		 (b (element seq (+ j 1))))
	    (when (test b a)
	      (set! flag #t)
	      (set! (element seq j) b)
	      (set! (element seq (+ j 1)) a))))))))

;; --

(define-method first ((seq <sequence>) #key (default (void)))
  (if (void? default)
    (element seq 0)
    (element seq 0 default: default)))

(define-method second ((seq <sequence>) #key (default (void)))
  (if (void? default)
    (element seq 1)
    (element seq 1 default: default)))

(define-method third ((seq <sequence>) #key (default (void)))
  (if (void? default)
    (element seq 2)
    (element seq 2 default: default)))

(define-method last ((seq <sequence>) #key (default (void)))
  (if (empty? seq)
    (if (void? default)
      (error "last: the empty sequence ~s has no last element"
             seq)
      default)
    (if (void? default)
      (element seq (- (size seq) 1))
      (element seq (- (size seq) 1) default: default))))

(define-method first-setter (value (seq <mutable-sequence>))
  (set! (element seq 0) value))

(define-method second-setter (value (seq <mutable-sequence>))
  (set! (element seq 1) value))

(define-method third-setter (value (seq <mutable-sequence>))
  (set! (element seq 2) value))

(define-method last-setter (value (seq <mutable-sequence>))
  (set! (element seq (- (size seq) 1)) value))

;; --

(define-method subsequence-position ((seq <sequence>) 
				     (pat <sequence>)
				     #key (test id?)
				          (count 1))
  (bind-exit (done)
    (bind ((seq-size (size seq))
	   (pat-size (size pat))
	   (seen 0))
      (bind-methods
        ((compare-from (i)
	   (bind-exit (fail)
	     (for ((seq-i i (+ seq-i 1))
		   (pat-i 0 (+ pat-i 1)))
		  ((= pat-i pat-size) 
		    (set! seen (+ seen 1))
		    (if (= seen count) (done i) #f))
	       (unless (test (element seq seq-i) (element pat pat-i))
		 (fail #f))))))
	(for ((i 0 (+ i 1)))
	     ((< (- seq-size i) pat-size) #f)
	  (compare-from i))))))
       
;; --


(define-method add! ((seq <mutable-sequence>) elt)
  (add seq elt))

;; --

(define-method = ((s1 <sequence>) (s2 <sequence>))
  (and (= (size s1) (size s2))
       (every? = s1 s2)))

;; --

(define-method map-as (class f (c <sequence>) #rest rest)
  (if (not (every? (rcurry instance? <sequence>) rest))
    (next-method)
    (select class subclass?
      ((<sequence>)
        (bind ((target-size
                 (reduce 
		   (method (acc seq)
		     (if acc
		       (bind ((sz (size seq)))
			 (if sz (min acc sz) acc))
		       (size seq)))
                   (size c)
                   rest)))
          (unless target-size
            (error "All sequences to be mapped are infinite - ~s"
                   (pair c rest)))
          (apply map-into 
                 (make class size: target-size)
		 f
		 c
		 rest)))
      (else:
        (next-method)))))

;; --

(define-method fill! ((s <mutable-sequence>) value #key (start 0) end)
  (for-each ((i (range from: start up-to: (or end (size s))))) ()
    (set! (element s i) value))
  s)

;;; NEW ITERATION PROTOCOL

(define identity-copy-state
  (method ((collection <sequence>) (state <integer>) #values (_ <integer>))
    state))

(define sequence-next-state 
  (method ((collection <sequence>) (state <integer>) #values (_ <integer>))
    (+ state 1)))

(define sequence-finished-state?
  (method ((collection <sequence>) (state <integer>) (limit <integer>)
	   #values (_ <boolean>))
    (= state limit)))

(define sequence-current-key 
  (method ((collection <sequence>) (state <integer>)) state))

(define sequence-current-element-setter
  (method ((collection <sequence>) (state <integer>))
    (error "Sequence ~S is immutable" collection)))

(define-method forward-iteration-protocol ((collection <sequence>))
  (values 0 (size collection) sequence-next-state sequence-finished-state?
	  sequence-current-key element sequence-current-element-setter
	  identity-copy-state))

(define sequence-previous-state
  (method ((collection <sequence>) (state <integer>) #values (_ <integer>))
    (- state 1)))

(define-method backward-iteration-protocol ((collection <sequence>))
  (values (- (size collection) 1) -1 sequence-previous-state 
          sequence-finished-state? sequence-current-key
          element sequence-current-element-setter identity-copy-state))

;; eof
