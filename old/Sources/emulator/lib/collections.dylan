;;
;;; Collections.
;;;
;;;   This file defines the iteration protocol generics, the
;;;   collection operations and their default implementations in terms
;;;   of the primary protocols. Concrete collection classes are
;;;   defined elsewhere. 
;;;
;;;   Apart from <collection> itself, <explicit-key-xxx> and
;;;   <mutable-xxx> collections are dealt with here too.
;;;
;;

;
;; Primary iteration protocols:
;;
;;   New collection classes must implement methods on the following
;;   base iteration protocol in order to gain access to the full range
;;   of derived collection operations.
;;
;

;; --

;; <collection> protocol:

(define-generic-function initial-state   (coln))
(define-generic-function next-state      (coln state))
(define-generic-function current-element (coln state))
(define-generic-function copy-state      (coln state))
(define-generic-function element         (coln key #key default))
(define-generic-function key-sequence    (coln))

;; <collection> defaults:

(define-method class-for-copy ((c <collection>))
  <list>)

;; --

;; <stretchy-collection> protocol

(define-class <stretchy-collection> (<collection>))

(define-generic-function size-setter (size coln))

;; --

;; <mutable-collection> protocol:

(define-generic-function current-element-setter (new-value mut-coln state))
(define-generic-function element-setter         (new-value mut-coln key))

;; <mutable-collection> defaults:

(define-method class-for-copy ((mc <mutable-collection>)) 
  (object-class mc))

;; --

;; <explicit-key-collection> protocol:

(define-generic-function current-key (coln state))

(define-method class-for-copy ((ekc <explicit-key-collection>))
  <table>)

(define-method class-for-copy ((mekc <mutable-explicit-key-collection>))
  (object-class mekc))

;; <explicit-key-collection> defaults:

(define-method key-sequence ((c <explicit-key-collection>))
  (if (instance? c <sequence>) (next-method)
    (for ((keys  '()               (pair (current-key c state) keys))
	  (state (initial-state c) (next-state c state)))
        ((not state) keys))))

(bind ((noob (list 'noob)))

  (define-method element ((c <explicit-key-collection>) 
			  key 
			  #key (default noob))
    (for ((state (initial-state c) (next-state c state)))
         ((or (not state) (= (current-key c state) key))
          (if state (current-element c state)
            (if (id? default noob) 
	      (error 
	        "element: key ~s not found in ~s and no default was supplied"
		key c)
              default)))))

)

;; --

;; <mutable-explicit-key-collection> defaults:

(define-method element-setter (new-value
                                 (c <mutable-explicit-key-collection>)
				 key)
  (for ((state (initial-state c) (next-state c state)))
       ((or (not state) (= (current-key c state) key))
        (if state
          (set! (current-element c state) new-value)
	  (error 
	    "element-setter: key ~s not found in ~s to update with ~s"
	    key c new-value)))))

;; --

;
;; Derived collection operations.
;;
;;   The generic implementations of the following collection
;;   operations rely only upon the correct implementation of the above
;;   primitive iteration protocols for a particular collection class.
;;
;

;; --

;; <collection> derived protocol:

(define-generic-function size (coln))
(define-generic-function empty? (coln))

(define-generic-function do (proc first-coln #rest other-colns))
(define-generic-function map (proc coln #rest other-colns))

(define-generic-function any? (proc coln #rest other-colns))
(define-generic-function every? (proc coln #rest other-colns))

(define-generic-function reduce (proc init-val coln))
(define-generic-function reduce1 (proc coln))

(define-generic-function member? (value coln #key (test id?)))

;; --

;; <mutable-collection> derived protocol:

(define-generic-function map-into (mutable-coln proc coln #rest more-colns))
(define-generic-function map-as (class proc coln #rest more-colns))
(define-generic-function map (proc coln #rest more-colns))

;; --

;; Some iteration tools first:

(define do-collection-return
  (method (state early late result)
    (if state 
      (case early
	((result:) result)
	(else:     early))
      late)))

(define do-collections-by-key
  (method (f colns early-return late-return)
    (bind ((keys (reduce1 intersection (map key-sequence colns)))
           (res  #f))
      (for ((ks (initial-state keys) (next-state keys ks)))
	   ((or (not ks) 
                (bind ((key (current-element keys ks)))
                  (set! res (f key))))
             (do-collection-return ks early-return late-return res))))))

(define do-1-collection-by-value
  (method (f c early-return late-return)
    (bind ((res #f))
      (for ((s1 (initial-state c) (next-state c s1)))
           ((or (not s1) (set! res (f (current-element c s1))))
             (do-collection-return s1 early-return late-return res))))))

(define do-2-collections-by-key
  (method (f c1 c2 early-return late-return)
    (bind ((keys (intersection (key-sequence c1) (key-sequence c2)))
           (res  #f))
      (for ((ks (initial-state keys) (next-state keys ks)))
           ((or (not ks)
                (bind ((key (current-element keys ks)))
                  (set! res (f key))))
             (do-collection-return ks early-return late-return res))))))

;; --

;; <collection> derived protocol defaults:

(define-method size ((c <collection>))
  (for ((s     (initial-state c) (next-state c s))
        (count 0                 (+ count 1)))
       ((not s) count)))

(define-method empty? ((c <collection>))
  (not (initial-state c)))

(define-method do (f (c <collection>) #rest rest)
  (if (empty? rest) 
    (do-1-collection-by-value
      (method (val) 
        (f val)
        #f) 
      c
      #f
      #f)
    (bind ((all (pair c rest)))
      (do-collections-by-key
        (method (key) 
          (apply f (map (method (c) (element c key)) all))
          #f)
        all
        #f
        #f))))

(define-method map (f (c <collection>) #rest rest)
  (apply map-as (class-for-copy c) f c rest))

;; Dody defaults for map-as:
;;
;;(define-method map-as (class f (c <collection>) #rest rest)
;;  (select class subclass?
;;    ((<sequence>)
;;      (apply map-into 
;;        (make class size: (+ (reduce1 max
;;                                      (reduce intersection 
;;				              (key-sequence c)
;;				              (map key-sequence rest)))
;;                             1))
;;	f
;;	c
;;	rest))
;;    ((<collection>)
;;      (error "there's a fighting chance it make sense to map-as ~s, but I dunno how" 
;;        class))
;;    (else:
;;      (error "don't know how to map-as a ~s" 
;;        class))))

;;(define-method map-as (class f (c <collection>) #rest rest)
;;  (apply map-into
;;         (make class size: (+ (reduce1 max
;;                                       (reduce intersection
;;                                               (key-sequence c)
;;                                               (map key-sequence rest)))
;;                              1))
;;         f
;;         c
;;         rest))

(define-method map-as (class f (c <collection>) #rest rest)
  (apply map-into
         (make class size: (size
			     (reduce intersection
				     (key-sequence c)
				     (map key-sequence rest))))
         f
         c
         rest))

;; Default as method for coillections given the above implementation of map-as
(define-method as ((class (subclass <collection>)) (c <collection>))
  (map-as class identity c))



(define-method any? (f (c <collection>) #rest more)
  (if (empty? more)
    (do-1-collection-by-value
      (method (val)
        (f val))
      c
      result:
      #f)
    (bind ((all (pair c more)))
      (do-collections-by-key
        (method (key)
          (apply f (map (method (c) (element c key)) all)))
        all
        result:
        #f))))

(define-method every? (f (c <collection>) #rest more)
  (if (empty? more)
    (do-1-collection-by-value
      (method (val)
        (not (f val)))
      c
      #f
      #t)
    (bind ((all (pair c more)))
      (do-collections-by-key
        (method (key)
          (not (apply f (map (method (c) (element c key)) all))))
        all
        #f
        #t))))

(define-method reduce (f init (c <collection>))
  (bind ((acc init))
    (for ((s (initial-state c) (next-state c s)))
	 ((not s) acc)
      (set! acc (f acc (current-element c s))))
    acc))

(define-method reduce1 (f (c <collection>))
  (when (empty? c)
    (error "reduce1 requires a non-empty collection, got ~s" c))
  (bind ((s (initial-state c))
         (acc (current-element c s)))
    (for ((s (next-state c s) (next-state c s)))
	 ((not s) acc)
      (set! acc (f acc (current-element c s))))
    acc))

(define-method member? (value (c <collection>) #key (test id?))
  (for-each ((elt c))
            ((test value elt) #t)))

(define-method find-key ((c <collection>) ok? #key (skip 0) (failure #f))
  (bind-exit (return)
    (bind ((keys (key-sequence c))
           (seen 0))
    (for ((state (initial-state keys) (next-state keys state)))
         ((not state) failure)
      (bind ((key (current-element keys state)))
        (when (ok? (element c key))
	  (set! seen (+ seen 1))
	  (when (< skip seen) (return key))))))))

;; --

(define-method map-into ((target <mutable-collection>) 
			 f 
			 (source <collection>) 
			 #rest rest)
  (if (empty? rest)
    (do-2-collections-by-key 
      (method (key)
        (set! (element target key) (f (element source key)))
        #f)
      target
      source
      target
      target)
    (bind ((all (pair source rest)))
      (do-collections-by-key
        (method (key)
          (set! (element target key) 
		(apply f (map (method (c) (element c key)) all)))
          #f)
        all
        target
        target))))

(define-method replace-elements! ((c <mutable-collection>)
                                  take?
                                  replace
                                  #key (count #f))
  (if (and count (zero? count)) c
    (bind-exit (return)
      (bind ((keys (key-sequence c))
             (done 0))
        (for ((state (initial-state keys) (next-state keys state)))
             ((not state) c)
          (bind ((key (current-element keys state))
                 (elt (element c key)))
            (when (take? elt)
              (set! (element c key) (replace elt))
              (set! done (+ 1 done))
	      (when (and count (= count done)) 
                (return c)))))))))

(define-method fill! ((c <mutable-collection>) value #key start end)
  (for ((state (initial-state c) (next-state c state)))
       ((not state) c)
    (set! (current-element c state) value)))

;; --

(define-method shallow-copy ((c <collection>))
  (map-as (class-for-copy c) identity c))

;; --

(define-method = ((c1 <collection>) (c2 <collection>))
  (bind ((c1-keys (key-sequence c1))
         (c2-keys (key-sequence c2)))
    (if (= (size c1-keys) (size c2-keys))
        (bind ((keys (intersection c1-keys c2-keys)))
          (and (= (size keys) (size c1-keys))
               (not (for-each ((key keys)) 
			((/= (element c1 key) (element c2 key))
			 #t)))))
        #f)))

;; eof
