module:    internal
language:  prefix-dylan
author:    Eliot Miranda and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-class <hashed-collection> 
    (<stretchy-collection> <mutable-collection>)
  ;; private slots
  ;; count of number of entries in table
  (tally    init-value: 0 type: <integer>)
    ;; Mask for generating in-range indices from hash codes.
    ;; Since elements is arranged as key value pairs mask is even
    ;; and 2 less than the size of elements.  Elements is guaranteed
    ;; to be at least of size 2 and of size a power of 2. e.g. if
    ;; elements has 4 slots (room enough for 2 key-value pairs) the
    ;; valid key indices are 0 and 2.  So if mask is 2 (4 - 2) anding
    ;; any hash with mask yields either 0 or 2.
  (mask     type: <integer>)
    ;; elements holds key-value pairs, keys at even indices, associated values
    ;; at following odd indices, void in a key position indicating an unused
    ;; location.  elements is guaranteed to contain at least one unused
    ;; location, so that index-for-key does not have to check for
    ;; wrap around.
  (elements type: <simple-object-vector>)
  ;; private void element
  ;; (void-element allocation: class init-value: (unbound))
  gc-state ;; !@#$ NO INIT-VALUE NEEDED
)

(define-generic-function table-protocol (table))

(define-method initialize ((table <hashed-collection>) #rest any-keys)
  (next-method)
  (set! (gc-state table) (current-gc-state)))

(define-method void-element ((table <hashed-collection>)) (unbound))

(define-method key-test ((table <hashed-collection>) #values (_ <function>))
  (bind ((test-function hash-function (table-protocol table)))
    test-function))

(define-generic-function rehash-using!
    ((collection <hashed-collection>) (new-collection <hashed-collection>)
     #values (new-collection <hashed-collection>)))

(define-method grow!
    ((table <hashed-collection>) #values (table <hashed-collection>))
  (rehash-using! table (make (object-class table)
                             raw-size: (* 2 (size (elements table)))))
  table)

(define-method rehash!
    ((table <hashed-collection>) #values (table <hashed-collection>))
  (rehash-using! table (make (object-class table)
                             raw-size: (size (elements table))))
  table)

;; Convenience method for DEBUGGING only

(define-method clear!
    ((table <hashed-collection>) #values (table <hashed-collection>))
  (fill! (elements table) (void-element table))
  (set! (tally table) 0)
  table)

(define-method remove-all-keys!
    ((table <hashed-collection>) #values (table <hashed-collection>))
  (fill! (elements table) (void-element table))
  (set! (tally table) 0)
  table)

;; Common Accessing Methods

(define-method size ((table <hashed-collection>) #values (size <integer>))
  (tally table))

(define-method empty? ((table <hashed-collection>) #values (_ <boolean>))
  (= 0 (tally table)))

;; Shared Iteration Protocol Methods

;; Simply use integer indices for the iteration protocol.
;; This is fine since we don't have to worry about stability; i.e. we
;; can assume that the table does not change during an iteration over its elements

(define-method copy-state
    ((table <hashed-collection>) (index <integer>) #values (index <integer>))
  index)

;;;;;
;;;;; HASH FUNCTIONS
;;;;;

(define $=hash-mash-rotate-bits 7)
(define $=hash-mash-total-bits 26)
(define $=hash-mash-mask
    (- (ash 1 (- $=hash-mash-total-bits $=hash-mash-rotate-bits)) 1))

(define-method merge-hash-ids 
    ((dst <integer>) (src <integer>) #key ordered)
  (if ordered
      (logxor (ash src (- $=hash-mash-rotate-bits $=hash-mash-total-bits))
	      (ash (logand src $=hash-mash-mask) $=hash-mash-rotate-bits)
	      dst)
      (logxor dst src)))
	  
(define $permanent-hash-state #F)

(define-method merge-hash-states 
    ((state-1 (union <integer> (singleton #f)))
     (state-2 (union <integer> (singleton #f))))
  (if (id? state-1 $permanent-hash-state)
      state-2
      (if (id? state-2 $permanent-hash-state)
          state-1
          (max state-1 state-2))))

(define-method merge-hash-codes 
    ((id-1 <integer>) (state-1 (union <integer> (singleton #f)))
     (id-2 <integer>) (state-2 (union <integer> (singleton #f)))
     #key ordered)
  (values (merge-hash-ids id-1 id-2 ordered: ordered)
          (merge-hash-states state-1 state-2)))

;;; OBJECT-HASH

(define-method object-hash (object)
  (values (address-of object) (current-gc-state)))

(define-method object-hash ((object <boolean>)) 
  (values (if object 0 1) $permanent-hash-state))

(define-method object-hash ((object <integer>)) 
  (values object $permanent-hash-state))

(define-method object-hash ((object <character>))
  (object-hash (as <integer> object)))

(define-method object-hash ((object <single-float>))
  ;; !@#$ ONLY APPROXIMATION FOR object-hash
  (object-hash (decode-single-float object)))

(define-method object-hash ((object <double-float>))
  ;; !@#$ ONLY APPROXIMATION FOR object-hash
  (bind ((low high (decode-double-float object)))
    (object-hash (logior low (ash high 32)))))

;;;;
;;;; CONCRETE HASHED CLASSES.
;;;;

(define-class <table> (<hashed-collection> <explicit-key-collection>))

;; Default table creation

(define-method make ((class (singleton <table>)) #rest all-keys)
  (apply make <object-table> all-keys))

;; Common methods

(define-method initialize ((table <table>) #key (size 4) raw-size)
  ;; round up size to nearest power of two & multiply by two because
  ;; keys & values appear as pairs in elements
  (next-method)

  (bind (((size <integer>) size) ;; *@@* TYPE
	 ((power_2_size <integer>) ;; *@@* TYPE
	  (or (and raw-size
		   (bind (((raw-size <integer>) raw-size)) raw-size)) 
	      (power-of-two-ceiling (* size 2))))
	 ((the-elements <simple-object-vector>) ;; *@@* TYPE
	  (make <simple-object-vector>
		size: power_2_size fill: (void-element table))))
    (set! (elements table) the-elements)
    (set! (mask table) (- power_2_size 2))
    table))

(define-method index-for-key
    ((table <table>) key #values (index <integer>) (present? <boolean>))
  (bind ((test-function hash-function (table-protocol table))
	 (void (void-element table))
	 (mask (mask table))
	 (elements (elements table))
         ((hash-value <integer>) hash-state (hash-function key)))
    (iterate search (((i <integer>) (logand mask hash-value)))
       (bind ((probe (element elements i)))
	 (if (id? void probe)
	     (if (or (id? hash-state $permanent-hash-state)
		     (= (gc-state table) (current-gc-state)))
                 (values i #F)
                 (begin
                  (rehash! table)
                  (set! (gc-state table) (current-gc-state))
                  (index-for-key table key)))
	     (if (test-function key probe)
		 (values i #T)
		 (search (logand (+ i 2) mask))))))))

;; Inlined "quick" grow/rehash inner rehash-using!

(define-method rehash-using! ((table <table>) (new-table <table>))
  (bind ((test-function hash-function (table-protocol table))
	 (the-new-mask (mask new-table))
         (limit (mask table))
         (void  (void-element table))
         (the-elements (elements table))
         (the-new-elements (elements new-table)))
     (bind-methods
	 ((index-for-absent-key (key #values (index <integer>))
	    (bind (((hashed-value <integer>) (hash-function key))) ;; *@@* TYPE
	      (iterate search
		  (((i <integer>)
                    (logand the-new-mask hashed-value)))
		(bind ((probe (element the-new-elements i)))
		  (if (id? void probe)
		      i
		      (search (logand (+ i 2) the-new-mask)))))))

	  (set-absent-element! (key value)
	    (bind (((index <integer>) (index-for-absent-key key))) ;; *@@* TYPE
	      (set! (element the-new-elements index) key)
	      (set! (element the-new-elements (+ index 1)) value))))

       (iterate search (((i <integer>) 0))
	 (if (> i limit)
	     (begin
	      ;; no need to do (set! (tally table) (tally new-table))
	      (set! (mask table) the-new-mask)
	      (set! (elements table) the-new-elements)
	      table)
	     (bind ((key (element the-elements i)))
	       (when (not (id? void key))   
		 (set-absent-element! key (element the-elements (+ i 1))))
	       (search (+ i 2))))))))

;; Table Accessing Methods

(define-method element ((table <table>) key #key (default $not-found))
  (bind (((index <integer>) present? (index-for-key table key))) ;; *@@* TYPE
    (if present?
        (element (elements table) (+ index 1))
        (if (id? $not-found default)
	    (signal (make <key-not-found-error>) collection: table key: key)
            default))))

(define-method element-setter (value (table <table>) key)
  (bind (((index <integer>) present? (index-for-key table key)) (tally (tally table))) ;; *@@* TYPE
    (if present?
        (set! (element (elements table) (+ index 1)) value)
        (if (<= (* 3 tally) (mask table)) ;; keep table < 2/3 full.
            (begin                        ;; should really be per-class const
	     (set! (tally table) (+ tally 1))
             (set! (element (elements table) index) key)
             (set! (element (elements table) (+ index 1)) value))
            (set! (element (grow! table) key) value)))))

(define-method includes-key? ((table <table>) key)
  (bind ((index present? (index-for-key table key)))
    present?))

(define-method remove-key! ((table <table>) key)
  (bind ((old-index present? (index-for-key table key)))
    (when present?
      (bind ((elements (elements table))
	     (void (void-element table))
	     (mask (mask table)))
	(set! (tally table) (- (tally table) 1))
	(set! (element elements old-index) void)
	(set! (element elements (+ old-index 1)) void)
        (rehash! table)
        #|
	(iterate fixup (((i <integer>) (logand mask (+ old-index 2))))
          (bind ((next-key (element elements i)))
	    (unless (id? void next-key)
	      (bind ((next-value (element elements (+ i 1)))
		     (next-index (index-for-key table next-key)))
		(unless (= i next-index)
		  (set! (element elements next-index) next-key)
		  (set! (element elements (+ next-index 1)) next-value)
		  (set! (element elements i) void)
		  (set! (element elements (+ i 1)) void)
		  (fixup (logand mask (+ i 2))))))))
        |#
	table))))

;; (signal (make <key-not-found-error>) collection: table key: key)

;;;
;;; Table Iteration Protocol Methods
;;;
;;; Need to be careful because the table could rehash during an interation
;;; Therefore we hang onto the elements before the rehash!.   NB Assumes
;;; that rehash! copies the elements.
;;;

(define-class <table-state> (<object>)
  (elements required-init-keyword: elements: type: <simple-object-vector>)
  (index required-init-keyword: index: type: <integer>))

(define-method copy-state
    ((table <table>) (state <table-state>)
     #values (state <table-state>))
  (make <table-state> elements: (elements state) index: (index state)))

(define-method initial-state
    ((table <table>) #values (_ (union <table-state> (singleton #F))))
  (if (= (tally table) 0)
      #F
      (bind (((index <integer>) 0) 
             (void (void-element table))
             (elements (elements table)))
        (while (id? void (element elements index)) 
          (set! index (+ index 2)))
        (make <table-state> elements: elements index: index))))

(define-method next-state
    ((table <table>) (state <table-state>)
     #values (state (union <table-state> (singleton #F))))
  (bind ((void (void-element table))
         (elements (elements state))
         (limit (mask table)))
    (iterate search (((i <integer>) (+ 2 (index state))))
      (if (> i limit)
	  #F
	  (bind ((probe (element elements i)))		 
	    (if (not (id? probe void))
                (begin
                 (set! (index state) i)
                 state)
		(search (+ i 2))))))))

(define-method current-element ((table <table>) (state <table-state>)) 
  (element (elements table) (+ (index state) 1)))

(define-method current-element-setter
    (new-value (table <table>) (state <table-state>) #values new-value)
  (set! (element (elements table) (+ (index state) 1)) new-value))

(define-method current-key ((table <table>) (state <table-state>)) 
  (element (elements table) (index state)))

;;;;
;;;; <OBJECT-TABLE>
;;;;

(define-class <object-table> (<table>))

(define-method table-protocol ((table <object-table>))
  (values id? object-hash))
