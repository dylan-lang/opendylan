;; This abstract table class will be redefined later by the
;; native table implementation.

(define-class <table> (<mutable-explicit-key-collection>
                       <stretchy-collection>))

(define-method make ((c (singleton <table>)) #key)
  (make <lisp-object-table>))

(define-class <lisp-object-table> (<table>)
  (table-table)
  ;; table-values is a doubly-linked list
  ;; entries are (value key . back-link)
  (table-values-slot))

(define-generic-function remove-key! (table key))

;; Specific:

(define-method initialize ((tab <lisp-object-table>) #key test)
  (next-method)
  (set! (table-table tab) (make/table))
  (set! (table-values-slot tab) #f))

(define-method size ((tab <lisp-object-table>))
  (size/table (table-table tab)))

(define-method empty? ((tab <lisp-object-table>))
  (id? (size/table (table-table tab)) 0))

(define build-table-iteration-list
  (method (lisp-table)
    (bind ((list (pair #f '()))
	   (list-tail list))
      (do/function*table (method (key entry)
			   (bind ((new-list-pair (pair entry '())))
			     ;; set back-link
			     (set! (tail (tail entry)) list-tail)
			     (set! (tail list-tail) new-list-pair)
			     (set! list-tail new-list-pair)))
			 lisp-table)
      list)))

(define table-values
  (method ((tab <lisp-object-table>))
    (or (table-values-slot tab)
	(set! (table-values-slot tab)
	      (build-table-iteration-list (table-table tab))))))

(bind ((noob (list 'noob)))

  (define-method element ((tab <lisp-object-table>) key #key (default noob))
    (bind ((cell (element/table*object (table-table tab) key #f)))
      (if cell (head cell)
          (if (== default noob)
	      (error
	        "element: key ~s not found in ~s and no default was supplied"
	         key tab)
              default))))

)

(define-method element-setter (value (tab <lisp-object-table>) key)
  (bind ((cell (element/table*object (table-table tab) key #f)))
    (cond
      (cell 
        (set! (head cell) value))
      (else:
        (bind ((table-values (table-values-slot tab))
	       (new-cell (pair value (pair key table-values))))
	  (set-element/table*object (table-table tab)
				    key new-cell)
	  (when table-values
	    (bind ((old-head (tail table-values))
		   (new-entry (pair new-cell old-head)))
	      (unless (== old-head '())
		(set! (tail (tail (head old-head))) new-entry))
	      (set! (tail table-values) new-entry)))
	  value)))))

(define-method remove-key! ((tab <lisp-object-table>) key)
  (bind ((hash (table-table tab))
         (cell (element/table*object hash key #f)))
    (when cell
      (remove-key/table*object hash key)
      (when (table-values-slot tab)
	(bind ((prev (tail (tail cell)))
	       (next (tail (tail prev))))
	      (set! (tail prev) next)
	      (unless (== next '())
		(set! (tail (tail (head next))) prev)))))
    tab))

(define-method clear ((tab <lisp-object-table>))
  (initialize tab))

;; Misplaced.

(define-method map-into ((target <stretchy-collection>)
			 f
			 (source <collection>)
			 #rest sources)
  (bind ((all-sources (pair source sources)))
    (do-collections-by-key
      (method (key)
	(set! (element target key)
	      (apply f (map (rcurry element key) all-sources)))
	#f)
      all-sources
      'ignored
      target)))

(define-method map-as ((class (singleton <table>)) f c #rest more)
  (apply map-into (make <table>) f c more))

;; Iteration:

(define-method initial-state ((tab <lisp-object-table>))
  (bind ((vals (tail (table-values tab))))
    (if (empty? vals) #f vals)))

(define-method next-state ((tab <lisp-object-table>) state)
  (bind ((next (tail state)))
    (if (empty? next) #f next)))

(define-method copy-state ((tab <lisp-object-table>) state)
  state)

(define-method current-element ((tab <lisp-object-table>) state)
  (head (head state)))

(define-method current-key ((tab <lisp-object-table>) state)
  (head (tail (head state))))

(define-method current-element-setter (value (tab <lisp-object-table>) state)
  (set! (head (head state)) value)
  value)


;;; NEW ITERATION PROTOCOL

(define table-current-element
  (method ((collection <lisp-object-table>) (state <list>) #values (_ <object>))
    (head (head state))))

(define table-current-key
  (method ((collection <lisp-object-table>) (state <list>) #values (_ <object>))
    (head (tail (head state)))))

(define table-current-element-setter
  (method (new-value (collection <lisp-object-table>) (state <list>) #values (_ <object>))
    (set! (head (head state)) new-value)))

(define-method forward-iteration-protocol ((collection <lisp-object-table>))
  (values (tail (table-values collection)) '()
          list-next-state list-finished-state?
	  table-current-key table-current-element table-current-element-setter
	  identity-copy-state))

;; Stretchy:

(define-method size-setter (size (tab <lisp-object-table>))
  size)

;; Coercion:

(define-method as ((class (singleton <lisp-object-table>)) (tab <lisp-object-table>))
  tab)

(define-method as ((class (singleton <lisp-object-table>)) (c <explicit-key-collection>))
  (bind ((new (make <lisp-object-table>)))
    (for ((state (initial-state c) (next-state c state)))
         ((not state) new)
      (set! (element new (current-key c state)) (current-element c state)))))

(define-method as ((class (singleton <lisp-object-table>)) (c <collection>))
  (bind ((new (make <lisp-object-table>))
         (keys (key-sequence c)))
    (for ((state (initial-state keys) (next-state keys state)))
         ((not state) new)
      (bind ((key (current-element keys state)))
        (set! (element new key) (element c key))))))

;; --

(define-method print ((tab <lisp-object-table>)
	              #key
                      (stream   (standard-output))
                      (verbose? #f))
  (format stream "{<object-table>, size: ~s}" (size/table (table-table tab)))
  tab)

;; eof

