;; Hacked doubly-linked list structure:

(define-class <doubly-linked-list> (<mutable-sequence>)
  (node-last)
  (node-value)
  (node-next))

(define null-node? null?)

(define cons-node!
  (method (last value next)
    (bind ((new (allocate <doubly-linked-list>)))
      (set! (node-last new) last)
      (unless (null? last)
        (set! (node-next last) new))
      (set! (node-value new) value)
      (set! (node-next new) next)
      (unless (null? next)
        (set! (node-last next) new))
      new)))

(define node-last!
  (method (node)
    (bind ((last (node-last node)))
      (set! (node-next last) '())
      last)))

(define node-next!
  (method (node)
    (bind ((next (node-next node)))
      (set! (node-last next) '())
      next)))

(define cons-node-after!
  (method (node value)
    (cons-node! node value (node-next node))))

(define cons-node-before!
  (method (node value)
    (cons-node! (node-last node) value node)))

(define delete-node!
  (method (node)
    (bind ((last (node-last node))
           (next (node-next node)))
      (set! (node-next last) next)
      (set! (node-last next) last)
      (node-value node))))

;; Iteration:

(define-method empty? ((l <doubly-linked-list>))
  (null-node? l))

(define-method size ((l <doubly-linked-list>))
  (if (null-node? l)
      0
      (+ 1 (size (node-next l)))))

(define-method initial-state ((l <doubly-linked-list>))
  (if (null-node? l) #f l))

(define-method next-state ((l <doubly-linked-list>) (state <doubly-linked-list>))
  (bind ((next-state (node-next state)))
    (if (null-node? l) #f next-state)))

(define-method current-element ((l <doubly-linked-list>) (state <doubly-linked-list>))
  (node-value state))

(define-method current-element-setter (value
                                         (l <doubly-linked-list>)
                                         (state <doubly-linked-list>))
  (set! (node-value state) value))

(define-method copy-state ((l <doubly-linked-list>) (state <doubly-linked-list>))
  state)

;; eof

