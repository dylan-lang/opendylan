;;; COMPATIBILITY

(define-method initial-state ((collection <collection>))
  (bind ((initial-state limit next-state finished-state? 
          current-key current-element current-element-setter copy-state
          (forward-iteration-protocol collection)))
    (if (finished-state? collection initial-state limit)
        #F
        initial-state)))

(define-method next-state ((collection <collection>) state)
  (bind ((initial-state limit next-state finished-state? 
          current-key current-element current-element-setter copy-state
          (forward-iteration-protocol collection))
         (state (next-state collection state)))
    (if (finished-state? collection state limit)
        #F
        state)))

(define-method previous-state ((collection <collection>) state)
  (bind ((final-state limit previous-state finished-state? 
          current-key current-element current-element-setter copy-state
          (backward-iteration-protocol collection))
         (state (previous-state collection state)))
    (if (finished-state? collection state limit)
        #F
        state)))

(define-method final-state ((collection <collection>))
  (bind ((final-state limit previous-state finished-state? 
          current-key current-element current-element-setter copy-state
          (backward-iteration-protocol collection)))
    (if (finished-state? collection final-state limit)
        #F
        final-state)))

(define-method current-element ((collection <collection>) state)
  (bind ((initial-state limit next-state finished-state? 
          current-key current-element current-element-setter copy-state
          (forward-iteration-protocol collection)))
    (current-element collection state)))

(define-method current-element-setter 
    (new-value (collection <collection>) state)
  (bind ((initial-state limit next-state finished-state? 
          current-key current-element current-element-setter copy-state
          (forward-iteration-protocol collection)))
    (set! (current-element collection state) new-value)))

(define-method current-key ((collection <collection>) state)
  (bind ((initial-state limit next-state finished-state? 
          current-key current-element current-element-setter copy-state
          (forward-iteration-protocol collection)))
    (current-key collection state)))

(define-method current-key ((collection <collection>) state)
  (bind ((initial-state limit next-state finished-state? 
          current-key current-element current-element-setter copy-state
          (forward-iteration-protocol collection)))
    (current-key collection state)))

(define-method copy-state ((collection <collection>) state)
  (bind ((initial-state limit next-state finished-state? 
          current-key current-element current-element-setter copy-state
          (forward-iteration-protocol collection)))
    (copy-state collection state)))

;;; OTHER DIRECTION

(define-method forward-iteration-protocol ((collection <collection>))
  (bind-methods
      ((finished-state?  
           ((collection <collection>) state limit #values (_ <boolean>))
	 (not state)))
    (values (initial-state collection) #F next-state finished-state?
	    current-key current-element current-element-setter copy-state)))

(define-method backward-iteration-protocol ((collection <collection>))
  (bind-methods
      ((finished-state?  
           ((collection <collection>) state limit #values (_ <boolean>))
	 (not state)))
    (values (final-state collection) #F previous-state finished-state?
	    current-key current-element current-element-setter copy-state)))

;; eof
