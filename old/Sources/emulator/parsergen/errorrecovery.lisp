;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/errorrecovery.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)

(export '(push-and-restart discard-input *token-pos-function* *symbol-to-string*))

(defvar *state-stack*)

(defvar *value-stack*)

(defvar *error-found*)

(defvar *token-pos-function*)

(defvar *recovery-symbol-list*)

(defvar *symbol-to-string*)

(defvar *error-productions*)
(defvar *error-action-nt-table*)
(defvar *nt-error-productions*)

(defun get-error-production-index (nt)
  (gethash nt *nt-error-productions*))

(defun get-error-production (index)
  (svref *error-productions* index))

(defun get-error-production-nt (index)
  (svref *error-action-nt-table* index))

(defvar *default-error-recovery*)

(defvar *custom-error-recovery*)

(defun recover-from-error()
  (when *custom-error-recovery*
    (funcall *custom-error-recovery*)
    (return-from recover-from-error))
  (let* ((*recovery-symbol-list* nil)
	 (current-state (car *state-stack*))
	 (expected-symbols (find-valid-symbols current-state))
	 (action-list (get-actions current-state)))
    (when *default-error-recovery*
	  (output-warning "Expecting ~S, found ~S"
		    (get-symbol-names expected-symbols)
;                    (get-symbol-name (current-symbol))
		    (get-symbol-name (current-symbol-value))))
#|    (if (boundp '*token-pos-function*)
        (multiple-value-bind (line column)
            (funcall *token-pos-function*)
          (output-warning "   at line ~A, column ~A"
                          line column)))
|#  
    (when *default-error-recovery*
	  (try-simple-recovery action-list))
    (setq *error-found* t)
    (let* ((error-state (pop-till-error-found))
	   (error-index (get-error-action error-state)))
      (if (simple-error-production-p error-index)
	  (progn 
	    ;;; The error rule is of the form A = error.
	    (call-error-action error-index)
	    (push (get-next-state error-state 
			      (get-error-production-nt error-index))
		  *state-stack*)
	    (push nil *value-stack*)
	    (find-ok-symbol *state-stack*)
	    (throw 'syntax-error nil))

	;;; Otherwise skip tokens till we find a string matching
	;;; (item-beta error-item) and call the action associated
	;;; with the matched rule
	(let ((itembeta (cdr (get-error-production error-index))))
	  (call-error-action error-index)
	  (push (get-next-state error-state 
			    (get-error-production-nt error-index))
		*state-stack*)
	  (push nil *value-stack*)
	  (skip-token-string itembeta)
	  (throw 'syntax-error nil))))))

(defun find-ok-symbol(stack)
  (when *default-error-recovery*
	(output-message "Discarding symbols:"))
  (loop
	;;; If the symbol has been tagged as recoverable
	;;; earlier in the stack, see if it can be handled
	;;; elsewhere. If so, set the stack.
   (when (has-action-on (current-symbol) stack)
	 (return))
   (when (and (member (current-symbol) *recovery-symbol-list*)
	      (set-state-stack (current-symbol)))
	 (output-message "Recovering on symbol ~S" (get-symbol-name (current-symbol-value)))
	 (return))
   (when *default-error-recovery*
	 (format t " ~S" (get-symbol-name (current-symbol-value))))
   (read-next-lexeme)))

#|
(defun try-simple-recovery (current-state action-list)
  ;;; Note that at this point, all actions are shifts.

  ;;; Is there a symbol missing ?
  (let ((possible-symbols nil))
    (dolist (symbol-action action-list)
       (let ((symbol (car symbol-action))
	     (action (cdr symbol-action)))
	 (when (and (is-shift action)
		    (has-action-on (current-symbol)
				   (cons (get-next-state current-state
							 symbol)
					 *state-stack*)))
	       (push symbol possible-symbols))))
    (when (= (length possible-symbols) 1)
	(let ((missing-symbol (first possible-symbols)))
	  (output-message "Inserting new symbol ~S" (get-symbol-name missing-symbol))
	  (push-and-restart missing-symbol nil))))

  ;;; Now see if there is a superfluous symbol
  (let ((next-symbol (peek-next-lexeme)))
    (when (has-action-on next-symbol *state-stack*)
	  (output-message "Discarding input symbol ~S"
			  (get-symbol-name (current-symbol-value)))
	  (discard-input))))

(defun has-action-on (symbol state-stack)
  (let* ((state (first state-stack))
	 (action (get-symbol-action symbol state)))
    (or (is-shift action)
	(and (is-reduction action)
	     (has-action-on symbol (symbolic-apply-reduction
				    (action-of-reduction action)
				    state-stack)))
	(let* ((default-action (get-default-action state)))
	  (if default-action
	      (has-action-on symbol
			     (symbolic-apply-reduction (action-of-reduction
							default-action)
						       state-stack)))))))
|#
(defun try-simple-recovery (action-list)
  ;;; Find all acceptable symbols
  (let ((possible-continuations nil)
	(next-symbol (peek-next-lexeme)))
    (dolist (symbol-action action-list)
	    (let ((result (has-action-on (car symbol-action)
					 *state-stack*)))
	      (if (and result (not (eq result :accept)))
		  (push (cons (car symbol-action) result) 
			possible-continuations))))
  
  ;;; Is there a symbol missing ?
    (let ((possible-symbols nil))
      (dolist (symbol-states possible-continuations)
	      (let ((symbol (car symbol-states))
		    (states (cdr symbol-states)))
		(when (and (has-action-on (current-symbol)
					  (cons (get-next-state (first states)
								symbol)
						states)))
		      (push symbol possible-symbols))))
      (when (= (length possible-symbols) 1)
	    (let ((missing-symbol (first possible-symbols)))
	      (output-message "Inserting new symbol ~S"
			      (get-symbol-name missing-symbol))
	      (push-and-restart missing-symbol nil))))

  ;;; Now see if there is a reasonable substitute symbol
    (let ((possible-symbols nil))
	(dolist (symbol-states possible-continuations)
		(let ((symbol (car symbol-states))
		      (states (cdr symbol-states)))
		  (when (and (has-action-on next-symbol
					    (cons (get-next-state (first states)
								  symbol)
						states)))
		      (push symbol possible-symbols))))
	(when (= (length possible-symbols) 1)
	      (let ((missing-symbol (first possible-symbols)))
		(output-message "Replacing symbol ~S with symbol ~S"
				(get-symbol-name (current-symbol-value))
				(get-symbol-name missing-symbol))
		(substitute-input missing-symbol nil))))

    ;;; Now see if there is a superfluous symbol
    (when (has-action-on next-symbol *state-stack*)
	  (output-message "Discarding input symbol ~S"
			  (get-symbol-name (current-symbol-value)))
	  (discard-input))))

(defun has-action-on (symbol state-stack)
  (let* ((state (first state-stack))
	 (action (get-symbol-action symbol state)))
    (cond ((is-shift action)
	   state-stack)
	  ((is-accept action)
	   :accept)
	  ((is-reduction action)
	   (has-action-on symbol (symbolic-apply-reduction
				  (action-of-reduction action)
				  state-stack)))
	  (t nil))))

(defun symbolic-apply-reduction (reduction state-stack)
  (let ((reduce-string-length (get-action-nargs reduction))
	(reduce-to-symbol (get-action-nt reduction)))
    (let ((reduced-stack (nthcdr reduce-string-length state-stack)))
      (cons (get-next-state (car reduced-stack) reduce-to-symbol)
	    reduced-stack))))

;;; Pop stack until state with error handler found.

(defun pop-till-error-found()
  (loop
    (unless *state-stack*
       (output-message "No error handler on stack: can't continue")
       (emergency-exit))
    (let* ((top-state (car *state-stack*))
	   (error-action (get-error-action top-state)))
      (when error-action
	  (return top-state))
      (pop *value-stack*)
      (pop *state-stack*))))

;;; Pops stack until a state with an actions for the given symbol is
;;; found.

(defun find-symbol-handler (symbol state-stack value-stack)
  (loop
   (if (null state-stack)
       (return nil)
     (when (has-action-on symbol state-stack)
	   (return (values state-stack value-stack))))
   (pop state-stack)
   (pop value-stack)))


(defun set-state-stack (symbol)
  (multiple-value-bind (state-stack value-stack)
    (find-symbol-handler symbol *state-stack* *value-stack*)
    (when state-stack
	  (setq *state-stack* state-stack
		*value-stack* value-stack)
	  t)))

(defun skip-match-string (string)
  (let* ((length (- (length string) 1))
	 (s1 (subseq string 1))
	 (s2 (subseq string 0 length)))
    (loop
     (when (equal s2 s1)
	 (return length))
     (setq length (- length 1))
     (setq s1 (subseq s1 1)
	   s2 (subseq s2 0 length)))))

  ;;; Read the input stream until the string s has been read leaving
  ;;; the pointer on the last symbol read

(defun search-for-string (string)
  (let ((token-index 0))
    (read-next-lexeme)
    (loop
     (if (eq (current-symbol) (elt string token-index))
	 (if (= (length string) (incf token-index))
	     (return t)
	   (read-next-lexeme))
       (if (not (= token-index 0))
	   (setq token-index (length 
			      (skip-match-string (subseq 
						  string
						  0
						  token-index))))
	 (read-next-lexeme))))))


  ;;; Advance the input stream to the symbol after the next occurrence
  ;;; of sequence s in the stream

(defun skip-token-string (string)
  (unless (search-for-string string)
    (output-message "Can't find continuation: ~A" string)
    (emergency-exit))
  (read-next-lexeme))

(defun find-valid-symbols (state)
  (let ((action-list (get-actions state)))
    (mapcar #'car (remove-if #'(lambda(symbol-action)
				 (let ((action (cdr symbol-action)))
				   (not (or (is-shift action)
					    (is-reduction action)))))
			     action-list))))

;;; In the local error recover action when input is skipped force the
;;; error recovery mechanism to use a handler which will handle symbol
;;; sym

(defun recover-on-symbols (&rest sym)
    (dolist (s sym)
       (push s *recovery-symbol-list*)))

;;; Restarts

(defun substitute-input (token value)
  (push-token token value)
  (read-next-lexeme)
  (throw 'syntax-error nil))

(defun push-and-restart (token value)
  (push-token (current-symbol) (current-symbol-value))
  (push-token token value)
  (read-next-lexeme)
  (throw 'syntax-error nil))

(defun discard-input ()
  (read-next-lexeme)
  (throw 'syntax-error nil))

;;; Abandon all hope !!!
(defun emergency-exit ()
  (setq *error-found* t)
  (throw 'parsed nil))

(defun get-symbol-name (symbol)
  (funcall *symbol-to-string* symbol))

(defun get-symbol-names (symbols)
  (mapcar #'get-symbol-name symbols))
