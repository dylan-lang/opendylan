;; -*- Lisp ; rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/parseractions.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

; parseractions.lisp
(in-package parsergen)

(defvar *action-table*)

(defvar *state-numbers*)

(defvar *actions-cache*)

(defun make-tables (grammar error-grammar)
  (format t "... pre processing grammar")
  (pre-process-grammar grammar error-grammar)
  (format t "... creating state table")
  (create-parse-table)
  (format t "... defining actions~%")
  (define-actions))

(defun create-parse-table ()
;;; The start symbol's rule is always the first of the grammar rules
  (reset-item-tables)
  (let* ((base-set (list (initial-item))))
    (propagate-gotos (kernel-sets base-set))))

(defun define-actions()
  (setq *action-table* (make-array *state-numbers*))
  (let ((*production-first-sets* (make-production-first-set-table))
	(*actions-cache*  (make-hash-table :test #'equal)))
    (declare (special *production-first-sets*))
    (dotimes (state *state-numbers*)
	     (make-actions state))))

(defun make-actions(state)
  (let ((*action-list* nil)
	(*action-hash-table* (make-hash-table :test #'eq))
	(item-set (item-set-of-state state)))
    (declare (special *action-list* *action-hash-table*))
    (dolist (item item-set)
	    (make-action state item))
    (precompute-shift-next-states state)
    (setf (svref *action-table* state) *action-list*)))

(defun precompute-shift-next-states (state)
  (declare (special *action-list*))
  (dolist (pair *action-list*)
    (when (eq (cdr pair) :shift)
      (setf (cdr pair) (make-shift-action state (car pair))))))

(defun make-shift-action (state symbol)
  (- -1 (get-next-state state symbol)))

(defun make-action (state item)
  (let ((itemb (item-b item)))
  (cond
   ((null itemb)
    (make-simple-reductions state item))

   ((is-terminal itemb)
    (set-action state
		item
                itemb
                :shift))

   (t (make-derived-actions state item)))))



;;; PJG 9Jan95
;;; Replace (cons :reduce index) with index

(defun make-simple-reductions (state item)
  (let ((index (production-index-of item)))
    (dolist (lookahead (get-lookaheads state item))
	    (if (and (= index 0)         ; Quick Hack.
		     (eq lookahead :eoi))
		(set-action item state :eoi :accept)
	      (set-action state item lookahead 
                          index ;; (cons :reduce index)
                          )))))



;;; Modify the action list to place a default reduction at the end, if
;;; there is one.
;;; Treat an action on :eoi as a special case.

;;;  PJG 9Jan95
;;; Modify so that a reduction is specified as an integer and not a
;;; (cons :reduce integer)


(defun add-default-action (action-list)
  (let ((default-action
	 (some #'(lambda (symbol-action)
		   (let ((symbol (car symbol-action))
			 (action (cdr symbol-action)))
		     (and (not (eq symbol :eoi))
	 (is-reduction action)
			  action)))
	       action-list)))
    (if default-action
	(nconc (delete-if #'(lambda (symbol-action)
			      (let ((action (cdr symbol-action)))
				(and (consp action)
				     (equal action default-action))))
			  action-list)
	       (list (cons :any default-action)))
      action-list)))


(defun set-error-action (state action)
  (declare (ignore state))
  (declare (special *action-list*))
  (let ((old-action (cdr (assoc :error *action-list*))))
    (if old-action
	(when (> action old-action)
	      (setf (cdr (assoc :error *action-list*))
		    action))
      (push (cons :error action) *action-list*))))
      
(defun set-action (state item symbol action)
  (declare (special *action-list* *action-hash-table*))
  (if (null symbol)(break "Trying to set an action for NIL"))
  (let ((old-action (cdr (assoc symbol *action-list*))))
    (if old-action 
	(if (equal old-action action)
	    (when (< (production-index-of item)
		     (production-index-of (gethash symbol
						   *action-hash-table*)))
		  (setf (gethash symbol *action-hash-table*)
			item))
          ;;; There is a conflict
	  (let ((old-item (gethash symbol *action-hash-table*)))
	    (output-warning "Conflict in state ~S for symbol ~S"
			    state symbol)
	    (output-message "   Action ~S (~A)"
			    action (print-item item nil))
	    (output-message "   Action ~S (~A)"
			    old-action (print-item old-item nil))
	    (let ((old-rule (production-index-of old-item))
		  (rule (production-index-of item)))
	      (cond ((< old-rule rule)
		     (output-message "  Using action ~S~%"
				     old-action))

		    ((> old-rule rule)
		     (output-message "  Using action ~S~%"
				     action)
		     (setf (gethash symbol *action-hash-table*) item)
		     (setf (cdr (assoc symbol *action-list*))
			   action))

		    ((eq action :shift);; Favour reduction for
		     ;; self conflict
		     (output-message "  Using action ~S~%"
				     old-action))

		    ((eq old-action :shift)
		     (output-message "  Using action ~S~%"
				     action)
		     (setf (gethash symbol *action-hash-table*) item)
		     (setf (cdr (assoc symbol *action-list*))
			   action))

		    ((break "Error in set-action"))))))
      (progn
	(setf (gethash symbol *action-hash-table*) item)
	(push (cons symbol (install-action action)) *action-list*)))))

;;; Cache actions here.

(defun install-action(action)
  (if (atom action) action
    (or (gethash action *actions-cache*)
	(setf (gethash action *actions-cache*) action))))
