;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/generate-actions.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)

(defun make-derived-actions (state item)
  (generate-shifts state item)
  (generate-errors state item)
  (generate-reductions state item))

(defun generate-shifts (state item)
  (dolist (term (get-production-first-set (item-B item)))
	  (set-action state
		      item
		      term
		      :shift)))

(defun get-production-first-set (nt)
  (declare (special *production-first-sets*))
  (gethash nt *production-first-sets*))

(defun make-production-first-set-table ()
  (let ((table (make-hash-table)))
    (dolist (nt (non-terminals) table)
       (setf (gethash nt table) (production-first-set nt)))))

(defun production-first-set(sym)
  (let ((derivs (derivs-of sym)))
    (remove-duplicates (apply #'append (mapcar #'(lambda(d)
						   (get-initial-set (car d)))
					       derivs)))))

;;; Return the list of terminals t such that
;;; nt -> t alpha is a production.

(defun get-initial-set (nt)
  (remove-if #'(lambda(s)(or (null s)
			     (is-non-terminal s)))
	     (mapcar #'car (productions-of nt))))

(defun generate-errors (state item)
  (let ((derivs (derivs-of (item-b item))))
    (dolist (nt (mapcar #'car  derivs))
	(let ((index (get-error-production-index nt)))
	  (when index
	      (set-error-action state index))))))

(defun generate-reductions (state item)
  (let* ((derivs (derivs-of (item-b item)))
	 (possible-reductors (remove-if
			      #'(lambda (deriv)(not (has-empty-production-p
						     (car deriv))))
			      derivs)))
    (if possible-reductors
	(let ((lookaheads (get-lookaheads state item)) 
	      (beta-lookaheads  (get-first-set-string (item-beta item))))
	  (dolist (deriv possible-reductors)
		  (let* ((las (cdr deriv))
			 (reducing-input las))
		    (when (member nil las)
			  (setq reducing-input (append beta-lookaheads
						       reducing-input))
			  (when (member nil beta-lookaheads)
				(setq reducing-input (append lookaheads reducing-input))))
		    (dolist (token (remove-duplicates reducing-input))
			    (if token
				(enter-empty-reduction state (car deriv) token)))))))))


;;; PJG 9Jan95
;;; Reductions are now identified as integers (Save on consing)

(defun enter-empty-reduction (state nt token)
  (let ((index (epsilon-production-of nt)))
    (set-action state
		(epsilon-item-of nt)
		token
		index ;; (cons :reduce index) ;; PJG 9Jan95
                )))

(defun has-empty-production-p (nt)
  (member '() (productions-of nt)))
