;; -*- lisp ; rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/pre-process-gram.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

; pre-process-gram.lisp
(in-package parsergen)

(defvar *first-sets*)

(defvar *string-table*)

(defun pre-process-grammar (grammar error-grammar)
  (setup-symbols grammar)
  (setup-first-sets)
  (setup-derivs grammar)
  (setup-reduction-table)
  (setup-item-functions grammar)
  (setup-error-tables error-grammar))

(defun setup-first-sets ()
  (setq *string-table* (make-hash-table :test #'equal))
  (setq *first-sets* (make-hash-table :test #'eq))
  (dolist (s (get-grammar-symbols))
	  (get-first-set s)))

(defun get-first-set (symbol &key visited-rules)
  (or (gethash symbol *first-sets*)
      (setf (gethash symbol *first-sets*)
	    (first-set symbol
		       :visited-rules visited-rules))))

(defun get-first-set-string (string)
  (cond ((null string) '(nil))
	((or (is-terminal (car string))
	     (null (cdr string)))
	 (get-first-set (car string)))
	(t (or (gethash string *string-table*)
	       (setf (gethash string *string-table*)
		     (first-set-string string))))))

(defun first-set-string (string)
  (if (null string)
      (list nil)
    (let ((first-set (get-first-set (car string))))
      (if (member nil first-set)
	  (let ((rest-of-first-set (get-first-set-string (cdr string))))
	    (combine-first-sets first-set rest-of-first-set))
	first-set))))

(defun combine-first-sets (alpha beta)
  (if (not (member nil alpha))
      alpha
    (let ((result beta))
      (dolist (la alpha result)
	      (unless (or (null la)
			  (member la beta))
		      (push la result))))))

;;; This code is very cruddy -- replace sometime.

(defun first-set (sym &key visited-rules)
    (if (not (is-non-terminal sym))
        (list sym)
      (let ((sym-rules (productions-of sym)))
        (remove-duplicates
         (apply #'append (mapcar 
                           #'(lambda(rule)
		                (first-set-of-rule 
			         rule
                                 visited-rules))
		           sym-rules))))))

(defun first-set-of-rule(rule visited-rules)
  (if (not (member rule visited-rules))
      (if (null rule)
          (list nil)
	(let ((initial-first-set
	       (get-first-set (car rule)
			  :visited-rules (cons rule visited-rules))))
	  (if (member nil initial-first-set) 
	      (append initial-first-set     
		      (first-set-of-rule (cdr rule) visited-rules))
	    initial-first-set)))))

(defvar *error-productions*)
(defvar *error-action-nt-table*)
(defvar *nt-error-productions*)

(defun setup-error-tables (error-grammar)
  (declare (special *error-productions* *nt-error-productions*
		    *error-action-nt-table*))
  (setf *error-productions* (make-array (length error-grammar)))
  (setf *error-action-nt-table* (make-array (length error-grammar)))
  (setf *nt-error-productions* (make-hash-table))
  (let ((index 0))
    (dolist (prod error-grammar)
       (let ((nt (car prod))
	     (rhs (cdr prod)))
	    (setf (gethash nt *nt-error-productions*)
		  index)
	    (setf (svref *error-productions* index) rhs)
	    (setf (svref *error-action-nt-table* index) nt)
	    (incf index)))))

