;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/newkerns.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)
(defvar *goto-table*)

(defun add-to-goto-table (state symb newstate)
  (push (cons symb newstate)
	(gethash state *goto-table*)))

(defun goto-state (state symb)
  (cdr (assoc symb (gethash state *goto-table*))))

(defvar *reduction-table*)

(defun kernel-sets(item-set)
  (setq *goto-table* (make-hash-table))
  (let ((*reduction-table* (setup-reduction-table)))
    (make-set-closure #'(lambda(state)
			  (kernel-goto-transitions (item-set-of-state state)))
		      (list (state-of-item-set item-set)))))

(defun non-terminal-reduces (from-nt to-nt)
  (member to-nt (gethash from-nt *reduction-table*))) ;; Use of *reduction-table*

(defun setup-reduction-table()
  (let ((table (make-hash-table)))
    (dolist (nt (non-terminals) table)
	    (setf (gethash nt table)
		  (mapcar #'car (derivs-of nt))))))

(defun kernel-goto-transitions(item-set)
  (let ((result nil)
	(goto-sets (make-goto-table item-set))
	(state (state-of-item-set item-set)))
    (maphash #'(lambda (symb new-state)
		 (let ((goto-state (state-of-item-set (make-canonical new-state))))
		   (add-to-goto-table state symb goto-state)
		   (push goto-state result)))
	     goto-sets)
    result))

(defun make-canonical (state)
  (if (cdr state)
      (sort state #'<)
    state))

(defun make-goto-table (state)
  ; Returns a hash table containing the goto sets for the given
  ; states.
  ; That is, (gethash symbol (make-goto-table state)) is the set of
  ; items in the set GOTO(state, symbol).

  (let ((newstates (make-hash-table :rehash-size 2.0)))
    (dolist (item state newstates)
	    (let ((next-symb (item-B item)))
	      (when next-symb
		    (add-to-local-goto-table next-symb
				       (shift-dot item)
				       newstates)
		    (if (is-non-terminal next-symb)
			(dolist (nont
				 (gethash next-symb
					  *reduction-table*)) ;; The
							      ;; other
							      ;; use 
				(dolist (start-item (start-items-of nont))
				 (let ((first-symb (item-B start-item)))
				   (when first-symb
					 (add-to-local-goto-table first-symb
							    (shift-dot start-item)
							    newstates)))))))))))

(defun add-to-local-goto-table (symb item table)
  (unless (member item (gethash symb table))
	  (push item (gethash symb table))))

