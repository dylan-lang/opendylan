;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/lr1close.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

; Functions for finding the LR1 closure of an item quickly.
(in-package parsergen)

(defun lr1-closure (item lookahead)
  (if (not (is-non-terminal (item-B item)))
      (list (list item lookahead))
    (let* ((result nil)
	   (item-b (item-B item))
	   (beta-first-set (get-first-set-string (item-beta item)))
	   (beta-b-first-set (if (member nil beta-first-set)
				 (if (member lookahead beta-first-set)
				     (remove nil beta-first-set)
				   (cons lookahead
					 (remove nil beta-first-set)))
			       beta-first-set))
	   (nont-derivs (derivs-of item-b)))
      (push (list item lookahead) result)
      (dolist (deriv nont-derivs result)
	      (let* ((nont (car deriv))
		     (zeta (cdr deriv))
		     (start-items (start-items-of nont))
		     (zeta-beta-b-first-set
		      (combine-first-sets zeta beta-b-first-set)))
		(dolist (item start-items)
			(push (cons item zeta-beta-b-first-set)
			      result)))))))

(defun add-to-lookaheads (item term table)
  (unless (member term (gethash item table))
	  (push term (gethash item table))))

(defun add-list-to-lookaheads (item list table)
  (dolist (term list)
	  (add-to-lookaheads item term table)))
