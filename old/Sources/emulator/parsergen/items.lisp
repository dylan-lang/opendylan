;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/items.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)

(defvar  *state-numbers*)
;; Stores the total number of states so far defined.

(defvar  *state-hash-table*)
; Maps item-sets -> states

(defvar *item-set-table*)
; Maps state -> item sets

(defun state-of-item-set (item-set)
  (or (gethash item-set *state-hash-table*)
      (let ((index *state-numbers*))
	(setf (gethash index *item-set-table*) item-set)
	(setf (gethash item-set *state-hash-table*) index)
	(incf *state-numbers*)
	index)))

(defun item-set-of-state (state)
  (or (gethash state *item-set-table*)
      (break (format nil
		     "Getting item set of non-existent state ~A"
		     state))))

(defun  reset-item-tables ()
     (setq *state-hash-table* (make-hash-table :test #'equal))
     (setq *item-set-table* (make-hash-table))
     (setq  *state-numbers*  0)
     )

