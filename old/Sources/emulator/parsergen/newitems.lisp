;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/newitems.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)

;;; Returns the number of items for a grammar - the sum of the lengths
;;; of the productions.

(defun get-number-of-items (grammar)
  (let ((result 0))
    (dolist (prod grammar)
	    (setq result (+ result (length prod))))
    result))

(defvar *item-array*)
(defvar *item-production-array*)
(defvar *nt-start-items-table*) ; Hash table of the items A -> .alpha
(defvar *production-array*)
(defvar *epsilon-productions*) ; hash table of nts to indices of any
			       ; epsilon productions it has.
(defvar *epsilon-items*)      ; ditto for item numbers

;; Sets up item data structures

(defun setup-item-functions (grammar)
  (let ((number-of-items (get-number-of-items grammar)))
    (setq *item-array* (make-array number-of-items))
    (setq *item-production-array* (make-array number-of-items))
    (setq *production-array* (make-array (length grammar)))
    (setq *nt-start-items-table* (make-hash-table))
    (setq *epsilon-productions* (make-hash-table))
    (setq *epsilon-items* (make-hash-table))
    (let ((item-index 0)
	  (production-index 0))
      (dolist (prod grammar)
	 (let ((nt (car prod))
	       (rhs (cdr prod)))
	      (when (null rhs)
		    (if (gethash nt *epsilon-productions*)
			(format t "~%Error -- Multiple epsilon production for ~A"
				nt)
		      (setf (gethash nt *epsilon-productions*)
			    production-index
			    (gethash nt *epsilon-items*)
			    item-index)))
	      (let ((item (cdr prod)))
		(push item-index (gethash nt
					  *nt-start-items-table*))
		(loop
		 (setf (aref *item-array* item-index)
		       item)
		 (setf (aref *item-production-array* item-index)
		       production-index)
		 (incf item-index)
		 (if item (pop item) (return)))
		(setf (aref *production-array* production-index) prod)
		(incf production-index)))))))

(defmacro item-b (item)
  `(car (aref *item-array* ,item)))

(defmacro item-beta (item)
  `(cdr (aref *item-array* ,item)))

(defmacro production-index-of (item)
  `(aref *item-production-array* ,item))

(defmacro production-of (item)
  `(aref *production-array* (production-index-of ,item)))

(defmacro production-of-index (index)
  `(aref *production-array* ,index))

(defmacro start-items-of (nt)
  `(gethash ,nt *nt-start-items-table*))

(defmacro shift-dot (item)
  `(+ 1 ,item))

(defmacro initial-item ()
  0)

(defmacro initial-item-p (item)
 ` (= ,item 0))

(defmacro item-terminal (item)
  `(car (production-of ,item)))


(defmacro epsilon-production-of (nt)
  `(gethash ,nt *epsilon-productions*))

(defmacro epsilon-item-of (nt)
  `(gethash ,nt *epsilon-items*))

;;; This function shouldn't be used except in printing.
(defun item-alpha (item)
  (let ((prod (cdr (production-of item)))
	(string (aref *item-array* item))
	(result ()))
    (loop
     (if (equal string prod)
	 (return (reverse result)))
     (push (pop prod) result))))

(defun advance-items(items)
  (let ((result nil))
    (dolist (old-item items result)
	    (if (item-b old-item)
		(push (shift-dot old-item)
		      result)))))

(defun print-item (item &optional (stream *standard-output*))
  (format stream "~A -> ~{~A ~}. ~{~A ~}"
	  (item-terminal item)
	  (item-alpha item)
	  (aref *item-array* item)))
