;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/utilities.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)

(defun display-grammar (grammar)
  ;; Grammar is list of productions
  (let ((index 0)
	(error-index 0))
    (dolist (ntprods grammar)
	    (let ((nt (caar ntprods))
		  (prod (cdar ntprods))
	          (action (cadr ntprods)))
	      (if (eq (car prod) :error)
		  (format t "~%~%Error Rule ~A: ~A -> ~{~A ~}~%"
			  (prog1 error-index (incf error-index))
			  nt
			  (mapcar #'get-grammar-symbol-string prod))
		      
		(format t "~%~%Rule ~A: ~A -> ~{~A ~}~%"
			(prog1 index (incf index))
			nt
			(mapcar #'get-grammar-symbol-string prod)))
	      (write action :pretty t :case :downcase :length nil
                     :level nil)))))


		  
		  
  
