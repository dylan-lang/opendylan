;; -*- lisp ; rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/close.lisp,v 1.1 2004/03/12 00:41:15 cgay Exp $" -*-

;; #<harlequin copyright marker>

; close.lisp

(in-package parsergen)

;;; Definition of make-set-closure

;;; Scuddy Waltonesque thing for doing closures, I guess it works.

(defun complete-set-closure (f sets-to-close closed-set
			       &optional (member-test #'eq))
  (let ((closed-test-table (make-hash-table :test member-test)))
    (loop 
     (if (null sets-to-close)
	 (return closed-set)
       (let ((top-set (car sets-to-close)))
	 (let* ((closed-p (gethash top-set closed-test-table))
		(new-sets-to-close (if closed-p 
				       (cdr sets-to-close)
				     (append (funcall f top-set) 
					     (cdr sets-to-close))))
						
		(new-closed-set (if closed-p 
				    closed-set
				  (cons
				   top-set
				   closed-set))))
	   (if (not closed-p) (setf (gethash top-set
					     closed-test-table) 
				    t))
	   (setq sets-to-close new-sets-to-close
		 closed-set new-closed-set)))))))


(defun make-set-closure (f sets &key (member-test #'eq))
  (complete-set-closure f sets nil member-test))


