;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/messages.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)
(export '(*indent* indent-in indent-out output-message output-warning
		   output-error init-indent))

(defvar *error-found*)
(defvar *indent* 0)

(defun indent-in ()
  (incf *indent*))

(defun indent-out ()
  (if (> *indent* 0)
      (decf *indent*)
    (output-warning "Trying to indent beyond left margin")))

(defun output-message (string &rest args)
  (apply #'format t
	 (concatenate 'string "~%~VT" string)
	 (* 2 *indent*)
	 args))

(defun output-warning (string &rest args)
  (apply #'output-message
	 (concatenate 'string "Warning: " string)
	 args))

(defun output-error (string &rest args)
  (setq *error-found* t)
  (apply #'output-message
	 (concatenate 'string "Error: " string)
	 args))

(defun init-indent ()
  (setq *indent* 0))
