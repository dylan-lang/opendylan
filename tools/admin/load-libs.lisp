;; Filename: load-libs.lisp
;;
;;   Author: Shri Amit (amit)
;; Synopsis: The following code defines the functions required
;;           to load a list of libraries. This is used by the
;;           script compile-libs to compile libraries for the
;;           nightly builds.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package cl-user)

(defun read-library-file (file)
  (with-open-file (stream file)
		  (loop for line = (read-line stream nil nil)
			while line
			collect (string-trim '(#\Space #\Tab #\Newline) line))))

(defun load-libraries (library-file)
  (with-build-handler
   ;; use the serial constraints to force recompilations 
   (setq dylan::*enforce-serial-constraints* t) 
   (let ((failures '()))
     (dolist (library (read-library-file library-file))
	     (format t "~%~%;;;;;;; Ensuring library: ~s~%" library)
	     (handler-case
	      (dylan::ensure-library 
	       (intern (string-upcase library) :keyword))
	      (error (c)
		     (push library failures)
		     (format t "; Caught error: ~a~%" c)
		     (format t "; Moving on~%"))))
     (format t "Failures:~%~{~s~%~}" failures)
     (format t "Number of failures: ~s~%" 
	     (length failures)))))

;; eof