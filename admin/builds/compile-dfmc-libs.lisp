;; Filename: compile-dfmc-libs.lisp
;;
;;   Author: Shri Amit (amit)
;; Synopsis: The following code defines the functions required
;;           to compile a list of libraries in the dfmc. This is
;;           used by the script compile-dfmc-libs to compile 
;;           libraries for the nightly builds.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package dylan)

(defun read-library-file (file)
  (with-open-file (stream file)
		  (loop for line = (read-line stream nil nil)
			while line
			collect (string-trim '(#\Space #\Tab #\Newline) line))))

(defun compile-library-list (library-file)
  (user::with-build-handler
   (let ((failures '()))
     (dolist (library (read-library-file library-file))
	     (format t "~%~%;;;;;;; Compiling library: ~s~%" library)
	     (handler-case
	      (dylan+dylan/projects::compile-library
	       (intern (string-upcase library) :keyword) :force-compile? t)
	      (error (c)
		     (push library failures)
		     (format t "; Caught error: ~a~%" c)
		     (format t "; Moving on~%"))))
     (format t "Failures:~%~{~s~%~}" failures)
     (format t "Number of failures: ~s~%" 
	     (length failures)))))

;; eof