;; Filename: load-dfmc.lisp
;;
;;   Author: Shri Amit (amit)
;; Synopsis: The following code compiles the dfmc libs
;;           and is used by the script build-kan-dfmc. It
;;           pre-loads certain libs from a file with a list.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that this only loads the webster library. Once
;; webster is loaded the code for loading the libraries
;; and setting the context name to webster is defined but
;; it is not invoked here as the list of libs might be different.
;; The wrapper bash script build-kan-dfmc invokes those from
;; its 'here' document.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package dylan)

(defun read-library-file (file)
  (with-open-file (stream file)
		  (loop for line = (read-line stream nil nil)
			while line
			collect (string-trim 
				 '(#\Space #\Tab #\Newline) line))))

(defun load-webster ()
  (user::with-build-handler
   (ensure-library 'webster)))

(load-webster)

(defun load-libraries (library-file)
  (user::with-build-handler
   (dylan+dylan/projects-implementation::canonicalize-project-sources 
    (dylan+dylan/projects-implementation::lookup-named-project 'dylan))
   (dylan+dylan/dfmc-debug::compile-library-to-models 'dylan)
   (dolist (lib (read-library-file library-file))
	   (format t "~%~%----dfmc loading library ~a~%~%" lib)
	   (dylan+dylan/projects-implementation::canonicalize-project-sources 
	    (dylan+dylan/projects-implementation::lookup-named-project lib)))
   (map nil
	'(lambda (proj) 
	   (dylan+dylan/projects-implementation::project-personal-library?-setter 
	    dylan::*dylan-canonical-false* proj))
	dylan+dylan/projects-implementation::*all-open-projects*)))

(defun set-name ()
  (set-module-context-by-name 'webster))

;; eof