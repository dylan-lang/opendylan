;; Language top level:

;; Has to duplicate code in "~clc/editor/listener.lisp" in order to make 
;; recursive top level invocations work in the right way - that is, to bind
;; the dynamic dbg::*recursive-top-level* so that the error system sends
;; unhandled conditions to be debugged in the correct environment -
;; specifically with the Dylan command reader in place.

(in-package :system)

(defun language-listener-top-level (stream &rest args)

  (declare (special *debug-io*
		    *query-io*
		    *top-loop-history-list*
		    *ask-the-user-and-eval-func*
		    *ask-the-user-func*
		    *ask-the-user-for-sexp-func*

		    dbg::*recursive-top-level*))
  (let
      ((*standard-output* stream)
       (*standard-input* stream)
       (*trace-output* stream)
       (*error-output* stream)
       (*debug-io* stream)
       (*query-io* stream)
       (system::*in-listener* t)
       (editor::*in-shell-window* nil)
       (dbg::*recursive-top-level* 'tools::language-listener-recursive-top-level)
       (*top-loop-history-list* (make-history-list))
       (*ask-the-user-for-sexp-func* 'listener-ask-the-user-for-sexp)
       (editor::*listener-last-path* nil)
       (*ask-the-user-func* 'listener-ask-the-user)
       (*ask-the-user-for-file-func* 'listener-ask-the-user)
       (*ask-the-user-and-eval-func* 'listener-ask-the-user-and-eval)
;       (editor::*command-loop* 'listener-command-loop) 
       )

  (loop 
   (catch 'top-level-catch   ;;for the editor command Throw Out of Debugger.
			     ;;CRB jan 92 
   (conditions::with-simple-restart (conditions::abort
				     "return to top loop level ~d." 0)

    (apply
     'ccl::%top-level
     :read-stream stream
     :print-stream stream
     :buffered nil
     :read-a-command *command-reader*
     args))))))

(defun language-listener-top-loop (&rest args)
  (setf (getf args :read-a-command) *command-reader*)
  (setf (getf args :buffered) nil)
  (loop 
      (apply 'ccl::%top-level :top-loop-hook *top-loop-hook* args)))

;; We have to do some hacking to parameterise the printer:

(defparameter *output-prompt* #'(lambda (s) (format s "~&")))
(defparameter *output-writer* #'(lambda (x s) (format s "~s" x)))

(defadvice (print-evaluation-results language :around) (this-eval stream)
  (setq /// // // / / this-eval)
  (setq *** (symbol-value$symbol '**)
	** (symbol-value$symbol '*)
	* (car this-eval))
  (dolist (x this-eval)
    (funcall *output-prompt* stream)
    (if *describe-instead-of-print* 
	(when (eq *describe-instead-of-print* T) (describe x))
        (funcall *output-writer* x stream))))

;; eof
