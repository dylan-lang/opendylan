(in-package :dylan)

(export '(tty-infix-dylan-listen))


;; andrewa, 08 Oct 1996 - added support for printing the inspector level
(defun generic-env::dylan-prompt-string (hist level)
  (let ((name (module-name *current-module*)))
    (with-output-to-string (stream)
      (format stream "~%~A ~A"
              name hist)
      (unless (zerop level)
        (format stream " : ~A" level))
      (when (> ccl::*inspect-level* 0)
        (format stream " [inspect ~A]"
                ccl::*inspect-level*))
      (format stream " ? "))))

(in-package :generic-env)

(export '(tty-infix-dylan-listen generic-load generic-compile))

(defun dylan::tty-infix-dylan-listen ()
  (let ((*package* (find-package :dylan)))
    (tty-language-listen :title "Infix Dylan Listener"
			 :class 'infix-dylan-listener
			 :command-reader 'dylan-infix-command-reader
			 :input-prompt 'dylan-prompt-string
			 :evaluator 
			 #'(lambda (x)
			     (dylan::dylan-interactive-eval x))
			 :output-writer 
			 #'(lambda (x s)
			     (dylan::dylan-print x :stream s)))))

(in-package :tools)

(export '(language-listener language-listen tty-lanaguage-listen))

(defun generic-env::tty-language-listen (&rest args
					       &key &allow-other-keys)
  
  (declare (special ccl::*command-reader*))
  (with-saved-global-state
   (let ((stream *standard-input*))
     (apply 'tty-language-listener-top-level
	    stream
	    args))))

(defun tty-language-listener-top-level (stream
                                    &rest args
                                    &key (command-reader)
                                         (input-prompt)
                                         (evaluator)
                                         (output-prompt system::*output-prompt*)
                                         (output-writer system::*output-writer*)
                                    &allow-other-keys)
  (let* ((dbg::*debug-listener* stream) ;
	 (system::*command-reader* command-reader)
         (system::*output-prompt* output-prompt)
         (system::*output-writer* output-writer)
         (*evaluator* (wrap-evaluator command-reader
				      input-prompt
				      evaluator
				      output-prompt
				      output-writer))
         (*input-prompt* input-prompt))
    (declare (special system::*output-writer*
                      system::*output-prompt*))
    (catch 'bye
      (apply #'system::language-listener-top-level stream
	     :eval-function
	     *evaluator*
	     (if (ccl::featurep :lispworks3.1)
	     :*prompt*
	     :prompt)
	   (wrap-prompt input-prompt)
           :allow-other-keys t
           args))))

(defvar dylan::*dylan-restart-hooks* '())

(defun dylan::reset-restart-hook()
  (setq dylan::*dylan-restart-hooks* '()))

(defun dylan::install-restart-hook(fun)
  (setq dylan::*dylan-restart-hooks* 
	(cons #'(lambda()
		  (let 					
		      ((*package* (find-package :dylan))
		       (*readtable* dylan::*infix-dylan-load-eval-readtable*))
		    (declare (special *package *readtable*))
		    (funcall fun)))
	      dylan::*dylan-restart-hooks*)))


(defun dylan::initialize-windows()
  (tools:start-lispworks :start-functions dylan::*dylan-restart-hooks*
			 :init-streams nil
			 :initialize-editor nil))

(defun dylan::make-window-app()
  (setq *init-file-name* nil)
  (setq dylan::*emulator-restart-hooks* 
	(cons #'dylan::initialize-windows dylan::*emulator-restart-hooks*)))
