;; A hack towards a bit of a generic "language listener" contact -
;; attempts to just use as much of the lisp listener code as possible.

(in-package :tools)

(export '(language-listener language-listen))

;; Interesting CL state information:

(defvar *package-before-listen*   nil)
(defvar *readtable-before-listen* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defmacro with-saved-global-state (&rest forms)
    `(let ((*package-before-listen*   *package*)
           (*readtable-before-listen* *readtable*))
       #+comment
       (format t "Saved state ~s ~s~%"
               *package-before-listen*
               *readtable-before-listen*)
       ,@forms))

  (defmacro using-saved-global-state (&rest forms)
    `(progn
       #+comment
       (format t "Restoring state ~s ~s~%" 
               *package-before-listen* 
               *readtable-before-listen*)
       (let ((*package*   *package-before-listen*)
             (*readtable* *readtable-before-listen*))
         ,@forms)))

)

;; Language listener contact:

(defcontact generic-env::language-listener (listener) ())

(defun generic-env::language-listen (&rest rest
                                           &key (parent (current-root))
                                           (title "Language Listener")
                                           &allow-other-keys)
  (apply 'fork-window
	 #'make-language-listener title
	 parent
	 rest))

(defun make-language-listener (root
                               &rest args 
                               &key (class 'language-listener)
                               &allow-other-keys)
  (declare (special ccl::*command-reader*))
  (with-saved-global-state
    (let* ((tl (make-contact class :parent root))
           (stream (listener-stream tl)))
      (if nil
        (apply 'two-process-language-listener-top-level 
               stream
               args)
        (apply 'language-listener-top-level
               stream
               args)))))

(defparameter *input-prompt* 
  #'(lambda (&rest args)
      (format t "HELP! SOMETHING FUNNY IS HAPPENING > ")))
(defparameter *evaluator* #'eval)

(defun language-listener-top-level (ed
                                    &rest args
                                    &key (command-reader)
                                         (input-prompt)
                                         (evaluator)
                                         (output-prompt system::*output-prompt*)
                                         (output-writer system::*output-writer*)
                                    &allow-other-keys)
  (let* ((dbg::*debug-listener* ed) ;
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
    (apply #'system::language-listener-top-level (interactive-stream-stream ed)
	   :eval-function
	     *evaluator*
	   (if (ccl::featurep :lispworks3.1)
	     :*prompt*
	     :prompt)
	   (wrap-prompt input-prompt)
           :allow-other-keys t
           args)))

(defun two-process-language-listener-top-level (tl &rest args)
  (let ((exec-process (mp::process-run-function
                       (format nil "Execution ~A"
                               (mp::process-name mp::*current-process*))
                       '()
                       #'(lambda ()
                           (let ((editor::*in-two-process-listener* t))
                             (declare 
                               (special editor::*in-two-process-listener*))
                             (apply 'language-listener-top-level tl args))))))
    (setf (getf (mp::process-plist mp::*current-process*)
		'evaluator-process)
          exec-process)
    (push `(kill-2-listener-exec ,exec-process)
          (getf (mp::process-plist mp::*current-process*)
		'mp::kill-cleanup))))
  
(defun language-listener-recursive-top-level (&rest args 
                                              &key (extra-commands)
                                                   (eval-function)
                                                   (read-stream)
                                                   (print-stream)
                                                   (*prompt*)
                                              &allow-other-keys)
  (declare (ignore eval-function))
  (system::language-listener-top-loop
    :extra-commands extra-commands
    :eval-function *evaluator*
    :read-stream read-stream
    :print-stream print-stream
    (if (ccl::featurep :lispworks3.1)
      :*prompt* 
      :prompt)
    (wrap-prompt *input-prompt*)))

(defun wrap-prompt (fn)
  #'(lambda ()
      (funcall fn
               (system::next-event-number system::*top-loop-history-list*)
               system::*debug-level*)))


(defun wrap-evaluator (command-reader
		       input-prompt
		       evaluator
		       output-prompt
		       output-writer)
  #'(lambda (exp)
      (if (and (consp exp)
               (keywordp (first exp)))
        (case (first exp)
          ((:load-request)
            (apply #'generic-env:generic-load (rest exp)))
          ((:compile-request)
            (apply #'generic-env:generic-compile (rest exp)))
          ((:compile-load-request)
            (apply #'generic-env:generic-compile (append (rest exp) '(:load t))))
	  ((:inspect-request)
	    (apply #'tools::generic-inspect 
		   command-reader
		   input-prompt
		   evaluator
		   output-prompt
		   output-writer
		   (rest exp)))
          (otherwise
            (funcall evaluator exp)))
        (funcall evaluator exp))))

;; Default resources:

(defun sane-funcall-or-editor-command (fn)
  (using-saved-global-state
    (if (functionp fn) (funcall fn) (funcall-or-editor-command fn))))

(defun sane-call-command-callback (&rest args)
  (using-saved-global-state
     (apply #'call-command-callback args)))

(define-resources 

  (* language-listener * motif-menu-bar items)
  '(("File") ("Value") ("Restart") ("History"))

  (* language-listener * motif-menu-bar menus)
  '((:name language-listener-file-menu)
    (:name language-listener-value-menu)
    (:name language-listener-restart-menu)
    (:name execute-history-menu))

  (* language-listener * application-menu notify) 
    '(sane-call-command-callback :window-commands)
  (* language-listener-file-menu notify) 'sane-funcall-or-editor-command
  (* language-listener-value-menu notify) 'sane-funcall-or-editor-command
  (* language-listener-restart-menu notify) '(call-command-callback :editor-command)

  (* language-listener-file-menu items)
  `(("Compile & Load" . editor::language-listener-compile-and-load)
    ("Compile" . editor::language-listener-compile)
    ("Load" .    editor::language-listener-load)
    ("Edit" .    lispworks-edit))

  (* language-listener-value-menu items) 
  `(("Inspect" . editor::language-listener-inspect-star)
    ("Push" . editor::listener-push-star)
    ("Pop" .  editor::listener-pop-star))

  (* language-listener-restart-menu items)
  '(("Abort" .    "Debugger Abort")
    ("Continue" . "Debugger Continue")
    ("Debug" .    listener-debug)))

;; eof

