(in-package :tools)

(defun make-dylan-inspector (&rest x)
  (let* ((contact (apply 'make-contact x))
         (listener (slot-value contact 'listener)))
    (when listener
      (let ($ $$ $$$)
	(declare (special $ $$ $$$))
	(set-$-and-$$-and-$$$ contact)
        (editor::flag-window-buffer (slot-value listener 'window) :listener) ;; nick 16.09.92
	(update-window-state listener)
	(setf (handler-parent listener) contact) 

	;; Tim: this startup should be better abstracted as it's
        ;;      identical to the listener startup code - whatever way
        ;;      you think's best.

        (let ((*package* (find-package :dylan)))
          (tools::language-listener-top-level
	    ;;(interactive-stream-stream listener)
            listener
	    :command-reader 'generic-env::dylan-command-reader
	    :input-prompt 'generic-env::dylan-prompt-string
	    :evaluator 
	      #'(lambda (x)
		  (dylan::dylan-interactive-eval x))
	    :output-writer 
	      #'(lambda (x s)
		  (dylan::dylan-print x :stream s))
            :top-loop-hook 
	      (inspector-top-loop-hook listener nil)))))))

(defun w-dylan-inspect (object &key (parent (current-root)))
  (let ((proc
         (fork-window #'make-dylan-inspector
                      "Inspector"
                      'inspector 
                      :parent parent
                      :object object
                      :title (next-inspector-title))))
    (mp::process-wait "Waiting for Inspector"
                      'process-window
                      proc)
    (process-window proc)))

(let ((done nil))
  (defun ensure-inspector-loaded ()
    "Use load-on-demand mechanism to load the inspector when this variable is touched."
    (unless done
      ;; Should just be: (lw::do-demand-pre-loads :inspector-values)
      (find-class 'ccl::unbound)
      (lw::do-demand-pre-loads :window-inspector)
      (setq done t))))

(defun generic-inspect (command-reader
			input-prompt
			evaluator
			output-prompt
			output-writer
			object)
  (ensure-inspector-loaded)
  (let ((proc
         (fork-window 
	   #'(lambda (&rest x)
	       (let* ((contact (apply 'make-contact x))
		      (listener (slot-value contact 'listener)))
		 (when listener
		   (let ($ $$ $$$)
		     (declare (special $ $$ $$$))
		     (set-$-and-$$-and-$$$ contact)
		     (editor::flag-window-buffer (slot-value listener 'window)
						 :listener) ;; nick 16.09.92
		     (update-window-state listener)
		     (setf (handler-parent listener) contact) 

		     (tools::language-listener-top-level
		       ;;(interactive-stream-stream listener)
		       listener
		       :command-reader command-reader
		       :input-prompt input-prompt
		       :evaluator evaluator
		       :output-prompt output-prompt 
		       :output-writer output-writer
		       :top-loop-hook 
		       (inspector-top-loop-hook listener nil))))))
	   "Inspector"
	   'inspector 
	   :parent (current-root)
	   :object object
	   :title (next-inspector-title))))
    (mp::process-wait "Waiting for Inspector"
                      'process-window
                      proc)
    (process-window proc))
  (values))

;; eof

