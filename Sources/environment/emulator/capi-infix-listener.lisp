(in-package "CL-USER")

(defclass capi-infix-listener-pane (capi:listener-pane)
  ()
  (:default-initargs
   :top-level-function 'capi-infix-dylan-top-level))

(defun capi-infix-dylan-top-level (top-level listener-pane stream)
  (declare (ignore stream))
  (capi::initialize-interactive-stream top-level listener-pane)
  (let* ((command-reader 'generic-env::dylan-infix-command-reader)
         (input-prompt 'generic-env::dylan-prompt-string)
         (evaluator
	  #'(lambda (x)
	      (dylan::dylan-interactive-eval x)))
         (output-prompt system::*output-prompt*)
         (output-writer 
          #'(lambda (x s)
	      (dylan::dylan-print x :stream s)))
         (system::*command-reader* command-reader)
         (system::*output-writer* output-writer)
         (tools::*evaluator*
          (tools::wrap-evaluator command-reader
				 input-prompt
				 evaluator
				 output-prompt
				 output-writer))
         (tools::*input-prompt* input-prompt)
         ;; Seems to be necessary for the emulator...
         (*package* (find-package "DYLAN")))
    (declare (special system::*output-writer*
                      system::*output-prompt*))
    (system::language-listener-top-level
     (capi:interactive-stream-stream listener-pane)
     :eval-function tools::*evaluator*
     :prompt (tools::wrap-prompt input-prompt)
     :allow-other-keys t)))

(defun listener-value ()
  (if (> (length /) 1)
      /
    *))

(defun capi-infix-listener-pane-value (pane)
  (listener-value)
  #+comment
  (capi:apply-in-pane-process pane 'listener-value))
