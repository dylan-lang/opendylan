(in-package "EDITOR")


;;----------------------------------------------------------------------------
;; Compile/Evaluate Dylan definitions 
;;----------------------------------------------------------------------------

(defhvar "Compile Region Function"
  "Function to compile a region in a buffer"
  :value 'region-infix-dylan-compile
  :mode "Infix-Dylan")

(defhvar "Evaluate Region Function"
  "Function to evaluate a region in a buffer"
  :value 'region-infix-dylan-eval
  :mode "Infix-Dylan")


;; With-input-from-buffer is shaken out of the image. Hence the string
;; conversion step to save having to load any LW sources to compile the
;; translator.

(defun process-infix-dylan-region (process start end)
  (editor::clear-echo-area)
  (editor:message "Processing region...")
  (let ((string (points-to-string start end)))
    (with-input-from-string (stream string)
      (when (and start
	         (or (eq start t) (= (point-position start) 0)))
        (dylan::read-dylan-file-header stream))
      (loop
        (handler-case 
          (let ((form (dylan::read-infix-dylan stream)))
	    (let ((vals (multiple-value-list 
                          (funcall process `(dylan::begin ,form)))))
              (dolist (val vals)
                (format *background-standard-output* "~&~s " val))))
	  (end-of-file ()
            (editor::clear-echo-area)
            (editor:message "Finished processing.")
            (return-from process-infix-dylan-region)))))))

(defun region-infix-dylan-eval (buffer start end print)
  (let ((string (points-to-string start end)))
    ;; (format *terminal-io* "Evaluating Dylan region: ~s~%" string)
    (process-infix-dylan-region 'eval start end)))
  
(defun region-infix-dylan-compile (buffer start end print)
  (let ((string (points-to-string start end)))
    ;; (format *terminal-io* "Evaluating Dylan region: ~s~%" string)
    (compiler::print-optimization-level-message)
    (let ((lw::*compile-default-messages* 1))
      (declare (special lw::*compile-default-messages*))
      (process-infix-dylan-region 'compile start end))))

