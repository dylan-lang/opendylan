(in-package :dylan)

;; Rubber!

(defun rubber-read-infix-dylan (*standard-input*)
  (editor::stream-rubout-handler
    *standard-input*
    nil
    'read-infix-dylan-with-error-catch))

(defun rubber-read-an-infix-dylan-command (*standard-input*)
  (editor::stream-rubout-handler
    *standard-input*
    nil
    'read-an-infix-dylan-command-with-error-catch))

(defun read-an-infix-dylan-command-with-error-catch (&rest args)
  (let (hairy-read
        result
        more-than-one-form-p
        (stream (car args)))
    (flet ((report-hairy-read (x) 
	     (editor::quiet-message "Error while reading: ~A" x)
	     (setq hairy-read t)
	     (ccl::cond-throw-to-tag 'hairy-read (values) t)))
      (handler-bind
       (#-Harlequin-PC-Lisp (conditions::reader-error #'report-hairy-read)
        #+Harlequin-PC-Lisp (conditions::error #'report-hairy-read)
        (conditions::warning #'report-hairy-read))
       (multiple-value-setq (result more-than-one-form-p)
         (values (apply 'read-an-infix-dylan-command args) nil)))
      (when hairy-read
        (let ((reader-error-text
               (editor::points-to-string
	        (editor::editor-region-stream-start stream)
	        (editor::editor-region-stream-end stream))))
          (editor::update-history-list stream t reader-error-text)
          (with-input-from-string (new-stream reader-error-text)
	    (multiple-value-setq (result more-than-one-form-p)
	      (apply 'read-an-infix-dylan-command new-stream (cdr args))))))
;    (editor::redisplay) is this really necessary?
      (values result more-than-one-form-p))))

(defun read-an-infix-dylan-command (*standard-input*)
  "read a line of input from the terminal
It returns what was read and whether the line was a single sexpr or several"
  (let* ((eof (load-time-value (cons nil nil)))
	 (token (read-infix-dylan *standard-input*)))
    (if (ccl::at-end-of-line)
      (values token nil)
      (let ((list-of-tokens (list token)))
	(loop (when (ccl::at-end-of-line)
		(return (values (nreverse list-of-tokens) t)))
	      (let ((new-token 
		     (read-infix-dylan *standard-input*)))
		(push new-token
		      list-of-tokens)))))))

;; eof

