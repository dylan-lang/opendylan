; copyright: 1996 Functional Objects, Inc. All rights reserved.
;; Remote listeners

(in-package generic-env)

(clue:defcontact remote-dylan-listener (language-listener) ())

(defparameter *dylan-remote-host* nil)
(defparameter *dylan-remote-app* "IDVM-LISTENER.exe")

(defun dylan::remote-dylan-listen ()
  (let ((*package* (find-package :dylan)))
    (loop do (setq *dylan-remote-host*
                   (editor:prompt-for-string 
                    :prompt  (format nil "Remote host (~a): " 
                                     *dylan-remote-host*)
                    :default *dylan-remote-host*))
          until *dylan-remote-host*)
    (loop do (setq *dylan-remote-app*
                   (editor:prompt-for-string 
                    :prompt  (format nil "Remote app (~a): " 
                                     *dylan-remote-app*)
                    :default *dylan-remote-app*))
          until *dylan-remote-app*)

    (dylan::dylan-connect-to-host *dylan-remote-host*)
    (dylan::dylan-start-remote-app *dylan-remote-app*)
    (language-listen :title (concatenate 'string (pathname-name (pathname *dylan-remote-app*)) " Listener")
                     :class 'remote-dylan-listener
                     :command-reader 'dylan-remote-command-reader
                     :input-prompt
                       #'(lambda (h l)
                           (dylan-remote-prompt-string "" h l))
                     :evaluator 
                       #'(lambda (x)
                           (dylan::dylan-remote-eval x))
                     :output-writer 
                       #'(lambda (x s)
                           (dylan::dylan-remote-print x :stream s)))))

(defun dylan::remote-recursive-dylan-listen (prompt)
  (let ((tools::*input-prompt* 
	  #'(lambda (h l)
	      (dylan-remote-prompt-string prompt h l))))
    (tools::language-listener-recursive-top-level
     :read-stream system::*standard-input*
     :print-stream  system::*standard-output*)))

(defun dylan::remote-command-reader ()
  (dylan::dylan-remote-eval (dylan-remote-command-reader system::*standard-input*)))

(defun dylan-remote-command-reader (s)
  (let ((*readtable* dylan::*infix-dylan-load-eval-readtable*)
        (*package* (find-package :dylan)))
    (let ((command (system::rubber-read-a-command s)))
      (if (or (keywordp command)
              (and (consp command)
                   (keywordp (first command))))
          command
        `(dylan::begin ,command)))))

(defun dylan-recursive-remote-command-reader (s)
  ;; some extra stuff, maybe
  (dylan-remote-command-reader s))

(defun dylan-remote-prompt-string (prompt hist level)
  (if (zerop level)
    (format nil "~%~a~a ~a ? " 
            (dylan::dylan-prompt) prompt hist)
    (format nil "~%~a~a ~a : ~a ? "
            (dylan::dylan-prompt) prompt hist level)))


(let ((lw:*handle-warn-on-redefinition* :warn))
  (editor:defcommand "Start Dylan App" (p)
       "Put current buffer in Dylan mode." 
       "Put current buffer in Dylan mode."  
    (declare (ignore p))
    (dylan::remote-dylan-listen))
)

;; eof
