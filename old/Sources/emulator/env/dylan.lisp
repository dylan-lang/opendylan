;; Listeners for Dylan:

(in-package :generic-env)

;;; Make a Dylan listener

(defun dylan::dylan-listen ()
  (let ((*readtable* dylan::*readtable*)
        (*package* (find-package :dylan)))
    (language-listen :title "Dylan Listener"
                     :class 'dylan-listener
                     :command-reader 'dylan-command-reader
                     :input-prompt 'dylan-prompt-string
                     :evaluator 
                       #'(lambda (x)
			   (dylan::dylan-interactive-eval x))
                     :output-writer 
                       #'(lambda (x s)
                           (dylan::dylan-print x :stream s)))))

(clue:defcontact dylan-listener (language-listener) ())

(defun dylan-command-reader (s)
  (let ((*readtable* dylan::*dylan-readtable*)
        (*package* (find-package :dylan)))
    (let ((command (system::rubber-read-a-command s)))
      (if (or (keywordp command)
              (and (consp command)
                   (keywordp (first command))))
          command
        `(dylan::begin ,command)))))

;;; Make and infix Dylan listener

(defun dylan::infix-dylan-listen ()
  (let ((*package* (find-package :dylan)))
    (language-listen :title "Infix Dylan Listener"
                     :class 'infix-dylan-listener
                     :command-reader 'dylan-infix-command-reader
                     :input-prompt 'dylan-prompt-string
                     :evaluator 
                       #'(lambda (x)
			   (dylan::dylan-interactive-eval x))
                     :output-writer 
                       #'(lambda (x s)
                           (dylan::dylan-print x :stream s)))))

(clue:defcontact infix-dylan-listener (language-listener) ())

(defun dylan-infix-command-reader (s)
  (let ((*package* (find-package :dylan))
        (*readtable* dylan::*infix-dylan-load-eval-readtable*))
    (let ((command (system::rubber-read-a-command s)))
      (if (or (keywordp command)
              (and (consp command)
                   (keywordp (first command))))
          command
        `(dylan::begin ,command)))))

(defun dylan-prompt-string (hist level)
  (if (zerop level)
    (format nil "~%~a ? " 
            hist)
    (format nil "~%~a : ~a ? "
            hist level)))

;;; Debugger support for Dylan listeners

(defun remove-unnecessary-begins (expr)
  (if (and (consp expr)
           (eq (first expr) 'dylan::begin)
           (eq (length expr) 2))
      (remove-unnecessary-begins (second expr))
    expr))

;; andrewa, 08 Oct 1996 - added this based on the code in
;; ~ext/ccl/tty-inspect.lisp
(defun dylan::dylan-inspect-eval (expr)
  (let* ((expr
          (remove-unnecessary-begins expr))
         (recursive-inspection
	  (system::can-inspect-recursively expr
					   (caar system::*inspect-stack*))))
    (if recursive-inspection
	(progn 
	  (system::inspect1 (car recursive-inspection))
	  (values))
      (dylan::dylan-eval expr))))

(defun dylan::dylan-interactive-eval (expr)
  "If in debugger, eval expr in the lex env of current stack frame."
  (let ((expr (remove-unnecessary-begins expr)))
    (when (and (consp expr) (keywordp (first expr)))
      (when (dylan::maybe-process-command (first expr) (rest expr))
        (return-from dylan::dylan-interactive-eval (values))))
    (when (keywordp expr)
      (when (dylan::maybe-process-command expr '())
        (return-from dylan::dylan-interactive-eval (values))))
    (cond
     ((plusp system::*inspect-level*)
      (dylan::dylan-inspect-eval expr))
     ((plusp system::*debug-level*)
      (dbg::dbg-eval
       (dylan::translate-dylan-expr ; If we don't pass this env to
        expr		           ; xlator, then it makes lex
        (lisp->xlator-env (current-dbg-env))))) ;;; <-- vars special
     (t
      (dylan::dylan-eval expr)))))

(defun current-dbg-env ()
  ;; This needs to be (and is in fact) the same code used by dbg::dbg-eval
  ;; Restrictions:  Call only when inside the debugger
  (dbg::call-frame-environment-for-eval 
    (dbg::debugger-stack-current-frame dbg::*debugger-stack*)))

;; There is no defstruct for this, so this code from
;;       (method dbg:call-frame-environment-for-eval dbg:call-frame)
;;       in debug/interpreter-frames.lisp is the de-facto "constructor"
;;
;;   `(,(name-from-var-spec var-spec frame) ; name
;;     dbg::%debugger-var
;;     ,frame
;;     ,@var-spec))))

(proclaim '(inline env-slot-var-name))
(defun env-slot-var-name (call-frame-env-slot)
  (first call-frame-env-slot))

;;; Convert a LW eval environment into a Dylan Translator environment
(defun lisp->xlator-env (lisp-eval-env)
  (reduce #'(lambda (xlator-env binding)
	      (dylan::add-value-to-env (env-slot-var-name binding)
				       xlator-env))
	  lisp-eval-env
	  :initial-value (dylan::make-null-lexical-env)))

;; Default working module.

(setf (symbol-value 'dylan::*current-module*)
      (dylan::find-translator-module 'dylan::dylanworks-user))



;;; A patch to enable the inspection of stretchy-vactors

(in-package :dylan)

(defmethod get-inspector-values ((object <stretchy-vector>) mode)
  (declare (ignore mode))
  (get-inspector-values 
   (dylan+dylan/internal::stretchy-vector-data object) 
   mode))

;; And deques too.

(defun deque-values (object)
  (funcall (dylan-resolve as :module dylan)
     (dylan-resolve <list> :module dylan) object))

(defmethod get-inspector-values ((object <deque>) (mode (eql 'deque-values)))
  (let* ((deque-values (deque-values object)))
    (values (loop for i below (length deque-values) collect i)
	    deque-values
            nil
            nil
            "DEQUE VALUES")))

(defmethod inspector-default-mode ((self <deque>))
  (declare (ignore self))
  'deque-values)

;; eof
