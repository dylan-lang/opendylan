;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/despatch.lisp,v 1.1 2004/03/12 00:41:15 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)

(defparameter *value-stack-size* (* 1024 64))

(defvar *value-stack* nil)
(defvar *value-stack-ptr* nil)

;; A different initial value stack binding per process.

(defvar *initial-value-stack* nil)
(push '(*initial-value-stack* . *initial-value-stack*) 
      mp:*process-initial-bindings*)

;; If *value-stack* is already bound, we're entering the parser recursively
;; and we need to cons a new stack. Otherwise use this thread's stack,
;; allocating it the first time if necessary.

(defmacro with-initial-value-stack (&body body)
  `(let ((*value-stack* 
            (if *value-stack* (make-array *value-stack-size*)
              (or *initial-value-stack*
                  (setq *initial-value-stack* 
                        (make-array *value-stack-size*)))))
         (*value-stack-ptr* 0))
     (declare (special *value-stack* *value-stack-ptr*))
     ,@body))

(declaim 
  (inline push-value-stack pop-value-stack)
  (optimize (speed 3) (safety 0) (fixnum-safety 0) (debug 0)))

(defun push-value-stack (expr)
  (declare (special *value-stack* *value-stack-ptr*))
  (prog1
      (setf (svref *value-stack* *value-stack-ptr*) expr)
    (incf *value-stack-ptr*)))

(defun pop-value-stack ()
  (declare (special *value-stack* *value-stack-ptr*))
  (let ((new-ptr (decf *value-stack-ptr*)))
    (prog1 
        (svref *value-stack* new-ptr)
      ;; GC paranoia
      (setf (svref *value-stack* new-ptr) nil))))

(defvar *action-function-table*)
(defvar *action-nargs-table*)
(defvar *action-nt-table*)
(defvar *recognize-only* nil)

(declaim (inline get-stack-args))
(defun get-stack-args (nargs)
  (let ((result nil))
    (dotimes (i nargs result)
       (push (pop-value-stack) result))))

(declaim (inline get-action-function))
(defun get-action-function (action-number)
  (svref *action-function-table* action-number))

(declaim (inline get-action-nt))
(defun get-action-nt (action-number)
  (svref *action-nt-table* action-number))

(defun get-action-nargs (action-number)
  (svref *action-nargs-table* action-number))

(defmacro funcall-with-stack-values (f n)
  (let ((names '()) (pops '()))
    (dotimes (i n) 
      (push (gensym) names)
      (push `(pop-value-stack) pops))
    `(let ,(mapcar #'list names pops)
       ;; (format *terminal-io* "~s~s~%" ,f (list ,@(reverse names)))
       (funcall ,f ,@(reverse names)))))

(declaim (inline call-parser-action))
(defun call-parser-action (action-number)
  (unless *recognize-only*
    (let* ((function (get-action-function action-number))
	   (nargs (get-action-nargs action-number)))
      (push-value-stack
        (case nargs
          ((0) (funcall function))
	  ((1) (funcall-with-stack-values function 1))
	  ((2) (funcall-with-stack-values function 2))
	  ((3) (funcall-with-stack-values function 3))
	  ((4) (funcall-with-stack-values function 4))
	  ((5) (funcall-with-stack-values function 5))
	  ((6) (funcall-with-stack-values function 6))
	  ((7) (funcall-with-stack-values function 7))
	  ((8) (funcall-with-stack-values function 8))
	  ((9) (funcall-with-stack-values function 9))
          (otherwise
            (let ((args (get-stack-args nargs)))
              (apply function args))))))))

(declaim (inline read-next-lexeme))
(defun read-next-lexeme ()
  (multiple-value-bind (token value) (funcall *lexer*)
     (setq *symbol* token)
     (setq *value* value)
     (values token value)))

(declaim (inline current-symbol))
(defun current-symbol ()
  *symbol*)

(declaim (inline current-symbol-value))
(defun current-symbol-value()
  *value*)

;; MJS 06Feb95: reordered the loop to accomodate
;; SWITCH-STATE-LOOKAHEAD which attempts to reduce as soon as the next
;; state is known (i.e. before reading the next lexeme).
(defun run-parser()
  (with-initial-state-stack
    (with-initial-value-stack
      (let ((*error-found* nil)
	    (read-input t))
        (reset-lexer)
        (catch 'parsed
          (loop (catch 'syntax-error
	          (loop (loop for state-result = (switch-state-lookahead)
			      while state-result
			      do (call-parser-action state-result))
		        (if read-input
			    (read-next-lexeme)
		          (setq read-input t))
		        (loop for state-result = (switch-state
                                                   (current-symbol))
			      while state-result
			      do (call-parser-action state-result))
		        (push-value-stack (current-symbol-value))))
	        ;; after a syntax-error, do not call read-next-lexeme
                ;; immediately
	        (setq read-input nil)))
        (unless *error-found* 
          (pop-value-stack))))))

;; eof
