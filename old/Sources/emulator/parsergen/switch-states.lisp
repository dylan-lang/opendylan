;; -*- lisp ; rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/switch-states.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

; switch-states.lisp

(in-package parsergen)

(defparameter *state-stack-size* (* 1024 64))

(defvar *state-stack* nil)
(defvar *state-stack-ptr* nil)

;; A different initial state stack binding per process.

(defvar *initial-state-stack* nil)
(push '(*initial-state-stack* . *initial-state-stack*) 
      mp:*process-initial-bindings*)

;; If *state-stack* is already bound, we're entering the parser recursively
;; and we need to cons a new stack. Otherwise use this thread's stack,
;; allocating it the first time if necessary.

(defmacro with-initial-state-stack (&body body)
  `(let ((*state-stack* 
            (if *state-stack* (make-array *state-stack-size*)
              (or *initial-state-stack*
                  (setq *initial-state-stack* 
                        (make-array *state-stack-size*)))))
         (*state-stack-ptr* 0))
     (declare (special *state-stack* *state-stack-ptr*))
     (push-state-stack 0)
     ,@body))

(declaim 
  (inline push-state-stack pop-state-stack 
          peek-state-stack reduce-state-stack)
  (optimize (speed 3) (safety 0) (fixnum-safety 0) (debug 0)))

(defun push-state-stack (expr)
  (declare (special *state-stack* *state-stack-ptr*))
  (prog1
      (setf (svref *state-stack* *state-stack-ptr*) expr)
    (incf *state-stack-ptr*)))

;; The state stack only contains integers so there's no need for GC
;; paranoia as there is with the value stack.

(defun pop-state-stack ()
  (declare (special *state-stack* *state-stack-ptr*))
  (svref *state-stack* (decf *state-stack-ptr*)))

(defun peek-state-stack ()
  (declare (special *state-stack* *state-stack-ptr*))
  (svref *state-stack* (- *state-stack-ptr* 1)))

(defun reduce-state-stack (n)
  (declare (special *state-stack* *state-stack-ptr*))
  (decf *state-stack-ptr* n))

(defvar *action-table*)

(defvar *goto-table*)

(defvar *error-action-function-table*)

(defvar *accept-without-eoi-p*)

;;; General functionicules on actions & states.

;;; PJG 9Jan95
;;; Reduction is now represented as an integer. (Nolonger cons)
;;; MJS 09Feb95: positive fixnum now

(declaim (inline is-reduction))
(defun is-reduction (action)
  (and (fixnump action) (>= action 0)))

;;; PJG 9Jan95
;;; Reduction is now represented as an integer. (Nolonger cons)

(declaim (inline action-of-reduction))
(defun action-of-reduction (action)
  action)

;;; MJS 09Feb95: shift is a negative fixnum now (was :shift)
(declaim (inline is-shift))
(defun is-shift (action)
  (and (fixnump action) (< action 0)))

(declaim (inline next-state-of-shift))
(defun next-state-of-shift (action)
  (the fixnum (- -1 (the fixnum action))))

(declaim (inline is-accept))
(defun is-accept (action)
  (eq action :accept))

(declaim (inline get-actions))
(defun get-actions (state)
  (svref *action-table* state))  

(declaim (inline get-symbol-action))
(defun get-symbol-action (symbol state)
  (cdr (assoc symbol (get-actions state) :test #'eq)))

(declaim (inline get-default-action))
(defun get-default-action (state)
  (cdr (assoc :any (get-actions state) :test #'eq)))

(declaim (inline get-error-action))
(defun get-error-action (state)
  (cdr (assoc :error (get-actions state))))

(declaim (inline is-all-same-reduction))
(defun is-all-same-reduction (state)
  (let* ((actions (get-actions state))
         (first (cdr (car actions)))
	 (action (when (is-reduction first)
		   (action-of-reduction first))))
    (and action
         (loop for x in (cdr actions)
               for first = (cdr x)
               unless (and (is-reduction first)
                           (= action (action-of-reduction first)))
               do (return nil)
               finally (return t))
         #|
	 (every #'(lambda (x &aux (first (cdr x))) 
		    (when (is-reduction first)
		      (= action 
			 (action-of-reduction first))))
		(cdr actions))
         |#
         first)))

(declaim (inline is-all-accept))
(defun is-all-accept (state)
  (equal (get-actions state) '((:eoi . :accept))))

(defun simple-error-production-p (error-index)
  (eq 1 (length (get-error-production error-index))))

(defun call-error-action (index)
  (funcall (svref *error-action-function-table* index)))

(declaim (inline get-next-state))
(defun get-next-state (state symbol)
  (cdr (assoc symbol (gethash state *goto-table*))))

(declaim (inline get-action))
(defun get-action (state symbol)
    (or (get-symbol-action symbol state)
	(get-default-action state)))

(declaim (inline shift-state))
(defun shift-state (state)
  (push-state-stack state)
  nil)
  
(declaim (inline reduce-state))
(defun reduce-state (action)
  (let ((reduce-string-length (get-action-nargs action))
	(reduce-to-symbol (get-action-nt action)))
    (reduce-state-stack reduce-string-length)
    (push-state-stack (get-next-state (peek-state-stack) reduce-to-symbol))
    action))

(declaim (inline accept-state))
(defun accept-state ()
  (throw 'parsed t))

(declaim (inline switch-state))
(defun switch-state(symbol)
  (let* ((current-state (peek-state-stack))
	 (action (get-action current-state symbol)))
    (cond
     ((null action) (recover-from-error))
     ((is-accept action) (accept-state))
     ((is-shift action)
      (shift-state (next-state-of-shift action)))
     ((is-reduction action) 
      (reduce-state (action-of-reduction action)))
     (T (output-error "Unknown action found :~A" action)))))

(declaim (inline switch-state-lookahead))
(defun switch-state-lookahead ()
  (let* ((state (peek-state-stack))
         (reduction (is-all-same-reduction state)))
    (cond (reduction
	   (reduce-state (action-of-reduction reduction)))
          ((and *accept-without-eoi-p* (is-all-accept state))
	   (accept-state))
	  (t nil))))

;; eof

