(in-package :dylan)

;;
;;; Runtime Support.
;;;
;;;   Miscellaneous CL functions exported for use by Dylan's libraries.
;;;
;;

(declaim (inline dylan-ignore))
(defun dylan-ignore (&rest ignored-args)
  (declare (ignore ignored-args))
  (values))

(declaim (inline dylan-ignorable))
(defun dylan-ignorable (&rest ignorable-args)
  (declare (ignore ignorable-args))
  (values))

;; Just to practice with:

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro dylan-boolean (exp)
    `(let ((res ,exp))
       (if res *dylan-canonical-true* *dylan-canonical-false*)))

  (defmacro define-dylan-boolean (name cl-name ll)
    `(defun ,name ,ll
       (dylan-boolean (,cl-name ,@ll))))

)

(defun dylan-format (sm sg &rest stuff)
  (apply #'format
         (cond
           ((eq sm *dylan-canonical-true*) t)
           ((eq sm *dylan-canonical-false*) nil)
           (t sm))
         sg
         stuff))

(defmethod dylan-break (condition &rest arguments)
  (apply 'break condition arguments))

;; andrewa, 12 Feb 1997 - support Dylan-style format strings
(defmethod dylan-break ((format-string string) &rest arguments)
  (apply 'break
         (dylan+dylan/internal::dylan-to-lisp-format-string format-string)
         arguments))

(defmethod dylan-cerror (restart-description condition &rest arguments)
  (apply 'cerror restart-description condition arguments))

;; andrewa, 12 Feb 1997 - support Dylan-style format strings
(defmethod dylan-cerror (restart-description (format-string string) &rest arguments)
  (apply 'cerror
         restart-description
         (dylan+dylan/internal::dylan-to-lisp-format-string format-string)
         arguments))

(defun dylan-gethash (tab key fail)
  (gethash key tab fail))

(defun dylan-sethash (tab key value)
  (setf (gethash key tab) value))

(defun dylan-remhash (tab key)
  (remhash key tab))

(defun dylan-maphash (function tab)
  (maphash function tab))

(defun dylan-remove (elt seq &key (test #'eq) (count nil))
  (if count
    (remove elt seq :test test :count count)
    (remove elt seq :test test)))

(define-dylan-boolean dylan-oddp oddp (x))
(define-dylan-boolean dylan-evenp evenp (x))
(define-dylan-boolean dylan-zerop zerop (x))
(define-dylan-boolean dylan-plusp plusp (x))
(define-dylan-boolean dylan-minusp minusp (x))

;; (define-dylan-boolean dylan-eq eql (x y))

(defun dylan-eq (x y)
  (dylan-boolean
   (or (eql x y)
       (and x y (symbolp x) (symbolp y)
	    (string-equal (symbol-name x) (symbol-name y))))))

;; (define-dylan-boolean dylan-equal equal (x y))

(defun dylan-equal (x y)
  (dylan-boolean
   (or (equal x y)
       (and x y (symbolp x) (symbolp y)
	    (string-equal (symbol-name x) (symbol-name y))))))

(define-dylan-boolean dylan-null null (x))
(define-dylan-boolean dylan= = (x y))
(define-dylan-boolean dylan< < (x y))
(define-dylan-boolean dylan-instance? clos::dylan-instance? (x y))
(define-dylan-boolean dylan-keyword? keywordp (x))

(define-dylan-boolean dylan-slot-initialized? clos::dylan-slot-initialized? (x y))
(define-dylan-boolean dylan-subclass? clos::dylan-subclass? (x y))

(define-dylan-boolean dylan-find-method clos::dylan-find-method (x y))

(define-dylan-boolean dylan-feature-present? feature-present-p (x))

(define-dylan-boolean dylan-logbit? logbitp (x y))

(defun dylan-intern-symbol (sym)
  (intern sym *the-dylan-package*))

(defun dylan-intern-keyword (kw)
  (intern kw (find-package *keyword-package*)))

(defun dylan-aref-setter (val array &rest indices)
  (setf (apply #'aref array indices) val))

(defun dylan-head-setter (val l)
  (setf (car l) val)
  val)

(defun dylan-tail-setter (val l)
  (setf (cdr l) val)
  val)

(defun dylan-every? (ok? &rest seqs)
  (dylan-boolean
   (apply #'every
	  #'(lambda (x) (not (eq (funcall ok? x) *dylan-canonical-false*)))
	  seqs)))

(defun dylan-any? (ok? &rest seqs)
  (dylan-boolean
   (apply #'some
	  #'(lambda (x)
              (let ((val (funcall ok? x)))
                (when (not (eq val *dylan-canonical-false*))
                  (return-from dylan-any? val))))
	  seqs)))

(defgeneric dylan-print (o &key stream verbose?))

(defmethod dylan-print ((o t)
                        &key 
                        (stream *standard-output*)
                        (verbose? t))
  (if (typep (class-of o) 'clos::dylan-class)
    (format stream "{instance of ~a}" (class-name (class-of o)))
    (format stream "~s" o))
  o)

(defvar *print-object-considers-dylan* t)

(defmethod print-object :around ((o standard-object) s)
  (if *print-object-considers-dylan*
    (if (typep (class-of o) 'clos::dylan-class)
      (dylan-print o :stream s)
      (call-next-method))
    (let ((*print-object-considers-dylan* t))
      (call-next-method))))

(defmethod dylan-print
     ((o condition) &key (stream *standard-output*) &allow-other-keys)
  (call-next-method))

(defun dylan-standard-output ()
  *standard-output*)

(defun dylan-standard-input ()
  *standard-output*)

(defmethod dylan-function-name ((f function))
  (let ((name (system::function-name f)))
    (cond ((null name) "{anonymous}")
	  ((integerp name) (format nil "~d" name))
	  ((listp name) (format nil "~a" name))
	  (t name))))

(defmethod dylan-function-name ((f generic-function))
  (system::generic-function-name f))

;; For cuteness:

(defparameter *value-name-translations* (make-hash-table :test #'eq))
(defparameter *class-name-translations* (make-hash-table :test #'eq))

(defun register-value-name-translation (dyl cl)
  (setf (gethash cl *value-name-translations*) dyl))

(defun translate-cl-value-name (cl)
  (or (gethash cl *value-name-translations*) cl))

(defun register-class-name-translation (dyl cl)
  (setf (gethash cl *class-name-translations*) dyl))

(defun translate-cl-class-name (cl)
  (or (gethash cl *class-name-translations*) cl))

;; Conditions:

(defclass dylan-restart (condition) ())
(defclass dylan-simple-restart (dylan-restart) ())

(defmethod dylan-restart-query ((r dylan-restart))
  (format t "Ignoring restart-query on ~s~%" r)
  r)

(defmethod dylan-return-query ((c condition))
  ())

(defmethod dylan-default-handler ((c condition))
  *dylan-canonical-false*)

(defmethod dylan-default-handler ((c dylan-restart))
  (error "The default handler was invoked on restart ~s" c))

(defmethod dylan-error ((c condition) &rest args)
  (signal c)
  (dylan-default-handler c)
  (error "The handlers for ~s returned." c))

(defmethod dylan-signal ((c condition) &rest ignore)
  (signal c)
  (dylan-default-handler c))

(defmethod dylan-signal ((r dylan-restart) &rest ignore)
  (let ((options (compute-restarts)))
    (dolist (o options)
      ;; (format t "Checking out ~s~%" o)
      (when (and (eq (slot-value o 'conditions::name) 'dylan-restart)
		 (funcall (slot-value o 'conditions::test-function) r))
        (invoke-restart o r)))
    ;; (format t "No restart accepted.~%")
    (dylan-default-handler r)))

(define-dylan-boolean dylan-probe-file probe-file (x))

;; Float support

(defmethod dylan-as-float (x) 
  (float x))

(defmethod dylan-as-single-float (x) 
  (coerce x 'single-float))

(defmethod dylan-as-double-float (x) 
  (coerce x 'double-float))

(defconstant %single-float-sign-byte          (byte  1 31))
(defconstant %single-float-exponent-byte      (byte  8 23))
(defconstant %single-float-significand-byte   (byte 23  0))
(defconstant %single-float-signif-hidden-byte (byte  1 23))
(defconstant %single-float-bias               126)
(defconstant %single-float-digits             24)

(defmethod dylan-decode-single-float ((x single-float))
  (if (= x 0.0)
      0
    (multiple-value-bind (significand exponent sign) (integer-decode-float x)
       (dpb significand %single-float-significand-byte
	    (dpb (+ exponent %single-float-bias %single-float-digits)
		 %single-float-exponent-byte
		 (if (> sign 0) 0 -1))))))

(defun dylan-encode-single-float (bits)
  (let* ((raw-sig (ldb %single-float-significand-byte bits))
	 (raw-exp (ldb %single-float-exponent-byte bits))
	 (raw-sgn (ldb %single-float-sign-byte bits))
         (sig     (dpb 1 %single-float-signif-hidden-byte raw-sig))
	 (exp     (- raw-exp %single-float-bias %single-float-digits)))
    (if (and (= raw-sig 0) (= raw-exp 0) (= raw-sgn 0))
        0.0
        (* (scale-float (coerce sig 'single-float) exp)
           (if (= raw-sgn 1) -1 1)))))

(defconstant %double-float-sign-byte          (byte  1 63))
(defconstant %double-float-exponent-byte      (byte 11 52))
(defconstant %double-float-significand-byte   (byte 52  0))
(defconstant %double-float-signif-hidden-byte (byte  1 52))
(defconstant %double-float-bias               1022)
(defconstant %double-float-digits             53)

(defmethod dylan-decode-double-float ((x double-float))
  (if (= x 0.0)
      (values 0 0)
    (multiple-value-bind (significand exponent sign) (integer-decode-float x)
       (let ((bignum (dpb significand %double-float-significand-byte
			  (dpb (+ exponent %double-float-bias %double-float-digits)
			       %double-float-exponent-byte
			       (if (> sign 0) 0 -1)))))
	 (values (ldb (byte 32  0) bignum)
		 (ldb (byte 32 32) bignum))))))

(defun dylan-encode-double-float (low high)
  (let* ((bits    (dpb high (byte 32 32) low))
	 (raw-sig (ldb %double-float-significand-byte bits))
	 (raw-exp (ldb %double-float-exponent-byte bits))
	 (raw-sgn (ldb %double-float-sign-byte bits))
         (sig     (dpb 1 %double-float-signif-hidden-byte raw-sig))
	 (exp     (- raw-exp %double-float-bias %double-float-digits)))
    (if (and (= raw-sig 0) (= raw-exp 0) (= raw-sgn 0))
        0.0
        (* (scale-float (coerce sig 'double-float) exp)
           (if (= raw-sgn 1) -1 1)))))

;; eof 
