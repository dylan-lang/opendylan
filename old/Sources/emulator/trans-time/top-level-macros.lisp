(in-package :dylan)

;;; Top level lisp macro access to Dylan translation:

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *defining-forms* '())

  (defun dylan-defining-form-p (form)
    (and (consp form)
         (symbolp (car form))
         (member (car form) *defining-forms* :test #'eq)))

  (defmacro define-top-level-macro (name)
    `(progn
       (defmacro ,name (&whole form &rest r)
         (translate-dylan-expr form))
       (push ',name *defining-forms*)))

  ;; Defining forms:

  (define-top-level-macro define-generic-function)
  (define-top-level-macro define-method)
  (define-top-level-macro define)
  (define-top-level-macro define-class)
  (define-top-level-macro define-syntax)

  (define-top-level-macro undefine)

  ;; A couple of useful forms:

  (define-top-level-macro begin)
  (define-top-level-macro bind)
  (define-top-level-macro bind-methods)
  (define-top-level-macro import-cl-values)
  (define-top-level-macro import-cl-functions)
  (define-top-level-macro import-cl-classes)

)

;; eof
