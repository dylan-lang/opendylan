(in-package dylan)

(defvar *null-stream* (make-broadcast-stream))

(defun dylan+dylan/dfmc-macro-expander::compile-prefix-dylan-in (mod code)
  (let ((*current-module* (find-translator-module mod))
        (*standard-output* *null-stream*)
        (*standard-error* *null-stream*))
    (declare (special *current-module* *standard-output* *standard-error*))
    ;; (funcall (compile nil `(lambda () ,(translate-dylan-expr code))))
    ;; (eval (translate-dylan-expr code)))
    ;; #|
    (let ((code (walker:walk-form (translate-dylan-expr code))))
      ;; (break "Code")
      (let ((f (eval code)))
        #'(lambda (env form)
            (let ((exp (funcall f env form)))
              ;; (break "Expansion")
              exp))))
    ;; |#
))

;; eof
