;; Extensions for supporting fluid binding.

(in-package dylan)

(defun fluidize (name)
  (%runtime-resolve-name name))

(define-dylan-translator define-fluid translate-define-fluid (form env)
  (destructuring-bind (_ name value) form
    (multiple-value-bind 
      (all-vars vars specs next rest keys) (parse-lambda-list `(,name))
      (let ((var (fluidize (first vars)))
            (spec (first specs)))
        `(compiler::top-level-form (define-fluid ,var)
           (dylan-define-fluid ,var ,(translate-dylan-expr value env)))))))

(define-dylan-translator fluid-bind translate-fluid-bind (form env)
  (destructuring-bind (_ binds &rest body) form
    (if (null binds)
      (translate-dylan-expr `(begin ,@body) env)
      (destructuring-bind (_ ((var val) &rest more) &rest body) form
        (let ((fluid-var (fluidize var)))
          `(let ((,fluid-var ,(translate-dylan-expr val env)))
	     (declare (special ,fluid-var))
	     ,(translate-dylan-expr `(fluid-bind ,more ,@body) env)))))))

;; Backend macro support.

(defmacro dylan-define-fluid (name value)
  `(progn
      (defparameter ,(%runtime-resolve-name name)
        ,value)
      (values)))

;; eof
