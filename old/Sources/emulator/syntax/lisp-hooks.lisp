;; Hook to the translator:

(in-package :dylan)

(define-dylan-macro compile-stage (_ &rest forms)
  (dylan-eval `(begin ,@forms)))

(define-dylan-translator define-macro define-macro-translator (form env)
  (destructuring-bind (_ name init) form
    `(define-dylan-macro ,name (&rest form)
       (funcall ,(translate-dylan-expr `(macro-function ,init) env)
                form))))

(define-dylan-macro expression-case (_ form &rest cases)
  `(%do-expression-case ,form
     (list
       ,@(mapcar
           #'(lambda (case)
               (format t "case: ~s~%" case)
               (let* ((pattern (first case))
                      (code    (rest case))
                      (vars    (pattern-bound-variables pattern)))
                 `(list 
                    (pattern-expression ,(first case))
                    (method (,@vars) ,@code))))
	  cases))))

(define-dylan-macro expression (_ form)
  (let ((vars (pattern-bound-variables form)))
    `(%do-expression ',vars
                     (list ,@vars)
                     'context
                     ',form)))

(define-dylan-macro pattern-expression (_ form)
 `(%do-pattern-expression 'context ',form))
  

(defun pattern-bound-variables (pat)
  (cond
    ((null pat) '())
    ((and (symbolp pat)
          (char= (elt (symbol-name pat) 0) #\?))
      (list pat))
    ((symbolp pat)
      '())
    ((consp pat)
      (append (pattern-bound-variables (car pat))
              (pattern-bound-variables (cdr pat))))
    (t '())))

;; eof
