;; Expander with hygiene:

(define make-unique-name
  (bind ((table (make <table>)))
    (method (name)
      (bind ((count 
               (or (element table name default: #f)
		   (set! (element table name) 0)))
             (unique
               (as <symbol>
                   (concatenate (as <string> name)
                                "."
                                (format #f "~s" count)))))
        (set! (element table name) (+ 1 count))
        unique))))
;; 

(define-class <variable> (<object>)

  (name-of
    required-init-keyword: name:)

  (unique-name-of)

)

(define-method initialize ((v <variable>) #rest args #key name)
  (next-method)
  (set! (unique-name-of v) (make-unique-name name))
  v)

(define-class <module-> (<object>))
(define-class <local-> (<object>))

(define-class <value-> (<object>))

(define-class <macro-> (<object>)

  (transformer-of 
    required-init-keyword: transformer:)

)

(define-class <module-value-variable> (<module-> <value-> <variable>))
(define-class <module-macro-variable> (<module-> <macro-> <variable>))

(define-class <local-value-variable> (<local-> <value-> <variable>))
(define-class <local-macro-variable> (<local-> <macro-> <variable>))

;; A lexical context encodes the default free binding environment of a
;; variable - it's top-level module context plus some lexical structure if it
;; was the product of a local macro.

(define-class <lexical-context> (<object>)

  (module-context-of
    init-keyword: module-context:
    init-value:   #f)

  (local-context-of
    init-keyword: local-context:
    init-value:   '())

)

;; Simple expression expansion:

(define-generic-function expand-expression (exp))

(define expand-expressions
  (method (exps)
    (map expand-expression exps)))

(define-method expand-expression ((x <literal-constant>) e)
  x)

(define-method expand-expression ((x <variable-name>) e)
  x)

(define-method expand-expression ((x <combination>) e)
  (expression-case c
    (() c)
    ((?op ?arg ...)
      (bind ((?expanded-op (expand-expression op e)))
        (if (not (variable-name? expanded-op))
          (bind ((?expanded-arg (expand-expressions ?arg)))
            (expression 
              (?expanded-op ?expanded-arg ...)))
	  (expand-combination-using-operator (resolve-expression ?op e)
                                             c
                                             ?op
                                             ?arg
                                             e))))))

(define-method expand-combination-using-operator ((v <macro->) 
                                                  exp
                                                  op
                                                  args
                                                  e)
  (do-macro-call (transformer-of v) exp))

(define-method expand-combination-using-operator ((v <value-)
                                                  exp
                                                  ?op
                                                  ?arg
                                                  e)
  (bind ((?arg (expand-expressions ?arg)))
    (expression (?op ?arg ...))))

(define-method do-macro-call (m x e)
  (m (close-expression x e)))

(define-method close-expression ((x <expression>) e)
  (do-expression
    (method (x)
      (when (siblings?
  x)

(define-method close-expression ((

;; eof
