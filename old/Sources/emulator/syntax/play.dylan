;; Playin' about...

(define expander
  (macro
    ((? ?case ...)
      (expression 
        (method (.form.)
	  (expression-case .form.
			   ?case ...))))))

(define given
  (macro
    ((? ?test bind-to: ?var in: ?form ...)
      (expression 
        (bind ((tmp ?test))
	  (if tmp (bind ((?var tmp)) ?form ...) #f))))
    ((? ?test ?form ...)
      (expression
        (if ?test (begin ?form ...) #f)))))

(define binding-when
  (macro
    ((? (?var ?test) ?form ...)
      (expression 
        (bind ((?var ?test))
          (when ?var ?form ...))))))

(define-macro named-bind
  (expander
    ((? ?name (?bindings ...) ?body ...)
      (bind ((?vars (map (method (b) 
                           (expression-case b
                             ((?var ?init) ?var)
			     (?var         ?var)))
                         ?bindings))
             (?inits (map (method (b) 
                            (expression-case b
                              ((?var ?init) ?init)
			      (?var         (expression #f))))
                          ?bindings)))
        (expression 
          (bind-methods ((?name (?vars ...) ?body ...))
	    (?name ?inits ...)))))))

(define-macro define-simple-class
  (expander
    ((? ?name (?super ...)
       (?default-name 
         #key ?init-value
              ?init-function
              ?init-keyword
              ?allocation
              ?getter) ...)
     ; =>
     (bind ((?getter-name
              (map (method (default declared)
                     (or declared default))
                   ?default-name
                   ?getter)))
     (expression
       (begin

         (define-generic-function ?getter-name (x)) ...

         (define ?name
           (make <class>
                 debug-name: '?name
                 superclasses: (list ?super ...)
                 slots: (list
                          (list getter: ?getter-name) ...)))))))))
                                

(define-macro defclass
  (expander
    ((? ?name (?super ...)
       (?slot-name
         #key ?init-value
              ?init-function)
       ...)
     (format #t "vars: ~s~%" 
             (list ?name ?super ?slot-name ?init-value ?init-function))
     (expression
       (quote (defclass ?name (?super ...) 
                 #key a b c
                 (?init-value ...)))))))

                (?init-value ...)
                (?init-function ...)))))))

(define key-test
  (macro
    ((? ?name #key ?a (?b "b-default") other)
      #`(list ?name ?a ?b))))

;; Ahem...

(define-class <module-expansion> (<object>)
  (name           required-init-keyword: name:)
  (use-info       init-value: '())
  (created-names  init-value: '())
  (exported-names init-value: '()))

(define define-module
  (macro 
    ((? ?name ?clause ...)
      (bind ((info (make <module-expansion> name: (strip (syntax name)))))
        (parse-module-clauses ?clause info)
        (bind ((?use (use-info info))
	       (?create (created-names info))
	       (?export (exported-names info)))
          #`(%define-module '?name
                            uses:    (list ?use ...)
                            creates: '(?create ...)
                            exports: '(?export ...)))))))

(define-method parse-module-clauses (clauses info)
  (for-each ((clause clauses)) ()
    ((expression-case clause 
       ((use    ?spec ...) parse-use-clause)
       ((create ?spec ...) parse-create-clause)
       ((export ?spec ...) parse-export-clause))
     clause
     info)))

(define-method parse-use-clause (clause info)
  (expression-case clause
    ((? ?used-module ?option ...)
      (set! (use-info info)
            (pair #`(list '?used-module '?option ...)
                  (use-info info))))))

(define-method parse-create-clause (clause info)
  (expression-case clause 
    ((? ?name ...)
      (set! (created-names info)
            (concatenate ?name
                         (created-names info))))))

(define-method parse-export-clause (clause info)
  (expression-case clause
    ((? ?name ...)
      (set! (exported-names info)
            (concatenate ?name 
                         (exported-names info))))))
(define-module wop (a))

;; eof
