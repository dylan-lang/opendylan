Module:   internal
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Module definition macro. 

;; This does generic high-level destructuring of the syntax, passing the 
;; results to a module registration pseudo-function. Pseudo because it 
;; will be trapped by whatever compiler/translator processes the resulting 
;; code.

(define-class <module-expansion> (<object>)
  (name           required-init-keyword: name:)
  (use-info       init-value: '())
  (created-names  init-value: '())
  (exported-names init-value: '())
  (init-arguments init-value: '()))

(define-translator-syntax define-module (form) ()
  ((_ ?name ?clause ...)
    (bind ((info (make <module-expansion> name: (strip (syntax name)))))
      (parse-module-clauses (syntax (?clause ...)) info)
      (with-syntax (((?use ...) (use-info info))
                    ((?create ...) (created-names info))
                    ((?export ...) (exported-names info))
                    ((?init ...) (init-arguments info)))
        (syntax
          (%define-module '?name
            uses:    (list ?use ...)
            creates: '(?create ...)
            exports: '(?export ...)
            ?init ...))))))

(define-method parse-module-clauses (clauses info)
  (do-syntax
    (method (clause)
      ((syntax-case clause (use create export)
         ((use    ?spec ...) 
           parse-use-clause)
         ((create ?spec ...)
           parse-create-clause)
         ((export ?spec ...)
           parse-export-clause)
         ((?key ?more ...) (keyword? (strip (syntax ?key)))
            parse-init-clause))
        clause
        info))
    clauses))

(define-method parse-use-clause (clause info)
  (syntax-case clause ()
    ((_ ?used-module ?option ...)
      (set! (use-info info)
            (pair (syntax (list '?used-module '?option ...))
                  (use-info info))))))

(define-method parse-create-clause (clause info)
  (syntax-case clause ()
    ((_ ?name ...)
      (set! (created-names info)
            (concatenate (strip (syntax (?name ...)))
                         (created-names info))))))

(define-method parse-export-clause (clause info)
  (syntax-case clause ()
    ((_ ?name ...)
      (set! (exported-names info)
            (concatenate (strip (syntax (?name ...)))
                         (exported-names info))))))

(define-method parse-init-clause (clause info)
  clause)

(define-translator-syntax define-module-alias (form) ()
  ((_ ?alias ?name)
    (syntax (%define-module-alias '?alias '?name))))

;;; Utilities.

(define-method do-syntax (f form)
  (syntax-case form ()
    (() (values))
    ((?item ?form ...)
      (f (syntax ?item))
      (do-syntax f (syntax (?form ...))))))
    
;; eof
