Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Support functions used in rewriting infix to prefix
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; The binding forms of the infix syntax do not follow their natural 
;; lexical nesting in terms of the syntax specification. The parser
;; inserts marker tokens for the begining of their scope containing
;; the binding information, and the following rewrites the code into
;; the equivalent correctly nested prefix forms.

(define-class <let-marker> (<object>)
  (bindings-of init-keyword: bindings:))

(define-class <let-handler-marker> (<object>)
  (exception-of init-keyword: exception:)
  (expression-of init-keyword: expression:))

(define-class <local-marker> (<object>)
  (methods-of init-keyword: methods:))

(define-class <local-macro-marker> (<object>)
  (name-of init-keyword: name:)
  (fragment-of init-keyword: fragment:))

(define make-bind-marker (curry make <let-marker>))
(define make-bind-handler-marker (curry make <let-handler-marker>))
(define make-local-marker (curry make <local-marker>))
(define make-local-macro-marker (curry make <local-macro-marker>))

(define-method empty-body? (body)
  (= body '(begin)))

(define-method empty-case-body? (body)
  (and (= (size body) 1)
       (= (size (first body)) 2)
       (instance? (first (first body)) <symbol>)
       (= (second (first body)) '(begin))))

(define-method empty-select-body? (body)
  (= (size body) 1))

(define-method rewrite-parsed-body (body)
  (bind-methods
    ((rewrite (body)
       (if (empty? body) '()
         (bind ((hd (head body))
                (tl (tail body)))
           (select hd instance?
             ((<let-marker>)
               `((bind ((,@(first (bindings-of hd))
                           ,(second (bindings-of hd))))
                   ,@(rewrite tl))))
             ((<local-marker>)
               `((bind-methods ,(methods-of hd)
		  ,@(rewrite tl))))
             ((<let-handler-marker>)
               `((handler-bind (,(head (exception-of hd))
                                ,(expression-of hd)
                                ,@(tail (exception-of hd)))
                   ,@(rewrite tl))))
             ((<local-macro-marker>)
               `((,(name-of hd) 
                  ,(sequence-fragment 
                     fragments: (concatenate 
                                  (fragment-of hd)
                                  (list
                                    *semicolon*
                                    (parsed-fragment
                                      token-class: parsed-body:
                                      token-value: 
                                        (rewrite-parsed-body tl))))))))
             (else:
               (pair hd (rewrite tl))))))))
    `(begin ,@(rewrite body))))

(define-method rewrite-parsed-statement (statement)
  (bind ((name (head statement))
         (clauses (tail statement))
         (nclauses (size clauses)))
    (case name
      ((infix-begin)
        (second (first clauses)))
      ((infix-block)
        (bind ((form (third (first clauses))))
          ;;(for ((clause in clauses))
          (iterate rewrite ((clauses clauses))
            (if (empty? clauses) form
             (bind ((clause (first clauses)))
              (case (first clause)
                ((block)
                  (if (empty? (second clause)) 
                     (rewrite (reverse (tail clauses)))
		    `(bind-exit (,(first (first (second clause)))) 
				,(rewrite (reverse (tail clauses))))))
                ((afterwards)
                  `(values-begin-1 ,(rewrite (tail clauses))
                     ,(second clause)))
                ((cleanup)
		  `(unwind-protect ,(rewrite (tail clauses))
                     ,(second clause)))
                ((exception)
                  (bind ((header-clause (first (second clause)))
                         (var (first header-clause))
                         (name type
                               (if (instance? var <pair>)
                                   (values (first var) (second var))
                                 (values '.no-variable. var)))
                         (props (tail (second clause))))
		    `(handler-case ,(rewrite (tail clauses))
		       ((,type condition: ,name ,@props)
			,(third clause)))))))))))
      ((infix-if)
        `(cond 
           ,@(map (method (clause)
                    (case (first clause)
                      ((if elseif)
                        `(,(first (second clause)) ,(third clause)))
                      ((else)
                        `(else: ,(second clause)))
                      (else:
                        (reader-error 
                          #f "Illegal clause ~s in if" (first clause)))))
                 clauses)))
      ((infix-unless infix-until infix-while)
        (bind ((clause (first clauses)))
          `(,(first clause) ,(first (second clause)) ,(third clause))))
      ((infix-for)
        (bind ((clause (first clauses)))
          `(for ,(rewrite-for-clauses (second clause)) ,(third clause)
             ,@(if (= nclauses 1) '() (second clauses)))))
      ((infix-case)
        (if (empty-case-body? clauses) `(cond)
          `(cond
             ,@(map (method (clause)
                      (bind ((lhs (first clause)))
                        `(,(case lhs
                             ((otherwise) else:)
                             (else: (first lhs)))
                           ,@(body-or-nothing (second clause)))))
                    clauses))))
      ((infix-select)
        (bind ((header ;; (first clauses) 
                 (if (empty-select-body? clauses)
                   (second (first clauses))
                   (first clauses))))
          `(select 
             ,(first header)
             ,(if (= (size header) 1) '== (third header))
             ,@(map (method (clause)
                      (bind ((lhs (first clause)))
                        `(,(case lhs
                             ((otherwise) else:)
                             (else: lhs))
                           ,@(body-or-nothing (second clause)))))
		    (tail clauses)))))
      (else:
        statement))))

;; Hack the property list into the equivalent prefixy while/until

(define-method rewrite-for-clauses (clauses)
  (cond
    ((empty? clauses) '())
    ((keyword? (first clauses))
      `((,(as <symbol> (as <string> (first clauses))) ,(second clauses)) 
        ,@(rewrite-for-clauses (tail (tail clauses)))))
    (else:
      (pair (head clauses) (rewrite-for-clauses (tail clauses))))))

(define-method body-or-nothing (body)
  (if (empty-body? body) '() (list body)))

(define-method body-until (sbody names)
  (iterate walk ((body sbody) (sofar '()))
    (cond
      ((empty? body) 
        (values #f sbody))
      ((member? (head body) names)
        (values (reverse sofar) body))
      (else:
        (walk (tail body) (pair (head body) sofar))))))

(define $nothing (list 'nothing))

(define $max-precedence 9)

(define-method operator-precedence (op)
  (case op
    ((|^|) 9)
    ((* /) 8)
    ((+ -) 7)
    ((= == ~= ~== < > <= >=) 6)
    ((and or) 4)
    ((set! set*!) 2)
    (else: 
      (operator-precedence '=))))

(define-method operator-associativity (op)
  (case op
    ((set! |^|) right:)
    (else: left:)))

;; Bogus!

(define-method as-values ((s <sequence>))
  (apply values s))

(define-method reduce-binop-series (series context-prec)
  (if (empty? (tail series)) series
    (bind ((left op right #rest subseries (as-values series)))
      (bind ((op-prec (operator-precedence op)))
        (cond
          ((< op-prec context-prec) series)
          ((empty? subseries) `((,op ,left ,right)))
          (else:
            (bind ((next-op #rest subsubseries (as-values subseries)))
              (cond
               ((id? op next-op)
                 (case (operator-associativity op)
                   ((left:) 
		     (reduce-binop-series 
		       `((,op ,left ,right) ,@subseries)
		      context-prec))
                   ((right:) 
		     (reduce-binop-series
                       `(,left ,op
                               ,@(reduce-binop-series `(,right ,@subseries) 
                                                      op-prec))
                      context-prec))))
               ((< op-prec (operator-precedence next-op))
	        (reduce-binop-series
		 `(,left ,op ,@(reduce-binop-series `(,right ,@subseries)
						    op-prec))
		 context-prec))
               (else:
	         (reduce-binop-series `((,op ,left ,right) ,@subseries)
                                      context-prec))))))))))

(define-method rewrite-binop-series (series)
  (head (reduce-binop-series series -1)))

(define-method convert-binop (name)
  (case name
    ((|:=|) 'set!)
    ((|&|)  'and)
    ((\|)   'or)
    (else: name)))

(define-method make-getter-name (name) name)

(define-method concatenate-symbols (#rest symbols)
  (as <symbol> (apply concatenate (map (curry as <string>) symbols))))

(define-method concatenate-atom (list atom)
  (iterate walk ((cursor list))
    (if (id? cursor '()) 
        atom 
      (pair (head cursor) (walk (tail cursor))))))

;; Junk:

(define pair? (rcurry instance? <pair>))

;; eof
