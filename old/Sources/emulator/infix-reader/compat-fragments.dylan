Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Code fragments
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-fragment-constructor body-fragment (#key constituents)
  (evaluable-fragment (rewrite-parsed-body constituents)))

(define-fragment-constructor expression-fragment (#key series)
  (rewrite-binop-series series))

(define-fragment-constructor name-fragment (#key name)
  name)

(define-fragment-constructor property-list-fragment (#key items)
  items)

(define-fragment-constructor modifiers-fragment (#key items)
  items)

;;;; Statements

(define-fragment-constructor clauses-statement-fragment 
    (#key word begin-clause body clause-body)
  (rewrite-parsed-statement
    `(,(infix-macro-name word)
         (,@begin-clause ,body)
         ,@clause-body)))

(define-fragment-constructor clause-body-fragment (#key clauses)
  clauses)

(define-fragment-constructor clause-fragment (#key intermediate-clause body)
  `(,@intermediate-clause ,body))

(define-fragment-constructor cases-statement-fragment (#key word begin-clause case-body)
  (rewrite-parsed-statement
    `(,(infix-macro-name word)
         ,@(tail begin-clause)
         ,@case-body)))

(define-fragment-constructor case-body-fragment (#key cases)
  cases)

(define-fragment-constructor case-fragment (#key lhs rhs)
  `(,lhs ,rhs))

;; Clauses

(define-fragment-constructor clause-with-body-fragment (#key clause body)
  `(,@clause ,body))

;; Begin clauses

(define-method word ((f <list>))
  (head f))

(define-fragment-constructor details-begin-clause-fragment
    (#key word variable-name detail-info)
  `(,word ,@(if variable-name `(,variable-name) '()) ,detail-info))

(define-fragment-constructor expr-begin-clause-fragment
    (#key word variable-name expression detail-clauses)
  `(,word ,@(if variable-name `(,variable-name) '()) 
          (,expression ,@detail-clauses)))

(define-fragment-constructor simple-begin-clause-fragment (#key word)
  `(,word))

;; Intermediate clauses

(define-fragment-constructor details-intermediate-clause-fragment (#rest args #key)
  (apply prefix-details-begin-clause-fragment args))

(define-fragment-constructor expr-intermediate-clause-fragment (#rest args #key)
  (apply prefix-expr-begin-clause-fragment args))

(define-fragment-constructor simple-intermediate-clause-fragment (#rest args #key)
  (apply prefix-simple-begin-clause-fragment args))

(define-fragment-constructor intermediate-clauses-fragment (#key clauses)
  clauses)

;; Operands

(define-fragment-constructor call-fragment (#key function arguments)
  `(,function ,@arguments))

;; Items

(define-fragment-constructor 
    item-fragment (#key modifiers word names type property-list)
  `(,@modifiers ,word ,@(if type `((,@names ,type)) names) ,@property-list))

;;;; Components

(define-fragment-constructor detail-info-fragment (#key detail-list)
  detail-list)

(define-fragment-constructor 
     detail-list-with-properties-fragment (#key details property-list)
  `(,@details ,@property-list))

(define-fragment-constructor 
     detail-list-with-begin-word-fragment (#key details word expression)
  `(,@details (,word ,expression)))

(define-fragment-constructor detail-fragment 
     (#key variables first-detail-clause detail-clauses)
   (unless first-detail-clause
     (set! first-detail-clause '()))
  `(,variables ,@first-detail-clause ,@detail-clauses))

(define-fragment-constructor detail-clauses-fragment (#key clauses)
  (reduce concatenate '() clauses))

(define-fragment-constructor detail-clause-fragment (#key symbol expression)
  `(,symbol ,expression))

(define-fragment-constructor fragment-statement-fragment (#key word fragment)
  `(,word ,fragment))

(define-fragment-constructor 
     fragment-defining-form-fragment (#key word modifiers fragment)
  `(,(concatenate-symbols word '-definer) 
    ,(if (empty? modifiers) fragment (vector modifiers fragment))))

(define-fragment-constructor 
     fragment-function-call-fragment (#key word fragment)
  `(,word ,fragment))

(define-method evaluable-fragment (frag)
  frag)

(define-method as-evaluable-fragment (frag)
  frag)

(define-method as-evaluable-fragment ((frag <pair>))
  frag)

;; eof
