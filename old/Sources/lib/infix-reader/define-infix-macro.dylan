Module:    infix-reader
Language:  prefix-dylan
Synopsis:  A macro for defining infix macros
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method infix-macro-name (name #key (modifiers '()))
  (concatenate-symbols 'infix- name))

(define-method infix-defining-macro-name (name #key (modifiers '()))
  (concatenate-symbols 'infix-define- name))

(define-method infix-macro-full-name (name type)
  (case type
    ((<defining-word>
      <method-defining-word>
      <generic-function-defining-word>
      <clauses-defining-word>
      <macro-defining-word>)
      (concatenate-symbols 'define- name))
    (else:
      name)))

;; Probably has to define the syntax in the compiler as well.

(define-syntax-everywhere define-infix-macro (form) ()
  ((_ ?name ?kind ?stuff ...)
     (with-syntax
       ((?infix-name (infix-macro-name
                       (infix-macro-full-name
                         (strip (syntax ?name))
                         (strip (syntax ?kind))))))
       (syntax
         (begin
           (define-syntax-everywhere ?infix-name ?stuff ...)
           ((access infix-reader register-infix-macro-word) '?name '?kind)
           (values))))))

(define-syntax-everywhere define-infix-intermediate-word (form) ()
  ((_ ?name ?kind)
    (syntax
      (begin
        ((access infix-reader register-infix-macro-word) '?name '?kind)
	(values)))))

;; eof
