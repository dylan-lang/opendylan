Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Hacks to macros to hook expander code generation into the 
           emulator. Bootstrapping only.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-infix-macro pattern-test <macro-defining-word> (form) ()
  ((_ ...)
    (expand-test-macro-thing
       form '-pattern-test
       (syntax do-define-core-definition))))

(define-method expand-test-macro-thing (form suffix thunk)
  (syntax-case form ()
    ((_ ?mods ?name (?rule ...) (?aux-set ...) ?stuff ...)
      (with-syntax
         ((?code ((access dfmc-macro-expander compile-compiler-pattern)
		    (fragments
                      (pattern
                        (pattern (head (strip (syntax (?rule ...))))))))))
         (syntax ?code)))))

(define-infix-macro macro-case macro-case: (form) ()
  ((_ ?form (?rule ...) (?aux-set ...) ?stuff ...)
    (with-syntax
       ((?code
          ((access dfmc-macro-expander compile-compiler-fragment-case)
              main-rules:    (strip (syntax (?rule ...)))
              aux-rule-sets: (strip (syntax (?aux-set ...))))))
      (syntax 
        (bind ((_f*_ ((access dfmc-macro-expander as-fragment-tokens) ?form)))
          ?code)))))

(define-infix-macro &converter <macro-defining-word> (form) ()
  ((_ ...)
    (expand-native-compiler-macro-thing
       (syntax (env context form)) form '-converter
       (syntax do-define-core-converter))))

(define-infix-macro &definition <macro-defining-word> (form) ()
  ((_ ...)
    (expand-native-compiler-macro-thing
       (syntax (env form)) form '-definition
       (syntax do-define-core-definition))))

(define-infix-macro &macro <macro-defining-word> (form) ()
  ((_ ...)
    (expand-native-compiler-macro-thing
       (syntax (env form)) form '-expander
       (syntax do-define-core-macro))))

(define-method expand-native-compiler-macro-thing
    (parameters form suffix thunk)
  (syntax-case form ()
    ((_ ?mods ?name (?rule ...) (?aux-set ...) ?stuff ...)
      (with-syntax (((?parameters ...) parameters)
                    (?expander-name 
                      (concatenate-symbols (strip (syntax ?name)) suffix))
                    (?thunk thunk))

       (syntax
	 (begin
            (define-method ?expander-name (?parameters ...)
              (macro-call-case ?name form
                (?rule ...) (?aux-set ...) ?stuff ...))
	    (bind ((word word-class
                       (classify-macro-in-variable-name-using-main-rules
                          (?rule ...) ?name)))
		(?thunk 
                   '?name word word-class
                   (vector '?name word word-class)
                   ?expander-name))))))))

(define-translator-syntax 
     classify-macro-in-variable-name-using-main-rules (form) ()
  ((_ (?rule ?more ...) ?name)
     (bind ((word word-class
              ((access 
                  dfmc-macro-expander compiler-macro-word-in-variable-name)
                (strip (syntax ?rule)) (strip (syntax ?name)))))
       (with-syntax ((?word word) (?word-class word-class))
         (syntax (values '?word ?word-class))))))

(define-translator-syntax macro-call-case (form) ()
  ((_ ?name ?form (?rule ...) (?aux-set ...) ?stuff ...)
    (with-syntax
       ((?code
          ((access dfmc-macro-expander compile-compiler-macro-call-case)
              name:          (strip (syntax ?name))
              main-rules:    (strip (syntax (?rule ...)))
              aux-rule-sets: (strip (syntax (?aux-set ...))))))
      (syntax 
        (bind ((_f*_ 
                 ((access dfmc-macro-expander call-as-fragment-tokens) 
                    ?form)))
          ?code)))))

;; Extra constrainy to keep the emulator happy.

(define-method constraint-name? ((name (singleton 'body!))) #t)

;; eof
