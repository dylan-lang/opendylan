Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Standard infix macros
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Definitions.

(define-infix-macro variable <defining-word> (form) (thread atomic)
  ((_ (atomic thread ?mods ...) ?stuff ...) 
   (syntax (define-thread-variable ?stuff ...)))

  ((_ (thread ?mods ...) ?stuff ...) 
   (syntax (define-thread-variable ?stuff ...)))

  ((_ ?mods ?stuff ...) 
   (syntax (define-variable ?stuff ...))))



(define-infix-macro constant <defining-word> (form) ()
  ((_ ?mods ?stuff ...) (syntax (define ?stuff ...))))

(define-infix-macro generic <generic-function-defining-word> (form) ()
  ((_ ?mods ?stuff ...) (syntax (define-generic-function ?stuff ...))))

(define-infix-macro method <method-defining-word> (form) ()
  ((_ ?mods ?stuff ...) (syntax (define-method ?stuff ...))))

(define-infix-macro class <defining-word> (form) ()
  ((_ ?mods ?name (?super ...) ?slot ...)
    (with-syntax
      (( (?prefix-slot ...)
	(map (method (slot)
               (syntax-case slot (slot sealed keyword required)
                 ((slot ?stuff ...)
                   (process-slot-part 
                     (syntax (?stuff ...)) (syntax ())))
                 ((sealed slot ?stuff ...)
                   (process-slot-part 
                     (syntax (?stuff ...)) (syntax (sealed: #t))))
                 ((?allocation slot ?stuff ...)
                   (process-slot-part 
                     (syntax (?stuff ...)) 
                     (syntax (allocation: ?allocation))))
                 ((sealed ?allocation slot ?stuff ...)
                   (process-slot-part 
                     (syntax (?stuff ...)) 
                     (syntax (allocation: ?allocation sealed: #t))))
                 ((?allocation sealed slot ?stuff ...)
                   (process-slot-part 
                     (syntax (?stuff ...)) 
                     (syntax (allocation: ?allocation sealed: #t))))
                 ((keyword ?stuff ...)
                   (process-slot-part 
                     (syntax (?stuff ...)) 
                     (syntax (required: #f))))
                 ((required keyword ?stuff ...)
                   (process-slot-part 
                     (syntax (?stuff ...)) 
                     (syntax (required: #t))))))
             (strip (syntax (?slot ...))))))
      (syntax
        (define-class ?name (?super ...)
          ?prefix-slot ...)))))

(define-method process-slot-part (form extra)
  (with-syntax (( (?extra ...) extra))
    (syntax-case form ()
      ((((?var) ?default) ?init ...)
        (syntax 
          (?var init-function: (method () ?default)
                ?extra ...
                ?init ...)))
       ((((?var ?type) ?default) ?init ...)
        (syntax 
          (?var type: ?type 
                init-function: (method () ?default)
                ?extra ...
                ?init ...)))
      (((?var ?type) ?init ...)
        (syntax 
          (?var type: ?type ?extra ... ?init ...)))
      ((?var ?init ...)
        (syntax
          (?var ?extra ... ?init ...))))))

(define-infix-macro module <defining-word> (form) ()
  ((_ ?mods ?name ?clause ...)
    (syntax
      (define-module ?name
        ?clause ...))))

(define-infix-macro library <defining-word> (form) ()
  ((_ ?mods ?name ?clause ...)
    (syntax
      (define-library ?name
        ?clause ...))))

;; Statements:

(define-infix-macro iterate <details-begin-word> (form) ()
  ((_ (_ ?name (?bind ...) ?body))
    (with-syntax
      (( (?prefix-bind ...)
        (map (method (bnd)
               (syntax-case bnd (=)
                 ((?var = ?init) (syntax (?var ?init)))))
             (strip (syntax (?bind ...))))))
      (syntax
        (iterate ?name (?prefix-bind ...) ?body)))))

(define-infix-macro when <expr-begin-word> (form) ()
  ((_ (_ (?expr) ?body))
    (syntax 
      (when ?expr ?body))))

(define-infix-macro fluid-bind <details-begin-word> (form) ()
   ((_ (_ (?binding ...) ?body))
    (with-syntax
      (( (?prefix-binding ...)
        (map (method (bnd)
               (syntax-case bnd (=)
                 ((?var = ?init) (syntax (?var ?init)))))
             (strip (syntax (?binding ...))))))
      (syntax (fluid-bind (?prefix-binding ...) ?body)))))

;; Infix macros:

(define-infix-macro define-macro macro: (form) ()
  ((_ ?mods ?name (?rule ...) (?aux-set ...) ?stuff ...)
    (bind ((infix-macro-object word
             (process-infix-macro-definition (strip (syntax ?name))
                main-rules: (strip (syntax (?rule ...)))
                aux-rule-sets: (strip (syntax (?aux-set ...))))))
      (with-syntax ((?macro-name word)
                    (?macro-object infix-macro-object)
                    (?dots (syntax ...))
                    ((?forms ...) 
                       (compile-macro-case-rules infix-macro-object)))
        ;; Function macros, having no special syntactic category, yield
        ;; grammar output for their calls in a form different from other
        ;; kinds of macro.
        (if (instance? 
              (first (main-rules infix-macro-object)) <function-rule>)
          (syntax
            (begin
              ((access infix-reader register-macro) ?macro-object)
              ((access infix-reader install-procedural-templates)
                 ?macro-object ?forms ...)
              (define-modular-syntax-everywhere ?macro-name (form) ()
                ((_ ?arg ?dots)
                  ((access infix-reader process-infix-macro-call)
                     ?macro-object
                     ((access internal strip) (syntax (?arg ?dots))))))))
          (syntax
            (begin
              ((access infix-reader register-macro) ?macro-object)
              ((access infix-reader install-procedural-templates)
                 ?macro-object ?forms ...)
              (define-modular-syntax-everywhere ?macro-name (form) ()
                ((_ ?call-body) 
                  ((access infix-reader process-infix-macro-call )
                     ?macro-object 
                       ((access internal strip) (syntax ?call-body))))))))))))

(define-method compile-macro-case-rules (macro-rules)
  (bind ((forms '()))
    (do-rules
      (method (rule)
        (unless (instance? (template rule) <template>)
          (set! forms (pair (compile-macro-case-rule rule) forms))))
      macro-rules)
    (reverse! forms)))

(define-method compile-macro-case-rule (rule)
  (with-syntax
     ((?form (template rule))
      ((?name ...) (compile-procedural-pattern (pattern rule))))
    (syntax
      (pair '(?name ...) (method (?name ...) ?form)))))

;; eof
