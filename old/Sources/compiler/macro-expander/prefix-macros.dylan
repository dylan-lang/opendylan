Module:   infix-reader
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-translator-syntax syntax-template (form) ()
  ((_ ?template)
     (bind ((names values sequence-names sequence-values
               (compile-procedural-template
		  (strip (syntax ?template)))))
       (with-syntax (((?name ...) names)
                     ((?value ...) values)
                     ((?sequence-name ...) sequence-names)
                     ((?sequence-value ...) sequence-values))
          (syntax
            ((access infix-reader evaluate-procedural-template)
               '?template '(?name ...) (list ?value ...)
               '(?sequence-name ...) (list ?sequence-value ...)))))))


;; Don't do this - just turn the expressions into methods and call
;; a run-time interpreter for the macro. Much simpler (and slower -
;; never mind).

(define-infix-macro macro-case macro-case: (form) ()
  ((_ ?form (?rule ...) (?aux-set ...) ?stuff ...)
    (bind ((macro-rules 
             (process-macro-rules 
                main-rules:    (strip (syntax (?rule ...)))
                aux-rule-sets: (strip (syntax (?aux-set ...))))))
      (with-syntax (((?forms ...)  (compile-macro-case-rules macro-rules))
                    (?macro-rules macro-rules))
        (syntax
          (with-procedural-templates (_macro-rules ?macro-rules ?forms ...)
            ((access infix-reader run-macro-rules)
               _macro-rules ?form)))))))

(define-translator-syntax with-procedural-templates (form) ()
  ((_ (?name ?macro-object ?template ...) ?form ...)
    (syntax
      (bind ((?name
	       ((access infix-reader install-procedural-templates)
		  ?macro-object ?template ...)))
        (unwind-protect (begin ?form ...)
          ((access infix-reader uninstall-procedural-templates) ?name))))))

;; eof
