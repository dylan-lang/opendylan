Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Hacks to hook the new procedural templates into the emulator
           so that they can be generated.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-translator-syntax emulator-syntax-template (form) ()
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

(define-translator-syntax compiler-syntax-template (form) ()
  ((_ ?template)
       (with-syntax 
            ((?code ((access dfmc-macro-expander compile-compiler-template)
                       (tokens (strip (syntax ?template))))))
          (syntax ?code))))

;; Emulator template mode.

(define-translator-syntax start-emulator-templates (form) ()
  ((_) 
     (syntax (define-translator-syntax syntax-template (form) ()
               ((_ ?template)
                  (syntax (emulator-syntax-template ?template)))))))

;; Compiler template mode.

(define-translator-syntax end-emulator-templates (form) ()
  ((_) 
     (syntax (define-translator-syntax syntax-template (form) ()
               ((_ ?template)
                  (syntax (compiler-syntax-template ?template)))))))

;; Dynamically switchable template code.

(define-translator-syntax start-switchable-templates (form) ()
  ((_) 
     (syntax (define-translator-syntax syntax-template (form) ()
               ((_ ?template)
                  (syntax 
                    (if *native-template-evaluation?*
                      (compiler-syntax-template ?template)
                      (emulator-syntax-template ?template))))))))

;; eof
