module:    idvm-harp
language:  prefix-dylan
Synopsis:  Macro support for the Idvm instruction set
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


;;;; Macro support for the idvm instruction set


;;  idvm-template defines the idvm backend pattern matchers.

(define-template-definer idvm-template idvm-instructions)



;; idvm-method defines methods for idvm clash functions etc.

(define-infix-macro idvm-method <details-begin-word> (form) ()
   ((_ (__ (()) ?form))
    (syntax (method ((backend <idvm-back-end>) (ins <integer>))
              ?form)))
   ((_ (__ ((?type)) ?form))
    (with-syntax ((?with-name (ins-with-name (syntax ?type))))
      (syntax (method ((backend <idvm-back-end>) (ins <integer>))
                (bind ((sv-ins (sv-instructions (variables backend))))
                  (?with-name (sv-ins ins)
                    ?form)))))))


;; Macro to initialize info slot with vector of operations
;;
;; Want
;;
;; initialize-idvm-address-mode-info-for (add, ADD)
;;
;; to expand to
;;
;;with-ops-in idvm-instructions (add)
;;    info := vector(IDVM-RES-LOC-ADD,
;;                   IDVM-RES-LIT-ADD,
;;                   IDVM-LOC-ADD,
;;                   IDVM-LIT-ADD)
;;end;
