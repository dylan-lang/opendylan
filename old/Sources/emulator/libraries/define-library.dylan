Module:   internal
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Library definition macro - reuses the module code.

(define-translator-syntax define-library (form) ()
  ((_ ?name ?clause ...)
    (bind ((info (make <module-expansion> name: (strip (syntax name)))))
      (parse-module-clauses (syntax (?clause ...)) info)
      (with-syntax (((?use ...) (use-info info))
                    ((?create ...) (created-names info))
                    ((?export ...) (exported-names info))
                    ((?init ...) (init-arguments info)))
        (syntax
          ((cl-function (dylan define-library)) '?name
            uses:    (list ?use ...)
            creates: '(?create ...)
            exports: '(?export ...)
            ?init ...))))))

(define-translator-syntax ensure-emulator-library (form) ()
  ((_ ?name ...)
    (syntax 
      (begin
        ((cl-function (dylan ensure-library)) ?name) ...))))

(define-translator-syntax require-emulator-library (form) ()
  ((_ ?name ...)
    (syntax 
      (begin
        ((cl-function (dylan ensure-library)) ?name) ...))))

(define-translator-syntax load-emulator-library (form) ()
  ((_ ?name ...)
    (syntax 
      (begin
        ((cl-function (dylan load-library)) ?name) ...))))

;; eof
