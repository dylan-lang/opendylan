Module:   infix-reader
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(import-cl-functions
  make-load-form
  make-load-form-saving-slots
  read-from-string)

(define-method make-loadable (#rest classes)
  (for ((c in classes))
    (add-method make-load-form
      (method ((form c) #next next-method)
        (make-load-form-saving-slots form)))))

(make-loadable
  <table>
  <macro> <template-macro> <pattern-object>
  <fragment> <rule> <aux-rule-set> <template> <template-closure>)

(define-method make-load-form ((form (singleton *comma*)))
  '(dylan-eval '*comma* :module-name 'infix-reader))

(define-method make-load-form ((form (singleton *semicolon*)))
  '(dylan-eval '*semicolon* :module-name 'infix-reader))

;; eof
