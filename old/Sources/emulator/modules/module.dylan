;;; Modules.

(define-class <module> (<namespace>))

(define-method namespace-export-representation ((space <module>) name)
  (pair name space))

(define-class <magic-module> (<module>))

(define-method element ((mod <magic-module>) key #key default)
  (or (element (local-key-collection mod) key default: #f)
      (set! (element (local-key-collection mod) key) (pair key mod))))

;; eof
