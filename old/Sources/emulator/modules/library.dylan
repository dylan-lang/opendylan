;;; Libraries.

(define-class <library> (<namespace>))

(define-method namespace-export-representation ((lib <library>) name)
  (pair name lib))

;; eof
