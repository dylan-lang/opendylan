;;;; Hooks into the module system for the translator.

(define-method canonicalize-module-variable (module name)
  (bind ((home-library home-module original-name
           (munge-city)))
    (values home-library home-module original-name)))

;; eof
