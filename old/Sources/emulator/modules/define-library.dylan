;;; Library definition macro.

(define-class <library-expansion> (<module-expansion>))

(define-syntax-everywhere define-library (form) ()
  ((_ ?name ?clause ...)
    (bind ((info (make <library-expansion> name: (strip (syntax name)))))
      (parse-module-clauses (syntax (?clause ...)) info)
      (with-syntax (((?use ...) (use-info info))
                    ((?create ...) (created-names info))
                    ((?export ...) (exported-names info)))
        (syntax
          (%define-library '?name
            uses:    (list ?use ...)
            creates: '(?create ...)
            exports: '(?export ...)))))))

;; eof
