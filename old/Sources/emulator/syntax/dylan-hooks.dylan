;; Hooks into the macro system for the translator.

(define %do-expression-case
  (method (form all-cases)
    (bind-methods
      ((doit (cases)
         (when (empty? cases)
           (error "expression-case - no match for ~s in ~s"
                  form (map first all-cases)))
         (bind ((one-case (first cases))
                (pattern  (first one-case))
                (md       (second one-case)))
           (bind ((vals 
                    (bind-exit (return)
                      (match-pattern pattern 
                                     form
                                     (method (x y)
                                       (format #t "Failed on ~s vs ~s~%" x y)
                                       (return #f))))))
             (if (not vals) (doit (tail cases))
               (bind ((ret (apply md vals)))
                 (or ret (doit (tail cases)))))))))
      (doit all-cases))))

(define *expression-cache* (make <table>))

(define sexp->pattern 
  (method (sexp template?)
    (or (element *expression-cache* sexp default: #f)
        (bind ((pat (compile-pattern-expression
                      (build-expression sexp #f)
                      0
                      template?: template?)))
          (set! (element *expression-cache* sexp) pat)))))

(define %do-expression
  (method (var-names var-vals context expr)
    (generate-template-expression
      (sexp->pattern expr #t)
      (rcurry lookup-in var-names var-vals))))

(define %do-pattern-expression
  (method (context expr)
    (sexp->pattern expr #f)))

;; eof

