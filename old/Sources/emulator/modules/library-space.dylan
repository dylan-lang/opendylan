;;; The library namespace.

(define *translator-libraries* (make <table>))

(define-method find-library (name #key (default (unsupplied)))
  (or (element *translator-libraries* name default: #f)
      (if (supplied? default) default
        (error "No such library ~s and no default supplied" name))))

(define-method find-library-setter ((lib <library>) name)
  (set! (element *translator-libraries* name) lib))

;; eof
