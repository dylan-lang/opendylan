;; Dylan condition classes and signaling
;; The buld of this is implemented in run-time/support.dylan

(define-class <simple-warning> (lisp/<simple-warning>))
(define-class <simple-error> (lisp/<simple-error>))

;;--- What a bletcherous kludge!  --swm
(define <simple-condition> <simple-warning>)

(define-method signal ((s <string>) #rest args)
  (signal (make <simple-warning>
                format-string:    s
                format-arguments: args)))

(define-method error ((s <string>) #rest args)
  (error (make <simple-error>
               format-string:    s
               format-arguments: args)))

(import-cl-functions
  (invoke-debugger as: lisp/invoke-debugger))

(define *error-presentation-hook*
  (method ((c <condition>))
    (lisp/invoke-debugger c)))

(define *warning-presentation-hook*
  (method ((c <condition>))
    (format #t "Warning: ~a~%" c)
    #f))

(define-method default-handler ((c <serious-condition>))
  (*error-presentation-hook* c))

(define-method default-handler ((c <warning>))
  (*warning-presentation-hook* c))

;; eof
