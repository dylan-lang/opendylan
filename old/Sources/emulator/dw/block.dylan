Module:   internal
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Copyright (c) 1993, Jonathan Bachrach
;;; Copyright (c) 1993, Functional Objects, Inc.
;;; Copyright (c) 1993, IRCAM
;;; All rights reserved.

(define-translator-syntax block (form) ()
  ((_ () ?form ...)
    (expand-block #f (syntax (?form ...))))
  ((_ (?exit) ?form ...)
    (expand-block (syntax ?exit) (syntax (?form ...)))))

(define-method expand-block (var body)
  (bind ((main cleanups exceptions (collect-begin-body-clauses body))
         (form #f))

    ;; Install core form.
    (with-syntax (((?form ...) main))
      (set! form (syntax (begin ?form ...))))

    ;; If needed, wrap with unwind-protect.
    (when cleanups
      (with-syntax ((?form form) ((?cleanup ...) cleanups))
        (set! form (syntax (unwind-protect ?form ?cleanup ...)))))

    ;; If needed, wrap with handler-case.
    (when exceptions
      (with-syntax (((?exception ...) exceptions)
		    (?form form))
        (set! form (syntax (handler-case ?form ?exception ...)))))

    ;; If needed, wrap with bind-exit.
    (when var
      (with-syntax ((?var var) (?form form))
        (set! form (syntax (bind-exit (?var) ?form)))))

    form))

(define-method collect-begin-body-clauses (body)
  (bind ((keys     '(cleanup exception))
         (table    (make <table>))
         (main clauses (body-clauses-to-next-key body keys))
         (cleanups '())
         (exceptions '()))
    (bind-methods
      ((walk (clauses)
        (when clauses
          (syntax-case clauses (cleanup exception)
            (() 
              (begin 
                (set! (element table inside) (reverse! forms))))
            ((cleanup ?form ...)
              (bind ((code clauses
                       (body-clauses-to-next-key (syntax (?form ...)) keys)))
                (set! cleanups (concatenate cleanups code))
                (walk clauses)))
            ((exception ((?var ?type) ?spec ...) ?form ...)
              (bind ((code clauses
                       (body-clauses-to-next-key (syntax (?form ...)) keys)))
                (set! exceptions
                      (pair
                        (pair (syntax (?type condition: ?var ?spec ...)) code)
                        exceptions))
                (walk clauses)))))))
      (walk clauses)
      (values main cleanups exceptions))))

(define-method body-clauses-to-next-key (body keys)
  (bind-methods
    ((walk (body forms)
       (syntax-case body (cleanup exception)
         (() 
	   (values (reverse! forms) #f))
	 ((?id ?form ...)
           (and (identifier? (syntax ?id))
                (member? (strip (syntax ?id)) keys))
           (values (reverse! forms) (syntax (?id ?form ...))))
          ((?member ?form ...)
	    (walk (syntax (?form ...))
		  (pair (syntax ?member) forms))))))
    (walk body (syntax ()))))

;; eof
