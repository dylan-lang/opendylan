Module:   parser-run-time
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(import-cl-functions
  ((dylan run-parser-using-lisp) as: run-parser-using-lisp))

(define-method parser-error (info (s <string>) #rest args)
  (apply reader-error info s args))

(define-class <parser> (<object>)

  (action-table-of
    init-keyword: action-table:)

  (action-function-table-of
    init-keyword: action-function-table:)

  (action-nargs-table-of
    init-keyword: action-nargs-table:)

  (action-nt-table-of
    init-keyword: action-nt-table:)

  (goto-table-of
    init-keyword: goto-table:)

  (error-productions-of
    init-keyword: error-productions:)

  (error-action-function-table-of
    init-keyword: error-action-function-table:)

  (error-action-nt-table-of
    init-keyword: error-action-nt-table:)

)

(define-class <lexer> (<object>)

  (function-of
    init-keyword: function:)

)

;; Override the way tablification works:

(define-method tablify (l)
  (bind ((tab (make <table>))
	 (lisp-table ((access internal table-table) tab)))
    (for-each ((entry l)) ()
      (bind ((key (head entry))
	     (value (tail entry)))
	((access internal set-element/table*object)
	 lisp-table key (pair key value))))
    lisp-table))

(define-method initialize ((p <parser>) 
                           #rest initargs
                           #key action-table goto-table)
    (next-method)
    #|
    (set! (action-table-of p)
          (map tablify action-table))
    |#
    (set! (action-table-of p) action-table)
    (set! (goto-table-of p) 
          (tablify 
            (map 
              (method (cons)
                (pair (head cons) (tail cons)))
	      goto-table)))
    p)
    
(define-method run-parser 
    (info (p <parser>) (l <function>) #key (on-error recover))
  (run-parser-using-lisp
     (action-table-of p)
     (action-function-table-of p)
     (action-nargs-table-of p)
     (action-nt-table-of p)
     (goto-table-of p)
     l
     on-error))

(define-method run-parser
    (info (p <parser>) (l <lexer>) #key (on-error recover))
  (run-parser info p (function-of l) on-error: on-error))

(define-method recover (symbol value history)
  (bind ((dodgy-string value))
    (parser-error
      #f
      "unexpected ~s ~s after \"~a\"" 
      symbol
      dodgy-string
      (reduce (method (acc string)
                (concatenate acc 
                  (format #f "~a" string)
                  " "))
              (make <string>)
              (reverse! (tail history))))))

;; eof
