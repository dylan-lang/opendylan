Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Code to drive infix reading 
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define *debug-infix* #f)

(define-method infix-debugging? ()
  *debug-infix*)

(define-method infix-debugging?-setter (state)
  (set! *debug-infix* state))

(define-class <restart-reading-at-top-level> (<restart>))

(define-method read-infix 
    (#key (stream (standard-input)) (eof-value (unsupplied)))
  (bind-exit (return)
    (when (supplied? eof-value)
      (handler-case
         (for ((char = (input stream) then (input stream))
               (while (whitespace? char)))
           finally
            (undo-input stream))
        ((<end-of-file> condition: c)
          (return eof-value))))
    (while #t
      (handler-case
          (bind ((native
	           (run-parser stream
                               infix-dylan-parser
		               (make <lexer> 
			             function: (method ()
                                                 (tokenize-infix stream)))))
                 (code
                   (as-evaluable-fragment native)))
            (when (infix-debugging?)
              (format #t "code: ~s~%" code))
            (return code))
        ((<restart-reading-at-top-level> condition: c)
          #t)))))

;; eof
