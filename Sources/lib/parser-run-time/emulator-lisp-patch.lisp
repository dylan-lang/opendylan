; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package dylan)

(do-demand-pre-loads :run-parser)

(defun run-parser-using-lisp
     (action-table action-function-table action-nargs-table
      action-nt-table goto-table lexer recoverer)
  (let* ((parsergen::*action-table* action-table)
         (parsergen::*action-function-table* action-function-table)
         (parsergen::*action-nargs-table* action-nargs-table)
         (parsergen::*action-nt-table* action-nt-table)
         (parsergen::*goto-table* goto-table)
         (parsergen::*accept-without-eoi-p* t)
         (history '())
         (parsergen::*lexer* 
           #'(lambda ()
               (multiple-value-bind 
                   (type value string) (funcall lexer)
                 (push string history)
                 (values type value))))
         (parsergen::*custom-error-recovery* 
           #'(lambda ()
               (funcall
                 recoverer 
                 (parsergen::current-symbol)
		 (parsergen::current-symbol-value)
                 history))))
    (declare (special parsergen::*action-table*
                      parsergen::*action-function-table*
                      parsergen::*action-nargs-table*
                      parsergen::*action-nt-table*
                      parsergen::*goto-table*
                      parsergen::*accept-without-eoi-p*
                      parsergen::*lexer*
                      parsergen::*symbol-to-string*
                      parsergen::*custom-error-recovery*))
    (parsergen::run-parser)))

;; eof
