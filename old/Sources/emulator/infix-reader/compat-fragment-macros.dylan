Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Compatability fragment macros
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-translator-syntax define-fragment-constructor (form) ()
  ((_ ?name ?args ?form ...)
    (with-syntax ((?prefix-name
		     (as <symbol>
			 (concatenate "prefix-" 
                                      (as <string>
					  (strip (syntax ?name)))))))
        (syntax
          (begin
            (define-method ?prefix-name ?args ?form ...)
            (define-method ?name (#rest args)
              (apply ?prefix-name args)))))))

;; eof
