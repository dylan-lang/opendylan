module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-generic-function position (big element #key test count)) ;; N'EXISTE PAS

(define-method position
    ((sequence <sequence>) (target <object>) #key (test id?) (count 1))
  (bind-exit (return)
    (for ((index from 0)
	  (item in sequence))
      (bind ((matched? (test target item)))
	(when matched?
	  (set! count (- count 1))
	  (when (= count 0)
	    (return index)))))))

