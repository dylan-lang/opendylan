module:    literal-table
language:  prefix-dylan
author:    Eliot Miranda and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;;
;;;; <LITERAL-TABLE>
;;;;

(define-class <literal-table> (<table>))

(define literal-table-test-function
  (method (x y #values (_ <boolean>))
    (and (id? (object-class x) (object-class y))
         (= x y))))

(define-method literal-equal-hash (x)
  (equal-hash x))

(define-method table-protocol ((table <literal-table>))
  (values literal-table-test-function equal-hash))
