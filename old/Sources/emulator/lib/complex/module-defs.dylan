Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the complex number module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Module definitions for the complex library.

(define-module complex
  (use internal
       import: (<rectangular-complex>)
       export: (<rectangular-complex>))
  (export 
    real-part imag-part angle))

;; eof
