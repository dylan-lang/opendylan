module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-variable (*gen-sym-counter* <integer>) 0)

(define-method gen-sym ((string <string>))
  (set! *gen-sym-counter* (+ *gen-sym-counter* 1))
  (make <symbol> name: (format #F "~A-~D" string *gen-sym-counter*)))

