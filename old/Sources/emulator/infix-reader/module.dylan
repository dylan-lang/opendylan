Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the infix-reader module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-module infix-reader
  (use dylan)
  (use internal
    import: (unsupplied unsupplied? supplied?))
  (use stream)
  (use parser-run-time)
  (use syntax-case)
  (export

    read-infix

    <restart-reading-at-top-level>

    infix-debugging?
    infix-debugging?-setter

    register-defining-macro
    register-statement-0-macro
    register-statement-1-macro
    register-statement-2-macro

  )
)

;; eof
