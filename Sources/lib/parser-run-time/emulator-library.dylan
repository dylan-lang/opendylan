Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the parser library
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-library parser-run-time 
  (use dylan)
  (use stream)
)

(define-module parser-run-time
  (use dylan)
  (use stream)
  (export

    <lexer> function-of
    <parser>

    run-parser
  
  )
)

;; eof
