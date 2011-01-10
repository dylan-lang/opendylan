Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the parser module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-module parser 
  (use dylan)
  (use stream)
  (export

    <parser>
    <lexer>

    run-parser
  
  )
)

;; eof
