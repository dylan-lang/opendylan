Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the alien module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Module definition.

(define-module alien
  (use dylan)
  (use stream)
  (export
    
    ensure-alien-modules

    make-alien
    make-alien-array
    push-pointer
    do-cast-as
    genericize
    dylan-alien-address

    <alien-system-object>

    <%memory>
    <%signed-byte>
    <%byte>

    <%unsigned-short>
    <%signed-short>
    <%short>

    <%unsigned-long>
    <%signed-long>
    <%long>

    <%integer>

    <%character>
    <%string>
    <%pointer>

    <C/short>
    <C/short*>
    <C/short**>

    <C/int>
    <C/int*>
    <C/int**>

    <C/char>
    <C/char*>
    <C/char**>

    <C/string>
    <C/string*>
    <C/string**>

  )
)

;; eof
