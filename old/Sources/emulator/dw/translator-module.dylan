Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the translator module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Module definition.

(define-module translator
  (use internal
    export:
      (

       add-feature

       file-header-default-module-name
       file-header-default-module-name-setter

       class-debug-name function-debug-name debug-name

       translate-cl-class-name
       call-system-showing-output
       mark-and-sweep
       clean-down

     )
  )
)

;; eof

