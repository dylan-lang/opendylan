Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the syntax-case module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Module definition.

(define-module syntax-case
  (use internal
    export: 
     (

      %define-syntax
      *boot-syntax*

      *next-mark-number*
      *internal-context*
      append
      ellipsis?
      gen-concatenate
      gen-map
      generate-temporaries
      gen-pair
      gen-ref
      match-pattern
      <syntax-object>
      <context>
      $empty-wrapper
      identifier?
      syntax-error
      bound-identifier=?
      id-name-marks-and-context
      id-name
      id-wrapper
      implicit-identifier
      new-mark-wrapper
      join-wrappers
      strip
      wrap
      ;; number
      ;; expression
      ;; wrapper
      ;; context
      id-module
      id-name-and-module
      *default-context*
      <identifier>
    )
  )
)

;; eof
