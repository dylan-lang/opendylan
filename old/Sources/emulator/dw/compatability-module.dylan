Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the compatability module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Module definition.

(define-module compatability
  (use internal
    export:
      (
       
       in-module
       export

       $no-value
       $not-found
       $local-not-found

       decode-single-float
       %immediate

       concatenate!
       difference
       position
       clear

       power-of-two-ceiling
       find-value

       <type>
       <top-class>
       <byte-string>
       <rectangular-complex>

      )
  )
)

;; eof
