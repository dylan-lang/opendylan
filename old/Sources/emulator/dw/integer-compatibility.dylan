Module:    internal
Language:  prefix-dylan
Synopsis:  Define some compatabilities
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define <abstract-integer> <integer>)
(define <big-integer> <integer>)
(define <double-integer> <integer>)

;; While the emulator has arbitrary precision arithmetic, we need to "fix"
;; the values of these constants to something useful.  So, we'll make them 
;; the same as in our product (i.e., for a 32-bit target)
(define $maximum-integer (- (ash 1 29) 1))
(define $minimum-integer (- 0 $maximum-integer 1))

;; eof
