module:    step
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-class <step> (<object>)
  (start required-init-keyword: start:)
  (then required-init-keyword: then:))

(define-method initial-state ((step <step>)) start)

(define-method next-state ((step <step>) (state <function>)) then)

(define-method current-element ((step <step>) (state <function>)) 
  (state step))

