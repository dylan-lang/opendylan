Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the mop module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Module definition.

(define-module mop
  (use dylan)
  (use internal import: (allocate))
  (export
    
    <slot-descriptor>

    slot-descriptors
    slot-descriptor
    slot-getter
    slot-setter
    slot-value
    slot-value-setter
    
    slot-type
    slot-allocation
    init-value
    init-function
    init-keyword

    initialize-defaults
    reinitialize

    allocate-instance
    specializers
    
  )
)

;; eof
