Module:    dylan-user
Language:  prefix-dylan
Synopsis:  Define the table module
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Module definition.

(define-module table
  (use internal 
    export: (object-hash <hashed-collection>
             tally mask elements void-element gc-state
             current-gc-state clear! remove-all-keys! rehash! rehash-using! grow! 
             index-for-key merge-hash-ids merge-hash-states includes-key?)) 
  #|
  (export
    
    ;; number-probes
    ;; compute-statistics
    ;; distribution-table
    ;; mask 
  )
  |#
)

;; eof
