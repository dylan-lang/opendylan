module:    dylan
language:  prefix-dylan
author:    Eliot Miranda and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method print
    ((collection <hashed-collection>)
     #key (stream (standard-output)) (verbose? #F))
  (format stream "[~A " (class-debug-name (object-class collection)))
  (print-table-contents
     collection stream: stream verbose?: verbose?)
  (format stream "]")
  collection)

(define-method print-table-contents
    ((collection <table>) #key (stream (standard-output)) (verbose? #F))
  (format stream "[")
  (for ((first? = #T then #F)
        (state = (initial-state collection)
               then (next-state collection state))
        (while state))
    (unless first?
      (format stream " "))
    (format stream "(")
    (print (current-key collection state) stream: stream verbose?: verbose?)
    (format stream " -> ")
    (print
     (current-element collection state) stream: stream verbose?: verbose?)
    (format stream ")" ))
  (format stream "]")
  collection)
