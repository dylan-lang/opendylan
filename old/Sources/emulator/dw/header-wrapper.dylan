;; Wraps the header parser for access to significant values.

; Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
;               All rights reserved.
; License:      Functional Objects Library Public License Version 1.0
; Dual-license: GNU Lesser General Public License
; Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-generic-function read-file-header-values (stream))

(define *default-module-name* "internal")

(define-method file-header-default-module-name ()
  *default-module-name*)

(define-method file-header-default-module-name-setter (name)
  (set! *default-module-name* name))

(define-method read-file-header-values ((s <stream>))
  (bind ((table (maybe-read-file-header s)))
    (values
      (first (or (element table language: default: #f)
                 '("infix-dylan")))
      (first (or (element table module: default: #f)
                 (list (file-header-default-module-name)))))))

;; eof

