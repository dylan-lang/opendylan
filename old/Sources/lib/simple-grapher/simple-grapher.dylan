Module:    simple-grapher
Language:  prefix-dylan
Synopsis:  The simple-grapher library and its modules
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Grapher!

;; ((cl-function do-demand-pre-loads) graph-pane:)

(import-cl-functions
  ((lisp make-instance) as: lw/make-contact)
  ((color get-all-color-names) as: lw/get-all-color-names)
  ((toolkit graph-node-object) as: lw/graph-node-object))

(import-cl-values
  ((toolkit *display*)     as: lw/*display*))

(define-method graph (o #rest args)
  (bind ((tl (lw/make-contact 'tk:top-level
                              parent: lw/*display*
                              title: "Graph")))
    (apply lw/make-contact 'tk:graph-pane
           parent: tl
           graph:  o
           args)))

;; eof
