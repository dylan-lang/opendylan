Module: source-db
Language: prefix-dylan
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(import-cl-functions
	(file-position as: lisp/file-position))

(define-method file-position ((s emulator/<stream>))
	(lisp/file-position s))

(define-method file-position-setter ((pos <integer>) (s emulator/<stream>))
	(lisp/file-position s pos))
