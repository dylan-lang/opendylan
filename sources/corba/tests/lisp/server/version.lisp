;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/version.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CORBA")

(defvar *test-suite-server-version*
  (file-write-date 
   (current-pathname)))
