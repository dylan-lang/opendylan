;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/deliver.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(load (current-pathname "load"))

(deliver 'run-all-tests "c:/temp/test.exe" 0
         :packages-to-keep-symbol-names '(:keyword)
         :multiprocessing t)
