;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(load (current-pathname "load"))

(deliver 'run-all-tests "c:/temp/test.exe" 0
         :packages-to-keep-symbol-names '(:keyword)
         :multiprocessing t)
