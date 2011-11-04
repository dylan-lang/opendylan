;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(load (current-pathname "defsys"))

(compile-system "all-lisp-client-tests" :load t)

(constant-tests)
