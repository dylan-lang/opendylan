;; Filename: load-clim-env.lisp
;;
;;   Author: Shri Amit (amit)
;; Synopsis: The following code loads the clim env and
;;           the clim dylan env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package cl-user)

(defun load-clim-env ()
  (with-build-handler
   (setq sys::*handle-warn-on-redefinition* :warn)
   (clim-defsys::set-system-source-file 'clim-environment "~swm/clim/env/sysdcl")
   (clim-defsys::load-system 'clim-environment)
   (remprop 'write-line 'compiler::clc-transforms)
   (format t "~&Loaded clim-env.~%")))

(defun load-clim-dylan-env ()
  (with-build-handler
   (clim-defsys::load-system 'clim-dylan-environment)
   (format t "~&Loaded clim-dylan-env.~%")))
  
(load-clim-env)
(load-clim-dylan-env)
