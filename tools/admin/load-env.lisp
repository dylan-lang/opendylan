;; Filename: load-env.lisp
;;
;;   Author: Shri Amit (amit)
;; Synopsis: The following code ensures the dylan env
;;           libraries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package cl-user)

(defun load-capi ()
  (with-build-handler  
   (dylan::ensure-library 'dylan::capi-duim)
   (format t "~&Loaded capi-duim~%")))

(defun load-env ()
  (with-build-handler
   (dylan::ensure-library 'dylan::emulator-environment)
   (format t "~&Loaded emulator-environment~%")))

(load-capi)
(load-env)
