;; In case we want special conditions later I guess...

(in-package :dylan)

(defun define-error (where &rest args)
  (apply 'error args))

(defun define-warning (where &rest args)
  (apply 'warn args))

(defun translate-error (where &rest args)
  (apply 'error args))

;; eof
