; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package dylan)

(defun locate-package-variable (object package)
  (do-symbols (symbol package)
    (when (and (boundp symbol) (eq (symbol-value symbol) object))
      (return-from locate-package-variable
        (intern (symbol-name symbol) *the-dylan-package*))))
  *dylan-canonical-false*)

(defun package-variable-value (symbol package &key (default *dylan-canonical-false*))
  (if symbol
      (let ((bind (intern (symbol-name symbol) package)))
        (if (boundp bind) (symbol-value bind) default))
      *dylan-canonical-false*))

;; eof
