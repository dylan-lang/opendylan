(in-package :dylan)

(defun current-gc-state ()
  sys::*%objects-movements-counter)

