(in-package user)

(load (merge-pathnames "defsys.lisp" *load-pathname*))
(compile-system 'dylan-infix-macros :load t)

;; eof
