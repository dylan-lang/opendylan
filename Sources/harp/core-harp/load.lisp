(in-package cl-user)

(load (merge-pathnames "defsys" (pathname *load-pathname*)))
(compile-system "dylan-harp-all" :load t)
