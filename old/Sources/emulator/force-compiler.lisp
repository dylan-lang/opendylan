(format t "Demand loading the Dylan translator.~%")

(load
  (merge-pathnames "defsys" 
		   (pathname *load-pathname*)))

(system::without-warning-on-redefinition
  (load "~ext/editor/macros"))

(time
  (let ((*handle-warn-on-redefinition* :warn))
    (compile-system 'dylan::dylan-lod-systems :force t :load nil)
    (compile-system 'dylan::dylan-systems :force t :load t)))


