(format t "Demand loading the Dylan translator.~%")

(load
  (merge-pathnames "defsys" 
		   (print (pathname *load-pathname*))))

(time
  (let ((*handle-warn-on-redefinition* :warn))
    (compile-system 'dylan::dylan-lod-systems :load nil)
    (compile-system 'dylan::dylan-systems :load t)))



