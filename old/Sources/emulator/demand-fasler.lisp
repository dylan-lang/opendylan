(format t "Demand fasling the Dylan translator.~%")

(load
  (merge-pathnames "defsys" 
		   (pathname *load-pathname*)))

(time
  (let ((*handle-warn-on-redefinition* :warn))
    (compile-system 'dylan::dylan-systems)
    (concatenate-system "dylan-translator" 'dylan::dylan-systems)))




