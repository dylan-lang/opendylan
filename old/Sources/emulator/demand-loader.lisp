(in-package :lisp)

(format *terminal-io* "Demand loading the Dylan translator.~%")

(load
  (merge-pathnames "defsys" 
		   (truename *load-pathname*)))

(let ((*handle-warn-on-redefinition* :warn))
  (load-system 'dylan::dylan-systems))

(format *terminal-io* "Loaded the Dylan translator.~%")
