(in-package :dylan)

(define-dylan-translator define-booted-class translate-define-class (form env)
  (destructuring-bind (_ name superclasses &rest slots) form
    (expand-define-class '(find-class 'clos::dylan-class)
		         name 
		         superclasses
		         slots
                         env)))
