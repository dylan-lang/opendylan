(in-package clos)

(defmethod shared-initialize :around 
    ((class dylan-class)  slot-names &rest initargs)
  (apply #'call-next-method 
         class 
         slot-names 
	 (munge-initarg-keys 
           initargs 
	   '((:debug-name . :name)
             #-lispworks3.2
	     (:superclasses . :direct-superclasses) 
             #-lispworks3.2
	     (:slots . :direct-slots)
             #+lispworks3.2
	     (:superclasses . :superclasses) ; non AMOP
             #+lispworks3.2
	     (:slots . :slots)            ; non AMOP
          )))
  ;; Nasty hack to make certain LW things work with Dylan classes
  (unless (find-class (class-name class) nil)
    (setf (find-class (class-name class)) class))
  class)

;; eof

