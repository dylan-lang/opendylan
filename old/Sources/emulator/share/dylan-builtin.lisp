;;
;;; Hack the LispWorks built-in class tree to mirror Dylan more closely.
;;;
;;;   Take out life in our hands and hack the LispWorks built-in class
;;;   tree to look more like we need it to look. Although the result
;;;   is largely a superset, there are differences that may throw up
;;;   bugs - esp. everything inheriting from STANDARD-OBJECT instead
;;;   of just T. 
;;;
;;

(in-package :clos)

(defparameter *subclass-specializers* t)

(defparameter *dylan-built-in-classes*
    '(

      (cl-sequence (t) t)

      (structure-object (standard-object) standard-object t)

      #-:harlequin-pc-lisp
      (ffi::alien (structure-object)
        structure-object standard-object t)

      (stream (structure-object) structure-object standard-object t)

      (collection 
        (standard-object) 
	standard-object t)

      (explicit-key-collection 
        (collection) 
	collection standard-object t)

      (mutable-collection 
        (collection) 
	collection standard-object t)

      (sequence 
        (collection) 
        collection standard-object t)

      (mutable-explicit-key-collection 
        (explicit-key-collection mutable-collection)
        explicit-key-collection mutable-collection collection
	standard-object t)
                                       
      (mutable-sequence
        (mutable-collection sequence)
        mutable-collection sequence collection standard-object t)

      (array 
        (mutable-sequence) 
        mutable-sequence mutable-collection sequence collection
        standard-object t)

      (abstract-vector         ; <vector>
        (array)
        array mutable-sequence mutable-collection sequence collection
        standard-object t)
      (abstract-simple-vector  ; <simple-vector>
        (abstract-vector)
        abstract-vector array mutable-sequence mutable-collection sequence
        collection standard-object t)
      (vector                  ; <simple-object-vector>
        (vector)
        cl-sequence
        abstract-simple-vector abstract-vector array
	mutable-sequence mutable-collection
        sequence collection standard-object t)

      (list 
        (mutable-sequence) 
        cl-sequence
        mutable-sequence mutable-collection sequence collection standard-object t)

      (cons 
        (list)
        cl-sequence
        list mutable-sequence mutable-collection sequence collection standard-object t)

      (null 
        (symbol list) 
        cl-sequence
        symbol list mutable-sequence mutable-collection sequence collection 
        standard-object t)

      (abstract-string    ; <string>
        (mutable-sequence)
        mutable-sequence 
        mutable-collection sequence collection 
        standard-object t)

      (string             ; <byte-string>
        (abstract-string vector)
        cl-sequence 
        abstract-string
        vector array mutable-explicit-key-collection mutable-sequence 
        explicit-key-collection mutable-collection sequence collection 
        standard-object t)

      (bit-vector (vector) vector array sequence standard-object t)
      (character (standard-object) standard-object t)
      (number (standard-object) standard-object t)
      (complex (number) number standard-object t)
      (cl::real (number) number standard-object t)
      (float (cl::real) cl::real number standard-object t)
      (function (standard-object) standard-object t)
      (integer (rational) rational cl::real number standard-object t)
      (ratio (rational) rational cl::real number standard-object t)
      (rational (cl::real) cl::real number standard-object t)
      (symbol (standard-object) standard-object t)
    ;; we have already done this  (symbol t)
    ;; this is already made too (t)
  ))

(defun define-dylan-built-in-classes ()
  (loop for (class-name supers . rest) in *dylan-built-in-classes*
	for cpl = (cons class-name rest)
	for class = (find-class class-name nil)
	for wrapper = (make-wrapper)
	do 
        (unless class
          (setf class (make-instance 'built-in-class))
	  (initialize-wrapper wrapper)
	  (setf (wrapper-class wrapper) class)
          #+:harlequin-pc-lisp
	  (%fill (standard-instance-static-slots class) nil)
          #-:harlequin-pc-lisp
	  (fill (standard-instance-static-slots class) nil)
	  (setf (find-class class-name) class
	        (standard-instance-access class 'wrapper) wrapper
	        (standard-instance-access class 'name) class-name)))
  (loop for (class-name immediate-superclasses . rest) in *dylan-built-in-classes*
	for class =  (find-class class-name)
	for supers = (mapcar 'find-class immediate-superclasses)
	do 
	(setf (standard-instance-access class 'precedence-list)
	      (cons class (mapcar 'find-class rest))
	      (standard-instance-access class 'finalizedp) t
	      (standard-instance-access class 'direct-superclasses)
	      supers)
	(loop for super in supers
	      do (push class
		       (standard-instance-access super
						 'direct-subclasses)))))

(defmethod validate-metaclass-change 
     ((class built-in-class) (meta class))
  t)

(defun define-dylan-built-in-metaclasses ()
  (loop for (class-name supers . rest) in *dylan-built-in-classes*
	for cpl = (cons class-name rest)
	for class = (find-class class-name nil)
	for wrapper = (make-wrapper)
	do 
        (when (typep class 'built-in-class)
          (format t "Changing class of ~s~%" class)
          (let ((meta (make-instance 'standard-class 
                        :superclasses (list (find-class 'built-in-class))
			:name (metaclass-name class-name))))
            (change-class class meta))))
  (loop for (class-name immediate-superclasses . rest) 
	    in *dylan-built-in-classes*
	for class =  (find-class class-name)
	for supers = (mapcar 'find-class immediate-superclasses)
	for meta =   (class-of class)
        for meta-supers = (mapcar #'class-of supers)
        for meta-cpl = (mapcar
                         #'(lambda (name)
                             (class-of (find-class name)))
                        rest)
	do 
        (when (typep class 'built-in-class)
	  (setf (standard-instance-access meta 'precedence-list)
	        (cons meta 
                      (append meta-cpl
			      (class-precedence-list (find-class 'class))))
	        (standard-instance-access meta 'finalizedp) t
	        (standard-instance-access meta 'direct-superclasses)
	        meta-supers)
	  (loop for super in meta-supers
	        do (push meta
		         (standard-instance-access super
						   'direct-subclasses))))))

(defun metaclass-name (name)
  (intern (concatenate 'string "METACLASS-FOR-" (symbol-name name))
          (symbol-package name)))

(define-dylan-built-in-classes)

(when *subclass-specializers*
  (define-dylan-built-in-metaclasses))

;; eof

