(in-package :clos) ;; Stoopid.

;; Utilities:

(defun strip-angle-brackets (s)
  (intern (string-trim '(#\< #\>) (symbol-name s))
          (symbol-package s)))

(defun munge-initarg-keys (initargs replace)
  (loop for (key value . rest) on initargs by 'cddr 
        for pair = (assoc key replace) do
        when pair
          collect (cdr pair)
          and collect value
        else
          collect key
          and collect value))

(defun munge-initarg-values (initargs replace)
  (loop for (key value . rest) on initargs by 'cddr 
        for pair = (assoc key replace) do
        when pair
          collect key
          and collect (funcall (cdr pair) value)
        else
          collect key
          and collect value))
      
;; Accessors to unique slot names:

(defun dylan-symbol-p (sym)
  (and (symbolp sym)
       (eq (symbol-package sym) dylan::*the-dylan-package*)))

(defpackage "DYLAN-SLOTS" (:use))

(defvar *dylan-slots-package* (find-package "DYLAN-SLOTS"))

(defparameter *getter-map* (make-hash-table :test #'eq))
(defparameter *setter-map* (make-hash-table :test #'eq))

(defun getter->name (getter &optional slot-name)
  (multiple-value-bind (name foundp) (gethash getter *getter-map*)
    (if foundp name
      (let ((name (or (and (not (dylan-symbol-p slot-name)) slot-name)
                      (gentemp (symbol-name 
                        (dylan::dylan-variable-standard-form
                          (generic-function-name getter)))
                               *dylan-slots-package*))))
        (setf (gethash getter *getter-map*) name)
        (setf (symbol-function name) getter)
        name))))

(defun setter->name (setter &optional slot-name)
  (multiple-value-bind (name foundp) (gethash setter *setter-map*)
    (if foundp name
      (let ((name (or (and (not (dylan-symbol-p slot-name)) slot-name)
                      (gentemp (symbol-name
                        (dylan::dylan-variable-standard-form
                          (generic-function-name setter)))
                               *dylan-slots-package*))))
        (setf (gethash setter *setter-map*) name)
        (setf (symbol-function name) setter)
        name))))

(defun dylan-slot-initialized? (obj getter)
  (multiple-value-bind (name foundp) (gethash getter *getter-map*)
    (if (not foundp)
      (error "slot-initialized? - ~s is not a valid getter for ~s" getter obj)
      (slot-boundp obj name)))) ; HACK!!!

(defun dylan-slot-offset (slotd class)
  (slot-definition-location slotd))

(defun dylan-slot-element-setter (new-value instance offset)
  (fast-set-standard-instance-access instance offset new-value))

;;;; The class of Dylan classes.

;; Supported initargs are - superclasses: debug-name: slots:
;; For each slot - getter: setter: type: init-value: init-function:
;;                 init-keyword: required-init-keyword: allocation:
;;
;; Where we can we munge initargs to look like CLOS initargs where 
;; possible to reuse code.

(defvar *dylan-slot-allocation-types*
  '(instance class each-subclass constant virtual inherited))

(defun get-slot-allocation-type (type)
  (ecase type
    (dylan::instance :instance)
    (dylan::class :class)
    (dylan::each-subclass :each-subclass)
    (dylan::constant :constant)
    (dylan::virtual :virtual)
    (dylan::inherited :instance)))

(defun get-clos-slot-allocation-type (type)
  (ecase type
    (dylan::instance :instance)
    (dylan::class :class)
    (dylan::each-subclass :each-subclass)
    (dylan::constant :instance)
    (dylan::virtual :instance)
    (dylan::inherited :instance)))

(defclass dylan-class (standard-class)
  ((instance-initialized-slots
     :accessor class-instance-initialized-slots)
   (each-subclass-values 
     :accessor class-each-subclass-values))
  (:metaclass standard-class))

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
  (setf (find-class (class-name class)) class)
  class)

;; A LW feature/bug means this method needs to be defined at compile-time 
;; too. Way too liberal but what the hell.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((class dylan-class) (super standard-class))
    (declare (ignore class super))
    t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((class class) (super class))
    (declare (ignore class super))
    t)
  (defmethod validate-superclass ((super class) (class class))
    (declare (ignore class super))
    t))

;; To get subclass specialisation, we make a mirror metaclass for 
;; each class.

(when *subclass-specializers*

(defmethod make-instance :around ((c (eql (find-class 'dylan-class)))
                                  &rest initargs 
                                  &key superclasses (debug-name 'unnamed))
  (declare (dynamic-extent initargs))
  (let ((new-meta 
          (make-instance   'standard-class
             :name         (intern
			     (string-upcase
                               (format nil "class-for-~a" debug-name))
                             dylan::*the-dylan-package*)
             :superclasses 
               (compute-shadow-metaclass-supers superclasses)
             :slots        '())))
    (apply #'make-instance new-meta initargs)))

(defmethod reinitialize-instance :around 
    ((c dylan-class) &rest initargs &key superclasses (debug-name 'unnamed))
  (declare (dynamic-extent initargs))
  ;; Have to hack the cpl of the superclasses too...
  (apply #'reinitialize-instance (class-of c) 
	 :name (intern
		 (string-upcase
		   (format nil "class-for-~a" debug-name))
		 dylan::*the-dylan-package*)
	 :superclasses 
           (compute-shadow-metaclass-supers superclasses)
	 :slots    
           '()
         '())
  (call-next-method))

(defun compute-shadow-metaclass-supers (supers)
  (let ((meta-supers (mapcar #'class-of supers)))
    (if (some #'(lambda (super) (subtypep super (find-class 'dylan-class)))
              meta-supers)
       meta-supers
      (cons (find-class 'dylan-class) meta-supers))))

)

(defclass dylan-slotd-mixin ()
  ((required-init-arg-p
     :initform nil 
     :initarg :required-init-arg-p
     :accessor required-init-arg-p)
   (dylan-allocation
     :initform :instance
     :initarg :dylan-allocation
     :accessor slot-definition-dylan-allocation)
   (init-type
     :initform nil
     :accessor slot-definition-init-type)
   (init-value
     :initform nil 
     :initarg :init-value
     :accessor slot-definition-init-value)
   (init-function
     :initform nil
     :initarg :init-function
     :accessor slot-definition-init-function)
   (getter
     :initform nil
     :initarg :getter
     :accessor slot-definition-getter)
   (setter
     :initform nil
     :initarg :setter
     :accessor slot-definition-setter)))

(defclass dylan-dslotd (dylan-slotd-mixin standard-direct-slot-definition) 
  ((delayed-type 
     :accessor slot-definition-delayed-type
     :initarg  :delayed-type
     :initform #'(lambda () 't))))

(defmethod direct-slot-definition-class ((class dylan-class) slot-spec)
  (declare (ignore class slot-spec ))
  ;;; record required init keywords here.
  (find-class 'dylan-dslotd))

(defmethod shared-initialize :around 
    ((slot dylan-dslotd) slot-names &rest initargs 
     &key getter-name setter-name)
  (multiple-value-bind (key getter rest) (get-properties initargs '(:getter))
   (multiple-value-bind (key setter rest) (get-properties initargs '(:setter))
    (unless rest
      (error "no getter provided for slot ~s slot in initargs ~s"
             slot initargs))
    (let ((initarg nil))
      (multiple-value-bind 
        (key reqd rest) (get-properties initargs '(:required-init-keyword))
        (when rest
          (setq initarg reqd)))
      (multiple-value-bind 
        (key opt rest) (get-properties initargs '(:init-keyword))
        (when rest
          (when initarg
            (error "both init-keyword: ~s and required-init-keyword: ~s supplied for ~s"
                   initarg opt slot))
          (setq initarg opt)))
      (let ((name (getter->name getter getter-name))
            (clos-allocation
              (multiple-value-bind (key val rest) (get-properties initargs '(:allocation))
                (if (not rest) :instance
                  (get-clos-slot-allocation-type val)))))
        (apply #'call-next-method
               slot
               slot-names
	       `(:name ,name
		 :readers (,name)
		 ,@(when setter
		     `(:writers (,(setter->name setter setter-name))))
		 :initargs ,(if initarg (list initarg) '())
		 :allocation ,clos-allocation
                 ,@(munge-initarg-values 
		     (munge-initarg-keys
                       initargs 
		       '((:allocation . :dylan-allocation)))
		     `((:dylan-allocation . 
                          ,#'get-slot-allocation-type))))))))))

(defmethod shared-initialize :after ((slot dylan-dslotd)
				     slot-names
				     &rest slot-specification
				     &key
				     (name nil name-svar)
				     (init-value nil init-value-svar)
				     (init-function nil init-function-svar)
				     (type t type-svar)
                                     allocation
				     (initargs nil initargs-svar)
                                     (flags *default-slot-flags*)
                                     (required-init-keyword nil reqp)
				     (documentation 
				      (lw::fast-documentation slot 'slot)
				      documentation-svar)
				     &allow-other-keys)
  (declare (ignore slot-names))
  (declare (ignore name initform initfunction type initargs documentation))
  (setf (slot-definition-init-type slot) 
        (cond (init-value-svar :one-shot)
	      (init-function-svar :per-allocation)))
  (setf (slot-definition-name slot) name
	(slot-definition-init-value slot) 
          (if init-function init-function #'(lambda () init-value))
        (slot-definition-flags slot) flags 
	(slot-definition-type slot) type
	(slot-definition-initargs slot) (if reqp
                                            (list required-init-keyword)
                                          initargs)
        (required-init-arg-p slot) reqp
	(documentation slot 'slot) documentation
	)
  (if allocation (setf (slot-definition-allocation slot) allocation)))

;;; need a effective-slot-definition-for dylan
(defclass dylan-eslotd (dylan-slotd-mixin standard-effective-slot-definition) ())

(defmethod effective-slot-definition-class ((class dylan-class) direct-slot-definitions)
  (declare (ignore class direct-slot-defintions))
  ;;; record required init keywords here.
  
  (find-class 'dylan-eslotd))

;;; well I guess in reality there should be some semantic checking either 
;;; here or earlier.
(defmethod compute-effective-slot-definition ((class dylan-class)
					      name
					      direct-slot-definitions)
  (let ((slotd (call-next-method))
        (requiredp nil)
        (slot-init-type nil)
        (getter nil)
        (setter nil))
    ;;guess I need to check the semantics here
    (loop for sd in direct-slot-definitions 
          do  
           (when (typep sd 'dylan-dslotd)
             (progn
                ;;;probably should be required in all slots
                (unless requiredp
                  (when (required-init-arg-p sd)
                    (setq requiredp
                          (setf (required-init-arg-p slotd) t))))
                (when (slot-definition-init-type sd)
                  ;; Just override anything for now
                  (unless slot-init-type
                    (setf (slot-definition-init-value slotd)
			  (if (eq :one-shot
				  (setq slot-init-type 
					(slot-definition-init-type sd)))
			      (funcall (slot-definition-init-value sd))
                            (slot-definition-init-value sd))
			  (slot-definition-init-type slotd)
			  slot-init-type)))
                (let ((a-getter (slot-definition-getter sd)))
                  (if (not getter)
                    (setq getter a-getter)
                    (unless (eq getter a-getter)
                      (error "getter function class for slot ~s" name))))
                (let ((a-setter (slot-definition-setter sd)))
                  (if (not setter)
                    (setq setter a-setter)
                    (unless (eq setter a-setter)
                      (error "setter function class for slot ~s" name)))))))
    (setf (slot-definition-getter slotd) getter)
    (setf (slot-definition-setter slotd) setter)
    (setf (slot-definition-dylan-allocation slotd) 
          (if (typep (first direct-slot-definitions) 'dylan-dslotd)
            (slot-definition-dylan-allocation (first direct-slot-definitions))
            :instance))
    (setf (slot-definition-type slotd) 
          (if (typep (first direct-slot-definitions) 'dylan-dslotd)
            (funcall (slot-definition-delayed-type (first direct-slot-definitions)))
            't))
    slotd))

;; Add accessor methods:

(defmethod finalize-inheritance ((c dylan-class))
  (call-next-method)
  (setf (class-instance-initialized-slots c)
    (loop for slotd in (class-effective-slots c)
          unless (member 
                   (slot-definition-dylan-allocation slotd)
                   '(:class :each-subclass))
          collect slotd
          else when (member slotd (class-direct-slots c) 
                            :test #'(lambda (s1 s2)
                                      (eq (slot-definition-name s1)
                                          (slot-definition-name s2)))) do
            (case (slot-definition-init-type slotd)
	      (:one-shot 
	        (setf (slot-value 
		        (class-prototype c) (slot-definition-name slotd))
		      (slot-definition-init-value slotd)))
	      (:per-allocation
	        (setf (slot-value 
		        (class-prototype c) (slot-definition-name slotd))
		      (funcall (slot-definition-init-value slotd)))))))
  c)

(defmethod print-object ((c dylan-class) (s t))
  (format s "{the class ~s}" (strip-angle-brackets (class-name c))))

;; We share lisp's standard-object for now, so:

(defgeneric dylan-initialize (obj &rest initargs)
  (declare (dynamic-extent initargs)))

(defmethod dylan-initialize ((o metaobject) &rest initargs)
  (apply #'initialize-instance o initargs))

(defmethod dylan-initialize ((gf generic-function) &rest initargs)
  )

(defmethod dylan-initialize ((c condition) &rest initargs)
  (if (typep (class-of c) 'dylan-class) 
    (call-next-method)
    (apply #'initialize-instance c initargs)))

(defmethod dylan-initialize ((r restart) &rest initargs)
  (if (typep (class-of c) 'dylan-class) 
    (call-next-method)
    (apply #'initialize-instance c initargs)))

(defun init-keyword-supplied-p (search-key initargs)
  (loop for (key val) on initargs by 'cddr
        when (eq search-key key) do (return t)
        finally (return nil)))

(defmethod dylan-initialize ((instance t) &rest initargs)
  instance)

(defun dylan-default-initialize (instance initargs)
  (declare (dynamic-extent initargs))
  (when (typep (class-of instance) 'dylan-class)
    (let ((slotds (class-instance-initialized-slots (class-of instance))))
      (loop for slotd in slotds
            do (let* ((init-key-given
                       (init-keyword-supplied-p 
                        (first (slot-definition-initargs slotd))
                        initargs)))
	         (when (required-init-arg-p slotd)
		   (unless init-key-given
		     (error "The required init keyword ~s for instances of ~s was not supplied" 
			    (first (slot-definition-initargs slotd))
                            (class-of instance))))
	         (unless init-key-given
		   (case (slot-definition-init-type slotd)
		     (:one-shot 
	              (setf (slot-value instance (slot-definition-name slotd))
			    (slot-definition-init-value slotd)))
		     (:per-allocation
		      (setf (slot-value instance (slot-definition-name slotd))
			    (funcall (slot-definition-init-value slotd)))))))))
  instance))

(defgeneric dylan-make (class &rest initargs)
  (declare (dynamic-extent initargs)))

(defmethod dylan-make ((class standard-class) &rest initargs)
  (declare (optimize (safety 0)) (dynamic-extent initargs))
  (let ((new (allocate-instance class))
        (defaulted-initargs 
          (if (class-default-initargs class)
            (default-initargs class initargs)
            initargs)))
    (apply #'initialize-instance new defaulted-initargs)
    (dylan-default-initialize new defaulted-initargs)
    (apply #'dylan-initialize new defaulted-initargs)
    new))

(defmethod dylan-make ((class (eql (find-class 'class))) &rest initargs)
  (declare (optimize (safety 0)) (dynamic-extent initargs))
  (let ((new (apply #'make-instance (find-class 'dylan-class) initargs)))
    (apply #'dylan-initialize new initargs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((class dylan-class) (super built-in-class))
    (declare (ignore class super))
    t))

(defun dylan-instance? (obj class)
  (cond
   ((and (consp class)
         (eq (first class) 'eql))
    (eql obj (second class)))
   (t
    ;; markt, 19/mar/97, was (subtypep (class-of obj) class), but V. slow in LW
    (typep obj class)
    )))

;
;; Generic functions and methods:
;;
;;   Take generics as is but provide funcallable methods just in case.
;;
;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((callable funcallable-standard-class)
				  (normal   standard-class))
    t)
  (defmethod validate-superclass ((callable funcallable-standard-class)
				  (dodgy    built-in-class))
    t))

(defclass dylan-method (standard-method function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod shared-initialize :after ((m dylan-method)
                                     slots
                                     &key function
                                     &allow-other-keys)
  ;; (set-funcallable-instance-function m function)
  m)

(defun set-dylan-method-function (m fn)
  (set-funcallable-instance-function m fn))

(defmethod print-object ((m dylan-method) (s t))
  (format s "{the method ~s}" 
          `(,(generic-function-name
               (method-generic-function m))
            ,@(mapcar
               #'(lambda (spec)
                   (if (typep spec 'class)
                       (class-name spec)
                     spec))
               (method-specializers m)))))

;; Special reflective ops on classes:

;; Yer not foolin' anyone but yerself..

(defun dylan-object-class (x)
  (cond
    ((eq x (find-class 'standard-object)) (find-class 'dylan-class))
    ((eq x (find-class 'dylan-class))     (find-class 'dylan-class))
    (t (class-of x))))

(defmethod dylan-subclass? ((x class) (y class))
  (member y (class-precedence-list x)))

(let ((blacklist (mapcar #'find-class '(t cl-sequence standard-class))))

  (defmethod dylan-direct-superclasses ((c class))
    (let ((supes (set-difference (class-direct-superclasses c) blacklist)))
      (if (not (null supes)) supes
        (if (not (eq c (find-class 'standard-object))) (list (find-class 'standard-object))
          '()))))

  (defmethod dylan-direct-subclasses ((c class))
    (set-difference (class-direct-subclasses c) blacklist))

)

;; On generics and methods:

(flet ((lambda-list-parameters (x)
         (multiple-value-bind
           (ll reqd opt restp keys othersp)
           (extract-gf-lambda-list-info x)
           (values reqd
                   (if restp dylan::*dylan-canonical-true* dylan::*dylan-canonical-false*)
                   (cond
                     ((eq keys t) '())
                     (keys
                       (mapcar #'(lambda (key-spec)
                                   (destructuring-bind ((key var) def) key-spec
                                     key))
                               keys))
		     (t
                       dylan::*dylan-canonical-false*))))))

  (defmethod dylan-function-arguments ((f function))
    (lambda-list-parameters (function-lambda-list f)))

  (defmethod dylan-function-arguments ((m method))
    (lambda-list-parameters (method-lambda-list f)))

)

(defmethod dylan-find-method ((gf generic-function) specs)
  (find-method gf '() specs :errorp nil))

(defmethod dylan-sorted-applicable-methods ((gf generic-function) &rest args)
  (compute-applicable-methods gf args))

;; Types:

(defclass dylan-type () ())

(defclass dylan-singleton (dylan-type) 
  ((object :initarg :object
           :reader  dylan-singleton-object)))

(defun dylan-singleton (obj)
  `(eql ,obj))

(defgeneric dylan-type-instance-p (x dylan-type))

(defmethod dylan-type-instance-p (x (o standard-object)) 
  nil)

(defmethod dylan-type-instance-p (x (c class))
  (dylan-instance? x c))

(defmethod dylan-type-instance-p (x (l list))
  (if (and (= (length l) 2)
           (eq (first l) 'eql))
    (eql x (second l))
    (error "~s is a invalid dylan type specifier" l)))

(defun dylan-check-type (x dylan-type)
  (if (dylan-type-instance-p x dylan-type) x
    (error 'type-error 
           :datum x
           :expected-type dylan-type)))

(defun convert-dylan-specializer (x)
  (cond
    ((eq x (find-class 'standard-object)) (find-class 't))
    (t x)))

;; Runtime error checkers.

(defun dylan::runtime-dylan-ensure-generic (name symbol lambda-list)
  (block hack
    (when (boundp symbol)
      (let ((value (symbol-value symbol)))
        (cond
          ((not (subtypep (class-of value) 'standard-generic-function))
	    (cerror "Discard the existing definition and create a new generic"
		    "~s is bound to ~s which is not a generic function"
		    name value))
	  ((not (dylan::dylan-compatible-generics-p 
                   lambda-list (function-lambda-list value)))
	    (cerror "Discard the current value and rebind to a new generic"
		    "Lambda list ~s is not congruent with the lambda list ~s of ~s"
		    lambda-list (function-lambda-list value) value))
	  (t 
            (return-from hack)))))
    (dylan::%define-variable name
			     symbol 
                             (make-instance 'standard-generic-function
				            :name name
				            :lambda-list lambda-list
				            :initial-methods '())))
  (values))

;; eof
