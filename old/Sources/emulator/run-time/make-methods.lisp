(in-package :clos)

(defclass dylan-class (standard-class)
  ((instance-initialized-slots
     :accessor class-instance-initialized-slots)
   (each-subclass-values 
     :accessor class-each-subclass-values)
   (default-initializer
     :accessor class-default-initializer))
  (:metaclass standard-class))

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
  (setf (class-default-initializer c)
    (if (and (class-all-dylan-p c)
             (eq (length (class-instance-initialized-slots c))
                 (length (class-effective-slots c))))
	(generate-and-compile-default-initialize-function c)
      #'dylan-default-initializer))
  c)

(defun class-all-dylan-p (class)
  (flet ((dylan-p (super)
           (or (typep super 'dylan-class)
               (eq super (find-class 'standard-object))
               (eq super (find-class 't)))))
    (every #'dylan-p (class-precedence-list class))))

(defun dylan-default-initializer (object &rest initargs)
  (declare (dynamic-extent initargs))
  (dylan-new-default-initialize object initargs))

(defmethod class-default-initializer ((class class))
  #'dylan-default-initializer)

(defmethod generate-and-compile-default-initialize-function
     ((class dylan-class))
  (let ((code (generate-default-initialize-function class)))
    ;; (format *terminal-io* "Code: ~s~%" code)
    (compile nil (second code))
    ;; (eval code)
    ))

(defmethod generate-default-initialize-function ((class dylan-class))
  (let ((key-specs '()) (set-specs '()))
    (dolist (slotd (class-instance-initialized-slots class))
      (multiple-value-bind (key-spec set-spec) 
          (generate-slot-initialize-code slotd) 
        (when key-spec (push key-spec key-specs))
        (push set-spec set-specs)))
    `#'(lambda 
            (object #| &rest initargs |# &key ,@key-specs
		    &allow-other-keys) 
	  (declare (optimize (speed 3) (safety 0)) (ignore object))
          ;; (format *terminal-io* "!~s!" (class-of object))
         ,@set-specs)))

(defmethod generate-slot-initialize-code ((slotd dylan-eslotd))
  (let* ((key (first (slot-definition-initargs slotd)))
         (init (slot-definition-init-value slotd))
         (init-exp 
           (case (slot-definition-init-type slotd)
             (:one-shot `',init)
             (:per-allocation `(funcall ,init))
             (otherwise
               (if (and key (required-init-arg-p slotd))
                 `(error "Missing required init keyword ~s for ~s." 
                         ,key (class-of object))
                 *slot-unbound*))))
         (offset (slot-definition-location slotd))
         ;; The offset is needed to disambiguate when two slots have
         ;; the same init keyword.
         (key-name (intern (format nil "~a-init-value-~a" key offset)
                           dylan::*the-dylan-package*))
         (key-spec (and key `((,key ,key-name) ,init-exp)))
         (val-spec (if key key-name init-exp))
             (set-spec 
	   `(fast-set-standard-instance-access object ,offset ,val-spec)))
    (values key-spec set-spec)))

;; Patches:

(defun dylan-new-default-initialize (instance initargs)
  (declare (optimize (safety 0)) (dynamic-extent initargs))
  (apply #'initialize-instance instance initargs)
  (when (typep (class-of instance) 'dylan-class)
    (let ((slotds (class-instance-initialized-slots (class-of instance)))
          ;; (called-shared-initialize nil)
          )
      (loop for slotd in slotds
            do (multiple-value-bind (init-key-given key-value)
		   (init-keyword-supplied-p 
                      (first (slot-definition-initargs slotd))
                      initargs)
	         (when (required-init-arg-p slotd)
		   (unless init-key-given
		     (error "The required init keyword ~s for instances of ~s was not supplied" 
			    (first (slot-definition-initargs slotd))
                            (class-of instance))))
	         (if init-key-given
                   ;; (setf (slot-value instance (slot-definition-name slotd))
	           ;;    key-value)
                   (fast-set-standard-instance-access
                      instance (slot-definition-location slotd) key-value)
		   (case (slot-definition-init-type slotd)
		     (:one-shot 
	              (setf (slot-value instance (slot-definition-name slotd))
			    (slot-definition-init-value slotd)))
		     (:per-allocation
		      (setf (slot-value instance (slot-definition-name slotd))
			    (funcall (slot-definition-init-value slotd))))))))))
  instance)

(defgeneric dylan-new-make (class &rest initargs)
  (declare (dynamic-extent initargs)))

(defmethod dylan-new-make ((class standard-class) &rest initargs)
  (declare (optimize (safety 0)) (dynamic-extent initargs))
  (let ((new (allocate-instance class))
        (defaulted-initargs 
          (if (class-default-initargs class)
            (default-initargs class initargs)
            initargs)))
    (apply (class-default-initializer class) new defaulted-initargs)
    (apply #'dylan-initialize new defaulted-initargs)
    new))

;; Install

(defmethod dylan-make ((class standard-class) &rest initargs)
  (declare (optimize (safety 0)) (dynamic-extent initargs))
  (let ((new (allocate-instance class))
        (defaulted-initargs 
          (if (class-default-initargs class)
            (default-initargs class initargs)
            initargs)))
    (apply (class-default-initializer class) new defaulted-initargs)
    (apply #'dylan-initialize new defaulted-initargs)
    new))

(defun init-keyword-supplied-p (search-key initargs)
  (declare (optimize (safety 0)))
  (loop for (key val) on initargs by 'cddr
        when (eq search-key key) do (return (values t val))
        finally (return (values nil val))))

            ;; when (and (not (typep slotd 'dylan-eslotd)) 
            ;;           (not called-shared-initialize))
            ;; do (progn
            ;;      (setq called-shared-initialize t)
            ;;      (apply #'initialize-instance instance initargs))

;; eof
