;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

;; Check out the grid example with explicitly activated POA

(defclass a-grid-implementation (omg.org/root:grid-servant)
  ((the-grid :initform (make-array '(10 10) 
                                   :initial-element 4
                                   #+Liquid :adjustable #+Liquid t))
   (forward :initarg :forward :initform nil))
  (:default-initargs :height 10 :width 10))

(corba:define-method (setf op:height) :after (new-height (self a-grid-implementation))
  (with-slots (the-grid) self
    (let ((dims (array-dimensions the-grid)))
      (setf the-grid
            (adjust-array the-grid (list (first dims) new-height))))))

(corba:define-method (setf op:width) :after (new-width (self a-grid-implementation))
  (with-slots (the-grid) self
    (let ((dims (array-dimensions the-grid)))
      (setf the-grid
            (adjust-array the-grid (list new-width (second dims)))))))

(corba:define-method op:set ((self a-grid-implementation) x y val)
  (with-slots (the-grid) self
    (setf (aref the-grid x y) val)))

(corba:define-method op:get ((self a-grid-implementation) x y)
  (with-slots (the-grid) self
    (aref the-grid x y)))

(corba:define-method op:invoke ((self a-grid-implementation) server-request)
      (if (eq (op:operation server-request) :|context_get_width|)
	  (progn
	    (setf (op:ctx server-request) t)
	    (op:arguments server-request nil)
	    (let ((result
		   (+ (let ((lookup (op:get_values (op:ctx server-request) nil nil '("Extra"))))
			(if (and lookup
				 (string= (corba:any-value
					   (op:argument (op:item lookup 0)))
					  "Yes"))
			    1
			  0))
		      (op:width self))))
	      (op:set_result server-request
			     (corba:any :any-typecode corba:_tc_short
					:any-value result))))
	(call-next-method)))


(defclass ServantActivator (portableserver:servantactivator) 
  ((forward-me-to :initarg :forward)))

(corba:define-method op:incarnate ((self ServantActivator)
                                   objectId
                                   adapter)
  (declare (ignore adapter))
  (with-slots (forward-me-to) self
    (if (string= objectId "wrong grid") 
        (error 'portableserver:poa/forwardrequest :forward_reference forward-me-to)
      (error 'corba:object_not_exist :minor 0 :completed :completed_no))))

(defun grid-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))
    (op:activate (op:the_poamanager rootPOA))
    (let* ((sub-poa (op:create_poa rootPOA "*transient-with-manager-sub-poa*"
                                   nil 
                                   :id_assignment_policy :user_id
                                   :lifespan_policy :persistent
                                   :request_processing_policy :use_servant_manager))
           (grid (op:narrow 'omg.org/root:grid
                            (op:create_reference_with_id sub-poa
                                                         "grid"
                                                         (op:id omg.org/root:_tc_grid))))
           (wrong-grid (op:narrow 'omg.org/root:grid
                                  (op:create_reference_with_id sub-poa
                                                               "wrong grid"
                                                               (op:id omg.org/root:_tc_grid)))))
      (op:activate_object_with_id sub-poa "grid" 
                                  (make-instance 'a-grid-implementation))
      (op:set_servant_manager sub-poa 
                              (make-instance 'ServantActivator :forward grid))
      (with-open-file (out (temp-file "grid.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb grid) out))
      (with-open-file (out (temp-file "wrong-grid.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb wrong-grid) out)))))



  
