(defparameter *each-subclass-unbound-value* (list 'unbound))

(defclass each-subclass-class-mixin ()
  ((each-subclass-slots :accessor class-each-subclass-slots)))

(defclass each-subclass-eslotd-mixin ()
  ((each-subclass-position :accessor eslotd-each-subclass-position)))

(defmethod finalize-inheritance ((c each-subclass-class-mixin))
  (call-next-method)
  (let ((count 0))
    (dolist (sd (class-slots c))
      (when (eq (slot-definition-allocation sd) :each-subclass)
        (setf (eslotd-each-subclass-position sd) count)
        (incf count)))
    (setf (class-each-subclass-slots c) 
          (make-array count :initial-element *each-subclass-unbound-value*)))
  c)

(defun each-subclass-slot-value (c sd)
  (aref (class-each-subclass-slots c) (eslotd-each-subclass-position sd)))

(defun set-each-subclass-slot-value (c sd val)
  (setf (aref (class-each-subclass-slots c) (eslotd-each-subclass-position sd)) val))

(defmethod slot-value-using-class ((c each-subclass-class-mixin) 
                                   i 
                                   (sd each-subclass-eslotd-mixin))
  (if (eq (slot-definition-allocation sd) :each-subclass)
    (let ((value (each-subclass-slot-value c sd)))
      (when (eq value *each-subclass-unbound-value*)
        (slot-unbound c i (slot-definition-name sd)))
      value)
    (call-next-method)))

(defmethod (setf slot-value-using-class) (val
                                          (c each-subclass-class-mixin) 
                                          i 
                                          (sd each-subclass-eslotd-mixin))
  (if (eq (slot-definition-allocation sd) :each-subclass)
    (set-each-subclass-slot-value c sd val)
    (call-next-method)))




  

  
