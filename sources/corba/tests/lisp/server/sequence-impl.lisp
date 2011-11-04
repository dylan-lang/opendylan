;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass a-sequencetest-implementation (omg.org/root:sequencetest-servant)
  ((implementation-objects :initform nil)))

(corba:define-method op:check_attribute_short_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_short_seq) self
    (let ((seq op:attribute_short_seq))
      (dotimes (i (length seq))
        (unless (= (elt seq i) (* i 11))
          (error 'omg.org/root:sequencetest/failure :index i))))))

(corba:define-method op:in_parameter_short_seq ((self a-sequencetest-implementation) inseq)
  (with-slots (op:attribute_short_seq) self
    (let ((seq op:attribute_short_seq))
      (dotimes (i (length seq))
        (unless (= (elt seq i) (elt inseq i))
          (error 'omg.org/root:sequencetest/failure :index i))))))

(corba:define-method op:inout_parameter_short_seq ((self a-sequencetest-implementation) inseq)
  (with-slots (op:attribute_short_seq) self
    (let ((seq op:attribute_short_seq))
      (dotimes (i (length seq))
        (unless (= (elt seq i) (elt inseq i))
          (error 'omg.org/root:sequencetest/failure :index i)))
      seq)))

(corba:define-method op:out_parameter_short_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_short_seq) self
    op:attribute_short_seq))

(corba:define-method op:result_short_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_short_seq) self
    op:attribute_short_seq))

(defparameter *numbers*
  #("one"  "two"  "three"  "four"  "five"  "six"  "seven"  "eight"  "nine"  "ten"))

(corba:define-method op:check_attribute_struct_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_struct_seq) self
    (let ((seq op:attribute_struct_seq))
      (dotimes (i (length seq))
        (let ((expected (omg.org/root:structure-% :name (elt *numbers* i) :info i)))
          (unless (equalp (elt seq i) expected)
            (error 'omg.org/root:sequencetest/failure :index i)))))))

(corba:define-method op:in_parameter_struct_seq ((self a-sequencetest-implementation) inseq)
  (with-slots (op:attribute_struct_seq) self
    (let ((seq op:attribute_struct_seq))
      (dotimes (i (length seq))
        (unless (equalp (elt seq i) (elt inseq i))
          (error 'omg.org/root:sequencetest/failure :index i))))))

(corba:define-method op:inout_parameter_struct_seq ((self a-sequencetest-implementation) inseq)
  (with-slots (op:attribute_struct_seq) self
    (let ((seq op:attribute_struct_seq))
      (dotimes (i (length seq))
        (unless (equalp (elt seq i) (elt inseq i))
          (error 'omg.org/root:sequencetest/failure :index i)))
      seq)))

(corba:define-method op:out_parameter_struct_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_struct_seq) self
    op:attribute_struct_seq))

(corba:define-method op:result_struct_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_struct_seq) self
    op:attribute_struct_seq))

(defclass testobject-impl (omg.org/root:testobject-servant) ())

(defun make-testobject (id)
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (testobject (make-instance 'testobject-impl :id id))
         (objectID (op:activate_object rootPOA testobject))
         (testobjectref (op:servant_to_reference rootPOA testobject)))
    (declare (ignore objectID))
    (with-slots (op:ior) testobject
      (setf op:ior (op:object_to_string orb testobjectref)))
    (values testobject (op:narrow 'omg.org/root:testobject testobjectref))))

(defmethod ior-string ((object omg.org/root:testobject))
  (let ((orb (op:orb_init :harlequin-orb)))
    (op:object_to_string orb object)))

(defmethod ior-string ((object testobject-impl))
  (with-slots (op:ior) object
    op:ior))

(defun test-= (object1 object2)
  (string= (ior-string object1) (ior-string object2)))

(corba:define-method op:testobject_factory ((self a-sequencetest-implementation) id)
  (multiple-value-bind (object reference)
      (make-testobject id)
    (with-slots (implementation-objects) self
      (push-end object implementation-objects)
      reference)))

(corba:define-method op:check_attribute_object_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_object_seq implementation-objects) self
    (let ((seq op:attribute_object_seq))
      (dotimes (i (length seq))
        (let ((expected (elt implementation-objects i)))
          (unless (test-= (elt seq i) expected)
            (error 'omg.org/root:sequencetest/failure :index i)))))))

(corba:define-method op:in_parameter_object_seq ((self a-sequencetest-implementation) inseq)
  (with-slots (op:attribute_object_seq) self
    (let ((seq op:attribute_object_seq))
      (dotimes (i (length seq))
        (unless (test-= (elt seq i) (elt inseq i))
          (error 'omg.org/root:sequencetest/failure :index i))))))

(corba:define-method op:inout_parameter_object_seq ((self a-sequencetest-implementation) inseq)
  (with-slots (op:attribute_object_seq) self
    (let ((seq op:attribute_object_seq))
      (dotimes (i (length seq))
        (unless (test-= (elt seq i) (elt inseq i))
          (error 'omg.org/root:sequencetest/failure :index i)))
      seq)))

(corba:define-method op:out_parameter_object_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_object_seq) self
    op:attribute_object_seq))

(corba:define-method op:result_object_seq ((self a-sequencetest-implementation))
  (with-slots (op:attribute_object_seq) self
    op:attribute_object_seq))

(defparameter *short-name* "Batman and Robin")
(defparameter *long-name* "abcdefghijklmnopqrstuvwxyz")

(corba:define-method op:set_short_name ((self a-sequencetest-implementation) name)
  (declare (ignore self))
  (dotimes (i (length *short-name*))
    (unless (char= (elt name i) (elt *short-name* i))
      (error 'omg.org/root:sequencetest/failure :index i))))

(corba:define-method op:set_long_name ((self a-sequencetest-implementation) name)
  (declare (ignore self name)))

(corba:define-method op:get_short_name ((self a-sequencetest-implementation))
  (declare (ignore self))
  *short-name*)

(corba:define-method op:get_long_name ((self a-sequencetest-implementation))
  (declare (ignore self))
  *long-name*)

(corba:define-method op:get_name ((self a-sequencetest-implementation))
  (declare (ignore self))
  (values (length *short-name*) *short-name*))

(corba:define-method op:reverse_name ((self a-sequencetest-implementation) name)
    (declare (ignore self))
    (reverse name))

(defun sequence-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (sequencetest (make-instance 'a-sequencetest-implementation))
         (objectId (op:activate_object rootPOA sequencetest))
         (sequencetestref (op:servant_to_reference rootPOA sequencetest)))
    (declare (ignore objectId))
    (with-open-file (out (temp-file "sequencetest.ior")
                         :direction :output :if-exists :supersede)
      (princ (op:object_to_string orb sequencetestref) out))
    (op:activate (op:the_poamanager rootPOA))))
