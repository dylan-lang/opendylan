;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/pseudo-objects-impl.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass testobjectX-impl (omg.org/root:testObjectX-servant) ())

(defclass testobjectA-impl (omg.org/root:testObjectA-servant) ())

(defclass testobjectB-impl (omg.org/root:testObjectB-servant testobjectA-impl) ())

(defclass testobjectC-impl (omg.org/root:testObjectC-servant testobjectB-impl testobjectX-impl) ())

;; This doesn't mixin the impl classes like the dylan example...

(defclass testobjectD-impl (omg.org/root:testObjectD-servant) ())

(corba:define-method op:destroy ((self testobjectA-impl))
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (objectId (op:servant_to_id rootPOA self)))
    (op:deactivate_object rootPOA objectId)))

(defmethod initialize-instance :after ((self testobjectA-impl) &key poa)
  (let* ((orb (op:orb_init :harlequin-orb))
         (reference (op:servant_to_reference poa self)))
    (with-slots (op:ior) self
      (setf op:ior (op:object_to_string orb reference)))))

(defclass pseudo-objects-test-impl (omg.org/root:PseudoObjectsTest-servant)
  ((poa :initarg :poa)))

(corba:define-method op:TestObjectX_factory ((self pseudo-objects-test-impl) id)
  (declare (ignore id))
  (with-slots (poa) self
    (let ((result (make-instance 'testobjectX-impl)))
      (op:narrow 'omg.org/root:testobjectX 
                 (op:servant_to_reference poa result)))))

(corba:define-method op:TestObjectA_factory ((self pseudo-objects-test-impl) id)
  (with-slots (poa) self
    (let ((result (make-instance 'testobjectA-impl :poa poa :id id)))
      (op:narrow 'omg.org/root:testobjectA
                 (op:servant_to_reference poa result)))))

(corba:define-method op:TestObjectB_factory ((self pseudo-objects-test-impl) id)
  (with-slots (poa) self
    (let ((result (make-instance 'testobjectB-impl :poa poa :id id)))
      (op:narrow 'omg.org/root:testobjectB
                 (op:servant_to_reference poa result)))))

(corba:define-method op:TestObjectC_factory ((self pseudo-objects-test-impl) id)
  (with-slots (poa) self
    (let ((result (make-instance 'testobjectC-impl :poa poa :id id)))
      (op:narrow 'omg.org/root:testobjectC
                 (op:servant_to_reference poa result)))))

(corba:define-method op:TestObjectD_factory ((self pseudo-objects-test-impl) id)
  (with-slots (poa) self
    (let ((result (make-instance 'testobjectD-impl :id id)))
      (op:narrow 'omg.org/root:testobjectD
                 (op:servant_to_reference poa result)))))

(corba:define-method op:testObjectX_nil_factory ((self pseudo-objects-test-impl))
  (declare (ignore self))
  nil)

(corba:define-method op:identity ((self pseudo-objects-test-impl) x)
  (declare (ignore self))
  x)

(corba:define-method op:check_object_attribute ((self pseudo-objects-test-impl) ior)
  (with-slots (op:object_attribute) self
    (let ((orb (op:orb_init :harlequin-orb)))
      (unless (string= ior (op:object_to_string orb op:object_attribute))
        (error 'omg.org/root:PseudoObjectsTest/failure)))))

(corba:define-method op:object_operation ((self pseudo-objects-test-impl) one two)
  (declare (ignore self))
  (values one one two))

(corba:define-method op:check_typecode_attribute ((self pseudo-objects-test-impl))
  (with-slots (op:typecode_attribute) self
    (unless (op:equal op:typecode_attribute omg.org/root:_tc_PseudoObjectsTest/failure)
      (error 'omg.org/root:PseudoObjectsTest/failure))))

(corba:define-method op:typecode_operation ((self pseudo-objects-test-impl) one two)
  (declare (ignore self))
  (values one one two))

(defun pseudo-objects-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (pseudoobjectsTest (make-instance 'pseudo-objects-test-impl :poa rootpoa))
         (pseudoobjectsTestref (op:servant_to_reference rootPOA pseudoobjectsTest)))
    (with-open-file (out (temp-file "PseudoObjectsTest.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb pseudoobjectsTestref) out))
    (op:activate (op:the_poamanager rootPOA))))

