;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/union-impl.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass union-impl (omg.org/root:UnionTest-servant) ())

(corba:define-method op:check_rle_entity_1_attribute ((self union-impl))
  (let ((expected (omg.org/root:RLE_entity_1/length 10)))
    (unless (equalp (op:rle_entity_1_attribute self)
                    expected)
      (error 'omg.org/root:UnionTest/Failure))))

(corba:define-method op:check_rle_entity_2_attribute ((self union-impl))
  (let ((expected (omg.org/root:RLE_entity_2/character #\h)))
    (unless (equalp (op:character (op:rle_entity_2_attribute self))
                    (op:character expected))
      (error 'omg.org/root:UnionTest/Failure))))


(corba:define-method op:check_rle_entity_3_attribute ((self union-impl))
  (let ((expected (omg.org/root:RLE_entity_3/length 20)))
    (unless (equalp (op:rle_entity_3_attribute self)
                    expected)
      (error 'omg.org/root:UnionTest/Failure))))

(corba:define-method op:check_rle_entity_4_attribute ((self union-impl))
  (let ((expected (omg.org/root:RLE_entity_4/character #\i)))
    (unless (equalp (op:rle_entity_4_attribute self)
                    expected)
      (error 'omg.org/root:UnionTest/Failure))))

(corba:define-method op:check_rle_entity_5_attribute ((self union-impl))
  (let ((expected (omg.org/root:RLE_entity_5/length 30)))
    (unless (equalp (op:rle_entity_5_attribute self)
                    expected)
      (error 'omg.org/root:UnionTest/Failure))))

(corba:define-method op:rle_entity_1_operation ((self union-impl) one two)
  (declare (ignore self))
  (values one two one))

(corba:define-method op:rle_entity_2_operation ((self union-impl) one two)
  (declare (ignore self))
  (values one two one))

(corba:define-method op:rle_entity_3_operation ((self union-impl) one two)
  (declare (ignore self))
  (values one two one))

(corba:define-method op:rle_entity_4_operation ((self union-impl) one two)
  (declare (ignore self))
  (values one two one))

(corba:define-method op:rle_entity_5_operation ((self union-impl) one two)
  (declare (ignore self))
  (values one two one))

(defun union-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (uniontest (make-instance 'union-impl))
         (uniontestref (op:servant_to_reference RootPOA uniontest)))
    (op:activate (op:the_poamanager rootPOA))
    (with-open-file (out (temp-file "uniontest.ior")
                         :direction :output :if-exists :supersede)
      (princ (op:object_to_string orb uniontestref) out))))
