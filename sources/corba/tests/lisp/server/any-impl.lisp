;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/any-impl.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defvar tree 
  (omg.org/root:tree 
   :label "1"
   :children (list (omg.org/root:tree 
                    :label "2"
                    :children (list 
                               (omg.org/root:tree :label "3")
                               (omg.org/root:tree :label "4")))
                   (omg.org/root:tree 
                    :label "5"
                    :children (list 
                               (omg.org/root:tree :label "6")
                               (omg.org/root:tree :label "7"))))))

(defclass any-impl (omg.org/root:AnyTest-servant) ())

(corba:define-method op:check_any_attribute ((self any-impl))
  (with-slots (op:any_attribute) self
    (unless (and (op:equal corba:_tc_short (corba:any-typecode op:any_attribute))
                 (equal 0 (corba:any-value op:any_attribute)))
      (error 'omg.org/root:anytest/failure))))

(corba:define-method op:check_any_tree_attribute ((self any-impl))
  (with-slots (op:any_tree_attribute) self
    (labels ((check-equal (a b)
                          (and (equal (op:label a) (op:label b))
                               (every 'check-equal
                                      (op:children a)
                                      (op:children b)))))
      (unless (check-equal op:any_tree_attribute tree)
        (error 'omg.org/root:anytest/failure)))))

(corba:define-method op:any_operation ((self any-impl) one two)
  (declare (ignore self))
  (values one two one))

(defun any-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (objectId (op:activate_object
                    rootPOA
                    (make-instance 'any-impl))))
    (op:activate (op:the_poamanager rootPOA))
    (with-open-file (out (temp-file "anytest.ior")
                         :direction :output :if-exists :supersede)
      (princ (op:object_to_string orb (op:id_to_reference rootPOA objectId)) out))))
