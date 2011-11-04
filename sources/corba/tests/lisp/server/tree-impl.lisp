;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass treetest-impl (omg.org/root:TreeTest-servant) ())

(corba:define-method op:depth ((self treetest-impl) tree) (declare (ignore self)) 0)

(corba:define-method op:identity ((self treetest-impl) tree) (declare (ignore self)) tree)

(corba:define-method op:identityB ((self treetest-impl) tree) (declare (ignore self)) tree)

(corba:define-method op:identityU ((self treetest-impl) tree) (declare (ignore self)) tree)

(corba:define-method op:extract_tree ((self treetest-impl) wrapper)
  (declare (ignore self))
  (op:real_tree wrapper))

(defun tree-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (treetest (make-instance 'treetest-impl))
         (objectId (op:activate_object rootPOA treetest)))
    (op:activate (op:the_poamanager rootPOA))
    (with-open-file (out (temp-file "treetest.ior")
                         :direction :output :if-exists :supersede)
      (princ (op:object_to_string orb (op:id_to_reference rootPOA objectId)) out))))
