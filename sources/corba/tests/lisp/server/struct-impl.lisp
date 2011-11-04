;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defvar *struct-a*
  (omg.org/root:structurea :a_long -40000
                           :a_ulong 40000
                           :a_octet 128))

(defvar *struct-b*
  (omg.org/root:structureb :b_boolean nil))

(defvar *struct-c* 
  (omg.org/root:structurec :c_string "Tinky Winky"
                           :c_struct *struct-b*
                           :c_char #\A))

(defvar *struct-d*
  (omg.org/root:structured :d_float 0.0
                           :d_double 1.0d0))

(defclass structtest-impl (omg.org/root:structtest-servant) ()
  (:default-initargs
   :struct_a *struct-a* :struct_b *struct-b*
   :struct_c *struct-c* :struct_d *struct-d*))

(corba:define-method op:get_a_long ((self structtest-impl))
  (with-slots (op:struct_a) self
    (op:a_long op:struct_a)))

(corba:define-method op:get_a_long ((self structtest-impl))
  (with-slots (op:struct_a) self
    (op:a_long op:struct_a)))

(corba:define-method op:get_a_ulong ((self structtest-impl))
  (with-slots (op:struct_a) self
    (op:a_ulong op:struct_a)))

(corba:define-method op:get_a_octet ((self structtest-impl))
  (with-slots (op:struct_a) self
    (op:a_octet op:struct_a)))

(corba:define-method op:get_b_boolean ((self structtest-impl))
  (with-slots (op:struct_b) self
    (op:b_boolean op:struct_b)))

(corba:define-method op:get_c_string ((self structtest-impl))
  (with-slots (op:struct_c) self
    (op:c_string op:struct_c)))

(corba:define-method op:get_c_struct ((self structtest-impl))
  (with-slots (op:struct_c) self
    (op:c_struct op:struct_c)))

(corba:define-method op:get_c_char ((self structtest-impl))
  (with-slots (op:struct_c) self
    (op:c_char op:struct_c)))

(corba:define-method op:get_d_float ((self structtest-impl))
  (with-slots (op:struct_d) self
    (op:d_float op:struct_d)))

(corba:define-method op:get_d_double ((self structtest-impl))
  (with-slots (op:struct_d) self
    (op:d_double op:struct_d)))

(corba:define-method op:in_parameter_a ((self structtest-impl) struct)
  (with-slots (op:struct_a) self
    (equalp struct op:struct_a)))

(corba:define-method op:in_parameter_b ((self structtest-impl) struct)
  (with-slots (op:struct_b) self
    (equalp struct op:struct_b)))

(corba:define-method op:in_parameter_c ((self structtest-impl) struct)
  (with-slots (op:struct_c) self
    (equalp struct op:struct_c)))

(corba:define-method op:in_parameter_d ((self structtest-impl) struct)
  (with-slots (op:struct_d) self
    (equalp struct op:struct_d)))

(corba:define-method op:result_a ((self structtest-impl))
  (with-slots (op:struct_a) self
    op:struct_a))

(corba:define-method op:result_b ((self structtest-impl))
  (with-slots (op:struct_b) self
    op:struct_b))

(corba:define-method op:result_c ((self structtest-impl))
  (with-slots (op:struct_c) self
    op:struct_c))

(corba:define-method op:result_d ((self structtest-impl))
  (with-slots (op:struct_d) self
    op:struct_d))

(defun struct-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (structtest (make-instance 'structtest-impl))
         (objectId (op:activate_object rootPOA structtest)))
    (op:activate (op:the_poamanager rootPOA))
    (with-open-file (out (temp-file "structtest.ior")
                         :direction :output :if-exists :supersede)
      (princ (op:object_to_string orb (op:id_to_reference rootPOA objectId)) out))))
