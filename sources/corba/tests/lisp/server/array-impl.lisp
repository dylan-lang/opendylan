;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass arrayTest-implementation (omg.org/root:arrayTest-servant) ())

(defparameter *messages*
  #("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(corba:define-method op:check_string_array_attribute ((self arrayTest-implementation))
  (with-slots (op:string_array_attribute) self
    (dotimes (i (length *messages*))
      (unless (string= (elt op:string_array_attribute i) (elt *messages* i))
        (error 'omg.org/root:arraytest/failure)))))

(corba:define-method op:string_array_operation ((self arrayTest-implementation) one two)
  (declare (ignore self))
  (values one two one))

(corba:define-method op:check_short_array_attribute ((self arrayTest-implementation))
  (with-slots (op:short_array_attribute) self
    (let ((counter 0))
      (dotimes (i 4)
        (dotimes (j 4)
          (unless (= (aref op:short_array_attribute i j) counter)
            (error 'omg.org/root:arraytest/failure))
          (incf counter))))))

(corba:define-method op:short_array_operation ((self arrayTest-implementation) one two)
  (declare (ignore self))
  (values one two one))

(corba:define-method op:check_float_array_attribute ((self arrayTest-implementation))
  (with-slots (op:float_array_attribute) self
    (let ((number 0.0))
      (dotimes (i 3)
        (dotimes (j 3)
          (dotimes (k 3)
            (let ((test (aref op:float_array_attribute i j k)))
              (unless (<= (- number 0.0001) test (+ number 0.0001))
                (error 'omg.org/root:arraytest/failure)))
            (incf number 0.7)))))))

(corba:define-method op:float_array_operation ((self arrayTest-implementation) one two)
  (declare (ignore self))
  (values one two one))

(defun array-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA"))
         (arraytest (make-instance 'arrayTest-implementation))
         (arraytestref (op:servant_to_reference rootPOA arraytest)))
    (with-open-file (out (temp-file "ArrayTest.ior")
                         :direction :output :if-exists :supersede)
      (princ (op:object_to_string orb arraytestref) out))
    (op:activate (op:the_poamanager rootPOA))))
