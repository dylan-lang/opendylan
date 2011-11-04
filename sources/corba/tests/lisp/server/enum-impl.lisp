;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass enumtest-impl (omg.org/root:enumtest-servant)
  ((next-in-parameter :initform :mercury)
   (next-result :initform :mercury)))

(defun succ (x)
  (second (member x 
                  '(:Mercury  :Venus  :Earth  :Mars  :Jupiter  :Saturn  :Uranus  :Neptune  :Pluto))))

(corba:define-method op:reset_in_parameter ((self enumtest-impl))
  (with-slots (next-in-parameter) self
    (setf next-in-parameter :mercury)))

(corba:define-method op:in_parameter ((self enumtest-impl) symbol)
  (with-slots (next-in-parameter) self
    (let ((ok (eq symbol next-in-parameter)))
      (setf next-in-parameter (succ next-in-parameter))
      ok)))

(corba:define-method op:reset_result ((self enumtest-impl))
  (with-slots (next-result) self
    (setf next-result :mercury)))

(corba:define-method op:result ((self enumtest-impl))
  (with-slots (next-result) self
    (let ((symbol next-result))
      (setf next-result (succ next-result))
      symbol)))

(defun enum-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references ORB "RootPOA"))
         (enumtest (make-instance 'enumtest-impl))
         (objectId (op:activate_object rootPOA enumtest))
         (enumtestref (op:servant_to_reference rootPOA enumtest)))
    (declare (ignore objectId))
    (with-open-file (out (temp-file "enumtest.ior")
                         :direction :output :if-exists :supersede)
      (princ (op:object_to_string orb enumtestref) out))
    (op:activate (op:the_poamanager rootPOA))))
