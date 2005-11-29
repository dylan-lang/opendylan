;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/chat-impl.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass chat-impl (omg.org/root:chat-servant)
  ((clients :initform nil)))

(corba:define-method op:sendmessage ((self chat-impl) message)
  (with-slots (clients) self
    (loop for client in clients
          do (op:newmessage client message))))

(corba:define-method op:registerclient ((self chat-impl) client name)
  (declare (ignore name))
  (with-slots (clients) self
    (push client clients)))

(corba:define-method op:removeclient ((self chat-impl) client name)
  (declare (ignore name))
  (with-slots (clients) self
    (removef clients client :test 'op:is_equivalent)))

(defun chat-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))
    (op:activate (op:the_poamanager rootPOA))
    (let* ((sub-poa (op:create_poa rootPOA "Chat POA"
                                   nil 
                                   :lifespan_policy :transient))
           (chat-server (op:activate_object
                         sub-poa
                         (make-instance 'chat-impl))))
      (with-open-file (out (temp-file "chat.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb (op:id_to_reference sub-poa chat-server)) out)))))
