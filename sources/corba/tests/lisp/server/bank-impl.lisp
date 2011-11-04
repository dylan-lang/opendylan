;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defvar *bank-poa* nil)

(defclass demo-account (omg.org/root:account-servant)
  ((name :initarg :name))
  (:default-initargs :balance 0.0))

(defmethod initialize-instance :after ((self demo-account) &key bank)
  (with-slots (name) self
    (with-slots (accounts) bank
      (when (gethash name accounts)
        (error 'omg.org/root:bank/reject :reason "cannot create new account with same name"))
      (setf (gethash name accounts) self)
      (op:activate_object *bank-poa* self))))

(corba:define-method op:makelodgement ((self demo-account) lodgement)
  (with-slots (op:balance) self
    (incf op:balance lodgement)))

(corba:define-method op:makewithdrawal ((self demo-account) withdrawl)
  (with-slots (name op:balance) self
    (when (< op:balance withdrawl)
      (error  "Warning: Account \"~A\" overdrawn." name))
    (setf op:balance (- op:balance withdrawl))))

(defclass current-account (omg.org/root:currentaccount-servant demo-account) ()
  (:default-initargs :overdraftlimit 0.0))

(corba:define-method op:makewithdrawal ((self current-account) withdrawl)
  (with-slots (name op:balance op:overdraftlimit) self
    (when (< (+ op:balance op:overdraftlimit) withdrawl)
      (error "ALERT: Account \"~A\" over agreed overdraft limit." name))
    (call-next-method)))

(defclass demo-bank (omg.org/root:bank-servant)
  ((accounts :initform (make-hash-table :test 'equalp))))

(corba:define-method op:newaccount ((self demo-bank) name)
  (op:servant_to_reference 
   *bank-poa*
   (make-instance 'demo-account :bank self :name name)))

(corba:define-method op:newcurrentaccount ((self demo-bank) name limit)
  (op:servant_to_reference 
   *bank-poa*
   (make-instance 'current-account :bank self :name name :overdraftlimit limit)))

(corba:define-method op:deleteaccount ((self demo-bank) ref)
  (let ((account (op:reference_to_servant *bank-poa* ref)))
    (with-slots (name) account
      (with-slots (accounts) self
        (remhash name accounts)))))

(defun bank-server-mainline ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))

    (unless *bank-poa*
      (setq *bank-poa* (op:create_poa rootPOA "Bank POA"
                                      nil 
                                      :lifespan_policy :transient)))

    (let* ((bank (make-instance 'demo-bank)))

      (op:activate_object *bank-poa* bank)

      (with-open-file (out (temp-file "bank.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb (op:servant_to_reference *bank-poa* bank)) out))

      (op:activate (op:the_poamanager *bank-poa*)))))
