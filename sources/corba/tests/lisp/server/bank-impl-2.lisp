;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defvar *account-poa* nil)

(defclass demo-account2 (omg.org/root:account-servant)
  ((name :initarg :name))
  (:default-initargs :balance 0.0))

(defmethod initialize-instance :after ((self demo-account2) &key bank)
  (with-slots (name) self
    (with-slots (accounts) bank
      (when (gethash name accounts)
        (error 'omg.org/root:bank/reject :reason "cannot create new account with same name"))
      (setf (gethash name accounts) self))))

(corba:define-method op:makelodgement ((self demo-account2) lodgement)
  (with-slots (op:balance) self
    (incf op:balance lodgement)))

(corba:define-method op:makewithdrawal ((self demo-account2) withdrawl)
  (with-slots (name op:balance) self
    (when (< op:balance withdrawl)
      (error "Warning: Account \"~A\" overdrawn." name))
    (setf op:balance (- op:balance withdrawl))))

(defclass account-activator (portableserver:servantactivator)
  ((account-activator-bank :initarg :bank)))

(corba:define-method op:incarnate ((self account-activator) objectID adapter)
  (declare (ignore adapter))
  (with-slots (account-activator-bank) self
    (with-slots (accounts) account-activator-bank
      (gethash objectID accounts))))

(corba:define-method op:etherealize ((self account-activator) objectId adapter 
                                     servant cleanup-in-progress reminaing-activations)
  (declare (ignore self objectId adapter servant 
                   cleanup-in-progress reminaing-activations)))

(defclass demo-currentaccount2 (omg.org/root:currentaccount-servant demo-account2) ()
  (:default-initargs :overdraftlimit 0.0))

(corba:define-method op:makewithdrawal ((self demo-currentaccount2) withdrawl)
  (with-slots (name op:balance op:overdraftlimit) self
    (when (< (+ op:balance op:overdraftlimit) withdrawl)
      (error "ALERT: Account \"~A\" over agreed overdraft limit." name))
    (call-next-method)))

(defclass demo-bank2 (omg.org/root:bank-servant)
  ((accounts :initform (make-hash-table :test 'equalp))))

(corba:define-method op:newaccount ((self demo-bank2) name)
  (make-instance 'demo-account2 
                 :name name 
                 :bank self)
  (op:create_reference_with_id *account-poa*
                               name
                               "IDL:account:1.0"))

(corba:define-method op:newcurrentaccount ((self demo-bank2) name limit)
  (make-instance 'demo-currentaccount2 
                 :name name :bank self :overdraftlimit limit)
  (op:create_reference_with_id *account-poa*
                               name
                               "IDL:currentAccount:1.0"))

(corba:define-method op:deleteAccount ((self demo-bank2) ref)
  (let ((account (op:reference_to_servant *account-poa* ref)))
    (with-slots (name) account
      (with-slots (accounts) self
        (remhash name accounts)))
    (op:deactivate_object *account-poa* (op:reference_to_id *account-poa* ref))))

(defun bank-server-mainline2 ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))

    (unless *account-poa*
      (setq *account-poa*
            (op:create_poa rootPOA "Account POA"
                           nil 
                           :lifespan_policy :transient
                           :id_assignment_policy :user_id
                           :request_processing_policy :use_servant_manager)))

    (let* ((bank (make-instance 'demo-bank2))
           (activator (make-instance 'account-activator :bank bank)))

      (op:set_servant_manager *account-poa* activator)

      (op:activate_object rootPOA bank)
      
      (with-open-file (out (temp-file "bank-2.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb (op:servant_to_reference rootPOA bank)) out))
      
      (op:activate (op:the_poamanager *account-poa*))
      (op:activate (op:the_poamanager rootPOA)))))

