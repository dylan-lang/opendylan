;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/bank-impl-3.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defvar *account-poa2* nil)

(defclass demo-account3 (omg.org/root:account-servant)
  ((name :initarg :name))
  (:default-initargs :balance 0.0))

(defmethod initialize-instance :after ((self demo-account3) &key bank)
  (with-slots (name) self
    (with-slots (accounts) bank
      (when (gethash name accounts)
        (error 'omg.org/root:bank/reject :reason "cannot create new account with same name"))
      (setf (gethash name accounts) self))))

(corba:define-method op:makelodgement ((self demo-account3) lodgement)
  (with-slots (op:balance) self
    (incf op:balance lodgement)))

(corba:define-method op:makewithdrawal ((self demo-account3) withdrawl)
  (with-slots (name op:balance) self
    (when (< op:balance withdrawl)
      (error "Warning: Account \"~A\" overdrawn." name))
    (setf op:balance (- op:balance withdrawl))))

(defclass account-locator (portableserver:servantlocator)
  ((account-locator-bank :initarg :bank)))

(corba:define-method op:preinvoke ((self account-locator) objectID adapter operation)
  (declare (ignore adapter operation))
  (with-slots (account-locator-bank) self
    (with-slots (accounts) account-locator-bank
      (gethash objectID accounts))))

(corba:define-method op:postinvoke ((self account-locator) objectId adapter 
                                    operation cookie servant)
  (declare (ignore self objectId adapter servant 
                   operation cookie)))

(defclass demo-currentaccount3 (omg.org/root:currentaccount-servant demo-account3) ())

(corba:define-method op:makewithdrawal ((self demo-currentaccount3) withdrawl)
  (with-slots (name op:balance op:overdraftlimit) self
    (when (< (+ op:balance op:overdraftlimit) withdrawl)
      (error "ALERT: Account \"~A\" over agreed overdraft limit." name))
    (call-next-method)))

(defclass demo-bank3 (omg.org/root:bank-servant)
  ((accounts :initform (make-hash-table :test 'equalp)))
  (:default-initargs :overdraftlimit 0.0))

(corba:define-method op:newaccount ((self demo-bank3) name)
  (make-instance 'demo-account3 
                 :name name 
                 :bank self)
  (op:create_reference_with_id *account-poa2*
                               name
                               "IDL:account:1.0"))

(corba:define-method op:newcurrentaccount ((self demo-bank3) name limit)
  (make-instance 'demo-currentaccount3 
                 :name name :bank self :overdraftlimit limit)
  (op:create_reference_with_id *account-poa2*
                               name
                               "IDL:currentAccount:1.0"))

(corba:define-method op:deleteAccount ((self demo-bank3) ref)
  (declare (ignore self))
  (let ((objectID (op:reference_to_id *account-poa2* ref)))
    (with-slots (account-locator-bank) 
        (op:get_servant_manager *account-poa2*)
      (with-slots (accounts) account-locator-bank
        (gethash objectID accounts)))))

(defun bank-server-mainline3 ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))

    (unless *account-poa2*
      (setq *account-poa2*
            (op:create_poa rootPOA "Account POA 2"
                           nil 
                           :lifespan_policy :transient
                           :id_assignment_policy :user_id
                           :servant_retention_policy :non_retain
                           :request_processing_policy :use_servant_manager)))

    (let* ((bank (make-instance 'demo-bank3))
           (locator (make-instance 'account-locator :bank bank)))
      
      (op:set_servant_manager *account-poa2* locator)
      
      (op:activate_object rootPOA bank)
      
      (with-open-file (out (temp-file "bank-3.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb (op:servant_to_reference rootPOA bank)) out))

      (op:activate (op:the_poamanager *account-poa2*))
      (op:activate (op:the_poamanager rootPOA)))))

