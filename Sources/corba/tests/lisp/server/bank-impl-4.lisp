;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/bank-impl-4.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defclass property-object () 
  ((object-properties :initform nil)))

(defmethod initialize-instance :after ((self property-object) &rest args)
  (with-slots (object-properties) self
    (setf object-properties args)))

(corba:define-method op:name ((self property-object))
  (with-slots (object-properties) self
    (getf object-properties :name)))

(corba:define-method op:balance ((self property-object))
  (with-slots (object-properties) self
    (getf object-properties :balance 0.0)))

(corba:define-method (setf op:balance) (value (self property-object))
  (with-slots (object-properties) self
    (setf (getf object-properties :balance) value)))

(defmethod initialize-account ((self property-object))
  (with-slots (object-properties) self
    (let* ((bank (getf object-properties :bank))
           (name (op:name self))
           (id (intern name :keyword)))
      (let ((existing-account (gethash id (accounts bank))))
        (when existing-account
          (error 'omg.org/root:bank/reject :reason "cannot create new account with same name"))
        (setf (gethash id (accounts bank)) self)
        (op:activate_object *bank-poa* 
                            (make-instance 'omg.org/root:account-tie :object self))))))

(corba:define-method op:makeLodgement ((self property-object) lodgement)
  (incf (op:balance self) lodgement))

(corba:define-method op:makeWithdrawal ((self property-object) withdrawl)
  (when (and (currentaccount? self)
             (< (+ (op:balance self) (op:overdraftlimit self))
                withdrawl))
    (error "ALERT: Account \"~A\" over agreed overdraft limit." (op:name self)))
  (when (< (op:balance self) withdrawl)
    (error "ALERT: Account \"~A\" overdrawn" (op:name self)))
  (decf (op:balance self) withdrawl))

(corba:define-method op:overdraftLimit ((self property-object))
  (with-slots (object-properties) self
    (getf object-properties :limit 0.0)))

(defmethod currentaccount? ((self property-object))
  (with-slots (object-properties) self
    (getf object-properties :limit)))

(defmethod accounts ((self property-object))
  (with-slots (object-properties) self
    (or (getf object-properties :accounts)
        (setf (getf object-properties :accounts)
              (make-hash-table)))))

(corba:define-method op:newAccount ((self property-object) name)
  (let ((account (make-instance 'property-object :bank self :name name)))
    (initialize-account account)
    (op:narrow 'omg.org/root:account
               (op:servant_to_reference *bank-poa* 
                                        (make-instance 'omg.org/root:account-tie :object account)))))

(corba:define-method op:newCurrentAccount ((self property-object) name limit)
  (let ((account (make-instance 'property-object :bank self :name name :limit limit)))
    (initialize-account account)
    (op:narrow 'omg.org/root:CurrentAccount
               (op:servant_to_reference *bank-poa* 
                                        (make-instance 'omg.org/root:CurrentAccount-tie :object account)))))

(corba:define-method op:deleteAccount ((self property-object) reference)
  (let* ((servant (op:reference_to_servant *bank-poa* reference))
         (account (corba:tied-object servant))
         (id (intern (op:name account) :keyword)))
    (remhash id (accounts self))
    (op:deactivate_object *bank-poa* 
                          (op:reference_to_id *bank-poa* reference))))


(defun bank-server-mainline4 ()
  (let* ((orb (op:orb_init :harlequin-orb))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))
    
    (setq *bank-poa*
          (op:create_poa rootPOA "Account POA 3" nil
                         :lifespan_policy :transient))

    (let* ((bank (make-instance 'omg.org/root:bank-tie :object (make-instance 'property-object))))

      (op:activate_object *bank-poa* bank)
      
      (with-open-file (out (temp-file "bank-4.ior")
                           :direction :output :if-exists :supersede)
        (princ (op:object_to_string orb (op:servant_to_reference *bank-poa* bank)) out))
      
      (op:activate (op:the_poamanager *bank-poa*)))))
