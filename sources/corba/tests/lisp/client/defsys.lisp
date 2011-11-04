;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(require "corba-orb")

(defsystem all-lisp-client-tests ()
    :members
  (
   "version"
   ("../../idl/constant.idl" :type :idl-file)
   "constant-client"
   )
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires (:load :previous)))))
