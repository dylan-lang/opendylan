;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/client/defsys.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

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
