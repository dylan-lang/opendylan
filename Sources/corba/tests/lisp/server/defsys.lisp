;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/defsys.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(require "corba-orb")

(defsystem all-lisp-server-tests ()
  :members
  (
   "version"
   ("../../idl/tree" :type :idl-file)
   ("../../idl/any.idl" :type :idl-file)
   ("../../idl/array" :type :idl-file)
   ("../../idl/bank" :type :idl-file :idl-options (:generate-tie t))
   ("../../idl/chat" :type :idl-file)
   ("../../idl/enum" :type :idl-file)
   ("../../idl/grid" :type :idl-file)
   ("../../idl/pseudo-objects" :type :idl-file)
   ("../../idl/union" :type :idl-file)
   ("../../idl/sequence" :type :idl-file)
   ("../../idl/struct" :type :idl-file)
   "any-impl"
   "array-impl.lisp"
   "bank-impl.lisp"
   "bank-impl-2.lisp"
   "bank-impl-3.lisp"
   "bank-impl-4.lisp"
   "chat-impl.lisp"
   "enum-impl.lisp"
   "grid-impl.lisp"
   "pseudo-objects-impl.lisp"
   "union-impl.lisp"
   "sequence-impl.lisp"
   "struct-impl.lisp"
   "tree-impl.lisp"

   "utils"
   )
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires (:load :previous)))))
