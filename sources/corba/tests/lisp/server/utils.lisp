;; -*- Mode: Lisp; rcs-header: "$Header: /scm/cvs/fundev/Sources/corba/tests/lisp/server/utils.lisp,v 1.1 2004/03/12 00:06:18 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defun temp-file (name)
  (merge-pathnames #-win32 "/mp" #+win32 "c:/temp/"
                   name))

(defun run-all-tests ()
  (any-server-mainline)
  (array-server-mainline)
  (bank-server-mainline)
  (bank-server-mainline2)
  (bank-server-mainline3)
  (bank-server-mainline4)
  (chat-server-mainline)
  (enum-server-mainline)
  (grid-server-mainline)
  (pseudo-objects-server-mainline)
  (union-server-mainline)
  (sequence-server-mainline)
  (struct-server-mainline)
  (tree-server-mainline))
