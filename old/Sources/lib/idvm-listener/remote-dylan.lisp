; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package dylan)


(defmethod dylan-remote-print (&rest any))

(defmethod dylan-remote-eval (form)
  (dylan+dylan/idvm-harp-cg::tm-for-tcp form))

(defmethod dylan-connect-to-host (host)
  (dylan+dylan/sockets::connect-to-server host))

(defmethod dylan-start-remote-app (app)
  (dylan+dylan/sockets::connect-to-application app))

(defmethod dylan-prompt ()
  (dylan+dylan/functional-extensions::debug-name (dylan+dylan/compiler-namespace::current-module)))
