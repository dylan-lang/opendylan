(in-package :user)

(defsystem generic-env
  (:default-type :lisp-file)

  :members (("pkg" :source-only t)

	    "inspector"     ;Tim 11/05/93
	    "inspector-patches" ;ditto
            "generic-file"
            "top-level"
            "listener"
            "editor")

  :rules ((:in-order-to :compile :all
                        (:requires (:load "pkg")))
          (:in-order-to :load :all
                        (:requires (:load :previous))))
)

;; eof
