;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/defsys.lisp,v 1.1 2004/03/12 00:41:15 cgay Exp $" -*-

;; #<harlequin copyright marker>


(in-package "SYSTEM")

(defsystem parser-runtime 
    (:optimize ((speed 3) (safety 0) (compilation-speed 0)))
  :members (pkg
	    messages
	    lexer
	    errorrecovery
	    switch-states
	    despatch
	    utilities
	    )
  :rules ((:in-order-to :compile :all
			(:requires (:load :serial)))))


(defsystem parsergen ()
  :members ((parser-runtime :type :system)
	    newitems
	    grammar
	    items
	    pre-process-gram
	    newkerns
	    lr1close
	    propagate
	    newnewderiv
	    close
	    generate-actions
	    parseractions
	    newbuild)
  :rules ((:in-order-to :compile :all
			(:requires (:load :serial)))))
