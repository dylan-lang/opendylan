;; -*- lisp ; rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/grammar.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

; grammar.lisp
;;; Auxiliary functions for handling grammars

;;; The only follow/first set function which is used externally to
;;; this file is first-set-string in close.lisp

(in-package parsergen)

(defvar *grammar-symbols*)

(defmacro get-grammar-symbols ()
  `*grammar-symbols*)

(defvar *production-table*) ; Associates non-terminals with its productions.

(defmacro productions-of (symbol)
  `(gethash ,symbol *production-table*))

(defvar *terminals*) ; Set to the list of terminals of the grammar.

(defmacro terminals ()
  `*terminals*)

(defvar *non-terminals*) ; Set to the list of non-terminals of the
			 ; grammar

(defmacro non-terminals ()
  `*non-terminals*)

(defvar  *terminal-cache*)

(defmacro is-terminal (symb)
     `(gethash ,symb *terminal-cache*))

(defvar  *non-terminal-cache*)

(defmacro  is-non-terminal (symb)
     `(gethash ,symb *non-terminal-cache*))

; cache-symbols sets up the global data for terminals, non-terminals
; and productions.

;; KJP 7March95
;; Modify not to use (apply #'append ...) to flatten a list.

(defun setup-symbols (grammar)
     (setq *terminal-cache* (make-hash-table))
     (setq *non-terminal-cache* (make-hash-table))
     (setq *production-table* (make-hash-table))

     (let ((symbols (let ((temp (flatten-one-level grammar)))
		      (remove nil (remove-duplicates temp))))
	   (non-terminals (remove-duplicates (mapcar #'car
						     grammar))))
       (setq *grammar-symbols* symbols)
       (setq *non-terminals* non-terminals)
       (setq *terminals* (set-difference symbols non-terminals)))
     (dolist (nont (non-terminals))
	  (setf (gethash nont *non-terminal-cache*)  'T))
     (dolist (prod grammar)
	     (push (cdr prod) (gethash (car prod) *production-table*)))
     (dolist (term (terminals))
	  (setf (gethash term *terminal-cache*)  'T))
     )

;; Flatten-one-level mustn't destructively modify the sublists.

(defun flatten-one-level (list)
  (loop for sublist in list nconc (copy-list sublist)))
