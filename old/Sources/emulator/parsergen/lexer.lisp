;; -*- rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/lexer.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

(in-package parsergen)
(export '(*lexer* read-next-lexeme peek-next-lexeme
                  push-token current-symbol current-symbol-value))

; A simple interface to the lexical analyser

(defvar *lexer*)  ;; The lexical analyser.
(defvar *symbol*) ;; Last symbol read by "read-next-lexeme"
(defvar *value*)  ;; Last value read by "read-next-lexeme"

(defvar *error-found*)

(defvar *symbol-queue* nil) ;The car of this queue is the next value that
                        ; will be read
(defvar *value-queue* nil)  ;The car is the next value that will be read

(defun reset-lexer ()
  (setq *symbol* nil)
  (setq *value* nil)
  (setq *symbol-queue* nil)
  (setq *value-queue* nil))

;; Main interface function -- check on queue for symbol

(defun read-next-lexeme ()
  (multiple-value-bind (token value)
      (if *symbol-queue*
          (values (pop *symbol-queue*)
                  (pop *value-queue*))
        (get-next-lexeme))
    (setq *symbol* token)
    (setq *value* value)
    (values token value)))

;; See what is next without reading it.

(defun peek-next-lexeme ()
  (if *symbol-queue*
      (values (car *symbol-queue*)
              (car *value-queue*))
    (multiple-value-bind (token value)(get-next-lexeme)
      (push token *symbol-queue*)
      (push value *value-queue*)
      (values token value))))

;; Put an extra token onto the output.  This becomes the next
;; token to be read.

(defun push-token (token &optional (value token))
  (push token *symbol-queue*)
  (push value *value-queue*))

;; Internal function that calls the lexer.
;; Should it check for end-of-file??

(defun get-next-lexeme ()
  (multiple-value-bind (symbol value) (funcall *lexer*)
    (unless symbol
      (setq symbol :eoi
            value ""))
    (when (and (eq symbol :eoi)
	       (eq *symbol* :eoi))
      (output-error "Unexpected end of file")
      (emergency-exit))
    (values symbol value)))

(defun current-symbol ()
  *symbol*)

(defun current-symbol-value()
  (if (eq :eoi *symbol*)
      ""
    *value*))
