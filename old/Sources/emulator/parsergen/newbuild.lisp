;; -*- lisp ; rcs-header: "$Header: /scm/cvs/fundev/old/Sources/emulator/parsergen/newbuild.lisp,v 1.1 2004/03/12 00:41:16 cgay Exp $" -*-

;; #<harlequin copyright marker>

; buildparser.lisp
(in-package parsergen)
(export '(defparser))


;;; PJG 9Jan95
;;; Changes
;;; 1. defparser nolonger produces a function which returns the full grammar.
;;;    Did we ever have a use for that?
;;; 2. We nolonger produce a actions function for every action. 
;;;    When two actions contain the same arguments + body, we know only
;;;    define the first occurrence.

(defvar *action-table*)
(defvar *goto-table*)
(defvar *action-function-table*)
(defvar *action-nargs-table*)
(defvar *action-nt-table*)
(defvar *error-productions*)
(defvar *error-action-function-table*)
(defvar *error-action-nt-table*)

;;; PJG 9Jan95
;;; Used for caching action defuns.
(defvar *current-actions* ) 

;;; PJG 27Jan95
;;; Smaller functions trying to seperate keywords from the overall
;;; grammar.

(defmacro defparser (parserfun &rest full-grammar)
  (multiple-value-bind (full-grammar keywords)
      (extract-keywords-from-grammar full-grammar)
  (let ((*action-table* nil)
	(*goto-table* nil)
	(*action-function-table* nil)
	(*action-nargs-table* nil)
	(*action-nt-table* nil)
	(*error-productions* nil)
	(*error-action-function-table* nil)
	(*error-action-nt-table* nil))
    (let* ((ok-full-grammar (remove-if #'(lambda (x)
					   (eq (cadar x) :error))
				       full-grammar))
	   (error-full-grammar (remove-if-not #'(lambda (x)
						  (eq (cadar x) :error))
					      full-grammar))
	   (grammar (mapcar #'car ok-full-grammar))
	   (actions (mapcar #'cdr ok-full-grammar))
	   (error-grammar (mapcar #'car error-full-grammar))
	   (error-actions (mapcar #'cdr error-full-grammar)))
      (make-tables grammar error-grammar)
      (let ((action-functions (make-action-functions grammar
						     actions
						     parserfun))
	    (error-functions (make-error-functions error-actions
						   parserfun)))
	(parsergen-output-forms parserfun action-functions error-functions keywords))))))



;;; PJG 6Feb95
;;; expand-rules will expand
;;;      (foo 
;;;        ((bar) $1)
;;;        ((bob tom) (+ $2 $1)))
;;; into
;;;      ((foo bar) $1)
;;;      ((foo bob tom) (+ $2 $1))

(defun expand-rules (rules)
  (let ((rule-name (first rules)))
    (loop for rule in (rest rules)
          do (setf (nth 0 rule) (cons rule-name (nth 0 rule)))
          collect rule)))

;;; PJG 27Jan95
;;; EXTRACT-KEYWORDS-FROM-GRAMMAR will extract the keywords from the grammar without
;;; altering any of the old-style behaviour.

(defun extract-keywords-from-grammar (full-grammar)
  (loop for keyword-valuep = nil then (and (null keyword-valuep)
					   (atom entry))
	for entry in full-grammar
	do
	if (or keyword-valuep
	       (atom entry))
	collect entry into keywords
	else if (atom (first entry))
	append (expand-rules entry) into grammar
	else
	collect entry into grammar
	finally return (values grammar keywords)
	))

;;; PJG 27Jan95
;;; Seperate this from the old top-level body of code.

(defun parsergen-output-forms (parserfun action-functions error-functions keywords)
  (let ((lexer (gensym))
        (symbol-to-string (gensym)))
    `(progn
       ;; PJG 9Jan95 : do we really need a spare copy of the grammar?
       ;;(defun ,(intern (format nil "~A-GRAMMAR" parserfun)) ()
       ;;  ',full-grammar)
       (defun ,parserfun (,lexer &optional (,symbol-to-string
					    #'identity) 
				 &key (default-error-recovery t) )
	 (let ((*lexer* ,lexer)
	       (*symbol-to-string* ,symbol-to-string)
	       (*action-table* ',*action-table*)
	       (*goto-table* ',*goto-table*)
	       (*action-function-table* ',*action-function-table*)
	       (*action-nargs-table* ',*action-nargs-table*)
	       (*action-nt-table* ',*action-nt-table*)
	       (*error-productions* ',*error-productions*)
	       (*error-action-function-table* ',*error-action-function-table*)
	       (*error-action-nt-table* ',*error-action-nt-table*)
               (*accept-without-eoi-p* ,(getf keywords
					      :accept-without-eoi-p))
	       (*default-error-recovery* default-error-recovery))
	   (declare (special *lexer* *symbol-to-string*
			     *action-table* *goto-table*
			     *action-function-table* *action-nargs-table*
			     *action-nt-table*
			     *error-action-function-table*
			     *error-action-nt-table*
			     *default-error-recovery*))
	   ,(or (getf keywords :run-parser-function) '(run-parser))))
       ,@action-functions
       ,@error-functions
       )
    ))

#| Old (pre-27Jan95) version of the top-level function
(defmacro defparser (parserfun &rest full-grammar)
  (let ((*action-table* nil)
	(*goto-table* nil)
	(*action-function-table* nil)
	(*action-nargs-table* nil)
	(*action-nt-table* nil)
	(*error-productions* nil)
	(*error-action-function-table* nil)
	(*error-action-nt-table* nil))
    (let* ((ok-full-grammar (remove-if #'(lambda (x)
					   (eq (cadar x) :error))
				       full-grammar))
	   (error-full-grammar (remove-if-not #'(lambda (x)
						  (eq (cadar x) :error))
					      full-grammar))
	   (grammar (mapcar #'car ok-full-grammar))
	   (actions (mapcar #'cdr ok-full-grammar))
	   (error-grammar (mapcar #'car error-full-grammar))
	   (error-actions (mapcar #'cdr error-full-grammar))
	   (lexer (gensym))
	   (symbol-to-string (gensym)))
      (make-tables grammar error-grammar)
      (let ((action-functions (make-action-functions grammar
						     actions
						     parserfun))
	    (error-functions (make-error-functions error-actions
						   parserfun)))
	`(progn
           ;; PJG 9Jan95 : do we really need a spare copy of the grammar?
           ;;(defun ,(intern (format nil "~A-GRAMMAR" parserfun)) ()
           ;;  ',full-grammar)
	   (defun ,parserfun (,lexer &optional (,symbol-to-string #'identity))
	     (let ((*lexer* ,lexer)
		   (*symbol-to-string* ,symbol-to-string)
		   (*action-table* ',*action-table*)
		   (*goto-table* ',*goto-table*)
		   (*action-function-table* ',*action-function-table*)
		   (*action-nargs-table* ',*action-nargs-table*)
		   (*action-nt-table* ',*action-nt-table*)
		   (*error-productions* ',*error-productions*)
		   (*error-action-function-table* ',*error-action-function-table*)
		   (*error-action-nt-table* ',*error-action-nt-table*))
	       (declare (special *lexer* *symbol-to-string*
				 *action-table* *goto-table*
				 *action-function-table* *action-nargs-table*
				 *action-nt-table*
				 *error-action-function-table*
				 *error-action-nt-table*))
	       (run-parser)))
	   ,@action-functions
	   ,@error-functions
	   )))))

|#

(defun make-parsing-tables (full-grammar)
  (let ((*action-table* nil)
	(*goto-table* nil)
	(*action-function-table* nil)
	(*action-nargs-table* nil)
	(*action-nt-table* nil)
	(*error-productions* nil)
	(*error-action-function-table* nil)
	(*error-action-nt-table* nil))
    (let* ((ok-full-grammar (remove-if #'(lambda (x)
					   (eq (cadar x) :error))
				       full-grammar))
	   (error-full-grammar (remove-if-not #'(lambda (x)
						  (eq (cadar x) :error))
					      full-grammar))
	   (grammar (mapcar #'car ok-full-grammar))
	   (error-grammar (mapcar #'car error-full-grammar)))
      (make-tables grammar error-grammar)
      (values *action-table* *goto-table*)
      )))


;;; PJG 9Jan95 : add extra argument real-name
(defun get-action-function-name (rule grammar pname real-name)
  (let* ((index (position rule grammar))
	 (name (or real-name (intern (format nil "~A-ACTION~A" pname index)))))
    (setf (svref *action-function-table* index) name)
    (setf (svref *action-nargs-table* index) (1- (length rule)))
    (setf (svref *action-nt-table* index) (car rule))
    name))

;;; PJG 9Jan95 : make-function-defuns does most the hard work now!

(defun make-action-functions (grammar actions name)
  (set-up-action-function-tables grammar)
  (let ((function-defuns (make-function-defuns grammar name actions)))
    (delete nil function-defuns)))

(defun make-error-functions (error-actions pname)
  (setq *error-action-function-table* (make-array (length error-actions)))
  (let ((index 0))
    (mapcar #'(lambda (action)
		(let ((name (intern (format nil "~A-ERROR-ACTION-~A"
					    pname index))))
		  (setf (svref *error-action-function-table* index)
			name)
		  (incf index)
		  `(defun ,name () ,@action)))
	    error-actions)))

;;; PJG 9Jan95 : make-function-defun now inserts the correct 
;;; body of code.

(defun make-function-defuns (grammar name actions)
  (let ((*Current-actions* (make-hash-table :test 'equal)))
  (mapcar #'(lambda(rule action)
	      (make-function-defun rule grammar name action)) grammar actions)))

;;; PJG 9Jan95 : make-function-defun now caches the previous defuns
;;; so that we nolonger produce several identical defuns per grammar.
;;; Using this optimisation then number of action defuns is reduced from
;;; around 620 to about 220 !!!

(defun make-function-defun (rule grammar name body)
  (let* ((hash-key (cons (length (cdr rule)) body))
         (hash-value (gethash hash-key *Current-actions*)))
    (if hash-value
        (progn
          (get-action-function-name rule grammar name hash-value)
          nil)
      (let* ((vars  (make-variables (cdr rule)))
	     (fname (get-action-function-name rule grammar name nil))
             (function `(defun ,fname ,vars
	                (declare (ignorable ,@vars))
	                ,@body)))
        (setf (gethash hash-key *Current-actions*) fname)
        function))))


(defun make-variables (rule)
  (let ((n 0))
    (mapcar #'(lambda (symbol)
		(declare (ignore symbol))
		(intern (format nil "$~D" (incf n))))
	    rule)))

(defun set-up-action-function-tables (grammar)
  (setq *action-function-table* (make-array (length grammar)))
  (setq *action-nargs-table* (make-array (length grammar)))
  (setq *action-nt-table* (make-array (length grammar))))
