; copyright: 1996 Functional Objects, Inc. All rights reserved.
; -*- lisp -*-
; buildparser.lisp
(in-package parsergen)

(lw::do-demand-pre-loads :defparser)

(defvar *action-table*)
(defvar *goto-table*)
(defvar *action-function-table*)
(defvar *action-nargs-table*)
(defvar *action-nt-table*)
(defvar *error-productions*)
(defvar *error-action-function-table*)
(defvar *error-action-nt-table*)

(defun dylan::generate-dylan-parser (parserfun full-grammar)
  (setq full-grammar (preprocess-dylan-grammar full-grammar))
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
      (let ((action-functions (make-dylan-action-functions grammar
						           actions
						           parserfun))
	    (error-functions (make-dylan-error-functions error-actions
						         parserfun)))
        (setq *goto-table*
          (let ((keep '()))
            (maphash
              #'(lambda (key val)
                  (push (cons key val) keep))
             *goto-table*)
            keep))
	`(dylan::begin
	   ,@action-functions
	   ,@error-functions
 	   (dylan::define ,parserfun
             (dylan::make dylan::<parser>
                :action-table ',*action-table*
                :goto-table   ',*goto-table*
                :action-function-table 
		  (dylan::vector
		    ,@(coerce *action-function-table* 'list))
                :action-nargs-table ',*action-nargs-table*
                :action-nt-table ',*action-nt-table*
                :error-productions ',*error-productions*
		:error-action-function-table 
		  (dylan::vector 
		    ,@(coerce *error-action-function-table* 'list))
		:error-action-nt-table ',*error-action-nt-table*))
	   )))))

(defun dylan::generate-dylanworks-parser (parserfun full-grammar)
  (setq full-grammar (preprocess-dylan-grammar full-grammar))
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
      (let ((action-functions (make-dylan-action-functions grammar
						           actions
						           parserfun))
	    (error-functions (make-dylan-error-functions error-actions
						         parserfun)))
        (setq *goto-table*
          (let ((keep '()))
            (maphash
              #'(lambda (key val)
                  (push (cons key val) keep))
             *goto-table*)
            keep))
	`(dylan::begin
	   ,@action-functions
	   ,@error-functions
 	   (dylan::define ,parserfun
             (dylan::make dylan::<parser>
                :action-table ',*action-table*
                :goto-table   ',*goto-table*
                :action-function-table 
		  (dylan::vector
		    ,@(coerce *action-function-table* 'list))
                :action-nargs-table ',*action-nargs-table*
                :action-nt-table ',*action-nt-table*
                :error-productions ',*error-productions*
		:error-action-function-table 
		  (dylan::vector 
		    ,@(coerce *error-action-function-table* 'list))
		:error-action-nt-table ',*error-action-nt-table*))
	   )))))

(defun get-dylan-action-function-name (rule grammar pname previous-name)
  (let* ((index (position rule grammar))
	 (name (or previous-name
                   (intern (format nil "~A-ACTION~A" pname index)))))
    (setf (svref *action-function-table* index) name)
    (setf (svref *action-nargs-table* index) (1- (length rule)))
    (setf (svref *action-nt-table* index) (car rule))
    name))

(defun make-dylan-action-functions (grammar actions name)
  (set-up-action-function-tables grammar)
  (let ((function-defuns (make-dylan-function-defuns grammar name actions)))
    (delete nil function-defuns)))

(defun make-dylan-error-functions (error-actions pname)
  (setq *error-action-function-table* (make-array (length error-actions)))
  (let ((index 0))
    (mapcar #'(lambda (action)
		(let ((name (intern (format nil "~A-ERROR-ACTION-~A"
					    pname index))))
		  (setf (svref *error-action-function-table* index)
			name)
		  (incf index)
		  `(define ,name (method () ,@action))))
	    error-actions)))

;; Share action functions that are structurally equivalent

(defvar *dylan-action-function-table*)

(defun make-dylan-function-defuns (grammar name actions)
  (let ((*dylan-action-function-table* (make-hash-table :test 'equal)))
    (let ((functions
            (mapcar #'(lambda(rule action)
	                (make-dylan-function-defun rule grammar name action))
                    grammar actions)))
      (format t ";; Unique action functions: ~a~%"
              (hash-table-size *dylan-action-function-table*))
      functions)))

(defun make-dylan-function-defun (rule grammar name body)
  (let* ((hash-key (cons (length (cdr rule)) body))
         (hash-val (gethash hash-key *dylan-action-function-table*)))
    (if hash-val
      (progn
        (get-dylan-action-function-name rule grammar name hash-val)
        nil)
      (let* ((fname (get-dylan-action-function-name rule grammar name nil))
	     (vars  (make-dylan-variables (cdr rule)))
             (function `(dylan::define-lambda ,fname ,vars
                          (declare (ignore ,@vars))
                          ,@body)))
        (setf (gethash hash-key *dylan-action-function-table*) fname)
        function))))

(defun make-dylan-variables (rule)
  (let ((n 0))
    (mapcar #'(lambda (symbol)
		(declare (ignore symbol))
		(intern (format nil "$~D" (incf n))
                        (find-package :dylan)))
	    rule)))

;; In the grammar source ... means recurse by name while ___ means
;; recurse by "value" - that is, just on the terms in this particular
;; production, and requires the introduction of a local rule.
;;
;;   constituents: 
;;     constituent ; ...
;; =>
;;   constituents:
;;     constituent
;;     constituents ; constituent
;;
;;   constituents: 
;;     constituent ; ___
;; =>
;;   constituents:
;;     constituents-aux
;;
;;   constituents-aux:
;;     constituent
;;     constituents-aux ; constituent

(defun preprocess-dylan-grammar (productions)
  (let ((collect '())
        (subrules '()))
    (loop 
      for production in productions
      for ((name . terms) . code) in productions 
      for last = (first (last terms))
      do
        (case last
          ((dylan::|...|)
	    (setq subrules 
		  (nconc (generate-ellipsis-rules name terms code)
			 subrules)))
          ((dylan::___)
           (let ((subrule-name
		   (progn (gensym-rule-name (symbol-name name)))))
             (push `((,name ,subrule-name) dylan::$1) collect)
	     (setq subrules 
		   (nconc (generate-ellipsis-rules subrule-name terms code)
			  subrules))))
          (otherwise 
            (push production collect))))
    (nconc (nreverse collect) subrules)))

(let ((count -1))
  (defun gensym-rule-name (name)
    (intern (format nil "~a~a" name (incf count))
            dylan::*the-dylan-package*)))

(defun generate-ellipsis-rules (name terms code)
  (let ((len (length terms)))
    (destructuring-bind (&key base rec) code
      (unless base
        (setq base ``(,dylan::$1)))
      (unless rec
        (setq rec
          (ecase len
	    ((2) ``(,@dylan::$1 ,dylan::$2))
	    ((3) ``(,@dylan::$1 ,dylan::$3)))))
      (if (= len 2)
          (let ((term (first terms)))
            `(((,name ,term) 
               ,base)
              ((,name ,name ,term)
	       ,rec)))
        (let ((sep (elt terms (- len 2)))
              (repeated (subseq terms 0 (- len 2))))
          `(((,name ,@repeated)
             ,base)
            ((,name ,name ,sep ,@repeated)
             ,rec)))))))

(defun dylan::compile-dylan-grammar-file (name &rest stuff)
  (let* ((stream (open name :direction :input))
         (form (progn (dylan::dylan-packaged-read 
                        :stream stream
                        :features '(:nasty-dylan))))
         (dylan (dylan::generate-dylan-parser (cadr form) (cddr form))))
    (close stream)
    (let* ((dylan-name (merge-pathnames (make-pathname :type "dylan") name))
           (stream (open dylan-name :direction :output))
           (*print-readably* t))
      (format stream "Module:   infix-reader~%")
      (format stream "Language: prefix-dylan~%~%")
      (format stream ";; Generated code!!~%~%")
      (format stream "~s~%" dylan)
      (format stream ";; eof~%")
      (close stream)
      (generic-env::generic-compile dylan-name))))

(generic-env::register-file-type 'dylan::dylan-grammar-file
  :source-extensions '("dylgram")
  :object-extensions '("wfasl")
  :compiler          'dylan::compile-dylan-grammar-file
  :object-loader      'dylan::load-dylan-file)

;; eof
