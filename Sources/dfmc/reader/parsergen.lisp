(in-package parsergen)

;;;; Entry points

(defun compile-grammar-file-for-emulator (input output)
  (let ((*target-emulator* t))
    (declare (special *target-emulator*))
    (compile-grammar-file input output)))

(defun compile-grammar-file-for-native (input output)
  (let ((*target-emulator* nil))
    (declare (special *target-emulator*))
    (compile-grammar-file input output)))

;;;; Keith's personal entry point...

(defun doit ()
  (compile-grammar-file-for-emulator
     "/u/keith/dylan/dfmc/reader/parser.dylgram"
     "/u/keith/dylan/dfmc/reader/emulator-parser.dylan")
  (compile-grammar-file-for-native
     "/u/keith/dylan/dfmc/reader/parser.dylgram"
     "/u/keith/dylan/dfmc/reader/infix-parser.dylan"))

(defun testit ()
  (compile-grammar-file-for-emulator
     "/u/keith/dylan/dfmc/reader/parser.dylgram"
     "/u/keith/dylan/dfmc/reader/emulator-parser-test.dylan")
  (compile-grammar-file-for-native
     "/u/keith/dylan/dfmc/reader/parser.dylgram"
     "/u/keith/dylan/dfmc/reader/infix-parser-test.dylan"))

;;;; The infix grammar parser

(defparameter *grammar-begin* "define parser")
(defparameter *grammar-end*   "end")

(defun compile-grammar-file (input-name output-name)
  (with-open-file (input input-name :direction :input)
    (with-open-file (output output-name 
                      :direction :output
                      :if-exists :supersede)
      (copy-header input output)
      (compile-grammar input output)
      (copy-footer input output))))

(defun copy-header (input output)
  (loop 
    for   line = (read-line input)
    until (string= line *grammar-begin* 
            :end1 (min (length line) (length *grammar-begin*)))
    do    (write-line line output)))

(defun copy-footer (input output)
  (loop 
    for   line = (read-line input nil)
    while line
    do    (write-line line output)))

(defun compile-grammar (input output)
  (let* ((rules (read-rules input))
         (prefix-rule (prefixify-rules rules))
         (*package* dylan::*the-dylan-package*)
         (*print-readably* t))
    (format output "`")
    (pprint (dylan::generate-dylan-parser 'parser prefix-rule) output)
    (format output "~%';~%")))

(defclass clause ()
  ((lhs :accessor clause-lhs :initarg :lhs)
   (rhs :accessor clause-rhs :initarg :rhs)))

(defclass rule ()
  ((name :accessor rule-name :initarg :name)
   (clauses :accessor rule-clauses :initarg :clauses)))

(defun read-rules (input)
  (let ((rules '()))
    (loop
      (let ((next (read-next-token input)))
        (cond
          ;; End of everything
          ((not (stringp next))
            (return))
          ;; Continued clause
          ;; ((string= next "=>")
          ;;  (let ((code (read-until input ";;")))
          ;;    (push code (clause-rhs (first (rule-clauses (first rules)))))))
          ;; New rule
          ((char= (elt next (- (length next) 1)) #\:)
            (let ((rule (make-instance 'rule :name next :clauses '())))
              (push rule rules)))
          ;; New clause
          (t
            (let* ((lhs 
                     (if (string= next "=>") ""
                        (concatenate 'string 
                           next " " (read-until input "=>"))))
                   (code (read-until input ";;"))
                   (clause (make-instance 'clause :lhs lhs :rhs (list code))))
              (push clause (rule-clauses (first rules))))))))
    (nreverse rules)))

(defun read-next-token (input)
  (skip-whitespace input)
  (let* ((char-list
           (loop for     c = (read-char input)
                 until   (system:whitespace-char-p c)
                 collect c))
         (string
           (coerce char-list 'string)))
    (if (string= string *grammar-end*) 
      (progn (read-line input) nil)
      string)))

(defun read-until (input string)
  (let* ((char-list '())
         (string-length (length string))
         (last-char (elt string (- string-length 1))))
    (flet ((foundp ()
             (when (char= (first char-list) last-char)
               (loop
                  for char in char-list
                  for i from 0 below string-length
                  unless (char= char (elt string (- string-length i 1))) do
                    (return nil)
                  when (= (+ i 1) string-length) do
                    (return t)))))
                  
      (loop 
         for  c = (read-char input)
         do   (push c char-list)
         when (foundp) do (return)))
    (coerce (nreverse (subseq char-list string-length)) 'string)))

(defun skip-whitespace (input)
  (loop for c = (read-char input)
        do
          (cond
            ((char= c #\/) 
              (read-line input))
            ((not (system:whitespace-char-p c))
              (unread-char c input)
              (return)))))

;;;; Prefixification.

(defun prefixify-rules (rules)
  (mapcan #'prefixify-rule rules))

(defun prefixify-rule (rule)
  (let ((name (intern 
                (string-upcase
                  (subseq (rule-name rule) 0 (- (length (rule-name rule)) 1)))
                dylan::*the-dylan-package*)))
    (loop
      for clause in (rule-clauses rule)
      collect 
        `((,name ,@(read-tokens-from-string (clause-lhs clause)))
	    ,@(read-code (clause-rhs clause))))))

(defun read-tokens-from-string (string)
  (let ((*package* dylan::*the-dylan-package*)
        (*readtable* dylan::*dylan-readtable*))
    (with-input-from-string (s string)
      (loop
        for token = (read-preserving-whitespace s nil nil)
        while token 
        collect token))))

(defun read-code (code)
  (let ((prefix-code 
          (loop
            for part in code
            collect
              (let ((*package* dylan::*the-dylan-package*))
                (with-input-from-string (s (concatenate 'string part ";"))
                  (dylan::read-infix-dylan s))))))
    (if (= (length prefix-code) 1) prefix-code
      `(:base ,(first prefix-code)
        :rec  ,(second prefix-code)))))

;;;; The infix parser table generator

(defvar *target-emulator* nil)

(defun alist-to-vector (l)
  (if *target-emulator* l
    (coerce (mapcan #'(lambda (pair) (list (car pair) (cdr pair))) l)
	    'vector)))

(defun is-all-same-reduction-for-annotation (actions)
  (let* ((first (cdr (car actions)))
	 (action (when (is-reduction first)
		   (action-of-reduction first))))
    (and action
         (loop for x in (cdr actions)
               for first = (cdr x)
               unless (and (is-reduction first)
                           (= action (action-of-reduction first)))
               do (return nil)
               finally (return t))
         first)))

;; This is very dubious, but we need an immediate, and you'd need a
;; purty big grammar to hit this.
(defparameter *all-same-code* 65535) 

(defun annotate-eager-reductions (actions)
  (if (is-all-same-reduction-for-annotation actions)
    `((,*all-same-code* . ,(cdr (first actions))) ,@actions)
    actions))

(defun dylan::generate-infix-dylan-parser (parserfun full-grammar)
  (setq full-grammar (preprocess-dylan-grammar full-grammar))
  (let ((*terminals* nil)
        (*action-table* nil)
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
	   (symbol-to-string (gensym))
           (encoding-table (make-hash-table)))
      (make-tables grammar error-grammar)
      (let ((action-functions 
              (make-infix-dylan-action-functions 
	         grammar actions parserfun))
	    (error-functions 
              (make-infix-dylan-error-functions 
                 error-actions parserfun))
            (terminal-definitions
              (make-infix-terminal-definitions *terminals* encoding-table)))
        ;; Turn alists and tables into flat vectors.
	(loop for i from 0 below (length *action-table*)
	      do (setf (svref *action-table* i)
                        (alist-to-vector 
			  (annotate-eager-reductions
			    (svref *action-table* i)))))
        (setq *goto-table*
          (let ((keep '()))
            (maphash
              #'(lambda (key val)
                  (push (cons key (alist-to-vector val)) keep))
             *goto-table*)
            (alist-to-vector keep)))
        ;; Turn symbols into numbers
        (setq *goto-table* 
              (encode-parser-structure *goto-table* encoding-table))
        (setq *action-table* 
              (encode-parser-structure *action-table* encoding-table))
        (setq *action-nt-table* 
              (encode-parser-structure *action-nt-table* encoding-table))
        (with-output-to-string (out)
          (dolist (def terminal-definitions)
            (format out "~a~%" def))
          (dolist (action action-functions)
            (format out "~a~%~%" action))
          (dolist (error error-functions)
            (format out "~a~%~%" error))
          (format out "define constant ~a :: <parser>~%" parserfun)
          (format out "  = make(<parser>,~%")
          (format out "action-table:~%")
          (dylan::dylan-print *action-table* :stream out)
          (format out ",~%")
          (format out "goto-table:~%")
          (dylan::dylan-print *goto-table* :stream out)
          (format out ",~%")
          (format out "action-function-table:~%vector(~%")
          (unless (= (length *action-function-table*) 0)
            (format out "~a" (elt *action-function-table* 0))
            (loop for i from 1 below (length *action-function-table*)
                  do  (format out ",~%~a" (elt *action-function-table* i))))
          (format out "),~%action-nargs-table:~%")
          (dylan::dylan-print *action-nargs-table* :stream out)
          (format out ",~%action-nt-table:~%")
          (dylan::dylan-print *action-nt-table* :stream out)
          (format out ", error-productions: #[], error-action-function-table: #[], error-action-nt-table: #[]);~%")
          #|
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
           |#
          out)))))

(defun make-infix-terminal-definitions (terminals table)
  (loop for terminal in terminals
        collect (format nil "define constant $~s-token = ~d;"
                        terminal (encode-parser-structure terminal table))))

(defun get-infix-dylan-action-function-name (rule grammar pname previous-name)
  (let* ((index (position rule grammar))
	 (name (or previous-name
                   (intern 
                     (string-upcase (format nil "~A-ACTION~A" pname index))))))
    (setf (svref *action-function-table* index) name)
    (setf (svref *action-nargs-table* index) (1- (length rule)))
    (setf (svref *action-nt-table* index) (car rule))
    name))

(defun make-infix-dylan-action-functions (grammar actions name)
  (set-up-action-function-tables grammar)
  (let ((function-defuns (make-infix-dylan-function-defuns grammar name actions)))
    (delete nil function-defuns)))

(defun make-infix-dylan-error-functions (error-actions pname)
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

(defvar *infix-dylan-action-function-table*)

(defun make-infix-dylan-function-defuns (grammar name actions)
  (let ((*infix-dylan-action-function-table* (make-hash-table :test 'equal)))
    (let ((functions
            (mapcar #'(lambda(rule action)
	                (make-infix-dylan-function-defun
                           rule grammar name action))
                    grammar actions)))
      (format t ";; Unique action functions: ~a~%"
              (hash-table-size *infix-dylan-action-function-table*))
      functions)))

(defun make-infix-dylan-function-defun (rule grammar name body)
  (let* ((hash-key (cons (length (cdr rule)) body))
         (hash-val (gethash hash-key *infix-dylan-action-function-table*)))
    (if hash-val
      (progn
        (get-infix-dylan-action-function-name rule grammar name hash-val)
        nil)
      (let* ((fname (get-infix-dylan-action-function-name rule grammar name nil))
	     (vars  (make-infix-dylan-variables (cdr rule)))
             (function 
               (format nil
                 "// MACHINE GENERATED! DO NOT EDIT!! I MEAN IT!!!~%define constant ~a~%  = method (~a) => (value)~%         ~a~%    end method;"
                 fname vars body)))
        (setf (gethash hash-key *infix-dylan-action-function-table*) fname)
        function))))

(defun make-infix-dylan-variables (rule)
  (let ((n 0) (string ""))
    (dolist (symbol rule)
      (declare (ignore symbol))
      (if (= n 0)
	  (setq string (format nil "arg$~d" (incf n)))
	(setq string (format nil "~a, arg$~d" string (incf n)))))
    string))

(defun emulator-testit ()
  (let ((*target-emulator* t))
    (declare (special *target-emulator*))
    (compile-grammar-file
      "/u/keith/dylan/dfmc/reader/parser.dylgram"
      "/u/keith/dylan/dfmc/reader/emulator-test-parser.dylan")))

(defun native-testit ()
  (let ((*target-emulator* nil))
    (declare (special *target-emulator*))
    (compile-grammar-file
       "/u/keith/dylan/dfmc/reader/parser.dylgram"
       "/u/keith/dylan/dfmc/reader/native-test-parser.dylan")))

(defun compile-grammar (input output)
  (let* ((rules (read-rules input))
         (prefix-rule (prefixify-rules rules))
         (*package* dylan::*the-dylan-package*)
         (*print-readably* t))
    (format output "~a~%"
            (dylan::generate-infix-dylan-parser 'parser prefix-rule))))

(defun read-code (code)
  (let ((prefix-code 
          (loop
            for part in code
            collect
              (let ((*package* dylan::*the-dylan-package*))
                ;; (with-input-from-string (s (concatenate 'string part ";"))
                ;;  (dylan::read-infix-dylan s))))))
                part))))
    (if (= (length prefix-code) 1) prefix-code
      `(:base ,(first prefix-code)
        :rec  ,(second prefix-code)))))

(defmethod encode-parser-structure (o table)
  o)

(defmethod encode-parser-structure ((o null) table)
  o)

(defmethod encode-parser-structure ((s symbol) table)
  (case s
    ((:eoi :accept :all-same) s)
    (t (or (gethash s table)
           (setf (gethash s table) (hash-table-count table))))))

(defmethod encode-parser-structure ((v vector) table)
  (loop for i from 0 below (length v)
        do (setf (elt v i) (encode-parser-structure (elt v i) table)))
  v)

(defmethod encode-parser-structure ((c cons) table)
  (setf (car c) (encode-parser-structure (car c) table))
  (setf (cdr c) (encode-parser-structure (cdr c) table))
  c)

;; eof
