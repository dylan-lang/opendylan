; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package dylan)

(declaim (optimize (speed 3) (safety 0)))

(declaim (inline token-values))

(defun token-values (type val)
  (values type val val))

(defmacro with-reader-values ((val type text) expr &rest body)
  `(let (*token-value* *token-type* *token-text*)
     (declare (special *token-value* *token-type* *token-text*))
     (let ((.val. ,expr))
       (when .val. (setq *token-value* .val.)))
     (let ((,val *token-value*)
           (,type *token-type*)
           (,text *token-text*))
       ,@body)))

(declaim (inline reader-values))

(defun reader-values (type val)
  (declare
    (special *token-value* *token-type* *token-text*))
  (setq *token-value* val)
  (setq *token-type* type)
  (setq *token-text* val)
  nil)

(defparameter *dylan-infix-token-readtable* (copy-readtable))

(declaim (inline make-name-in-context))

(defun make-name-in-context (name)
  (intern name *the-dylan-package*))

;; Useful macros.

(eval-when (:compile-toplevel)

  (defmacro define-punctuation ((char &optional class value) &rest code)
    `(set-macro-character 
       ,char 
       #'(lambda (stream char)
           (declare (ignore stream) (ignore char))
           ,(if class 
              `(reader-values ',class ,(if value value (string char)))
              `(progn ,@code)))
       nil
       *dylan-infix-token-readtable*))

  (defmacro define-non-terminating-punctuation
       ((char &optional class value) &rest code)
    `(set-macro-character 
       ,char 
       #'(lambda (stream char)
           (declare (ignore stream) (ignore char))
           ,(if class 
              `(reader-values ',class ,(if value value (string char)))
              `(progn ,@code)))
       t
       *dylan-infix-token-readtable*))

  (defmacro define-extended ((token &optional class value) &rest code)
    `(set-dispatch-macro-character 
       #\#
       ,(elt token 1) 
       #'(lambda (stream char arg)
           (declare (type stream stream) (ignore char) (ignore arg))
           ,(if class 
              `(progn
                 (read-remaining-infix-extended-token stream ,token)
                 (reader-values ',class ,(if value value token)))
              `(progn ,@code)))
       *dylan-infix-token-readtable*))

  (defmacro define-word ((token &optional class value) &rest code)
    `(core-register-word ',token ',class))

  (defmacro fixup-case ((stream) &rest cases)
    `(let ((next-char (peek-char nil ,stream nil nil)))
       (case next-char ,@cases)))

  (defmacro fixup-ecase ((stream) &rest cases)
    `(let ((next-char (peek-char nil ,stream nil nil)))
       (case next-char 
         ,@cases
         (otherwise
           (reader-error "Unexpected character '~a'" next-char)))))

  (defmacro fixup-cond ((stream) &rest cases)
    `(let ((next-char (peek-char nil ,stream nil nil)))
       (cond ,@cases)))

)

;; Token definitions.

(define-punctuation (#\, <var-sep>))
(define-punctuation (#\; <statement-sep>))
(define-punctuation (#\( <lbracket>))
(define-punctuation (#\) <rbracket>))
(define-punctuation (#\[ <lsbracket>))
(define-punctuation (#\] <rsbracket>))
(define-punctuation (#\{ <lcbracket>))
(define-punctuation (#\} <rcbracket>))

(define-non-terminating-punctuation (#\~)
  (fixup-case (stream)
    ((#\=)
      (read-char stream)
      (fixup-case (stream)
        ((#\=)
          (read-char stream)
          (reader-values '<binary-operator> '~==))
        (otherwise
          (reader-values '<binary-operator> '~=))))
    (otherwise
      (reader-values '<not> '~))))

(define-non-terminating-punctuation (#\-)
  (fixup-cond (stream)
     ((digit-char-p next-char)
       (let ((value (nth-value 1 (read-infix-token stream))))
         (reader-values '<literal> (- value))))
     (t
       (reader-values '<minus> '-))))

(define-non-terminating-punctuation (#\?)
  (fixup-case (stream)
    ((#\?)
      (read-char stream)
      (reader-values '<query-query> '??))
    ((#\=)
      (read-char stream)
      (reader-values '<query-equal> '?=))
    (otherwise
      (reader-values '<query> '?))))

(define-punctuation (#\.)
  (fixup-case (stream)
    ((#\.)
      (read-char stream)
      (read-char stream)
      (reader-values '<ellipsis> '|...|))
    (otherwise
      (reader-values '<dot> '|.|))))

(define-non-terminating-punctuation (#\| <binary-operator> '\|))

(define-punctuation (#\:)
  (fixup-case (stream)
    ((#\:)
      (read-char stream)
      (reader-values '<var-type-sep> '|::|))
    ((#\=)
      (read-char stream)
      (reader-values '<becomes> '|:=|))
    (otherwise
      (let ((constraint (read-preserving-whitespace stream)))
        (reader-values 
          '<symbol> 
          (make-name-in-context
            (concatenate 'string
                    ":" (symbol-name constraint))))))))

(define-punctuation (#\\)
  (fixup-case (stream)
    ((#\- #\~)
      (let ((dodgy (read-char stream)))
        (fixup-cond (stream)
          ((not (or (alphanumericp next-char)
                    (member next-char '(#\= #\< #\>))))
            (reader-values '<symbol> 
              (make-name-in-context (string dodgy))))
          (t 
           (multiple-value-bind (value) (read-preserving-whitespace stream)
             (reader-values '<symbol> 
               (make-name-in-context
                 (concatenate 'string 
                    (string dodgy) (symbol-name value)))))))))
    (t
      (multiple-value-bind (class value print) (read-infix-token stream)
        (case class
          ((:&next :&rest :&key :&all-keys)
            (reader-values 
              '<escaped-hash-word> 
              (funcall 
                (dylan-resolve literal-fragment :module-name infix-reader)
                :object value :token-class class :token-value value)))
          ((<query> <query-query> <query-equal> <ellipsis>
            <escaped-substitution>)
            (reader-values
              '<escaped-substitution>
              (funcall 
                (dylan-resolve literal-fragment :module-name infix-reader)
                :object value :token-class class :token-value value)))
          (otherwise
            (reader-values '<symbol> value)))))))

(define-punctuation (#\')
  (let ((char (read-infix-character stream)))
    (unless (eql (read-char stream) #\')
      (reader-error "No terminator for character ~s" char))
    (reader-values '<literal> char)))

(define-punctuation (#\")
  (let ((string (read-infix-string stream)))
    (reader-values '<string> string)))

(define-non-terminating-punctuation (#\/)
  (fixup-case (stream)
    ((#\/)
      (read-infix-line-comment stream)
      (values))
    ((#\*)
      (read-char stream)
      (read-infix-area-comment stream)
      (values))
    (otherwise
      (reader-values '<binary-operator> '/))))

(define-extended ("#t" <literal> *dylan-canonical-true*))
(define-extended ("#f" <literal> *dylan-canonical-false*))
(define-extended ("#next" :&next '&next))
(define-extended ("#rest" :&rest '&rest))
(define-extended ("#key" :&key '&key))
(define-extended ("#all-keys" :&all-keys '&all-keys))
(define-extended ("#\"")
  (let ((string (read-infix-string stream)))
    (reader-values '<literal> 
		   (intern (string-upcase string)
                           *the-dylan-package*))))
(define-extended ("#(" <list-open>))
(define-extended ("#[" <vector-open>))
(define-extended ("#{" <syntax-open>))
(define-extended ("##" <splice>))

;; Extra hack.

(define-punctuation (#\`)
  (let ((form (dylan-read stream)))
    (loop until (eql (read-char stream) #\'))
    (cond
      ((or (keywordp form) (numberp form)
           (and (consp form) (eq (car form) 'quote)))
         (reader-values :parsed-literal form))
      ((symbolp form)
         (reader-values :parsed-name form))
      (t
        (reader-values ':parsed-expression form)))))

;; Token classifications.

(defvar *dylan-infix-syntax-table* (make-hash-table))

(defun core-classify-word (word)
  (gethash word *dylan-infix-syntax-table*))

(defvar *dylan-classify-word* nil)
(defvar *dylan-register-word* nil)

(defun core-classify-word (word)
  (let ((type (gethash word *dylan-infix-syntax-table*)))
    (if type
      (token-values type word)
      (token-values '<symbol> word))))

(defun core-register-word (word type)
  (unless (keywordp (gethash word *dylan-infix-syntax-table*))
    (setf (gethash word *dylan-infix-syntax-table*) type)))

(defun classify (word)
  (when *dylan-classify-word*
    (let ((type (funcall *dylan-classify-word* word)))
      (when type
	(return-from classify (token-values type word)))))
  (let ((type (gethash word *dylan-infix-syntax-table*)))
    (if type
      (token-values type word)
      (token-values '<symbol> word))))

(defun register-word (word type)
  (if *dylan-register-word*
    (funcall *dylan-register-word* word type)
    (core-register-word word type))
  type)

;; The permanent reserved word set.

(define-word (define :define))
(define-word (end :end))
(define-word (let :let))
(define-word (handler :handler))
(define-word (local :local))
(define-word (method :method))
(define-word (generic :generic))
(define-word (macro :macro))
(define-word (otherwise :otherwise))

(define-word (^ <binary-operator>))
(define-word (* <binary-operator>))
(define-word (/ <binary-operator>))
(define-word (+ <binary-operator>))
(define-word (= <binds>))
(define-word (== <var-singleton-sep>))
(define-word (< <binary-operator>))
(define-word (> <binary-operator>))
(define-word (<= <binary-operator>))
(define-word (>= <binary-operator>))
(define-word (& <binary-operator>))
(define-word (>= <binary-operator>))
(define-word (=> <implies>))

;; For Tony...
(define-word (<-> <binary-operator>))

;; Doesn't work for some reason.
;;
;; (set-syntax-from-char #\| #\& 
;;   *dylan-infix-token-readtable* *dylan-infix-token-readtable*)

;; Various aux reader stuff.

(defun read-remaining-infix-extended-token (stream token-string)
  (declare 
    (type stream stream)
    (ignore token-string))
  (fixup-cond (stream)
    ((> (length token-string) 2)
      (read-preserving-whitespace stream))
    (t )))

(defun read-infix-string (stream)
  (declare (type stream stream))
  (let ((contents
          (loop for char = (peek-char nil stream)
                until (eql char #\")
                collect (read-infix-character stream)
                finally (read-char stream))))
    (map 'string 'identity contents)))

(defun read-infix-character (stream)
  (declare (type stream stream))
  (let ((next (read-char stream)))
    (case next
      ((#\\) 
         (read-infix-escape-character stream))
      ((#\newline)
         (notice-newline stream next)
         next)
      (otherwise 
         next))))

(defun read-infix-escape-character (stream)
  (declare (type stream stream))
  (let ((next (read-char stream)))
    (case next
      ((#\a) #\Bell)
      ((#\b) #\Backspace)
      ((#\e) #\Escape)
      ((#\f) #\Formfeed)
      ((#\n) #\Newline)
      ((#\r) #\Return)
      ((#\t) #\Tab)
      ((#\0) #\Null)
      (otherwise next))))

(defun read-infix-line-comment (stream)
  (declare (type stream stream))
  (do ((char (read-char stream nil nil) (read-char stream nil nil)))
      ((or (not char) (eql char #\newline))
         (notice-newline stream char))))

(defun read-infix-area-comment (stream)
  (declare (type stream stream))
  (do ((char (read-char stream nil nil) (read-char stream nil nil)))
      ((eq char nil)
         (reader-error "Area comment ended with eof"))
    (case char
      ((#\newline)
        (notice-newline stream char))
      ((#\*)
        (when (eql (peek-char nil stream nil nil) #\/)
          (progn
            (read-char stream)
            (return-from read-infix-area-comment))))
      ((#\/)
        (let ((next (peek-char nil stream nil nil)))
	  (case next
	    ((#\/)
              (read-char stream)
	      (read-infix-line-comment stream))
	    ((#\*)
              (read-char stream)
	      (read-infix-area-comment stream))))))))

(defun read-until-nan (stream)
  (let ((chars
          (loop for char = (peek-char nil stream nil nil)
                while (and char
                          (or (digit-char-p char)
			      (member char '(#\. #\e #\E #\d #\D))))
                do (read-char stream)
                collect char)))
    (coerce chars 'string)))

(defvar *the-keyword-package* (find-package 'keyword))

(defun read-infix-token (stream)
  (let ((*readtable* *dylan-infix-token-readtable*))
    (with-reader-values (value type text) (read-preserving-whitespace stream)
      (if type
        (values type value text)
        (etypecase value
          (number
            ;; Fixup: a number followed by a decimal point is a number
            (fixup-case (stream)
              ((#\.)
                (let ((point+ (read-until-nan stream)))
		  (let ((*readtable* system::*std-lisp-readtable*))
                    (token-values '<literal>
                      (read-from-string
                        (format nil "~a~a" value point+))))))
              (otherwise
                (token-values '<literal> value))))
          (symbol
            ;; Fixup: a symbol followed by a colon is a keyword
	   (fixup-case (stream)
             ((#\:)
               (read-char stream)
               (fixup-cond (stream)
                 ((or (alphanumericp next-char) (eql next-char #\*))
		   (token-values '<symbol>
                     (make-name-in-context
                       (concatenate 'string 
                         (symbol-name value)
                         ":"
                         (symbol-name (read-preserving-whitespace stream))))))
                 (t
	           (token-values '<keyword> 
			         (intern value *the-keyword-package*)))))
	     (otherwise
	       (classify value)))))))))

;; Patch the infix tokenizer...

;; Shadow calls into the tokeniser
(defun dylan+dylan/infix-reader::tokenize-infix (stream)
  (read-infix-token stream))
(defparameter dylan+dylan/infix-reader::tokenize-infix
    #'dylan+dylan/infix-reader::tokenize-infix)

;; Shadow calls that register new macro words
(defun dylan+dylan/infix-reader::register-infix-macro-word (word type)
  (register-word word type))
(defparameter dylan+dylan/infix-reader::register-infix-macro-word
    #'dylan+dylan/infix-reader::register-infix-macro-word)

(defun dylan+dylan/infix-reader::token-values (class value)
  (token-values class value))
(defparameter dylan+dylan/infix-reader::token-values
    #'dylan+dylan/infix-reader::token-values)

;; Used only for reclassification by the macro system. Escaped binary
;; operators like \+ and \* must not get reclassified back to operator
;; status, losing the escape.

(defun dylan+dylan/infix-reader::tokenize-symbol (name)
  (multiple-value-bind (type value string)
      (classify (intern name *the-dylan-package*))
    (case type
      ((<unary-operator> <binary-operator> 
        <minus> <binds> <not> <var-singleton-sep> <becomes>)
         (values '<symbol> value string))
      (otherwise
        (values type value string)))))
  
(defparameter dylan+dylan/infix-reader::tokenize-symbol
    #'dylan+dylan/infix-reader::tokenize-symbol)

;; Standard words.

(dolist (word '(seal))
  (core-register-word word '<seal-word>))

(dolist (word '(begin case))
  (core-register-word word '<simple-begin-word>))

(dolist (word '(if unless until while select))
  (core-register-word word '<expr-begin-word>))

(dolist (word '(block for))
  (core-register-word word '<details-begin-word>))

(dolist (word '(afterwards cleanup else finally))
  (core-register-word word '<simple-intermediate-word>))

(dolist (word '(elseif))
  (core-register-word word '<expr-intermediate-word>))

(dolist (word '(exception))
  (core-register-word word '<details-intermediate-word>))

(dolist (word '(constant variable class module library))
  (core-register-word word '<defining-word>))

;; Emulator setup. Uncomment to get an approximation to modular macro words.

#|

(setq *dylan-register-word* 
  #'(lambda (word type)
      (funcall 
        (dylan-resolve define-word-type-in-module :module-name internal)
        (current-module)
	word type)))

(setq *dylan-classify-word* 
  #'(lambda (word)
      (funcall 
        (dylan-resolve word-type-in-module :module-name internal)
        (current-module)
        word)))

|#

;; Newlines can appear:
;;
;;   Between tokens.
;;   Within strings.
;;   Within characters.
;;   Within comments.

(defvar *line-count* 0)

(set-macro-character #\newline 'notice-newline nil
  *dylan-infix-token-readtable*)

(defun notice-newline (stream char)
  (incf *line-count*)
  (values))

;; eof
