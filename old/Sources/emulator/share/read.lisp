;;
;;; The Dylan reader.
;;;
;;;   The standard Dylan readtable and reader functions for its use
;;;   are defined here. The only readable structures in Dylan that are
;;;   not mirrored directly in CL are #t and #f and these are defined
;;;   here for convenience' sake.
;;;
;;;   Exports: (DYLAN-READ &optional <stream>)
;;;            *DYLAN-READ-TABLE*
;;;            
;;;            *DYLAN-CANONICAL-TRUE*
;;;            *DYLAN-CANONICAL-FALSE*
;;;
;;

(in-package :dylan)

;;; Dylan booleans:

(when (not (boundp '*dylan-canonical-true*))

  (defstruct (dylan-boolean 
              (:print-function %print-dylan-boolean))
    name)

  (defun %print-dylan-boolean (b s d)
    (declare (ignore d))
    (format s "~a" (dylan-boolean-name b))
    b)

  (defvar *dylan-canonical-true*  (make-dylan-boolean :name "#t"))
  (defvar *dylan-canonical-false* (make-dylan-boolean :name "#f"))

  (defmethod make-load-form ((bool dylan-boolean))
    (if (eq bool *dylan-canonical-true*)
      '*dylan-canonical-true*
      '*dylan-canonical-false*))

)

;;; The standard Dylan reader:

(defun reader-error (string &rest args)
  (error 'conditions:simple-reader-error 
         :format-string    string
         :format-arguments args))

(defun terminatorp (char)
  (case char
    ((#\space #\newline #\tab #\) #\() t)
    (otherwise                         nil)))

;;; This is a nasty, dirty hack, not portable etc. but it is a cheap way 
;;; of getting a dylan reader. Included are hacks to allow the reading
;;; of the token |...| and symbols of the form a:b, which are read as
;;; the appropriate CL packaged symbol for now.

(defparameter *dylan-readtable* (copy-readtable))

#-:harlequin-pc-lisp
(defadvice (ccl::read-token dylan-read-token :around) 
  (stream firstchar %reader-state% &optional hash-colon)

  (cond
    ((not (eq *readtable* *dylan-readtable*))
      (call-next-advice stream firstchar %reader-state% hash-colon))
    ((char= firstchar #\.)
      (let ((dot2 (peek-char nil stream nil nil t)))
        (if (and dot2 (char= (peek-char nil stream t nil t) #\.))
          (progn
            (read-char stream t nil t)
	    (read-char stream t nil t)
	    'dylan::|...|)
          (call-next-advice stream firstchar %reader-state% hash-colon))))
    (t
      (let* ((result 
	       (call-next-advice stream firstchar %reader-state% hash-colon))
             (next (peek-char nil stream nil nil t)))
	(case next
	  ((#\:)
	    (when (symbolp result)
	      ;; just eat the colon
	      (read-char stream t nil t)
	      (let ((next (peek-char nil stream t nil t)))
                (if (terminatorp next)
		  (setq result 
                        (intern (symbol-name result) 
				(find-package 'keyword)))
		  (let ((next-form 
                          (read-preserving-whitespace stream t nil t)))
		    (setq result 
                          (intern next-form
				  (or (find-package result)
				      (reader-error "Cannot find package ~s"
                                                    result))))))))))
	result))))

;; Feature handling:

(defparameter *dylan-features* 
  '(:dylan 
    :functional-dylan
    :harlequin-lispworks-dylan-translator))

(defun add-feature (feature)
  (pushnew feature *dylan-features*))

(defun remove-feature (feature)
  (setq *dylan-features* (remove feature *dylan-features*)))

(defun feature-present-p (feature)
  (member feature *dylan-features*))

(defun +-reader (stream char arg)
  (declare (ignore arg char))
  (let* ((*readtable* *dylan-readtable*)
         (feature (read stream t nil t))
	 (next-form (read stream t nil t)))
    (if (feature-present-p feature)
	next-form
	(values))))
  
(defun --reader (stream char arg)
  (declare (ignore arg char))
  (let* ((*readtable* *dylan-readtable*)
         (feature (read stream t nil t))
	 (next-form (read stream t nil t)))
    (if (feature-present-p feature)
	(values)
	next-form)))

;; Token translation:

(defun hash-token-reader (stream char arg)
  (unread-char char stream)
  (let* ((*readtable* *dylan-readtable*)
         (obj (read-preserving-whitespace stream t nil t)))
    (unless (symbolp obj)
      (reader-error "Illegal token ~s" obj))
    (case obj
      ((f)       *dylan-canonical-false*)
      ((t)       *dylan-canonical-true*)
      ((next)   '&next)
      ((rest)   '&rest)
      ((more)   '&more)
      ((key)    '&key)
      ((all-keys) '&all-keys)
      ((values) '&return-types) ;; should be &values
      (otherwise
        (reader-error "Illegal token ~s" obj)))))
    
(defun hash-colon-reader (stream char arg)
  (declare (ignore arg))
  '&return-types)

(defun read-dylan-colon (stream char)
  (declare (ignore stream char))
  (let ((val (read stream t nil t)))
    (cond
      ((symbolp val)
        (intern (symbol-name val) (find-package :keyword)))
      (t
        (warn "misplaced colon")
        val))))

(defun read-up-arrow (stream char)
  (declare (ignore char))
  (list 'dylan::%immediate (read stream t nil t)))

;; Quotation:

(defvar *comma* 'unquote)
(defvar *comma-atsign* 'unquote-splicing)
(defvar *comma-dot* 'unquote-dot)

(defun read-quote (stream char)
  (declare (ignore char))
  (list 'quote (read stream t nil t)))

(defun read-back-quote (stream char)
  (declare (ignore char))
  (list 'quasi-quote (read stream t nil t)))

(defun read-comma (stream char)
  (declare (ignore char))
  (let ((next (read-char stream)))
    (case next
      (#\@ (list *comma-atsign* (read stream t nil t)))
      (#\. (list *comma-dot*    (read stream t nil t)))
      (t   (unread-char next stream)
           (list *comma*        (read stream t nil t))))))

;; Expression quotation.

(defun expression-reader (stream char arg)
  (let ((val (read stream t nil t)))
    `(expression ,val)))

;; Hack the readtable.

(eval-when (:execute :load-toplevel)
  (set-macro-character #\: 'read-dylan-colon nil *dylan-readtable*)
  (set-macro-character #\^ 'read-up-arrow    nil *dylan-readtable*)
  (set-macro-character #\' 'read-quote      nil *dylan-readtable*)
  (set-macro-character #\` 'read-back-quote nil *dylan-readtable*)
  (set-macro-character #\, 'read-comma      nil *dylan-readtable*)
  (set-dispatch-macro-character #\# #\: 'hash-colon-reader *dylan-readtable*)
  (set-dispatch-macro-character #\# #\+ '+-reader *dylan-readtable*)
  (set-dispatch-macro-character #\# #\- '--reader *dylan-readtable*)
  (set-dispatch-macro-character #\# #\` 'expression-reader *dylan-readtable*)
  (dolist (c '(#\n #\r #\m #\a #\k #\v #\t #\f))
    (set-dispatch-macro-character 
      #\# c 'hash-token-reader *dylan-readtable*)
    (set-dispatch-macro-character 
      #\# (char-upcase c) 'hash-token-reader *dylan-readtable*))
)

;; The Dylan reader sets up both the correct readtable as well as the
;; correct package for Dylan's symbols.

(defun dylan-read-from-string (string)
  (with-input-from-string (stream string)
    (dylan-read stream)))

(defun dylan-read (&rest stuff)
  (let ((*readtable* *dylan-readtable*)
	(*package*   *the-dylan-package*))
    (apply #'read stuff)))

(defun dylan-packaged-read (&key (stream *standard-input*)
                                 (features *dylan-features*)
                                 (eof-value *dylan-canonical-false*))
  (let ((*dylan-features* features))
    (dylan-read stream nil eof-value)))

;; eof


