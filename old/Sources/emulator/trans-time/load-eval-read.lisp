;; The reader used at load/eval time.

;; This implements a read table for use in reading Dylan files, with support
;; for parsing the header section. The roadblock will be set up such that when
;; the first character is read, the read is dispatched to the header parser,
;; while subsequent reads are passed to the "load-eval" readtable.
;;
;; To support this, a special must be set up prior to the read flagging
;; whether it's the first read or not. Nasty.

(in-package :dylan)

;; Translating readtable registry.

(defvar *translating-readtables* (make-hash-table :test #'eq))

(defmethod language-name ((name string))
  (intern (string-upcase name) *the-dylan-package*))

(defmethod language-name ((name symbol))
  name)

(defun find-translating-readtable (name)
  (or (gethash (language-name name) *translating-readtables*)
      (error "No translating readtable found under name ~s" name)))

(defun (setf find-translating-readtable) (readtable name)
  (setf (gethash (language-name name) *translating-readtables*) readtable))

(defun register-translating-readtable (name readtable)
  (setf (find-translating-readtable (language-name name)) readtable))

(defvar *dylan-reader-state* :read-expression) ;; or :read-header

(defparameter *dylan-load-eval-readtable* 
  (let ((table (copy-readtable *dylan-readtable*)))

    (labels

      ((character-hook (s c)
	 (unread-char c s)
         (ecase *dylan-reader-state*
           ((:read-header)
             (setq *dylan-reader-state* :read-expression)
             `(begin ,(read-dylan-file-header-dispatching s)))
           ((:read-expression)
	     (let ((*readtable* *dylan-readtable*)
                   (*package* (find-package :dylan)))
	       (let* ((dyl (read s t nil nil)))
	         (if (dylan-defining-form-p dyl)
                   dyl
	           `(begin ,dyl))))))))

      (dotimes (i 128)
        (let ((c (character i)))
          (unless (member c '
                          (#\space #\tab #\newline #\; #\# #\Page)
                          :test #'eql)
            (set-macro-character (character i) #'character-hook nil table))))

      (set-dispatch-macro-character #\# #\+ '+-reader table)
      (set-dispatch-macro-character #\# #\- '--reader table)

      table)))

(register-translating-readtable 'prefix-dylan *dylan-load-eval-readtable*)

;; eof
