;; Batch compilation operations:

(in-package :dylan)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defmacro with-saved-global-state (&rest forms)
    `(let ((*package-before-listen*   *package*)
           (*readtable-before-listen* *readtable*))
       ,@forms))

  (defmacro using-saved-global-state (&rest forms)
    `(progn
       (let ((*package*   *package-before-listen*)
             (*readtable* *readtable-before-listen*))
         ,@forms)))

)

(defparameter *std-readtable* *readtable*)
(defparameter *global-state-stack* '())

(defun peek-pushed-readtable ()
  (if (null *global-state-stack*)
    *std-readtable*
    (second (first *global-state-stack*))))

(defun push-global-state ()
  (push (list *package* *readtable*) *global-state-stack*))

(defun pop-global-state ()
  (let ((vals (pop *global-state-stack*)))
    (setq *package*   (first vals))
    (setq *readtable* (second vals))))

(defun load-dylan-file (file &rest args)
  (reading-as-translated-dylan (:header t)
    (unless (apply #'load file :if-does-not-exist nil args)
      (apply #'load 
             (merge-pathnames (make-pathname :type "dyl") (pathname file))))))

(defvar *eos* (list 'end-of-stream))

(defun translate-dylan-file (filename &key output-file)
  (let ((*print-circle* t))
    (with-open-file (dylan filename :direction :input)
      (with-open-file (lisp output-file :direction :output 
			                :if-exists :supersede)
	(print '(in-package :dylan) lisp)
        (do ((form (dylan-read dylan nil *eos*) (dylan-read dylan nil *eos*)))
            ((eq *eos* form))
          (write (macroexpand (translate-dylan-expr form))
		 :stream lisp))))
    t))

#+comment
(defun compile-dylan-file (file &rest args)
  (let* ((dylan-path (pathname file))
         (lisp-path  (merge-pathnames (make-pathname :type "lisp") dylan-path)))
    (translate-dylan-file dylan-path :output-file lisp-path)
    (apply #'compile-file lisp-path args)))

(defun compile-dylan-file (file &rest args)
  (let ((orig-pkg *package*)
        (orig-rt  *readtable*))
    (handler-bind
      ((condition 
         #'(lambda (ignore)
             (setq *package*   orig-pkg)
             (setq *readtable* orig-rt))))
      (let* ((dylan-path (pathname file))
             (lisp-path  (merge-pathnames (make-pathname :type "lisp") dylan-path)))
        (reading-as-translated-dylan (:header t)
          (apply #'compile-file dylan-path (apply #'args-for-compile-file args)))))))

(defun args-for-compile-file (&rest args &key target-machine &allow-other-keys)
  (let ((host-target (compiler:target-machine-info-keyword
                      (compiler:host-target-machine ))))
    (if (eq target-machine host-target)
        (list* :target-machine :dlt args)
      ;; We'd better not mess with a carefully set target
      args)))

(defun dylan-eval (expr &key module-name)
  (let ((*current-module*
	  (if module-name (find-translator-module module-name)
	    *current-module*)))
    (declare (special *current-module*))
    (eval 
      (translate-dylan-expr expr))))

(defun enter-dylan ()
  (push-global-state)
  (setq *package*   (find-package :dylan))
  (setq *readtable* *dylan-load-eval-readtable*)
  (format t "~&Entering Dylan.~%")
  (values))

(defun enter-dylan ()
  (pop-global-state)
  (format t "~&Leaving Dylan.~%")
  (values))

;; Header fudges.

(defparameter *non-file-header-characters* '(#\# #\; #\())

(defun read-dylan-file-header-values (s)
  (let ((name (%runtime-resolve-name 
                 'read-file-header-values :module-name 'internal)))
    (if (fboundp name)
      (funcall name s)
      (do ((char (read-char s nil nil) (read-char s nil nil)))
          ((or (not char) (not (sys:whitespace-char-p char)))
           (when char
             (unread-char char s)
             (when (not (member char *non-file-header-characters*))
               (do ((line (read-line s) (read-line s)))
                   ((every 'sys:whitespace-char-p line))
                 (format t ";;; ~s~%" line))))
	   (values "prefix-dylan" "dylan-user"))))))

(defun read-dylan-file-header (s)
  (multiple-value-bind (language module) (read-dylan-file-header-values s)
    `(in-code-context ,module ,language)))

(defun read-dylan-file-header-dispatching (s)
  (multiple-value-bind (language module) (read-dylan-file-header-values s)
    (setq *readtable* (find-translating-readtable language))
    `(in-code-context ,module ,language)))

;; Dylan files:

(generic-env:register-file-type 'dylan-file
  :source-extensions '("dylan" "dyl")
  :object-extensions (list compiler:*fasl-extension-string*)
  :compiler      'compile-dylan-file
  :source-loader 'load-dylan-file    
  :object-loader 'load-dylan-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This file allows some variable rebindings around the compile-file again restart. 
;;; This is necessary to make the readtable mechanism switch between header and body
;;; modes.


(defvar *compiler-symbols-to-rebind*
    '(dylan::*dylan-reader-state* *readtable*))

(defvar *compiler-values-for-rebinding*
    (list :read-header dylan::*dylan-load-eval-readtable*))

(in-package "COMPILER")


;;; Set up the Dylan code generator model

#-:harlequin-pc-lisp
(let* ((host-target :harlequin-unix-lisp)
       (host-reverse-p (target-machine-reverse-p host-target))
       (host-switch-fn (target-machine-switcher host-target)))
  (setf (target-machine-info :Dylan-Lisp-Translator)
        (make-target-machine-info
         :aliases '(:dlt :Dylan-Lisp-Translator)
         :fasl-extensions-fn #'(lambda () 
                                 (target-machine-fasl-extentions host-target))
         :reversep host-reverse-p
         :cg-switch-fn #'(lambda ()
                           (funcall host-switch-fn)
			   (values dylan::*compiler-symbols-to-rebind*
				   dylan::*compiler-values-for-rebinding*)))))



;; eof
