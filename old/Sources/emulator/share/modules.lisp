;;
;;; Modules.
;;;
;;;   Variable structs contain the name under which the variable was
;;;   defined, its home module and its internal name - ie its
;;;   canonical symbol binding within the package environment.
;;;
;;;   Module variables are not resolved until load time and then go
;;;   through symbol-value, so things are likely to be slow but at
;;;   least updates will be tracked.
;;;
;;

(in-package :dylan)

;;; Exported to Dylan.

(defun make-package-name (library module)
  (concatenate 'string 
               (string 'DYLAN)
               "+"
               (string library)
               "/"
               (string module)))

(defun make-library-module-package (library module)
  (let ((name (make-package-name library module)))
    (or (find-package name)
        (let ((pkg (make-package name :use '())))
          ;; Inspector support, not strictly Kosher --Tim
          (import '($ $$ $$$) pkg)
          pkg))))

;;; Exported protocol to be filled-in by Dylan.

(defgeneric canonicalize-module-variable (module name))
(defgeneric find-translator-module (name &key &allow-other-keys))

(defgeneric module-package (module))
(defgeneric module-name (module))

;; Boot methods.

(defparameter *boot-module*
  (make-library-module-package 'dylan 'internal))

(defmethod canonicalize-module-variable (module name)
  (values module name))

(defmethod find-translator-module (name &key &allow-other-keys)
  :boot-module)

(defmethod module-package ((name (eql :boot-module)))
  *boot-module*)

(defmethod module-name ((name (eql :boot-module)))
  'internal)

;;; State.

(defvar *current-module* :boot-module)

(defun current-module ()
  (typecase *current-module*
    (symbol
      (if (keywordp *current-module*) *current-module*
        (find-translator-module *current-module*)))
    (otherwise *current-module*)))

(defun (setf current-module) (module)
  (setq *current-module* module))

(defun current-module-name ()
  (module-name (current-module)))

;;; Libraries.

(defvar *current-library* nil)

(defun current-library-name ()
  *current-library*)

(defun set-current-library-by-name (name)
  (setq *current-library* name))

;;; Modules.

;;; Variable name creation and manipulation.

(defun dylan-variable-p (var)
  (or (symbolp var)
      (and (consp var)
           (= (length var) 2)
           (eq 'setter (first var))
           (dylan-variable-p (second var)))))

(defun make-setter-name (var)
  (let ((subvar (dylan-variable-standard-form var)))
    (intern (concatenate 'string 
			 (symbol-name subvar)
			 (symbol-name '-setter)))))

(defun dylan-variable-standard-form (var)
  (cond
    ((symbolp var) 
      ;; HACK!!!
      (case var
        ((t) 'something-which-used-to-be-t)
        (otherwise var)))
    ((and (consp var)
          (= (length var) 2)
          (eq 'setter (first var)))
      (format t "Warning: old style setter encountered - ~s~%" var)
      (make-setter-name (second var)))
    (t
      (translate-error "Bad Dylan variable: ~s" var))))

;;; Module environment:

#|
(defvar *dylan-modules* (make-hash-table))

(defstruct (module
            (:print-function %print-module))
  name
  package
  (variables          (make-hash-table :test 'eq))
  (imported-variables (make-hash-table :test 'eq))
  (exported-variables (make-hash-table :test 'eq)))

(defun %print-module (m s d)
  (declare (ignore d))
  (format s "#<Module ~s>" (module-name m)))

(defun make-packaged-module (name)
  (let ((package
          (make-library-module-package (current-library-name) name)))
    (import '($ $$ $$$) package) ;Inspector support, not strictly Kosher --Tim
    (make-module :name name
		 :package package)))

(defstruct (var-entry
            (:print-function %print-var-entry))
  type
  name
  module
  storage
  read-only)

(defun %print-var-entry (v s d)
  (declare (ignore d))
  (format s "#<Variable ~s @ ~s -> ~s>" 
          (var-entry-name v) 
          (module-name (var-entry-module v))
          (var-entry-storage v))
  v)



(defun %get-current-module ()
  *current-module*)

;;; needs to exist in run-time
(defun %get-module (name)
  (gethash name *dylan-modules* ))

(defun %set-current-module (module)
  (setq *current-module* module))

(defun set-current-module (name)
  (let ((module (%get-module name)))
    (unless module 
      (setq module (setf (gethash name *dylan-modules*) 
                         (make-packaged-module name))))
    (%set-current-module module)))

(defun get-module-variable (var-name module)
  (unless (symbolp var-name)
    (setq var-name (dylan-variable-standard-form var-name)))
  (or (gethash var-name (module-imported-variables module))
      (gethash var-name (module-variables module))))

(defun %get-internal-variable-name (variable-name 
                                    &optional (module (%get-current-module)))
  (let ((entry (get-module-variable variable-name module)))
    (if entry (var-entry-storage entry)
      (var-entry-storage (%add-variable variable-name module)))))

;;; needs to exist in runtime
(defun %runtime-get-variable-internal-name (name module)
  (%get-internal-variable-name name module))

(defun %runtime-resolve-name (name)
  (%runtime-get-variable-internal-name name (%get-current-module)))

(defun %add-variable (variable-name 
                      &optional (module    (%get-current-module)) 
		                (read-only nil)
                                (type      :value)
                                internal)
  (unless internal 
    (setq internal (%make-internal-variable-name variable-name)))

  (unless (symbolp variable-name)
    (setq variable-name (dylan-variable-standard-form variable-name)))

  (let ((entry (get-module-variable variable-name module)))
    (setf (gethash variable-name (module-variables module))
	  (or entry 
	      (make-var-entry 
	        :name (symbol-name variable-name)
		:module module
		:storage internal
		:read-only read-only
		:type type)))))

(defun %make-internal-variable-name (name &optional (module (%get-current-module)))
  (intern (symbol-name (dylan-variable-standard-form name))
	  (module-package module)))

|#
    
(defun %runtime-set-variable (dylan-name lisp-name value)
  (setf (symbol-value lisp-name) value)
  (setf (symbol-function lisp-name) 
        (if (functionp value) value
          #'(lambda (&rest args)
	      (error 
	        "Tried to call non-function ~s (bound to ~s) with arguments ~s"
		value dylan-name args))))
  value)

(defun %define-variable (name variable value)
  (when (boundp variable)
    (warn "~s was already defined - setting anyway" 
	  name (symbol-value variable)))
  (%runtime-set-variable variable variable value))

(defun %get-dylan-symbol (name) 
  (intern name))

;;; Primitive functions

(defvar *primitive-functions* (make-hash-table :test #'eq))

(defun get-resolved-primitive-function (resolved-name)
  (gethash resolved-name *primitive-functions*))

(defun set-primitive-function (name f &key module-name)
  (setf (gethash (%runtime-resolve-name name :module-name module-name)
                 *primitive-functions*) f))

;;; Code generation handles:

(defun gen-module-variable-ref (var)
  `(dylan-resolve ,var :module-name ,(current-module-name)))
#|
(defun gen-module-variable-set (var val)
  (let ((entry (get-module-variable var (%get-current-module))))
    (if (and entry (eq (var-entry-type entry) :function))
      (error "cannot assign to imported Common Lisp function ~s" var)
      `(setf (dylan-resolve ,var) ,val))))
|#

(defun gen-module-variable-set (var val)
  (let ((name (%runtime-resolve-name var)))
    `(%runtime-set-variable ',var ',name ,val)))

(defun gen-module-variable-setter-ref (var)
  `(dylan-resolve ,var :module-name ,(current-module-name)))

(defun gen-called-module-variable-expr (var args &optional resolved)
  ;; `((dylan-resolve-called ,var) ,@args)
  (let ((resolved-name (or resolved (%runtime-resolve-name var))))
    `(,(or (get-resolved-primitive-function resolved-name)
           resolved-name)
       ,@args)))

;;; Pick a module, any module...

;(set-current-library-by-name 'dylan)
;(set-current-module 'internal)

(defun set-module-context-by-name (name)
  (setf (current-module) (find-translator-module name)))

;; Patches.

(defun %runtime-resolve-name (name &key module-name)
  (multiple-value-bind 
      (home name) 
        (canonicalize-module-variable
          (if module-name (find-translator-module module-name)
            (current-module))
          name)
     (intern name (module-package home))))

(defun %runtime-defined-p (name &key module-name)
  (let ((symbol (%runtime-resolve-name name :module-name module-name)))
    (or (boundp symbol) (fboundp symbol))))

;; A different current module binding per-process.

(push '(*current-module* . *current-module*) mp:*process-initial-bindings*)
;(push '(*current-library* . *current-library*) mp:*process-initial-bindings*)

;; eof
