(in-package dylan)

(defvar *command-table* (make-hash-table))

(defun find-command (name)
  (gethash name *command-table*))

(defun register-command (name f)
  (setf (gethash name *command-table*) f))

(defun maybe-process-command (keyword args)
  (let ((f (find-command keyword)))
    (when f
      (apply f args)
      t)))

(defmacro define-dylan-command (keywords args &rest body)
  (unless (consp keywords)
    (setq keywords (list keywords)))
  `(let ((command #'(lambda ,args ,@body)))
     ,@(mapcar
         #'(lambda (keyword)
             `(register-command ,keyword command))
        keywords)))

;; Standard commands.

(define-dylan-command (:in :enter) (&optional module-name)
  (if module-name
    (progn
      (unless (find-translator-module module-name :default nil)
	(ensure-library module-name))
      (set-module-context-by-name module-name)
      (format t "~&Now in the ~s module~%" module-name))
    (format t "~&Currently in the ~s module~%" (current-module-name))))

(define-dylan-command :compile-file (file-name)
  (generic-env:generic-compile file-name :load t))

(define-dylan-command :load-file (file-name)
  (generic-env:generic-load file-name))

(define-dylan-command :recompile (library-name)
  (compile-library library-name)
  (values))

(define-dylan-command :require (library-name)
  (ensure-library library-name)
  (values))

(define-dylan-command :reload (library-name)
  (load-library library-name)
  (values))


(define-dylan-command :undefine (expr)
  (if (not (consp expr)) (eval `(dylan-undefine ,expr))
    (let* ((gf (dylan-interactive-eval (first expr)))
           (gf-specs (mapcar 
                       #'(lambda (expr)
                           (clos::convert-dylan-specializer
                             (dylan-interactive-eval expr)))
                      (rest expr)))
           (method (find-method gf '() gf-specs nil)))
      (if method (remove-method gf method)
        (format t "No matching method found on ~s.~%" gf))
      (values))))

(define-dylan-command :translate (expr)
   (format t "~s~%"
	   (translate-dylan-expr expr '())))

(define-dylan-command :expand (expr)
  (let ((expanded (maybe-expand expr)))
    (when expanded (format t "~s~%"))))

(define-dylan-command :expand* (expr)
  (let ((expanded (maybe-expand* expr)))
    (when expanded (format t "~s~%"))))

(define-dylan-command :trace-macros ()
  (funcall (dylan-resolve trace-macros :module-name infix-reader)))

(define-dylan-command :untrace-macros ()
  (funcall (dylan-resolve untrace-macros :module-name infix-reader)))

;; Expander helpers.

(defun maybe-expand (expr)
  (let ((string
          (funcall
	    (dylan-resolve expanded-macro-call :module-name infix-reader)
            expr)))
    (if string (progn (format t "~a~%" string) (values))
      (if (not (consp expr)) expr
        (let* ((translator (get-dylan-translator (first expr) '()))
               (expander (get-dylan-expander translator)))
          (if expander (apply expander expr) expr))))))

(defun maybe-expand* (expr)
  (let ((expanded (maybe-expand expr)))
    (if (eq expr expanded) expr
      (maybe-expand* expanded))))

;; More debugging commands

(define-dylan-command :which (name)
  (multiple-value-bind (home real-name)
      (canonicalize-module-variable (current-module) name)
    (format t "The binding ~s resolves to ~s in the ~s module"
            name real-name (module-name home))))

(define-dylan-command :trace (name)
  (dylan-trace name))

(define-dylan-command :trace-here (name)
  (dylan-trace name *standard-output*))

(defun dylan-trace (form &optional (stream '*standard-output*) (break nil))
  (eval `(trace (,(dylan-trace-spec form)
                  :trace-output ,stream
                  :break ,break))))

(defun dylan-trace-spec (form)
  (if (consp form)
    (let ((name (first form))
          (types (rest form)))
      `(define-method ,(%runtime-resolve-name name) ,types))
    (%runtime-resolve-name form)))

(define-dylan-command :untrace (&optional name)
   (if name (eval `(untrace ,(dylan-trace-spec name)))
     (untrace)))

(define-dylan-command :break (form)
  (dylan-trace form '*standard-output* t))

(define-dylan-command :break-here (form)
  (dylan-trace form *standard-output* t))

(define-dylan-command :undefined 
    (&optional (module-name (current-module-name)))
  (let* ((module
           (find-translator-module module-name))
         (undefined
	   (loop for sym being each symbol
		 in (module-package (find-translator-module module-name))
		 when (and (and (not (fboundp sym)) (not (boundp sym)))
			   (not (member (symbol-name sym) '("$" "$$" "$$$")
				        :test #'string=)))
		 collect sym)))
    (format t "~&Undefined in the ~a module: " (module-name module))
    (format-english-list t undefined)))

(define-dylan-command :inspect (expr)
  (let ((result (dylan-interactive-eval expr)))
    (cl:inspect result)))

(define-dylan-command :describe (expr)
  (let ((result (dylan-interactive-eval expr)))
    (cl:describe result)))

(define-dylan-command :save (file)
  (let ((*readtable* sys::*std-lisp-readtable*))
    (common-lisp-user::save-emulator-image file)))

(define-dylan-command :bye ()
;;  (let ((*readtable* sys::*std-lisp-readtable*))
    (throw 'bye 'done))

;; Library browsing

(define-dylan-command :loaded ()
  (let ((names (loaded-library-names)))
    (format t "~&Loaded libraries: ")
    (format-english-list t names)
    (format t "~&")))

(define-dylan-command :available ()
  (let ((names (available-library-names)))
    (format t "~&Available libraries: ")
    (format-english-list t names)
    (format t "~&")))

;; I know I could do this with a format string but I don't want to learn
;; how - on principle as much as anything else...

(defun format-english-list (stream list)
  (case (length list)
    ((0) (format stream "none"))
    ((1) (format stream "~a" (first list)))
    (otherwise
      (format stream "~a" (first list))
      (loop for l = (rest list) then (rest l)
            when (consp (rest l)) do
              (format stream ", ~a" (first l))
            else do    
              (format stream " and ~a" (first l))
              (loop-finish)))))


;;----------------------------------------------------------------------------
;; Environment commands
;;----------------------------------------------------------------------------

(defun lisp-environment-started? ()
  clue:*display*)


;;; Browse

(defmethod dylan-browse (object)
  (tools::win-inspect object))

(defmethod dylan-browse ((object generic-function))
  (tools::lispworks-gf-browse :gf object))

(defmethod dylan-browse ((object class))
  (tools::lispworks-class-browse :class object))

(define-dylan-command :browse (expr)
  (let ((result (dylan-interactive-eval expr)))
    (if (lisp-environment-started?)
        (dylan-browse result)
      (cl:inspect result))))


;;; Edit

(defmethod dylan-edit (object)
  (editor:find-definition object))

(defun find-dspec-for-expr (expr)
  (if (symbolp expr)
    (%runtime-resolve-name expr)
    `(define-method ,(%runtime-resolve-name (first expr)) ,(rest expr))))

;; andrewa, 28 Sep 1995 - try to edit the variable named by expr
(define-dylan-command :edit (expr)
  (let ((dspec (find-dspec-for-expr expr)))
    (if (lisp-environment-started?)
        (if dspec
	    (dylan-edit dspec)
          (warn "Cannot determine how to edit ~S" expr))
      (warn "Environment not started, so cannot edit %=" dspec))))


;; eof
