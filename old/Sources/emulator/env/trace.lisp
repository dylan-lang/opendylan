;; Tracing for Dylan functions/methods/generics.

(in-package :compiler)

;; Don't know what most of this junk does - just pattern matching
;; from the compiler's trace code.

(define-definition-spec dylan::define
  :handler 'dylan::define-handler
  :symbol-attributes '(:variable)
  :symbol-predicate #'(lambda (x) (and (boundp x) (not (constantp x))))
  :generator 
    #'(lambda (x) 
        `((dylan::define ,x))))

(define-definition-spec dylan::define-class
  :handler 'dylan::define-handler
  :symbol-attributes '(:variable)
  :symbol-predicate #'(lambda (x) (and (boundp x) (not (constantp x))))
  :generator 
    #'(lambda (x) 
        `((dylan::define-class ,x))))

(define-definition-spec dylan::define-generic
   :handler 'dylan::define-generic-handler
   :symbol-attributes '(:function)
   :symbol-predicate #'(lambda (x) 
			 (and (fboundp x)
			      (clos::generic-function-p
			       (symbol-function x))))
   :generator #'(lambda (x)
                  `(,x
	            ,@(let ((function (fdefinition x)))
	                (when (typep function 'generic-function)
	                  (loop for method in (generic-function-methods function)
				when (typep method 'standard-reader-method)
				collect `(defclass ,(class-name (first (method-specializers method))))))))))

(define-definition-spec dylan::define-method
  :handler 'dylan::define-method-handler
  :symbol-predicate #'(lambda (x) (declare (ignore x)) nil)
;;  :object-predicate #'(lambda (x) (typep x 'method))
  :generator 'generate-dspec-for-method
  )

(defun dylan::define-handler (dspec op &optional arg)
  (let* ((symbol (dylan::%runtime-resolve-name (second dspec))))
    (case op
      (:definition
        (symbol-value symbol))
      (:define
        (setf (symbol-value symbol) arg))
      (:definedp
        t)
      (t
        (default-definition-spec-handler dspec op arg)))))

(defun dylan::define-generic-handler (dspec op &optional arg)
  (dylan::define-handler dspec op arg))

(defun dylan::define-method-handler (dspec op &optional arg)
  (let ((definition-error-flagged nil))
    (prog1 
      (conditions::ignore-errors
        (let* ((name (second dspec))
               (gf (symbol-value name))) ;; (dylan::\%runtime-resolve-name name))))
	 (multiple-value-bind
	  (qualifiers specializers ignored-body)
	  (clos::parse-defmethod 
	   gf
	   (second dspec)
	   (cddr dspec))
	  (declare (ignore ignored-body))
	  (let ((method 
                  (clos::find-method gf 
				     qualifiers
				     (loop for s in specializers
					   collect 
                                             (let ((val (dylan::dylan-eval s)))
                                               (if (eq val 
                                                       (find-class
                                                         'standard-object))
                                                 (find-class 't)
                                                 val)))
				     nil)))
	    (case op
              (:supports
	       (or (member arg '(:definition :define :definedp :validate
                                 :undefine :undefining-form :object))
		   (default-definition-spec-handler dspec :supports arg)))
	      (:definition
	       (if (null method)
		   (progn (setq definition-error-flagged t)
		     nil)
		 (clos::method-function method)))
	      (:define 
	       (if method		; to fix untracing methods. davidp
		   (progn (setf (clos::method-function method) arg)
		     (clos::flush-method-from-combined-method-cache gf method)
		     (clos::invalidate-generic-function gf))
		 (progn (setq definition-error-flagged t) nil)))
	      (:definedp method)
	      (:validate (not (not method)))
	      (:undefine (clos::remove-method gf method)) ;MJS 10/04/91: Maybe
	      (:undefining-form
	       `(clos::undefmethod
                 ,name
                 ,@qualifiers
                 ,(loop for s in specializers
			collect (if (clos::eql-specializer-p s)
				    `(eql ', (second s))
				  s))))
              (:object method)
	      (t (default-definition-spec-handler dspec op arg)))))))
      ; make this a warning, not an error.   davidp
      (when definition-error-flagged
	(warn "Method does not exist: ~S" dspec)))))

;; Temp hack!!!

(defadvice (compiler::who-calls dylan :around) (spec)
  (call-next-advice
   (if (not (consp spec)) spec
     (case (car spec)
       ((dylan::define-method dylan::define)
	 (dylan::%runtime-resolve-name 
           (cadr spec) :module-name (editor::current-buffer-module)))
       (otherwise
	 spec)))))

;; eof
