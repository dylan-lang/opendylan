(in-package :dylan)

;; The name to put in dpec/function name declarations

(defun lw-debug-name (name)
  (%runtime-resolve-name name))

;;; The following definers need to be implemented:
;;; - DEFINE
;;; - DEFINE-CLASS (in "define-class.lisp")
;;; - DEFINE-GENERIC-FUNCTION
;;; - DEFINE-METHOD

(defmacro check-variable-name ((name guff) &body body)
  `(if (dylan-variable-p ,name)
       (progn ,@body)
     (define-error 'define "Illegal variable name ~s in ~s" ,name ,guff)))

(defmacro check-typed-variable-name ((name type (spec guff)) &body body)
  `(multiple-value-bind
     (,name ,type) (parse-dylan-variable ,spec)
     ,@body))

;;; DEFINE variable-name initial-value => variable-name - Public
;;; creates a variable accessible thru variable-name in the current module
;;; (thereby hangs a tail - note that the module is implicit - it is maintained
;;; by the environment whatever that may mean). initial-value provides the 
;;; initial  value of the variable. 
;;; variable-name is not evaluated. It must be a symbol or a list of the form 
;;; (setter symbol). 
;;; NB. this last might change.
;;; initial-value is evaluated and the value is stored into the new variable.

(define-dylan-translator define translate-define (form env)
  (destructuring-bind (_ &rest binding) form
    (unless (>= (length binding) 2)
      (translate-error 
        'define
        "~s is not a legal definition - must specify an init value"
        form))
    (let ((bind-spec (butlast binding))
          (init (first (last binding))))
      (cond
        ((and (= (length bind-spec) 1)
	      (consp init)
	      (eq (first init) 'method))
         `(compiler::top-level-form 
              (define ,(lw-debug-name (first bind-spec)))
            (dylan-define ,(first bind-spec)
		          ,(translate-dylan-expr 
			    (destructuring-bind (_ md-vars &rest md-body) init
			      `(method ,md-vars 
				,@(ccl::declare-source-level-debugging-macro)
				(declare 
				 (ccl::lambda-name 
                                   (define 
                                     ,(lw-debug-name (first bind-spec)))))
				,@md-body))
                            env))))
        ((and (= (length bind-spec) 1)
              (consp init)
              (eq (first init) 'macro))
          (translate-dylan-expr 
            `(define-macro ,(first bind-spec)
               (method (_form_)
                 (expression-case _form_ ,@(rest init))))))
        (t
          (multiple-value-bind
            (all-vars vars specs next rest keys) (parse-lambda-list bind-spec)
            (when (or next keys)
              (translate-error
               'define
               "~s is not a legal definition - #key and #next are not allowed"
               form))
            (let ((init 
                   (translate-dylan-expr init env))
		  (var-temps
                   (mapcar #'(lambda (x) (gensym)) vars))
                  (rest-temp
                   (gensym)))
              (multiple-value-bind
                  (binds tests) (gen-type-assertions vars env specs env)
                `(compiler::top-level-form
                     (define ,(lw-debug-name (first vars)))
                  (let (,@binds)
                    (multiple-value-call
                        #'(lambda (&optional
                                   ,@(mapcar
                                      #'(lambda (var)
                                          `(,var *dylan-canonical-false*))
                                      var-temps)
                                   &rest ,(if rest rest-temp (gensym)))
                            ,@(mapcar
                               #'(lambda (var tmp)
                                   `(dylan-define ,var ,tmp))
                               (append vars 
                                       (if rest (list rest)))
                               (append var-temps
                                       (if rest (list rest-temp)))))
                      ,init)
                    ,@tests
                    (values)))))))))))


(define-dylan-translator undefine translate-undefine (form env)
  (declare (ignore env))
  (destructuring-bind (_ variable-name &rest stuff) form
    (check-typed-variable-name (name type (variable-name form))
      `(dylan-undefine ,name))))

(define-dylan-macro define-variable (_ &rest stuff)
  `(define ,@stuff))

(define-dylan-translator 
  define-thread-variable translate-define-thread-variable (form env)
  (destructuring-bind (_ &rest binding) form
    (unless (>= (length binding) 2)
      (translate-error 
        'define-thread-variable
        "~s is not a legal definition - must specify an init value"
        form))
    (let ((bind-spec (butlast binding))
	  (init (first (last binding))))
      (multiple-value-bind
	  (all-vars vars specs next rest keys) (parse-lambda-list bind-spec)
	(when (or next keys)
	  (translate-error
	   'define-thread-variable
	   "~s is not a legal definition - #key and #next are not allowed"
	   form))
	(let ((init 
	       (translate-dylan-expr init env))
	      (var-temps
	       (mapcar #'(lambda (x) (gensym)) vars))
	      (rest-temp
	       (gensym)))
	  (multiple-value-bind
	      (binds tests) (gen-type-assertions vars env specs env)
	    `(compiler::top-level-form
		(define ,(lw-debug-name (first vars)))
	      (let (,@binds)
		(multiple-value-call
		    #'(lambda (&optional
			       ,@(mapcar
				  #'(lambda (var)
				      `(,var *dylan-canonical-false*))
				  var-temps)
			       &rest ,(if rest rest-temp (gensym)))
			,@(mapcar
			   #'(lambda (var tmp)
			       `(let ((sym-name ',(lw-debug-name var)))
                                  (dylan-define ,var ,tmp)
                                  (add-initial-thread-binding sym-name ,tmp)))
			   (append vars 
				   (if rest (list rest)))
			   (append var-temps
				   (if rest (list rest-temp)))))
		  ,init)
		,@tests
		(values)))))))))
        

;;; This is how we record thread local variables. The symbol and its initial value
;;; get pushed onto the list of bindings to perform at process creation time - which
;;; implements something slightly less than the specification of the threads library.
;;; This code should probably live in the threads library implementation - but it
;;; happens to be convenient to put it here.

(defun add-initial-thread-binding (sym val)
  (let ((existing (assoc sym mp:*process-initial-bindings*))
        (qval `',val))
    (if existing
        (setf (cdr existing) qval)
      (pushnew (cons sym qval) mp:*process-initial-bindings*))
    (values)))

;;; DEFINE-GENERIC-FUNCTION name parameter-list key1 option1 key2 option2 ...
;;;=> name name is a variable name. If this variable is bound in the current
;;;module then it must contain a generic function. If it contains a
;;;generic-function then that GF must be consistent with the call to
;;;DEFINE-GENERIC-FUNCTION. If name is unbound in the current module then a
;;;read-only variable is created and bound to a GF object as 
;;; specified by parameter-list and 
;;; the options.

(define-dylan-translator 
  define-generic-function translate-define-generic-function (form env)

  (destructuring-bind (_ name parameter-list &rest key-args) form
    (check-variable-name
      (name form)
      (let ((def-form 
              (%define-generic-function name parameter-list key-args env)))
        def-form))))

(defun %define-generic-function (name params keys env) 
  (declare (ignore keys))
  (multiple-value-bind 
      (all-binds vars specs next rest keys) (parse-lambda-list params)
    (declare (ignore all-binds specs))
    (when next
      (define-error 'define-generic-function
		    "illegal generic function lambda list: ~s" 
                    params))
    (let ((lambda-list 
	   (gen-cl-generic-function-lambda-list vars 
						rest 
						keys 
						env)))
      `(compiler::top-level-form
           (define-generic ,(lw-debug-name name))
	 (dylan-ensure-generic ,name ,lambda-list
			       :module-name ,(current-module-name))
	 (values)))))

;;; DEFINE-METHOD variable-name param-list form1 form2  ... => variable-name
(define-dylan-translator define-method translate-define-method (whole env)
  (destructuring-bind (_ variable-name param-list &rest body) whole
    (declare (ignore _))
    (check-variable-name
      (variable-name whole)
      (let ((def-form (%define-method variable-name param-list body env)))
        def-form))))

;;;The function that DEFINE-METHOD expands into - Internal
(defun %define-method (variable-name params body env)
  (multiple-value-bind 
    (all-binds vars specs next rest keys) (parse-lambda-list params)
      `(progn
         (dylan-ensure-generic 
	  ,variable-name 
	  ,(gen-cl-generic-function-lambda-list vars 
					        rest
					        keys
					        env)
          :module-name ,(current-module-name))
         (compiler::top-level-form
             (define-method ,(lw-debug-name variable-name) ,specs)
           (add-method (dylan-resolve ,variable-name 
                                      :module-name ,(current-module-name))
		       ,(translate-dylan-expr
		         `(method 
                              ,(gen-dylan-lambda-list vars 
						      specs 
						      (if next next 'next-method)
						      rest
						      keys)
                            ,@(ccl::declare-source-level-debugging-macro)
			    (declare (ccl::lambda-name 
                                      (define-method 
                                         ,(lw-debug-name variable-name)
                                        ,specs)))
                            ,@body)
		         env)))
         (values))))

;; DEFINE-LAMBDA for Zimmerman compatability

(define-dylan-macro define-lambda (_ name ll &rest body)
  `(define ,name (method ,ll ,@body)))

;; eof
