(in-package :dylan)

;;
;;; Translation of core Dylan -> CL.
;;;
;;;   We'll keep macro expansion as a seperate phase on the grounds that
;;;   it's coming but have no real idea what form it will take. 
;;;
;;

;; Core translation table:

(defvar *dylan-translators* (make-hash-table :test #'eq))

;
;; Lexical environment tracking.
;;
;;   Keep track of the static chain. We'll remember whether something 
;;   was bound as a function (via a generated LABELS) or as a value
;;   in order to do the decent thing when calling and passing.
;

(defvar *lexical-env* nil)

(defun make-null-lexical-env () '())

(defun lexical-binding-type (var env)
  (let ((bind (assoc var env)))
    (if bind (cdr bind) :top-level)))

(defun add-value-to-env (var env)
  (maybe-warn-if-shadowing var env)
  (acons var :value env))

(defun add-values-to-env (vars env)
  (do* ((vars vars (rest vars))
        (env (add-value-to-env (first vars) env) (add-value-to-env (first vars) env)))
       ((null (rest vars)) env)))

(defun add-function-to-env (var env)
  (maybe-warn-if-shadowing var env)
  (acons var :function env))

(defun add-functions-to-env (vars env)
  (do* ((vars vars (rest vars))
        (env (add-function-to-env (first vars) env)
             (add-function-to-env (first vars) env)))
       ((null (rest vars)) env)))

(defun add-translator-to-env (var fn env)
  (acons var fn env))

(defun make-lexical-name (var) 
  (declare (ignore var)) (gensym))

(defun gen-variable-ref (var env)
  (let ((trans (get-dylan-translator var env)))
    (if trans
      (funcall trans var env)
      (let* ((std-var (dylan-variable-standard-form var))
             (type (lexical-binding-type std-var env)))
        (case type
          ((:value)     std-var)
          ((:function)  `(function ,std-var))
          ((:top-level) (gen-module-variable-ref std-var))
          (t            (funcall type std-var env)))))))

(defun gen-variable-set (var val env)
  (when (get-dylan-translator var env)
    (translate-error 'variable-ref
      "the syntax keyword ~s is badly placed" var))
  (let* ((std-var (dylan-variable-standard-form var))
         (type (lexical-binding-type std-var env)))
    (case type
      ((:value)     `(setq ,std-var ,val))
      ((:function)  (error "cannot assign to method binding ~s" var))
      ((:top-level) (gen-module-variable-set var val))
      (t
        (error "translator: cannot assign to local special ~s" var)))))

(defun gen-variable-setter-ref (var env)
  (gen-variable-ref var env))

(defun gen-called-variable-expr (expr var args env)
  (let ((trans (get-dylan-translator var env)))
    (if trans
      ;; (funcall trans var env)
      (funcall trans expr env)
      (let* ((std-var (dylan-variable-standard-form var))
             (type (lexical-binding-type std-var env)))
        (case type
          ((:value)     
            `(funcall ,std-var 
		      ,@(translate-dylan-exprs args env)))
          ((:function)  
            `(,std-var ,@(translate-dylan-exprs args env)))
          ((:top-level) 
            (gen-called-module-variable-expr 
              var 
              (translate-dylan-exprs args env)))
          (t            
              (funcall type (cons var args) env)))))))

;; Shadowing checks.

;; Behaviour control: :WARN-MODULE, :WARN-ALL, or NIL (i.e., off)

(defvar *handle-shadowing-bindings* nil)
(export '*handle-shadowing-bindings*)

(defun maybe-warn-if-shadowing (var env)
  (when (and *handle-shadowing-bindings* var)
    ;; We sometimes get dummy nils, so we ignore them.
    (let ((type (lexical-binding-type var env)))
      (case type
        ((:value :function) 
          (when (eq *handle-shadowing-bindings* :warn-all)
            (warn 
              "Shadowing binding for ~s - a previous local binding exists."
	      var)))
        (otherwise
          (when (or (eq *handle-shadowing-bindings* :warn-module)
                    (eq *handle-shadowing-bindings* :warn-all))
            (when (%runtime-defined-p var)
              (warn 
                "Shadowing binding for ~s - a visible module binding exists."
	        var))))))))

;
;; Translation engine.
;;
;;   Standard stuff.
;

(defun get-dylan-translator (name env)
  (let ((type (lexical-binding-type name env)))
    (if (functionp type) type
      (and (eq type :top-level)
           (or (gethash name *dylan-translators*)
               (gethash (%runtime-resolve-name name) *dylan-translators*))))))

(defun set-dylan-translator (name translator)
  (setf (gethash 
          name
	  *dylan-translators*)
	translator))

(defun set-primitive-dylan-translator (name translator)
  (setf (gethash name *dylan-translators*) translator))

(defun set-modular-dylan-translator (name translator)
  (setf (gethash 
          (%runtime-resolve-name name)
	  *dylan-translators*)
	translator))

(defun remove-dylan-translator (name)
  (remhash 
    name
    *dylan-translators*)
  (remhash 
    (%runtime-resolve-name name)
    *dylan-translators*))

(defun translate-dylan-expr (expr &optional env)
  (let ((*lexical-env* (or env *lexical-env* (make-null-lexical-env))))
    (let ((translated-expr
            (cond 
              ((null expr)
               '())
              ((or (keywordp expr) (stringp expr) (numberp expr))
                expr)
              ((symbolp expr)
                (gen-variable-ref expr env))
              ((atom expr)
                (gen-literal expr))
              ((consp expr)
                (let ((op (first expr))
                      (args (rest expr)))
                  (cond
		    ((symbolp op)
		       (gen-called-variable-expr expr op args env))
		    (t
		      (gen-function-call 
		        (translate-dylan-expr (first expr) env)
			(translate-dylan-exprs (rest expr) env)
			expr)))))
              (t
                (translate-error "don't know how to translate ~s" expr)))))
      #-:harlequin-pc-lisp
      (compiler::source-level-transformation expr translated-expr)
      translated-expr)))

(defun translate-dylan-exprs (expr-list env)
  (mapcar #'(lambda (expr) (translate-dylan-expr expr env)) expr-list))

(defvar *dylan-expander-map* (make-hash-table))

(defun get-dylan-expander (translator)
  (gethash translator *dylan-expander-map*))

(defun make-dylan-expander (expander)
  (let ((translator
          #'(lambda (exp env)
              (translate-dylan-expr (apply expander exp) env))))
    (setf (gethash translator *dylan-expander-map*) expander)
    translator))

;; To appease JB's code:

(defun translate-dylan-type (type env)
  (translate-dylan-expr 
    (if (and (consp type) (member (car type) '(or union)))
      '<object>
      type)
    env))

(defun translate-dylan-types (types env)
  (mapcar #'(lambda (expr) (translate-dylan-type expr env)) types))
  
;
;; The core translators.
;;
;;   More than the core here but they've been written so we may as well
;;   keep them in this form!
;

(define-dylan-translator declare translate-declare (form env)
  form)

;; Core:

;; We take the existence of #next as the indicator as to whether to produce a
;; "lightweight" method or the real thing.

(define-dylan-translator method translate-method (form env)
  (destructuring-bind (_ lambda-list &rest body) form
    (declare (ignore _))
    (multiple-value-bind 
      (all-binds vars specs next rest keys) (parse-lambda-list lambda-list)
      (let* ((base-defaulting-env 
               (let ((*handle-shadowing-bindings* :ignore))
                 (declare (special *handle-shadowing-bindings*))
                 (add-values-to-env 
                   ;; Only works because nil can't be a var
                  (list* rest next vars) env)))
             (new-env 
               (add-values-to-env all-binds env))
             (cl-lambda-list
               (gen-cl-lambda-list vars rest keys base-defaulting-env))
             (assertions (mapcan
			   #'(lambda (var spec)
			       (gen-type-assertion var spec new-env))
			  vars
			  specs)))
        (if (not next)
          `(function 
             (lambda ,cl-lambda-list
               ,@assertions
               (locally
	         ,@(translate-dylan-exprs body new-env))))
          `(dylan-method ,next ,cl-lambda-list ,(translate-dylan-types specs env)
	     (progn ,@assertions)
             ,@(translate-dylan-exprs 
                 body 
                 (add-translator-to-env 
                   next 
                   #'(lambda (form env)
                       (if (symbolp form) '(function call-next-method)
                         `(call-next-method ,@(translate-dylan-exprs (rest form) env))))
                   new-env))))))))

(define-dylan-translator set! translate-set! (form env)
  (destructuring-bind (_ place &rest values) form
    (if (and (consp place) (eq (first place) :locations))
      (translate-dylan-expr `(set*! ,(rest place) ,@values) env)
      (let* ((value-binds (mapcar #'(lambda (val) (gensym)) values))
             (new-env (add-values-to-env value-binds env)))
        `(let ,(mapcar #'list value-binds (translate-dylan-exprs values env))
           ,(if (dylan-variable-p place)
                (gen-variable-set place (first value-binds) new-env)
              (destructuring-bind (func &rest args) place
	        (translate-dylan-expr 
                 `(,(make-setter-name func) ,@value-binds ,@args) new-env)))
           (values ,@value-binds))))))

(define-dylan-macro set*! (&rest form)
  (destructuring-bind (_ places expr) form
    (multiple-value-bind
      (locs restloc) (split-at places '&rest)
      (let ((tmps (mapcar #'(lambda (val) (gensym)) locs))
            (resttmp (gensym)))
        `(bind ((,@tmps ,@(if (null restloc) '() `((&rest ,resttmp)))
                   ,expr))
           ,@(mapcar
               #'(lambda (loc tmp)
                   `(set! ,loc ,tmp))
               locs
               tmps)
           ,@(if (null restloc) '()
               `((set! ,restloc ,resttmp)))
           (apply values ,@tmps ,(if (null restloc) '() resttmp)))))))

;; Constant fold if's when obvious

(define-dylan-translator if translate-if (form env)
  (destructuring-bind (if pred cons alt) form
    (declare (ignore if))
    (cond
      ((eq pred *dylan-canonical-false*)
         (translate-dylan-expr alt env))
      ((eq pred *dylan-canonical-true*)
         (translate-dylan-expr cons env))
      (t
        `(if (not (eq ,(translate-dylan-expr pred env) ,(false)))
             ,(translate-dylan-expr cons env)
           ,(translate-dylan-expr alt env))))))

(define-dylan-translator begin translate-begin (form env)
  (translate-begin-forms (rest form) env))

(defun translate-begin-forms (forms env)
  (if (null forms) (false)
    `(progn ,@(translate-dylan-exprs forms env))))

(define-dylan-translator bind translate-bind (form env)
   (destructuring-bind (bind bindings &rest body) form
     (declare (ignore bind))
     (translate-bind-internal bindings body env)))

(defun translate-bind-internal (bindings body env)
  (if (null bindings)
    (translate-begin-forms body env)
    (translate-binding (first bindings) (rest bindings) body env)))

(let ((*ignored* (gensym)))

(defun translate-binding (binding bindings body env)
  (when binding
    (unless (>= (length binding) 2)
      (translate-error 'bind 
                       "~s is not a legal binding: must specify an init form" binding))
    (let ((bind-spec (butlast binding))
          (init (last binding)))
      (cond 
        ((and (= (length bind-spec) 1)
              (consp init)
              (eq (first init) 'macro))
          (translate-dylan-expr
            `(bind-macros
               ((,(first bind-spec) (_form_)
                  (expression-case _form_
                    ,@(rest init))))
               (bind ,bindings 
                 ,@body))
           env))
        (t
          (multiple-value-bind
            (all-vars vars specs next rest keys) (parse-lambda-list bind-spec)
            (when (or next keys)
              (translate-error 'bind 
                               "~s is not a legal binding: #next and #key are not allowed" 
                               binding))
            (let* ((init (translate-dylan-expr (first (last binding)) env))
                   (new-env (add-values-to-env all-vars env))
                   (new-body (translate-bind-internal bindings body new-env)))
              (multiple-value-bind (binds tests) (gen-type-assertions vars new-env
                                                                      specs env)
                #+ACCURATE-VALUES-BEHAVIOUR
                `(let (,@binds)
                  (multiple-value-call
                      #'(lambda (&optional 
                                 ,@(mapcar 
                                    #'(lambda (var) 
                                        `(,var *dylan-canonical-false*)) vars)
                                 &rest ,(if rest rest *ignored*))
                          ,@tests
                          ,new-body)
                    ;; HACK!!! This works around a compiler bug in 3.1.0 which mistreats
                    ;; immediate values as the arguments to multiple value calls.
                    ,(if (atom init) `(values ,init) init)))
                #-ACCURATE-VALUES-BEHAVIOUR
                `(let (,@binds)
                  ,(cond
                     ((and (not rest) (= (length vars) 1))
                       `(let ((,(first vars) ,init))
                         ,@tests
                         ,new-body))
                     ((not rest)
                       `(multiple-value-bind ,vars
                            ,(if (atom init) `(values ,init) init)
                          ,@tests
                          ,new-body))
                     (t
                      `(multiple-value-call
                         #'(lambda (&optional ,@vars &rest ,(if rest rest *ignored*))
                             ,@tests
                             ,new-body)
                       ,(if (atom init) `(values ,init) init)))))))))))
))

)

;; Really just a hacked macro:

#|
(define-dylan-translator bind-methods translate-bind-methods (form env)
  (destructuring-bind (_ binds &rest body) form
    (declare (ignore _))
    (let ((labs (mapcar #'first binds)))
      (translate-dylan-expr
        `(bind ((,@labs (values)))
           ,@(mapcar
	       #'(lambda (bind)
                   (destructuring-bind (label lambda-list &rest body) bind
                     `(set! ,label (method ,lambda-list ,@body))))
	      binds)
           ,@body)
       env))))
|#

(define-dylan-translator bind-methods translate-bind-methods (form env)
  (destructuring-bind (_ binds &rest body) form
    (declare (ignore _))
    (let*
      ((labs 
         (mapcar #'first binds))
       (labs-env
         (add-functions-to-env labs env))
       (binds
         (mapcar
	   #'(lambda (bind)
               (destructuring-bind (name ll &rest body) bind
	         (multiple-value-bind
		   (all-vars vars specs next rest keys) 
		     (parse-lambda-list ll)
                   `(,name ,(gen-cl-lambda-list vars rest keys env)
                       ,(translate-begin-forms 
                          body 
			  (add-values-to-env all-vars labs-env))))))
          binds)))
      `(labels ,binds
         ,(translate-begin-forms body labs-env)))))

;; Looping-bind-methods assumptions:
;;
;;   o All methods are tail called. 
;;   o All methods take a fixed number of arguments.

;; Example expansion:

#|

  (looping-bind-methods
    ((f1 (a) ... (f2 a (+ a 1)))
     (f2 (b c) ... (f1)))
    (f1 2))
->
  (prog (arg1 arg2) 
     (progn (psetq arg1 2) (go 'f1-tag))
   f1-tag
     (let ((a arg1))
       (psetq arg1 a arg2 (+ a 1)) (fo 'f2-tag))
   f2-tage
     (let ((b arg1) (c arg2)) ...))

|#

(define-dylan-translator 
    looping-bind-methods translate-looping-bind-methods (form env)
  (destructuring-bind (_ binds &rest body) form
    (declare (ignore _))
    (let*
      ((labs (mapcar #'first binds))
       (labs-env env)
       (max-params 0)
       (param-temps nil)
       (methods-info (make-hash-table)))
      (flet ((translate-loop-call (form env)
	       (destructuring-bind (func &rest args) form
		 (let* ((info (gethash func methods-info))
			(assigns (mapcan #'list 
                                         param-temps
                                         (translate-dylan-exprs args env))))
		   `(progn (psetq ,@assigns) (go ,func))))))
      (dolist (bind binds)
	 (destructuring-bind (name ll &rest body) bind
	   (multiple-value-bind
	      (all-vars vars specs next rest keys) 
	        (parse-lambda-list ll)
	       (setf (gethash name methods-info) 
		     (cons all-vars body))
	       (setq labs-env 
		     (add-translator-to-env 
		        name #'translate-loop-call labs-env))
               (setq max-params (max max-params (length all-vars))))))
      (setq param-temps
            (loop for i from 0 below max-params
		  collect (gentemp "argtemp")))
      (let* ((bodies
	       (loop for info being each hash-value in methods-info
		       using (hash-key name)
		     append
		       `(,name
			   (return
                             (let ,(mapcar #'list (car info) param-temps)
			       ,(translate-begin-forms
			         (cdr info)
				 (add-values-to-env 
                                   (car info) labs-env))))))))
	`(prog ,param-temps
	   ,(translate-begin-forms body labs-env)
	   ,@bodies))))))

;; Control:

;; Useful:

(defun translate-dylan-exprs-or-default (forms env &key (default :false))
  (if (null forms) 
    (list
      (ecase default
        ((:false) *dylan-canonical-false*)
        ((:true) *dylan-canonical-true*)))
    (translate-dylan-exprs forms env)))

(define-dylan-translator when translate-when (form env)
  (destructuring-bind (when test &rest forms) form
    (declare (ignore when))
    (translate-dylan-expr `(if ,test 
                               (begin ,@forms) 
                             ,*dylan-canonical-false*)
                          env)))

(define-dylan-translator unless translate-unless (form env)
  (destructuring-bind (unless test &rest forms) form
    (declare (ignore unless))
    (translate-dylan-expr `(if (not ,test) 
                               (begin ,@forms) 
                             ,*dylan-canonical-false*)
                          env)))

;;; is (cond) legal dylan?
(define-dylan-translator cond translate-cond (form env)
  (let* ((seen-catch-all? nil)
         (new-clauses
           (mapcar 
             #'(lambda (clause) 
		 (multiple-value-bind (new-clause catch-all?)
                     (translate-cond-clause clause env)
                   (when catch-all? (setq seen-catch-all? t))
                   new-clause))
	    (rest form))))
    (if seen-catch-all?
      `(cond ,@new-clauses)
      `(cond ,@new-clauses (t ,(false))))))

(defun translate-cond-clause (clause env)
  (destructuring-bind (test &rest forms) clause
    (let ((new-forms (translate-dylan-exprs forms env)))
      (if (or (eq test *dylan-canonical-true*) (eq test :else))
        (values `(t ,@new-forms) t)
        (values `((dylan-true-p ,(translate-dylan-expr test env))
                    ,@new-forms)
                nil)))))
 
;;; we're going to leave aside duplicates in match lists!!! 
;;; by the way its a bummer to say its an error to have 
;;; have duplicates if we're meant to signal an error
;;;
;;;and by the way this is a pain - what about #t #f
;;; etc.???
(define-dylan-translator case translate-case (form env)
  (destructuring-bind (case test &rest clauses) form
    (declare (ignore case))
    `(let ((test ,(translate-dylan-expr test env)))
       (cond ,@(translate-case-clauses clauses 'test env)
             (t *dylan-canonical-false*)))))

(defun translate-case-clauses (clauses test-value env)
  (mapcar #'(lambda (clause)
              (translate-case-clause clause test-value env)) clauses))
  
(defun translate-case-clause (clause test-value env)
  `(,(translate-case-match-list (car clause) test-value)
    ,@(translate-dylan-exprs-or-default (rest clause) env 
                                        :default :false)))

(defun translate-case-match-list (match-list test-value)
  (if (eq match-list :else)
      :else
    (if (eq match-list *dylan-canonical-true*)
        t
      (if (listp match-list)
          `(or ,@(mapcar #'(lambda (item)
                             `(eql ,test-value
                                   ,(if (eq item *dylan-canonical-true*)
                                        (true)
                                      (if (eq item *dylan-canonical-false*)
                                          (false)
                                        `',item))))
                         match-list))
        (translate-error "~a is not a legal match list" match-list)))))

(define-dylan-translator select translate-select (form env)
  (destructuring-bind (select target-form test-form &rest clauses) form
    (declare (ignore select))
    (let ((target (gensym))
          (test (gensym)))
      `(let ((,target ,(translate-dylan-expr target-form env))
             (,test ,(translate-dylan-expr test-form env)))
         (cond ,@(translate-select-clauses clauses target test env)
               (t (error "~a does not match in select" ,target)))))))

(defun translate-select-clauses (clauses target test env)
  (mapcar #'(lambda (clause)
              (translate-select-clause clause target test env)) clauses))
  
(defun translate-select-clause (clause target test env)
  `(,(translate-select-match-list (car clause) target test env)
     ,@(translate-dylan-exprs-or-default (rest clause) env
                                         :default :false)))

(defun translate-select-match-list (match-list target test env)
  (if (eq match-list :else)
      :else
    (if (eq match-list *dylan-canonical-true*)
        t
      (if (listp match-list)
          `(or ,@(mapcar #'(lambda (item)
                             (gen-true-test 
                               `(funcall ,test ,target 
                                         ,(translate-dylan-expr item env))))
                         match-list))
        (translate-error "~a is not a legal match list" match-list)))))


(defparameter *or-temp* (gensym "OR-TEMP"))

(define-dylan-macro or (_ &rest forms)
  (cond
    ((null forms) 
      *dylan-canonical-false*)
    ((null (rest forms))
      (first forms))
    (t
      `(bind ((,*or-temp* ,(first forms)))
         (if ,*or-temp* ,*or-temp* (or ,@(rest forms)))))))

(defparameter *and-temp* (gensym "AND-TEMP"))

(define-dylan-macro and (_ &rest forms)
  (cond
    ((null forms) 
      *dylan-canonical-true*)
    ((null (rest forms))
      (first forms))
    (t
      `(bind ((,*and-temp* ,(first forms)))
         (if ,*and-temp* (and ,@(rest forms)) ,*dylan-canonical-false*)))))

#|
(define-dylan-translator not translate-not (form env)
   (let ((expr (second form)))
     `(let ((res ,(translate-dylan-expr expr env)))
        (if (eq res ,(false))
            ,(true)
          ,(false)))))
|#

(define-dylan-translator setter translate-setter (form env)
   (unless (= (length form) 2)
     (translate-error 'setter "illegal setter variable ~s" form))
   (gen-variable-setter-ref form env))


;;; EXITS
(define-dylan-translator bind-exit translate-bind-exit (form env)
   (destructuring-bind (b (var) &rest forms) form
     (declare (ignore b))
     (let ((tag (gensym)))
       (flet ((translate-return-call (form env)
                (if (symbolp form)
                  `#'(lambda (&rest args)
                       (return-from ,tag (apply 'values args)))
                  (destructuring-bind (func &rest args) form
                    `(return-from
                       ,tag (values ,@(translate-dylan-exprs args env)))))))
                    

	 (let ((new-env 
                 (add-translator-to-env var #'translate-return-call env)))
           `(block ,tag
              ,@(translate-dylan-exprs-or-default forms new-env)))))))

(define-dylan-translator unwind-protect translate-unwind-protect (form env)
   (destructuring-bind (u form &rest clean-ups) form
     (declare (ignore u))
     `(unwind-protect ,(translate-dylan-expr form env)
        (progn ,@(translate-dylan-exprs clean-ups env)))))

;;; EVALUATION
(define-dylan-translator quote translate-quote (form env)
   (declare (ignore env))
   (destructuring-bind (quote const) form
     (declare (ignore quote))
     (gen-literal const)))

(define-dylan-translator apply translate-apply (form env)
  (if (symbolp form)
    (gen-module-variable-ref form)
    (destructuring-bind (_ function &rest args) form
      (declare (ignore _))
      (when (null args)
        (error "The form ~S does not contain any arguments for apply." form)) 
      (let ((positional-args (butlast args))
            (sequence-arg (first (last args))))
	`(let ((.function. ,(translate-dylan-expr function env)))
           (apply .function.
              ,@(translate-dylan-exprs positional-args env)
	      (funcall 
                (dylan-resolve as-apply-list :module-name internal)
                .function.
	        ,(translate-dylan-expr sequence-arg env))))))))

;;; ITERATION
;;; are setter vars allowed and what about type info
(define-dylan-translator for translate-for (form env)
  (destructuring-bind 
    (for (&rest for-bindings) test-results &rest body-forms) form
    (declare (ignore for))
    (multiple-value-bind (bindings new-env) (process-for-bindings for-bindings env)
      `(do 
          ,bindings
          ,(when (not (null test-results))
	     `(,(gen-true-test (translate-dylan-expr (first test-results) new-env))
                ,@(translate-dylan-exprs-or-default (rest test-results) new-env)))
	 ,@(translate-dylan-exprs body-forms new-env)))))

(defun process-for-bindings (bindings env)
  (let ((new-env (add-for-vars bindings env)))
    (values
      (loop for (v init . more) in bindings
            when (null more)
	      collect `(,(parse-dylan-variable v)
                        ,(translate-dylan-expr init env))
            else when (null (rest more))
	      collect `(,(parse-dylan-variable v)
		        ,(translate-dylan-expr init env) 
		        ,(translate-dylan-expr (first more) new-env))
            else
              do (error "malformed FOR bindings ~s" bindings))
      new-env)))

;;; dont want to be boring but shouldn't (var specialiser) be allowed as a a var spec
(defun add-for-vars (bindings env)
  (let ((new-env env))
    (loop for (v . dont-care) in bindings
	  do (setq new-env (add-value-to-env (parse-dylan-variable v)
                                             new-env))
	  finally (return new-env))))

;;; FOR-EACH need to do collectins before we do this
;;; Do it as a macro in terms of DO for now.

(define-dylan-macro for-each (name binds test-consq &rest body)
  (let ((return (gensym)))
    `(bind-exit (,return)
       (do (method ,(mapcar #'(lambda (bind)
                                (parse-dylan-variable (first bind)))
                            binds)
             ,(if test-consq
                `(when ,(car test-consq)
                   (bind ((result1 &rest results 
                            (begin ,@(cdr test-consq))))
                     (apply ,return result1 results))))
             ,@body)
           ,@(mapcar #'second binds))
       ,*dylan-canonical-false*)))

;;; QUESTION is var still bound for result?
;;; what kind of var can be used - a setter?
(define-dylan-translator dotimes translate-dotimes (form env)
   (destructuring-bind 
       (d (var count &optional (result *dylan-canonical-false*)) &rest forms)
       form
     (declare (ignore d))
     (let* ((var (parse-dylan-variable var))
            (new-env (add-value-to-env var env))
            (counter (gensym)))
       `(let ((,counter ,(translate-dylan-expr count env)))
          (unless (integerp ,counter)
            (dylan-runtime-error "Illegal count value ~s in ~s" ,counter
                                 ,(gen-literal form)))
         (dotimes (,var ,counter ,(translate-dylan-expr result new-env))
           ,@(mapcar #'(lambda (form)
                         (translate-dylan-expr form new-env))
                     forms))))))

(define-dylan-translator while translate-while (form env)
   (destructuring-bind (w test &rest forms) form
     (declare (ignore w))
     `(loop (when (eq ,(translate-dylan-expr test env) ,(false))
              (return ,(false)))
            ,@(mapcar #'(lambda (form)
                          (translate-dylan-expr form env))
                      forms))))

(define-dylan-translator until translate-until (form env)
   (destructuring-bind (w test &rest forms) form
     (declare (ignore w))
     `(loop (when (not (eq ,(translate-dylan-expr test env) ,(false)))
              (return ,(false)))
            ,@(mapcar #'(lambda (form)
                          (translate-dylan-expr form env))
                      forms))))

;;; My own contribution:

(define-dylan-macro bind-method (_ label binds &rest body)
  (declare (ignore _))
  `(bind-methods
     ((,label ,(mapcar #'first binds) ,@body))
     (,label ,@(mapcar #'second binds))))

(define-dylan-macro iterate (_ label binds &rest body)
  (declare (ignore _))
  `(bind-methods
     ((,label ,(mapcar #'first binds) ,@body))
     (,label ,@(mapcar #'second binds))))

;; Conditions:

(define-dylan-translator handler-bind translate-handler-bind (form env)
  (destructuring-bind (_ (type fn &key test description) &rest body) form
    (declare (ignore _))
    `(dylan-handler-bind
       (,(translate-dylan-expr type env)
        ,(translate-dylan-expr fn env)
	,@(if test 
              `(:test ,(translate-dylan-expr test env)))
	,@(if description 
              `(:description ,(translate-dylan-expr description env))))
       ,@(translate-dylan-exprs body env))))

(define-dylan-translator values-begin-1 translate-values-begin-1 (form env)
  (destructuring-bind (_ &rest forms) form
    (declare (ignore _))
    `(multiple-value-prog1 ,@(translate-dylan-exprs forms env))))

(define-dylan-macro handler-case (_ protected &rest cases)
  (declare (ignore _))
  `(bind-exit (normal-return-from-handler-case)
     ((bind-exit (abnormal-return-from-handler-case)
        ,(expand-handler-case 
          `(bind ((&rest vals ,protected))
             (apply normal-return-from-handler-case vals))
          cases)))))

(defun expand-handler-case (code cases)
  (if (null cases) code
    (let ((case (first cases)))
      (destructuring-bind 
        ((type &key condition test description) &rest forms) case
        (expand-handler-case
          `(handler-bind
             (,type
              (method (,(if condition condition 'ignore-condn)
                        ignored-next-handler)
                (abnormal-return-from-handler-case 
                  (method () ,@forms)))
              ,@(if test `(:test ,test))
              ,@(if description `(:description ,description)))
             ,code)
         (rest cases))))))

;; Sealing:

(define-dylan-translator seal-generic translate-seal-generic (form env)
  (destructuring-bind (_ mods name (&rest types)) form
    (declare (ignore _ mods name types))
    (translate-dylan-expr `(begin ,@types (values)) env)))

;
;; Translator "back end".
;;
;;   The code above expands Dylan into CL via these functions for 
;;   abstracting variable lookup, symbol access etc.
;

(defun get-dylan-symbol (symbol)
  `(load-time-value (%get-dylan-symbol (symbol-name ',symbol))))
    
(defun false () `(dylan-false))

(defun true () `(dylan-true))

(defun gen-dylan-canonical-false () 
  `(load-time-value *dylan-canonical-false*))

(defun gen-dylan-canonical-true () 
  `(load-time-value *dylan-canonical-true*))

(defun gen-type-assertion (var class env)
  #-RUNTIME-TYPE-CHECKS
  '()
  #+RUNTIME-TYPE-CHECKS
  (if (eq class '<object>) '()
    (let ((cl-var (translate-dylan-expr var env))
          (cl-class (translate-dylan-type class env)))
      `((dylan-assert-type ,cl-var ,cl-class)))))

(defun gen-type-assertions (vars var-env classes class-env)
  #-RUNTIME-TYPE-CHECKS
  (values '() '())
  #+RUNTIME-TYPE-CHECKS
  (let ((binds '())
        (tests '()))
    (mapc
      #'(lambda (var class)
          (unless (eq class '<object>)
            (let ((class-val (gensym)))
              (push `(,class-val ,(translate-dylan-expr class class-env))
                    binds)
              (push `(dylan-assert-type ,(translate-dylan-expr var var-env) ,class-val)
                    tests))))
     vars
     classes)
    (values (nreverse binds) (nreverse tests))))

;
;; Formals parsing.
;;
;;   Manual says: ({reqd-var | (reqd-var specialiser)}^* 
;;                 [#next next-var]
;;                 [#rest rest-var]
;;                 [#key {key-var | ([keyword] key-var [default])}^*]
;;                 [#values <~recurse>])
;;
;;   Returns (values <reqd-vars> 
;;                   <specialisers>
;;                   <next-var>
;;                   <rest-var>
;;                   <key-vars>
;;                   (<reqd-types> <rest-type>))
;;   No translation is performed, just destructuring.
;;
;

(defun finalise-lambda-list-info (vars specs next rest keys)
  (values (append vars
                  (if next (list next) '())
                  (if rest (list rest) '())
                  (mapcar
                    #'(lambda (key-spec)
                        (destructuring-bind ((key var) def) key-spec
                          var))
                   (rest keys)))
          vars
          specs
          next
          rest
          keys))

(defun parse-dylan-variable (var)
  (cond
    ((dylan-variable-p var)
      (values
        (dylan-variable-standard-form var)
        '<object>))
    ((and (listp var) (= (length var) 2) (dylan-variable-p (first var)))
      (values
        (dylan-variable-standard-form (first var))
        (second var)))
    (t
      (define-error 'parse-spec-variable
                    "Bad variable ~s"
                    var))))
    
(defun parse-lambda-list (params)
  (loop for (required . rest) on params 
        when (eq required '&return-types)
          do (loop-finish)
        when (eq required '&next)
          do (return (parse-lambda-list-next args specializers rest))
        when (or (eq required '&rest) (eq required '&more))
          do (return (parse-lambda-list-rest args specializers nil rest))
        else when (eq required '&key) 
	  do (return (parse-lambda-list-keys args specializers nil nil `(&key ,@rest)))
        else when (eq required '&all-keys) 
	  do (return (parse-lambda-list-keys args specializers nil nil `(&all-keys ,@rest)))
        else when (dylan-variable-p required)
          collect (dylan-variable-standard-form required) into args and
          collect '<object> into specializers
        else when (and (listp required) (= (length required) 2))
          collect (dylan-variable-standard-form (first required)) into args and 
          collect (second required) into specializers
        else do (define-error 'define-method 
                              "Syntax error in param-list ~a" params)
        finally
        (return (finalise-lambda-list-info args specializers nil nil nil))))

(defun parse-lambda-list-next (args specs rest)
  ;;we just seen &next so (first rest) is the next arg
  (unless rest
    (define-error 'define-method 
                  "no next arg specified in method param-list"))
  (if (not (second rest))
      (finalise-lambda-list-info args specs (first rest) nil nil)
    (ecase (second rest)
      ((&rest &more)
        (parse-lambda-list-rest args specs (first rest) (rest (rest rest))))
      ((&key &all-keys)
        (parse-lambda-list-keys args specs (first rest) nil (rest rest)))
      (&return-types
        (finalise-lambda-list-info args specs (first rest) nil nil)))))

(defun parse-lambda-list-rest (args specs next rest)
  ;;we just seen &rest so (first rest) is the rest arg
  (unless rest
    (define-error 'define-method 
                  "no rest arg specified in method param-list"))
  (parse-lambda-list-keys args specs next (variable-name (first rest))
                          (rest rest)))

(defun parse-lambda-list-keys (args specs next rest keys)
  (if (null keys)
    (finalise-lambda-list-info args specs next rest '())
    (case (first keys)
      (&key
        (loop for key in (rest keys)
              when (or (eq key '&return-types) (eq key '&all-keys))
                do (loop-finish)
	      collect (parse-key-param key) into the-keys
              finally 
                (return (finalise-lambda-list-info args 
                                                   specs 
                                                   next 
                                                   rest 
                                                   `(&key ,@the-keys)))))
      (&all-keys
        (finalise-lambda-list-info args specs next rest '()))
      (&return-types
        (finalise-lambda-list-info args specs next rest '()))
      (t
        (unless (eq (first keys) '&key)
          (define-error 'define-method 
                        "Illegal syntax in parameter list"))))))
        
(defun parse-key-param (spec)
  (flet ((symbol->keyword (s) (intern (symbol-name s) (find-package :keyword))))
    (cond
      ((symbolp spec) 
        `((,(symbol->keyword spec) ,spec) ,*dylan-canonical-false*))
      ((consp spec)
        (case (length spec)
          ((1)
            `((,(symbol->keyword (first spec)) ,(first spec)) ,*dylan-canonical-false*))
          ((2)
            (if (keywordp (first spec))
              `((,(first spec) ,(second spec)) ,*dylan-canonical-false*)
              `((,(symbol->keyword (first spec)) ,(first spec)) ,(second spec))))
          ((3)
            (destructuring-bind (key var def) spec
              `((,key ,var) ,def)))))
      (t
        (error "parse-lambda-list: bad keyword spec ~s" spec)))))

(defun gen-cl-lambda-list (vars rest keys env)
  (append vars
          (if rest `(&rest ,rest))
          (if keys
            `(&key ,@(mapcar
		      #'(lambda (spec)
                          (destructuring-bind ((key var) def) spec
                            (let ((lisp (translate-dylan-expr def env)))
                              (setq env (add-value-to-env var env))
                              `((,key ,var) ,lisp))))
                      (rest keys))))))

(defun gen-cl-generic-function-lambda-list (vars rest keys env)
  (append vars
          (if rest `(&rest ,rest))
          (if keys
            `(&key ,@(mapcar
                       #'(lambda (spec)
                           (destructuring-bind ((key var) def) spec
                             `((,key ,var) ,(translate-dylan-expr def env))))
                      (rest keys))))))

(defun gen-dylan-lambda-list (vars specs next rest keys)
  (append (mapcar #'list vars specs)
          (if next `(&next ,next))
          (if rest `(&rest ,rest))
          (if keys
            `(&key ,@(mapcar
		      #'(lambda (spec)
                          (destructuring-bind ((key var) def) spec
                            `(,key ,var ,def)))
                      (rest keys))))))

;(defun gen-function-call (op args expr)
;  `(dylan-record ,expr
;                 (funcall ,op ,@args)))

(defun gen-function-call (op args expr)
  `(funcall ,op ,@args))

(defun gen-false-test (form)
  `(eq ,form *dylan-canonical-false*))

(defun gen-true-test (form)
  `(not (eq ,form *dylan-canonical-false*)))

;; Nasty:

(defun gen-literal (form)
  (cond
    ((eq form *dylan-canonical-true*) `(dylan-true))
    ((eq form *dylan-canonical-false*) `(dylan-false))
    (t
      (let ((dodgy? nil))
        (labels
            ((gen-body (form)
               (cond
                ((consp form)
	         `(cons ,(gen-body (car form))
                        ,(gen-body (cdr form))))
                ((simple-vector-p form)
                 `(coerce ,(gen-body (coerce form 'list)) 'vector))
                ((eq form *dylan-canonical-false*)
                 (setq dodgy? t)
                 '*dylan-canonical-false*)
                ((eq form *dylan-canonical-true*)
                 (setq dodgy? t)
                 '*dylan-canonical-true*)
                (t
                 `',form))))
          (let ((constructor (gen-body form)))
            (if dodgy? `(load-time-value ,constructor) `',form)))))))

;; A few special non-Dylan utilities for accessing CL values:

(defun cl-symbol-normal-form (cl)
  (if (symbolp cl) 
    (intern (symbol-name cl) 
            (find-package 'user))
    (intern (symbol-name (second cl))
            (find-package (first cl)))))

(define-dylan-translator cl-function translate-cl-function (form env)
  (destructuring-bind (_ ident) form
    (declare (ignore _))
      `(function ,(cl-symbol-normal-form ident))))

(define-dylan-translator cl-value translate-cl-value (form env)
  (destructuring-bind (_ ident) form
    (declare (ignore _))
      (cl-symbol-normal-form ident)))

(define-dylan-translator cl-class translate-cl-class (form env)
  (destructuring-bind (_ ident) form
    (declare (ignore _))
      `(find-class ',(cl-symbol-normal-form ident))))

(define-dylan-translator import-cl-functions translate-import-cl-functions (form env)
  (destructuring-bind (_ &rest r) form
    (declare (ignore _))
    `(export-cl-functions ,@r)))

(define-dylan-translator import-cl-values translate-import-cl-values (form env)
  (destructuring-bind (_ &rest r) form
    (declare (ignore _))
    `(export-cl-values ,@r)))

(define-dylan-translator import-cl-classes translate-import-cl-classes (form env)
  (destructuring-bind (_ &rest r) form
    (declare (ignore _))
    `(export-cl-classes ,@r)))

(define-dylan-translator import-cl-predicates translate-import-cl-predicates (form env)
  (destructuring-bind (_ &rest r) form
    (declare (ignore _))
    `(export-cl-predicates ,@r)))

;;; Module-specific.

(define-dylan-translator in-code-context translate-in-code-context (form env)
  (destructuring-bind (_ module-name syntax) form
    (let ((module-name
	   (etypecase module-name
             (symbol module-name)
             (string (intern (string-upcase module-name))))))
      (set-module-context-by-name module-name)
      `(format t ";         into the ~a module (derived from ~a code)~%" 
                ',module-name ',syntax))))

(define-dylan-translator access translate-access (form env)
  (destructuring-bind (_ module-name variable) form
    (let ((*current-module* (find-translator-module module-name)))
      (declare (special *current-module*))
      (macroexpand (translate-dylan-expr variable '())))))

(defun variable-name (var)
  (if (consp var) (first var) var))

(defun variable-type (var)
  (if (consp var) (second var) '<object>))

;; eof
