(in-package :dylan)

;; Back end macros:

;; Module variable definition/resolution:

(eval-when (:load-toplevel :execute)

  (defmacro dylan-record (src obj) obj)

  (defmacro dylan-define (dylan-name cl-expr)
    `(progn
       (%define-variable 
         ',dylan-name
         ',(%runtime-resolve-name dylan-name)
         ,cl-expr)
       (values)))


  #|
  (defmacro dylan-resolve (var)
    `(symbol-value (dylan-resolve-name ,var)))

  (defmacro dylan-resolve-name (var)
    `(load-time-value (%runtime-resolve-name ',var)))
  |#

  (defmacro dylan-resolve (var &key module-name)
    (let ((lisp-var (%runtime-resolve-name var :module-name module-name)))
      `(locally (declare (special ,lisp-var)) ,lisp-var)))

  (defmacro dylan-resolve-called (var &key module-name)
    (let ((lisp-var (%runtime-resolve-name var :module-name module-name)))
      lisp-var))

  (defmacro dylan-resolve-name (var &key module-name)
    `',(%runtime-resolve-name var :module-name module-name))

  (defun dylan-compatible-generics-p (ll1 ll2)
    (multiple-value-bind 
      (l1 reqd1 opt1 restp1 key1 othersp1)
      (clos::extract-gf-lambda-list-info ll1)
      (multiple-value-bind 
        (l2 reqd2 opt2 restp2 key2 othersp2)
        (clos::extract-gf-lambda-list-info ll2)
        (let ((stuff1 (or restp1 key1))
              (stuff2 (or restp2 key2)))
          (and (= reqd1 reqd2)
	       (or (and stuff1 stuff2)
		   (and (not stuff1) (not stuff2))))))))
    
  (defmacro dylan-ensure-generic (var lambda-list &key module-name)
    `(runtime-dylan-ensure-generic
       ',var 
      (dylan-resolve-name ,var :module-name ,module-name)
      ',lambda-list))

  (defmacro dylan-assert-type (var class)
    `(unless (clos::dylan-type-instance-p ,var ,class)
       (error "the variable ~s cannot be bound to ~s because it is not of type ~s"
	      ',var ,var ,class)))

  (defmacro dylan-undefine (dylan-name &key module-name)
    `(progn
       (makunbound
         (dylan-resolve-name ,dylan-name :module-name ,module-name))
       (fmakunbound
         (dylan-resolve-name ,dylan-name :module-name ,module-name))
       ',dylan-name))

)

;; Generic function/method generation:

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro dylan-generic-function (lambda-list)
    `(make-instance 'standard-generic-function 
                    :lambda-list ',lambda-list
                    :name        '"anon"))

  (defun gen-method-lambda (ll body env)
    (let ((gf (class-prototype (find-class 'standard-generic-function)))
          (md (class-prototype (find-class 'standard-method))))
      (multiple-value-bind 
        (body doc decls) (compiler::parse-body1 body)
        (make-method-lambda
          gf
          md
          ll
          decls
          `(progn ,@body)
          env))))

  (defmacro dylan-method (call-next 
                          lambda-list 
                          specialisers 
                          assertion 
			  &rest body
                          &environment env)
    (let ((md (gensym))
          (md-fn (gensym)))
      (multiple-value-bind
        (method-lambda initargs) (gen-method-lambda lambda-list body env)
	`(let* ((,md-fn
                  (function ,method-lambda))
		(,md 
                  (make-instance 'clos::dylan-method
				 :qualifiers  '()
				 :lambda-list ',lambda-list
				 :specializers 
                                   (mapcar 
                                     #'clos::convert-dylan-specializer
				    (list ,@specialisers))
				 :function ,md-fn
				 ,@initargs)))
           #+never-in-a-million-years
	   (clos::set-dylan-method-function ,md
              #'(lambda (&rest args)
                  (apply-method-function
                    (class-prototype (find-class 'standard-generic-function))
                    ,md
                    ,md-fn
                    '()
                    args)))
	   ,md))))

)

;; Common Lisp function export:

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun cl-symbol-normal-form (cl)
  (if (symbolp cl) 
    (intern (symbol-name cl) 
            (find-package 'user))
    (intern (symbol-name (second cl))
            (find-package (first cl)))))

(defmacro export-cl-functions (&rest specs)
  `(progn
     ,@(mapcar 
	 #'(lambda (spec)
	     (multiple-value-bind 
	       (cl-name dylan-name) 
                 (cond
                   ((symbolp spec) 
		     (values spec spec))
                   ((= (length spec) 1)
                     (values (cl-symbol-normal-form (car spec))
                             (cadar spec)))
                   (t
		     (values (cl-symbol-normal-form (car spec))
			     (caddr spec))))
	       (set-primitive-function dylan-name cl-name
                 :module-name (current-module-name))
               `(progn
                  (register-value-name-translation ',dylan-name ',cl-name)
                  (set-primitive-function ',dylan-name ',cl-name
                    :module-name ',(current-module-name))
                  (dylan-define ,dylan-name (function ,cl-name)))))
	specs)))

(defmacro export-cl-values (&rest specs)
  `(progn
     ,@(mapcar 
	 #'(lambda (spec)
	     (multiple-value-bind 
	       (cl-name dylan-name) 
                 (cond
                   ((symbolp spec) 
		     (values spec spec))
                   ((= (length spec) 1)
                     (values (cl-symbol-normal-form (car spec))
                             (cadar spec)))
                   (t
		     (values (cl-symbol-normal-form (car spec))
			     (caddr spec))))
               `(progn
                  (register-value-name-translation ',dylan-name ',cl-name)
                  (dylan-define ,dylan-name ,cl-name))))
	specs)))

(defmacro export-cl-classes (&rest specs)
  `(progn
     ,@(mapcar 
	 #'(lambda (spec)
	     (multiple-value-bind 
	       (cl-name dylan-name) 
                 (cond
                   ((symbolp spec) 
		     (values spec spec))
                   ((= (length spec) 1)
                     (values (cl-symbol-normal-form (car spec))
                             (cadar spec)))
                   (t
		     (values (cl-symbol-normal-form (car spec))
			     (caddr spec))))
               `(progn
                  (register-class-name-translation ',dylan-name ',cl-name)
                  (dylan-define ,dylan-name (find-class ',cl-name)))))
	specs)))

)

;; Conditions:

(eval-when (:load-toplevel :execute)

  (defmacro dylan-handler-bind ((condn 
                                 fn 
                                 &key (test 
                                        '(function 
                                           (lambda (x) *dylan-canonical-true*)))
                                      (description
                                        '(function
                                           (lambda (s)
                                             (format s "undescribed handler")))))
				&rest body)
    (let ((condn-var (gensym))
          (type-var (gensym))
          (fn-var (gensym))
          (test-var (gensym))
          (desc-var (gensym))
          (body-var (gensym)))
      `(let ((,type-var ,condn)
             (,fn-var ,fn)
             (,test-var ,test)
             (,desc-var ,description))
         (flet ((,body-var () ,@body))
           (if (subtypep ,type-var 'dylan-restart)
             (progn
               ;; (format t "Installing restart~%")
             (restart-bind
               ((dylan-restart
                  #'(lambda (condn)
                      (funcall ,fn-var condn #'(lambda () nil)))
                 :test-function
		   #'(lambda (condn) 
                       (if (not (typep condn 'dylan-restart)) t
                         (and (subtypep (class-of condn) ,type-var)
                              (not (eq (funcall ,test-var condn)
                                       ,(false))))))
                 :interactive-function
                   #'(lambda ()
                       (let ((res (clos::dylan-make ,type-var)))
                         (dylan-restart-query res)
                         (list res)))
                 :report-function
                   ,desc-var))
               (,body-var)))
             (progn
               ;; (format t "Installing handler~%")
	       (handler-bind
                ((condition
                  #'(lambda (,condn-var)
                      (when (and (clos::dylan-instance? ,condn-var ,type-var)
                                 (not (eq (funcall ,test-var ,condn-var)
                                          ,(false))))
                        (funcall ,fn-var ,condn-var #'(lambda () nil))))))
                (,body-var))))))))

)

;; Concise booleans.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro dylan-true-p (form)
    `(not (eq ,form ,(false))))

  (defmacro dylan-false-p (form)
    `(eq ,form ,(false)))

  (defmacro dylan-true ()
    (gen-dylan-canonical-true))

  (defmacro dylan-false ()
    (gen-dylan-canonical-false))

)

;; eof
