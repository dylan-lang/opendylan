;;; Copyright (c) 1993 jonathan.bachrach@ircam.fr, all rights reserved.

;;; Ported blind from JB's translator code.

(in-package :dylan)

;;;;;
;;;;; SYNTAX BOOTSTRAP
;;;;;

(defvar *symbol-bindings* nil)

(eval-when (compile eval load)
  (defvar *gen-id-counter* -1)
  (defun gen-id (&optional (name ""))
    (incf *gen-id-counter*)
    (intern (format nil "~A-~D" name *gen-id-counter*) *package*))
  )

(define-dylan-translator symbol-macrolet translate-symbol-macrolet (forms env)
  (destructuring-bind (_ bindings . body) forms
    (let ((*symbol-bindings*
	   (append bindings *symbol-bindings*)))
      `(progn ,@(translate-dylan-exprs body env)))))

(defun expand-symbol (symbol)
  (let ((binding (assoc symbol *symbol-bindings*)))
    (if binding
	(cadr binding)
	symbol)))

(defun parse-pattern (pattern literals)
  ;; (format t "PARSING ~S~%" pattern)
  (let ((pattern-vars nil))
    (labels
	((parse (pattern level)
	   (cond ((null pattern)
		  (cons 'dylan::atom nil))
		 ((simple-vector-p pattern)
		  (cons 'dylan::vector
			(parse (coerce pattern 'list) level)))
		 ((listp pattern)
		  (if (and (consp (cdr pattern))
			   (eq (cadr pattern) 'dylan::|...|)
			   (null (cddr pattern)))
		      (cons 'dylan::each (parse (first pattern) (1+ level)))
		      (multiple-value-bind
			  (results dotted?)
			  (parse-list pattern level)
			(cons (if dotted? 'dylan::list* 'dylan::list) results))))
		 ((symbolp pattern)
		  (if (keywordp pattern)
		      (cons 'dylan::atom pattern)
		      (let ((posn (position pattern literals)))
			(cond (posn
			       (cons 'dylan::literal posn))
			      (t
			       (push (cons pattern level) pattern-vars)
			       '(dylan::any))))))
		 (t
		  (cons 'dylan::atom pattern))))
	 (parse-list (list level)
	   (cond ((null list)
		  (values nil nil))
		 ((atom list)
		  (values (list (parse list level)) t))
		 ((and (consp (cdr list))
		       (eq (cadr list) 'dylan::|...|)
		       (null (cddr list)))
		  (values (list (parse list level)) t))
		 (t
		  (let ((first (parse (car list) level)))
		    (multiple-value-bind
			(rest dotted?)
			(parse-list (cdr list) level)
		      (values (cons first rest) dotted?)))))))
      (values (parse pattern 0) (nreverse pattern-vars)))))

(define-dylan-translator destructuring-bind translate-destructuring-bind (form env)
  ;; !@#$ COULD OPTIMIZE
  (destructuring-bind (_ vars bindings . body) form
    (declare (ignore _))
    (translate-dylan-expr
      `(dylan::apply (dylan::method ,vars ,@body) ,bindings)
      env)))

(define-dylan-translator syntax-case translate-syntax-case (form env)
  (destructuring-bind (_ expr literals . clauses) form
    (declare (ignore _))
    (let ((exit-name (gen-id "EXIT"))
	  (n-expr (gen-id "N-EXPR"))
	  (lit-vec (gen-id "LIT-VEC"))
	  (matches? (gen-id "MATCHES?"))
	  (bindings (gen-id "BINDINGS"))
	  (results (gen-id "RESULTS")))
      (translate-dylan-expr
       `(dylan::bind-exit (,exit-name)
	 (dylan::bind ((,n-expr ,expr)
		      (,lit-vec (dylan::quote ,(coerce literals 'vector))))
	   ,@(mapcar
	      #'(lambda (clause)
		  (multiple-value-bind
			(pattern fender output-expr)
		      (ecase (length clause)
			(2 (values 
                            (first clause)
                            *dylan-canonical-true*
                            (second clause)))
			(3 (values
			    (first clause) (second clause) (third clause))))
		    (multiple-value-bind
			  (parsed-pattern pattern-vars)
			(parse-pattern pattern literals)
		      (let ((pattern-var-temps
			     (mapcar #'(lambda (x) (gen-id (car x)))
				     pattern-vars)))
			`(dylan::bind ((,matches? ,bindings
				       ((access internal dylan::match-pattern)
					(dylan::quote ,parsed-pattern)
					,n-expr ,lit-vec)))
			  (dylan::when ,matches?
			    (dylan::destructuring-bind
				  ,pattern-var-temps ,bindings
                              ;; Hack!!! These references should get 
                              ;; optimised away - just want ignorable!
                              ,@pattern-var-temps
			      (dylan::symbol-macrolet
			       ,(mapcar
				 #'(lambda (var temp)
				     `(,(car var)
				       (pattern-var
					,(car var) ,(cdr var) ,temp)))
				 pattern-vars pattern-var-temps)
			       (dylan::when ,fender
				 (dylan::apply ,exit-name
			           (dylan::bind ((&rest ,results ,output-expr))
				     ,results)))))))))))
	      clauses)
	   ((access internal dylan::syntax-error) ,n-expr)))
       env))))

(defun gen-cons (car-form cdr-form)
  (cond ((constantp cdr-form)
	 (let ((cdr (eval cdr-form)))
	   (cond ((constantp car-form)
		  `(dylan::quote (,(eval car-form) . ,cdr)))
		 ((null cdr)
		  `(dylan::list ,car-form))
		 (t
		  `(dylan::pair ,car-form (dylan::quote ,cdr))))))
	((atom cdr-form)
	 `(dylan::pair ,car-form ,cdr-form))
	(t
	 (case (car cdr-form)
	   (dylan::list
	    `(dylan::list ,car-form ,@(cdr cdr-form)))
	   ((dylan::pair dylan::list*)
	    `(dylan::list* ,car-form ,@(cdr cdr-form)))
	   (t
	    `(dylan::pair ,car-form ,cdr-form))))))

(defun gen-append (list1 list2)
  (cond ((null list2) list1)
	((constantp list2)
	 (if (constantp list1)
	     `(dylan::quote ,(append (eval list1) (eval list2)))
	     `(dylan::concatenate ,list1 (dylan::quote ,(eval list2)))))
	((atom list2) `(dylan::concatenate ,list1 ,list2))
	(t
	 (case (car list2)
	   (dylan::concatenate
	    `(dylan::concatenate ,list1 ,@(cdr list2)))
	   (t
	    `(dylan::concatenate ,list1 ,list2))))))

(defun gen-map (expr map-env)
  (let ((vars (mapcar #'cdr map-env))
	(vals (mapcar #'car map-env)))
    (cond ((eq expr (car vars))
	   (car vals))
	  (t
	   `(dylan::map (dylan::method ,vars ,expr)
	               ,@vals)))))

(defun gen-ref (var level maps emitter)
  (if (zerop level)
      (funcall emitter var maps)
      (gen-ref
       var (1- level) (cdr maps)
       #'(lambda (outer-var outer-maps)
	   (let ((b (assoc outer-var (car maps))))
	     (if b
		 (funcall emitter (cdr b) maps)
		 (let ((inner-var (gensym)))
		   (funcall emitter inner-var
			    (cons (cons (cons outer-var inner-var)
					(car maps))
				  outer-maps)))))))))

(define-dylan-translator syntax translate-syntax (form env)
  (destructuring-bind (_ form) form
    (declare (ignore _))
    (labels ((grovel (form maps emitter)
	       (declare (type list maps)
			(type (function (t list) t) emitter))
	       ;; (format t "GROVELING FORM: ~S MAPS: ~S~%" form maps)
	       (cond
		 ((null form)
		  (funcall emitter form maps))
		 ((listp form)
		  (cond
		    ((and (consp (cdr form))
			  (eq (cadr form) 'dylan::|...|))
		     (grovel
		      (cddr form) maps
		      #'(lambda (cddr maps)
			  (grovel
			   (car form) (cons '() maps)
			   #'(lambda (car maps)
			       (if (null (car maps))
				   (error "extra ellipsis")
				   (funcall emitter
					    (gen-append (gen-map car (car maps))
							cddr)
					    (cdr maps))))))))
		    ((and (eq (car form) 'dylan::|...|)
			  (consp (cdr form))
			  (eq (cadr form) 'dylan::|...|)
			  (null (cddr form)))
		     ;; !@#$ might be dubious with extra quote
		     (funcall emitter '(dylan::quote dylan::|...|) maps))
		    (t
		     (grovel (car form) maps
			     #'(lambda (car maps)
				 (grovel (cdr form) maps
					 #'(lambda (cdr maps)
					     (funcall emitter
						      (gen-cons car cdr)
						      maps))))))))
		 ((symbolp form)
		  (cond ((keywordp form)
			 (funcall emitter form maps))
			(t
			 (let ((expansion (expand-symbol form)))
			   ;; (format t "EXPANDING ~S -> ~S~%" form expansion)
			   (if (and (listp expansion)
				    (eq (car expansion) 'pattern-var))
			       (let ((level (third expansion)))
				 (if (< (length maps) level)
				     (error "missing ellipsis")
				     (gen-ref (fourth expansion) level maps
					      #'(lambda (form maps)
						  (funcall
						   emitter form maps)))))
			       (funcall emitter `(dylan::quote ,form) maps))))))
		 ((simple-vector-p form)
		  (grovel (coerce form 'list) maps
			  #'(lambda (guts maps)
			      (funcall emitter
				       (cond
					 ((constantp guts)
					  (coerce (eval guts) 'simple-vector))
					 ((atom guts)
					  `(dylan::as dylan::<vector>
                                                      ,guts))
					 (t
					  (case (car guts)
					    (list
					     `(dylan::vector ,@(cdr guts)))
					    (t
					     `(dylan::as dylan::<vector>
                                                         ,guts)))))
				       maps))))
		 (t
		  (funcall emitter
			   (if (constantp form) form `(dylan::quote ,form))
			   maps)))))
      (translate-dylan-expr
       (grovel form nil
	       #'(lambda (form maps)
		   (declare (ignore maps))
		   form))
       env))))

(define-dylan-translator with-syntax translate-with-syntax (form env)
  (destructuring-bind (_ bindings . body) form
    (declare (ignore _))
    (labels ((grovel (bindings)
	       (if bindings
		   (destructuring-bind (pattern expression) (car bindings)
		     `(dylan::syntax-case ,expression ()
		       (,pattern ,(grovel (cdr bindings)))))
		   `(dylan::begin ,@body))))
      (translate-dylan-expr (grovel bindings) env))))

(define-dylan-translator define-translator-syntax 
  translate-define-translator-syntax (form env)
  (destructuring-bind (_ name (form) literals . clauses) form
    `(progn
       (setf (gethash ',name *dylan-translators*)
             (make-dylan-expander 
              ,(translate-dylan-expr
                `(method (&rest ,form)
                   ((access internal strip)
		    (syntax-case ,form ,literals
                                 ,@clauses)))
                env)))
       (values))))

(define-dylan-translator define-modular-translator-syntax 
  translate-define-modular-translator-syntax (form env)
  (destructuring-bind (_ name (form) literals . clauses) form
    `(eval-when (compile load eval)
       (progn
         (set-dylan-translator ',name 
             (make-dylan-expander 
              ,(translate-dylan-expr
                `(method (&rest ,form)
                   ((access internal strip)
		    (syntax-case ,form ,literals
                                 ,@clauses)))
                env)))
         (values)))))

(define-dylan-translator define-zimmerman-syntax
  translate-define-zimmerman-syntax (form env)
  (destructuring-bind (_ name forms literals . clauses) form
    (declare (ignore _))
    (let* ((form (first forms))
	   (wrapper (gen-id "WRAPPER-"))
	   (new-wrapper (gen-id "NEW-WRAPPER-"))
	   (new-form (gen-id "NEW-FORM-"))
	   (definition
	       `(dylan::method (,form ,wrapper)
		 (dylan::bind ((,new-wrapper ((access internal dylan::new-mark-wrapper)))
			      (,new-form
			       (dylan::syntax-case
				   ((access internal dylan::wrap)
				    ,form
				    ((access internal dylan::join-wrappers)
                                      ,new-wrapper
				      ,wrapper))
				   ,literals
				 ,@clauses)))
		   (values ((access internal dylan::wrap) ,new-form '()
			    :context 
                              (access internal dylan::*internal-context*))
			   ,new-wrapper)))))
      (translate-dylan-expr
        `((access internal dylan::%define-syntax) 'internal ',name ,definition)
        env))))

;; Local macros.

(define-dylan-translator bind-syntax translate-bind-syntax (form env)
  (destructuring-bind (_ bindings . body) form
    (if (null bindings)
      (translate-dylan-expr `(dylan::begin ,@body) env)
      (destructuring-bind (name (form) literals . clauses) (first bindings)
        (let ((expander
                (make-dylan-expander
                  (dylan-eval
		     `(method (&rest ,form)
                        ((access internal strip)
		          (syntax-case ,form ,literals
                             ,@clauses)))))))
          (translate-dylan-expr
            `(bind-syntax ,(rest bindings) ,@body)
             (add-translator-to-env name expander env)))))))

;; Zimmerman uses define-syntax by default.

(define-dylan-macro define-syntax (_ &rest stuff)
  `(begin
     (define-zimmerman-syntax ,@stuff)))

(define-dylan-macro define-syntax-everywhere (_ &rest stuff)
  `(begin
     (define-translator-syntax ,@stuff)
     (define-zimmerman-syntax ,@stuff)))

(define-dylan-macro define-modular-syntax-everywhere (_ &rest stuff)
  `(begin
     (define-modular-translator-syntax ,@stuff)
     (define-zimmerman-syntax ,@stuff)))

;; Infix macro defining forms.

(define-dylan-macro define-infix-defining-syntax (_ define name &rest stuff)
  `(begin
     (register-defining-macro ',name)
     (define-translator-syntax ,(concatenate-symbols 'infix-define- name)
       ,@stuff)))

(define-dylan-macro define-infix-block-0-syntax (_ name &rest stuff)
  `(begin
     (register-statement-0-macro ',name)
     (define-translator-syntax ,(concatenate-symbols 'infix- name)
       ,@stuff)))

(define-dylan-macro define-infix-block-1-syntax (_ name &rest stuff)
  `(begin
     (register-statement-1-macro ',name)
     (define-translator-syntax ,(concatenate-symbols 'infix- name)
       ,@stuff)))

(define-dylan-macro define-infix-block-2-syntax (_ name &rest stuff)
  `(begin
     (register-statement-2-macro ',name)
     (define-translator-syntax ,(concatenate-symbols 'infix- name)
       ,@stuff)))

;; eof
