(in-package :dylan)

;;
;;; The Dylan class definer.
;;;
;;;   Lifted wholesale from the LW CLOS sources and hacked a bit. 
;;;
;;

(define-dylan-translator define-class translate-define-class (form env)
  (destructuring-bind (_ name superclasses &rest slots) form
    (expand-define-class '(find-class 'clos::dylan-class)
		         name 
		         superclasses
		         slots
                         env)))

(defun expand-define-class (metaclass name superclasses slots env)
  (let ((inits '())
        (defs  '())
        (keywords '()))
    (loop for slot in slots
	  do
            (if (and (consp slot) (keywordp (first slot)))
              (multiple-value-bind
                  (keyword default-value default-function)
                    (canonicalize-define-class-keyword name slot env)
                (cond
                  (default-function
                    (push 
                      `(list ,keyword ',default-function ,default-function)
		     keywords))
                  (default-value
                    (push `(list ,keyword ,default-value 1)
                          keywords))
                  (t)))
	      (multiple-value-bind
                  (init-form def-form virtual?)
                    (canonicalize-define-class-slot name slot env)
                (unless virtual?
                  (push init-form inits))
		(push def-form defs))))
    `(compiler::top-level-form
         (define-class ,(lw-debug-name name))
       (progn
         ,@defs
         (let ((.the-initargs.
                 (list
                   :debug-name   ',name
		   :superclasses 
		     (list ,@(translate-dylan-exprs superclasses env))
	           :slots
		     (list ,@inits)
                   :default-initargs
                     (list ,@keywords))))
           (if (boundp 
                 (dylan-resolve-name ,name 
                   :module-name ,(current-module-name)))
               (apply #'reinitialize-instance 
                 (dylan-resolve ,name :module-name ,(current-module-name))
                 .the-initargs.)
             (dylan-define ,name 
               (apply #'make-instance ,metaclass .the-initargs.))))
         (values)))))

(defun canonicalize-define-class-slot (class-name slot env)
  (when (symbolp slot) (setq slot (list slot)))
  (let ((name (first slot))
	(getters nil)
	(setters nil)
        (no-setter nil)
	(type nil)
	(type-seen nil)
	(allocation nil)
	(allocation-seen nil)
	(init-value nil)
        (init-function nil)
        (init-seen nil)
        (keyword nil)
        (required-keyword nil)
        (keyword-seen nil)
        (required-keyword-seen nil))
    (cond
      ((keywordp name)
        (setq name nil))
      (t
        (setq slot (cdr slot))))
    (loop for (option value . rest) on slot by 'cddr
	  do
	  (case option
	    (:getter 
              (push value getters))
	    (:setter 
              (if (eq value *dylan-canonical-false*)
                nil ;; (setq no-setter t)
                (push value setters)))
	    (:type
	      (when type-seen 
		(error "repeated :type in ~S" slot))
	      (setq type-seen t
		    type value))
	    (:allocation
	      (unless (eq value 'inline)  ; ignore "inline"
	        (when allocation-seen
	          (error "repeated :allocation in ~S" slot))
	        (setq allocation-seen t
		      allocation value)))
	    (:init-value
	      (when init-seen 
                (error "repeated initialisation option in ~S" slot))
              (when required-keyword-seen 
                (error "both required keyword and init options in ~S" slot))
	      (setq init-seen (if (and (consp value)
				       (eq (car value ) 'load-time-value))
				  :load-time-value
				t)
		    init-value value))
	    (:init-function
	      (when init-seen
                (error "repeated initialisation option in ~S" slot))
              (when required-keyword-seen 
                (error "both required keyword and init options in ~S" slot))
	      (setq init-seen t
		    init-function value))
	    (:init-keyword
              (when keyword-seen (error "repeated init keyword option in ~S" slot))
              (setq keyword value
                    keyword-seen t))
	    (:required-init-keyword
              (when keyword-seen (error "repeated init keyword option in ~S" slot))
              (when init-seen (error "both required keyword and init options in ~S" slot))
              (setq required-keyword value
                    keyword-seen t))
	    (:sealed
	      nil)
	    (t
              (error "unrecognised slot option ~s ~s" option value))))
    (values
     `(list
       ,@(if getters 
	     `(:getter
	      (dylan-resolve ,(first getters)
			     :module-name ,(current-module-name))
	      :getter-name 
	      (dylan-resolve-name ,(first getters)
				  :module-name ,(current-module-name)))
	   (if name
	       `(:getter 
		(dylan-resolve ,name
			       :module-name ,(current-module-name))
		:getter-name
		(dylan-resolve-name ,name
				    :module-name ,(current-module-name)))))
       ,@(if setters 
	     `(:setter (dylan-resolve ,(first setters)
				      :module-name ,(current-module-name))
	      :setter-name (dylan-resolve-name ,(first setters)
					       :module-name ,(current-module-name)))
	   (if (and name (not no-setter))
	       `(:setter (dylan-resolve ,(make-setter-name name)
					:module-name ,(current-module-name))
		:setter-name (dylan-resolve-name ,(make-setter-name name)
						 :module-name ,(current-module-name)))))
       ,@(if allocation-seen `(:allocation ',allocation))
       ,@(if init-value 
	     `(:init-value ,(translate-dylan-expr init-value env)))
       ,@(if init-function 
	     `(:init-function ,(translate-dylan-expr init-function env)))
       ,@(if type `(:delayed-type #'(lambda () ,(translate-dylan-type type env))))
       ,@(if required-keyword `(:required-init-keyword ',required-keyword))
       ,@(if keyword `(:init-keyword ,keyword)))
     `(progn
       ,@(if getters `((dylan-ensure-generic ,(first getters) (x)
					     :module-name ,(current-module-name)))
	   (if name
	       `((dylan-ensure-generic ,name (x)
				       :module-name ,(current-module-name)))))
       ,@(if setters `((dylan-ensure-generic ,(first setters) (x y)
					     :module-name ,(current-module-name)))
	   (if (and name (not no-setter))
	       `((dylan-ensure-generic ,(make-setter-name name) (x y)
				       :module-name ,(current-module-name))))))
     (eq allocation 'virtual))))

(defun canonicalize-define-class-keyword (class-name slot env)
  (let ((init-function nil) (init-value nil) (init-seen nil))
    (loop for (option value . rest) on (rest slot) by 'cddr
          do
            (case option
              (:init-value
                 (when init-seen
                   (error "Second initialization option ~S in ~S of class ~S"
                          option slot class-name))
                 (setq init-value
                   (translate-dylan-expr value env)))
              (:init-function
                 (when init-seen
                   (error "Second initialization option ~S in ~S of class ~S"
                          option slot class-name))
                 (setq init-function
                   (translate-dylan-expr value env)))))
    (values (first slot) init-value init-function)))

;; eof

