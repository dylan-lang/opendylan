;; Emulation for a Dylan ffi on top of LW ffi system.

(in-package :dylan)

(defun concatenate-symbols-like (template &rest syms)
  (intern 
    (apply #'concatenate 'string (mapcar #'symbol-name syms))
    (symbol-package template)))

(defun concatenate-symbols (&rest syms)
  (apply #'concatenate-symbols-like (first syms) syms))

(define-dylan-translator
  define-alien-type translate-define-alien-type (form env)
  (destructuring-bind (_ name spec) form
    (multiple-value-bind
      (translated-spec reader-names writer-names) 
        (parse-alien-type-spec spec (symbol-name name))
      (let* ((ref-name (concatenate-symbols 'my- name '-ref))
             (maker-name (concatenate-symbols 'make- name))
             (array-maker (concatenate-symbols 'make- ref-name))
             (aget (concatenate-symbols ref-name '->))
             (aset (concatenate-symbols 'set- aget)))
        `(progn
           (ffi:define-foreign-type ,name ,translated-spec)
           (ffi:define-foreign-type ,ref-name (:pointer ,name))
           ,@(mapcar
              #'(lambda (reader-name writer-name)
                  (let ((lisp-reader 
                         (concatenate-symbols name '-> reader-name)))
		    `(progn
		       (dylan-define ,reader-name 
				     #',lisp-reader)
		       (dylan-define ,writer-name
				     #'(lambda (val o)
                                         (,(concatenate-symbols 'set-
                                                                lisp-reader)
                                           o val))))))
              reader-names
              writer-names)
	   (dylan-define ,name
              (,(%runtime-resolve-name 'fudge-alien-class)
                (if (not (fboundp ',maker-name)) *dylan-canonical-false*
		  (function ,maker-name))
                (if (not (fboundp ',array-maker)) *dylan-canonical-false*
		  (function ,array-maker))
                (if (not (fboundp ',aget)) *dylan-canonical-false*
		  (function ,aget))
                (if (not (fboundp ',aset)) *dylan-canonical-false*
		  (function ,aset))
		',name
		',ref-name)))))))

(defun parse-alien-type-spec (spec name)
  (if (symbolp spec)
    (values spec '() '())
    (destructuring-bind (constructor &rest args) spec
      (ecase constructor
        ((:pointer-to)
          (parse-alien-pointer-spec (first args)))
        ((:structure-of)
          (parse-alien-structure-spec args name))
        ((:array-of)
          (parse-alien-array-spec (first args) (third args)))))))

(defun parse-alien-pointer-spec (type-spec)
  `(:pointer ,(parse-alien-type-spec type-spec '<anon>)))

(defun parse-alien-structure-spec (fields name)
  (let* ((getters (mapcar #'first fields))
         (setters (mapcar #'make-setter-name getters))
         (types (mapcar #'second fields)))
    (values
      `(:structure
        ,@(mapcar #'(lambda (getter type)
                      `(,getter ,(parse-alien-type-spec type '<anon>)))
                  getters
                  types))
     getters
     setters)))

(defun parse-alien-array-spec (type size)
  `(:array ,(parse-alien-type-spec type '<anon>) ,size)) ;

(define-dylan-translator
  import-alien-function translate-import-alien-function (form env)
  (destructuring-bind (_ alien-name formals &key as) form
    (multiple-value-bind (args vals) (split-at formals '&return-types)
      `(progn
         (ffi:define-foreign-function 
           (,(%runtime-resolve-name as) ,alien-name :source)
	   ,(mapcar #'(lambda (formal)
                        `(,(car formal) ,@(translate-alien-formal formal)))
                    args)
           ,@(unless (null vals)
               `(:result-type ,@(translate-alien-formal (car vals))))
           :no-check t)
         (dylan-define ,as (function ,(%runtime-resolve-name as)))))))

(define-dylan-translator 
  export-as-alien-function translate-import-as-alien-function (form env)
  (destructuring-bind (_ alien-name formals &key function reference) form
    (multiple-value-bind (args vals) (split-at formals '&return-types)
      `(progn
         (ffi:foreign-callable 
           ,(%runtime-resolve-name function)
	   ,(mapcar 
	      #'(lambda (formal)
		  (let ((trans
			  (first (translate-alien-formal formal))))
		    (if (eq trans '<c/int>) 
                      :integer
                      `(:alien ,trans))))
	     args)
           :foreign-name ',(make-symbol alien-name)
           :no-check t
           ,@(unless (null vals)
               `(:result-type  ,(translate-alien-formal (car vals)))))
         (dylan-define ,reference 
                       #'(lambda ()
                           (ffi::make-typed-alien 
                             :function
			     (system::integer-raw-int 
                               (ffi::foreign-symbol-address 
                                 ,(ffi::lisp-name-to-foreign-name alien-name
                                                                   :encode :source))))))))))

(define-dylan-translator
  import-alien-variable translate-import-alien-variable (form env)
  (destructuring-bind (_ alien-name &key type as) form
    `(progn
       (ffi:define-foreign-variable 
        (,(%runtime-resolve-name as)
         ,alien-name :source)
        :result-type ,(parse-alien-type-spec type '<anon>))
       (dylan-define ,as (function ,(%runtime-resolve-name as))))))

(define-dylan-translator
  import-alien-constant translate-import-alien-constant (form env)
  (translate-import-alien-variable form env))

(defun translate-alien-formal (formal)
  (destructuring-bind (name type) formal
    `(,@(if (not (consp type))
	    (list (parse-alien-type-spec type '<anon>))
	  (ecase (first type)
	    ((:by-reference)
	      (list (parse-alien-type-spec (second type) '<anon>)
		    :reference)))))))


(defun split-at (list value)
  (let ((before '()))
    (do ((l list (cdr l)))
        ((null l) (values list '()))
      (when (eq (car l) value)
        (return-from split-at (values (reverse before) (cdr l))))
      (push (car l) before))))

;; eof
