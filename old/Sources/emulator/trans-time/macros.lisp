(in-package :dylan)

;; Macros used in the definition of the translator:

(eval-when (:load-toplevel :execute)

  (defmacro define-simple-dylan-translator (special-form translator)
    `(set-primitive-dylan-translator ',special-form ',translator))

  (defmacro define-complex-dylan-translator 
    (&whole whole special-form translator args &rest body)
    (if (= (length args) 2)
      `(progn
         (defun ,translator ,args ,@body)
         (define-simple-dylan-translator ,special-form ,translator))
      (error 
        "in ~s the arglist ~s must name just the form and the lexical-env"
        whole args)))

  (defmacro define-dylan-translator (special-form translator &rest rest)
    (if rest
      `(define-complex-dylan-translator ,special-form ,translator ,@rest)
      `(define-simple-dylan-translator ,special-form ,translator)))

  (defmacro undefine-dylan-translator (special-form &rest rest)
    `(remove-dylan-translator ',special-form))

  (defmacro define-dylan-macro (name &rest stuff)
    `(progn
       (set-primitive-dylan-translator 
         ',name
	 (make-dylan-expander #'(lambda ,@stuff)))))

  (defmacro define-modular-dylan-macro (name &rest stuff)
    `(progn
       (set-modular-dylan-translator 
         ',name
	 (make-dylan-expander #'(lambda ,@stuff)))))

  (defmacro reading-as-translated-dylan 
       ((&key (language ''prefix-dylan) (module nil) (header nil))
        &rest forms)
    `(let 
       ((*package* *the-dylan-package*)
	(*readtable* (find-translating-readtable ,language))
	(*dylan-reader-state* (if ,header :read-header *dylan-reader-state*))
        (*current-module* 
          ,(if module `(find-translator-module ,module) '*current-module*))
        (*current-library* *current-library*))
       (declare 
         (special *dylan-reader-state* *current-module* *current-library*))
       ,@forms))

)

;; eof

