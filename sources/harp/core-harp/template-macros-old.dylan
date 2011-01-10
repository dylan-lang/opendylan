module:    harp
language:  prefix-dylan
Synopsis:  Macro support for the HARP template pattern matchers
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


;;;; Macros for handling the backend templates
;;;; Started by Tony (8 Apr 94)

;;; First define the infix part


;; reserve the keywords

(define-infix-intermediate-word pattern <details-intermediate-word>)
(define-infix-intermediate-word options <details-intermediate-word>)



;;; DEFINE-TEMPLATE-DEFINER defines an infix macro for defining templates for 
;;; a given instruction set. The macros so defined adds a function definition
;;; into the code-gen-fn slot of the named op(s) for the instruction set. If 
;;; the SELF option is used, then the actual op is passed as a parameter.


(define-translator-syntax define-template-definer (form) ()
   ((_ ?name ?instruction-set)
    (with-syntax ((?dots (syntax ...)))
     (syntax
      (define-infix-macro ?name <clauses-defining-word> (form) ()
         ((_ (??modifiers ?dots) ??names ??dummy ??patterns ?dots)
          (syntax 
           (define-general-harp-template 
            (set-instruction-template-function ?instruction-set)
            (op-for-name ?instruction-set)
            ??names
            ??patterns ?dots))))))))




;;; DEFINE LOCAL-TEMPLATE defines a function into a namespace parallel
;;; to the module namespace, and accessible via LOCAL-FN. If the SELF 
;;; option is used, then the value of the normal named module variable 
;;; is passed as a parameter.

(define-infix-macro local-template <clauses-defining-word> (form) ()
   ((_ (?modifiers ...) ?names ?dummy ?patterns ...)
    (syntax 
     (define-general-harp-template 
      (define-local-template-function) 
      (local-name)
      ?names
      ?patterns ...))))




#|

;; special word BACKEND keeps the backend parameter (suppressed by default)
;; special word SELF passes the OP as an initial first parameter.

Usage in infix Dylan:

define harp-template (and)
 using pentium-instruction-set;
 options (self, no-backend);
 pattern (a by func, (b :: any by func2))
   a;
   b;

 pattern (..)
   ..

end;

|#


;;; now define the prefix part


(define-translator-syntax define-general-harp-template (form) (self no-backend)
   ((_ (?definer ...) (?self-getter ...) ?names ?expr ...)
    (with-syntax (((?name ...) (name-or-names (syntax ?names)))
                  (((?options ...) ?patterns ...)
                   (separate-template-options (syntax (?expr ...))))
                  ((?vars ...)
                   (template-parameters (syntax (?patterns ...))))
                  (?inner-function
                   (syntax (compile-template (?vars ...) ?patterns ...))))

      (syntax-case (syntax (?options ...)) (self no-backend)

        ((self no-backend)
         ;; self no-backend: Add extra self argument - suppress backend
         (with-syntax (((?self ?varlist ...) (syntax (?vars ...))))
           (syntax
            (bind ((template-fn ?inner-function))
              (?definer ... ?name
	       (method (backend ?varlist ...)
	         (bind ((self-name (?self-getter ... ?name))) 
		   (template-fn self-name ?varlist ...)))) ...
              ))))

        ((self)
         ;; self: Add extra self argument, keep backend
         (with-syntax (((?be ?self ?varlist ...) (syntax (?vars ...))))
           (syntax
            (bind ((template-fn ?inner-function))
              (?definer ... ?name
	       (method (backend ?varlist ...)
		 (bind ((self-name (?self-getter ... ?name))) 
		   (template-fn backend self-name ?varlist ...)))) ...
              ))))

        ((no-backend)
         ;; no-backend: Strip the backend parameter
         (syntax
          (bind ((template-fn (method (backend ?vars ...)
                                (?inner-function ?vars ...))))
            (?definer ... ?name template-fn) ...
            )))

        (()
         ;; no keys: Pass the parameters through exactly
         (syntax
          (bind ((template-fn ?inner-function))
            (?definer ... ?name template-fn) ...
            )))
        ))))


;;; Namespace accessors for harp templates:

(define-translator-syntax set-instruction-template-function (form) ()
   ((_ ?instruction-set ?name ?function)
    (with-syntax ((?template-fn (op-accessor-name (syntax code-gen-fn))))
      (syntax (set! (?template-fn (op-for-name ?instruction-set ?name))
                    ?function)))))

(define-translator-syntax op-for-name (form) ()
   ((_ ?instruction-set ?name)
    (with-syntax ((?op-name (ins-accessor-name (syntax ?name))))
      (syntax (?op-name ?instruction-set)))))


;;; Namespace accessors for local templates:

(define-translator-syntax define-local-template-function (form) ()
   ((_ ?name ?function)
    (with-syntax ((?ins-name (ins-accessor-name (syntax ?name))))
      (syntax (define ?ins-name ?function)))))


;; local-name find the SELF value for a local template. It just translates
;; into the module variable.

(define-translator-syntax local-name (form) ()
   ((_ ?name)
    (syntax ?name)))

;; local-fn returns the location of the function definition of a local 
;; template. It translates into HARP-<name>;

(define-translator-syntax local-fn (form) ()
   ((_ ?name)
    (with-syntax ((?ins-name (ins-accessor-name (syntax ?name))))
      (syntax ?ins-name))))


(define-method name-or-names (expr)
  ;; return a parenthesised list of names from either a 
  ;; single name or a parenthesised list.
  (syntax-case expr ()
    ((?name ...) (syntax (?name ...)))
    (?name       (syntax (?name)))))


(define-method separate-template-options (expr)
  (syntax-case expr (options)
    (((options (((?options) ...)) ?dummy) ?patterns ...)
     (syntax ((?options ...) ?patterns ...)))
    ((?patterns ...)
     (syntax (() ?patterns ...)))))


(define-method template-parameters (expr)
  ;; There must be a better way of doing this
  ;; NB I really can't imagine any template taking more that 8 args.
  (syntax-case expr (pattern)
    (() (syntax ()))

    (((pattern (?args) ?body) ?others ...)
     (syntax-case (syntax ?args) (pattern)
       (()
        (syntax ()))
       ((b1)
        (syntax (.a1.)))
       ((b1 b2)
        (syntax (.a1. .a2.)))
       ((b1 b2 b3)
        (syntax (.a1. .a2. .a3.)))
       ((b1 b2 b3 b4)
        (syntax (.a1. .a2. .a3. .a4.)))
       ((b1 b2 b3 b4 b5)
        (syntax (.a1. .a2. .a3. .a4. .a5.)))
       ((b1 b2 b3 b4 b5 b6)
        (syntax (.a1. .a2. .a3. .a4. .a5. .a6.)))
       ((b1 b2 b3 b4 b5 b6 b7)
        (syntax (.a1. .a2. .a3. .a4. .a5. .a6. .a7.)))
       ((b1 b2 b3 b4 b5 b6 b7 b8)
        (syntax (.a1. .a2. .a3. .a4. .a5. .a6. .a7. .a8.)))))))


(define-translator-syntax compile-template (form) ()
   ((_ (?varlist ...) ?pattern ...)
    (with-syntax ((?exit (syntax .exit.)))
      (syntax 
       (method (?varlist ...)
	 (bind-exit (?exit)
           (compile-clause ?exit (?varlist ...) ?pattern) ...
           (pattern-error ?varlist ...)
           ))))))



(define-translator-syntax compile-clause (form) (pattern)
   ((_ ?exit (?varlist ...) (pattern ((?pattern ...)) ?body))
    (syntax-case (template-clause-conditions
                  (syntax ((?pattern ...) (?varlist ...))) ())
                 ()
      (((?mapping ...) (?condition ...))
       (syntax 
	(bind (?mapping ...)
	  (when (and  ?condition ...)
	    (?exit ?body))))))))

   

;;; template-clause-conditions works out what bindings and tests to 
;;; do for each pattern variable.
;;;
;;; patterns is a syntax object of the form 
;;; ( (((pattern-var1 type) by fn) ...) (varlist-var1 ...) )
;;;
;;; mappings is an alist of (pattern-var . varlist-var) for all
;;; pattern variables encountered so far
;;;
;;; The returned value is a syntax object of the form
;;; ( ((pattern-var varlist-var) ...) (condition ...) )

(define-method template-clause-conditions (patterns mappings)
  (syntax-case patterns ()
    ((() ())
     (syntax (() ())))

    (((?thisp ?nextp ...) (?vvar ?nextv ...))
     (bind-methods ((process-template-binding (this-pvar this-cond)
       (bind ((last-binding (template-assoc this-pvar mappings))
	      (new-maps (if last-binding
                            mappings
                          (template-alist-add 
                           this-pvar (syntax ?vvar) mappings)))
	      (remaining-clauses  (template-clause-conditions
				   (syntax ((?nextp ...) (?nextv ...)))
				   new-maps)))
          (with-syntax ((?this-cond this-cond)
                        (?last-var last-binding)
                        (?pvar this-pvar))
            (syntax-case remaining-clauses ()
	      (((?mapping ...) (?condition ...))
	       (if last-binding
		 (syntax ((?mapping ...)
			  ((id? ?last-var ?vvar) ?condition ...)))
		 (syntax (((?pvar ?vvar) ?mapping ...)
                          (?this-cond ?condition ...))))))))))
       (syntax-case (syntax ?thisp) (any by)
	 (((?pvar any))
	  (process-template-binding (syntax ?pvar)
            (syntax (begin (set! ?pvar ?vvar) #T))))
	 ((?pvar)
	  (process-template-binding (syntax ?pvar)
            (syntax (set! ?pvar ?vvar))))
	 (((?pvar any) by ?function)
	  (process-template-binding (syntax ?pvar)
            (syntax (begin (set! ?pvar (?function ?vvar)) #T))))
         ((?pvar by ?function)
	  (process-template-binding (syntax ?pvar) 
	    (syntax (set! ?pvar (?function ?vvar)))))
       )))))

(define-method template-alist-add (pvar vvar (alist <list>))
  (pair (pair pvar vvar) alist))

(define-method template-assoc (pattern-var (alist <list>))
  (bind-exit (found)
    (do (method ((pvar-vvar <pair>))
          (bind ((pvar (head pvar-vvar))
                 (vvar (tail pvar-vvar)))
            (when (bound-identifier=? pattern-var pvar)
              (found vvar))))
        alist)
    #F))

