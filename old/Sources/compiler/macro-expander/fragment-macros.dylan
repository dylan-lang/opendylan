Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Fragment macros
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

#|

 Example:

   define fragment <slot-access-fragment> (<call-fragment>)
     fragment slots function, arguments;
   end fragment;

 ->

   define class <slot-access-fragment> (<call-fragment>)
     slot function, 
       required-init-keyword: function:;
     slot arguments,
       required-init-keyword: arguments:;
   end class;

   define method slot-access-fragment (#key function, arguments)
     make(<slot-access-fragment>, function: function, arguments: arguments);
   end method;

|#

(define-infix-macro fragment <defining-word> (form) ()
  ((_ ?mods ?name (?super ...) ?slot ...)
    (bind ((slots '())
           (vars '())
           (args '())
           (slot-val-args '()))
      (for ((slot in (syntax (?slot ...))))
	(syntax-case slot (slot slots fragment source properties)
          ((fragment slots ?var ...)
             (for ((var in (syntax (?var ...))))
               (with-syntax 
                 ((?var var)
		  (?key (as-keyword (strip var))))
                 (set! vars (pair (syntax ?var) vars))
                 (set! args (pair (syntax ?var) args))
                 (set! args (pair (syntax ?key) args))
                 (set! slot-val-args 
                       (pair (syntax (evaluable-fragment (?var f)))
                             slot-val-args))
                 (set! slot-val-args (pair (syntax ?key) slot-val-args))
                 (set! slots 
                       (pair (syntax (slot ?var required-init-keyword: ?key))
                             slots)))))
          ((source properties ?stuff ...)
             #t)
          ((?stuff ...)
            (set! slots (pair (syntax (?stuff ...)) slots)))))
      (with-syntax (( (?gen-slot ...) slots)
                    ( (?arg ...) args)
                    ( (?slot-val-arg ...) slot-val-args)
                    ( (?var ...) vars)
                    ( ?constructor 
		        (as <symbol>
                            (concatenate
                              (strip-angle-brackets (strip (syntax ?name))))))
                    ( ?prefix-constructor
                        (as <symbol>
                            (concatenate "prefix-" 
                              (as <string>
                                  (strip-angle-brackets
                                    (strip (syntax ?name))))))))
        (syntax
          (begin
            (infix-define-class ?mods ?name (?super ...) ?gen-slot ...)
            ;; (define-method ?constructor (#key ?var ...)
            ;;   (make ?name ?arg ...))
            (define-method ?constructor (#rest args)
              (apply make ?name args))
            ;; (define-method evaluable-fragment ((f ?name))
            ;;  (?prefix-constructor ?slot-val-arg ...))
))))))

(define-method strip-angle-brackets (symbol)
  (bind ((name (as <string> symbol)))
    (copy-sequence name start: 1 end: (- (size name) 1))))

;; eof
