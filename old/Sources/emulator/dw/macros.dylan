module:   internal
language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Copyright (c) 1993, Jonathan Bachrach
;;; Copyright (c) 1993, Functional Objects, Inc.
;;; Copyright (c) 1993, IRCAM
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without modifi-
;;; cation,  are not permitted without the express prior written permission of
;;; the copyright holders.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY    JONATHAN BACHRACH,     Functional Objects, Inc.
;;; LIMITED, AND IRCAM ``AS IS'' AND  ANY EXPRESS OR  IMPLIED WARRANTIES,  IN-
;;; CLUDING,  BUT  NOT LIMITED TO,  THE IMPLIED WARRANTIES OF  MERCHANTABILITY 
;;; AND  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.   IN NO EVENT  SHALL 
;;; JONATHAN BACHRACH, Functional Objects, Inc., OR IRCAM BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING,  BUT NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR  SER-
;;; VICES;  LOSS OF USE, DATA, OR PROFITS;  OR  BUSINESS INTERRUPTION) HOWEVER
;;; CAUSED AND  ON ANY THEORY OF LIABILITY,  WHETHER IN CONTRACT,  STRICT LIA-
;;; BILITY,  OR TORT  (INCLUDING NEGLIGENCE OR  OTHERWISE)  ARISING IN ANY WAY 
;;; OUT OF  THE USE OF THIS SOFTWARE,  EVEN IF  ADVISED OF THE POSSIBILITY  OF 
;;; SUCH DAMAGE.

;;; FOR LOOP WITH NEW ITERATION PROTOCOL

(define-method variable-name (variable-specification)
  (syntax-case variable-specification ()
    ((name type) 
     (identifier? (syntax name))
     (syntax name))
    (name
     (identifier? (syntax name))
     (syntax name))))

(define-method extract-multiple-value-bind ((binding <list>))
  (iterate extract
      (((variables <list>) '()) ((parameters <list>) '())
       ((remaining <list>) binding) ((rest? <boolean>) #F))
    (if (empty? (rest remaining))
	(values (first remaining) (reverse! variables) (reverse! parameters)
                rest?)
	(bind ((current (first remaining)))
	  ;; !@#$ seems bogus to access #rest this way
	  (if (id? (strip current) '#rest)
	      (if (= (size remaining) 3)
		  (extract (add! variables (second remaining))
                           (add! (add! parameters '#rest) (second remaining))
			   (rest (rest remaining)) #T)
		  (compiler-error "Invalid use of bind"))
	      (extract (add! variables current) (add! parameters current)
		       (rest remaining) #F))))))

(define-method extract-variables ((parameters <list>))
  (choose (method (x) (not (id? (strip x) '#rest))) parameters))

(define-method parse-step-clause (variable init-value next-value)
  ;;; extension for multiple-values ((x y) = (location point))
  (with-syntax 
      (((?parameter ...)
	(syntax-case variable ()
	  ((?variable ...) (syntax (?variable ...)))
	  (?variable (syntax (?variable)))))
       ((?variable ...)
        (extract-variables (syntax (?parameter ...))))
       ((?variable-name ...)
        (map variable-name (syntax (?variable ...))))
       ((?tmp-parameter ...)
        (map (method (variable) 
               (if (id? (strip variable) '#rest)
                   variable
                   (as <symbol>
                       (format #F "~A-tmp" (strip (variable-name variable))))))
             (syntax (?parameter ...))))
       ((?tmp-variable ...)
        (extract-variables (syntax (?tmp-parameter ...))))
       (?init-value init-value)
       (?next-value next-value))
    (values
     (syntax (?variable ...))
     '()
     (syntax (?variable ...))
     '()
     (list (syntax (?tmp-parameter ... ?init-value)))
     (syntax (?tmp-variable ...))
     (list (syntax (?tmp-parameter ... ?next-value)))
     (syntax (?tmp-variable ...))
     (syntax (?variable-name ...))
     '()
     #F)))

(define-method parse-in-clause 
    (variable key-variable collection 
     #key (protocol 'forward-iteration-protocol))
  (bind ((variable-name (strip (variable-name variable))))
     (with-syntax 
	 ((collection collection)
          (variable variable)
          (key-variable key-variable)
          (collection-temporary
	   (as <symbol> (format #F ".~A-collection." variable-name)))
	  (state-temporary
	   (as <symbol> (format #F ".~A-state." variable-name)))
	  (initial-state-temporary
	   (as <symbol> (format #F ".~A-initial-state." variable-name)))
	  (limit-temporary
	   (as <symbol> (format #F ".~A-limit." variable-name)))
	  (next-state-temporary
	   (as <symbol> (format #F ".~A-next-state." variable-name)))
	  (finished-state?-temporary
	   (as <symbol> (format #F ".~A-finished-state?." variable-name)))
	  (current-element-temporary
	   (as <symbol> (format #F ".~A-current-element." variable-name)))
	  (current-key-temporary
	   (as <symbol> (format #F ".~A-current-key." variable-name)))
	  (iteration-protocol protocol)
          ((ignorable-form ...)
           (if key-variable
               '()
               ;; (syntax ((ignorable current-key-temporary))))))
               (syntax (current-key-temporary)))))
       (values
	(list (syntax state-temporary))
	(concatenate
         (list (syntax (variable (current-element-temporary
                                  collection-temporary state-temporary))))
         (if key-variable
             (list (syntax (key-variable 
                            (current-key-temporary
			     collection-temporary state-temporary))))
             '()))
        '()
	(list (syntax (collection-temporary collection))
              (syntax (initial-state-temporary limit-temporary 
                       next-state-temporary finished-state?-temporary 
                       current-key-temporary current-element-temporary
                       (iteration-protocol collection-temporary))))
        '()
	(list (syntax (begin ignorable-form ... initial-state-temporary)))
        '()
	(list (syntax (next-state-temporary
                       collection-temporary state-temporary)))
	'()
	(list (syntax (finished-state?-temporary
                       collection-temporary state-temporary limit-temporary)))
	#F))))

(define-method parse-from-clause
    (variable limit from to above below increment)
  (with-syntax 
      ((limit limit)
       (increment increment)
       (variable-name (variable-name variable))
       (limit-temporary
	(as <symbol>
	    (format #F "~A-limit" (strip (syntax variable-name)))))
       (increment-temporary
	(as <symbol>
	    (format #F "~A-increment" (strip (syntax variable-name))))))
    (bind ((limit? (strip limit)))
      (values
       (list variable)
       '()
       (list (syntax variable-name))
       (concatenate (list (syntax (increment-temporary increment)))
                    (if limit? (list (syntax (limit-temporary limit))) '()))
       '()
       (list from)
       '()
       (list (syntax (+ variable-name increment-temporary)))
       (list (syntax variable-name))
       (if limit?
	   (list
            (cond ((strip below)
		   (syntax
		    (not (< variable-name limit-temporary))))
		  ((strip above)
		   (syntax
		    (not (< limit-temporary variable-name))))
		  (else:
		   (syntax
		    ((if (< increment-temporary 0) < >)
		     variable-name limit-temporary)))))
	   '())
       #F))))

(define-method parse-until-clause (end-test )
  (values '() '() '() '() '() '() '() '() '() '() end-test))

(define-method parse-while-clause (end-test)
  (with-syntax ((?end-test end-test))
    (values '() '() '() '() '() '() '() '() '() '() (syntax (not ?end-test)))))
    
#|
(unless (empty? rest-clauses)
  (error "Malformed loop clauses ~S" clauses))
|#

(define-method parse-for-clause  ((clause <list>))
  (syntax-case clause (in until while from = keyed-by)
    ((variable in collection optional-clauselet ...)
     (syntax-case (syntax (optional-clauselet ...)) (using)
       (()
        (parse-in-clause (syntax variable) #F (syntax collection)))
       ((using protocol)
        (parse-in-clause
         (syntax variable) #F (syntax collection) 
         protocol: (syntax protocol)))))
    ((variable keyed-by key-variable in collection optional-clauselet ...)
     (syntax-case (syntax (optional-clauselet ...)) (using)
       (()
        (parse-in-clause
         (syntax variable) (syntax key-variable) (syntax collection)))
       ((using protocol)
        (parse-in-clause
         (syntax variable) (syntax key-variable) (syntax collection)
         protocol: (syntax protocol)))))
    ((variable from start optional-clauselet ...)
     (syntax-case (syntax (optional-clauselet ...)) (to above below by)
       (()
	(parse-from-clause (syntax variable) #F (syntax start) #F #F #F 1))
       ((by increment)
	(parse-from-clause 
	 (syntax variable) #F (syntax start) #F #F #F (syntax increment)))
       ((to bound)
	(parse-from-clause 
	 (syntax variable) (syntax bound) (syntax start) (syntax bound)
         #F #F 1))
       ((to bound by increment)
	(parse-from-clause
	 (syntax variable) (syntax bound) (syntax start) (syntax bound) 
	 #F #F (syntax increment)))
       ((above bound)
	(parse-from-clause 
	 (syntax variable) (syntax bound) (syntax start) #F (syntax bound)
         #F -1))
       ((above bound by increment)
	(parse-from-clause
	 (syntax variable) (syntax bound) (syntax start) #F
	 (syntax bound) #F (syntax increment)))
       ((below bound)
	(parse-from-clause 
	 (syntax variable) (syntax bound) (syntax start) #F #F
         (syntax bound) 1))
       ((below bound by increment)
	(parse-from-clause 
	 (syntax variable) (syntax bound) (syntax start) 
	 #F #F (syntax bound) (syntax increment)))))
    ((while end-test)
     (parse-while-clause (syntax end-test)))
    ((until end-test)
     (parse-until-clause (syntax end-test)))
    ((parameter ...)
     (bind ((step-clause (syntax (parameter ...)))
            (equal-position 
             (find-key step-clause (method (x) (id? (strip x) '=)))))
       (unless equal-position
         (syntax-error "Invalid For Clause Syntax ~S" clause))
       (bind ((parameters 
               (copy-sequence step-clause end: equal-position)))
         (syntax-case (copy-sequence step-clause start: equal-position) (=)
           ((= init-value optional-clauselet ...)
            (syntax-case (syntax (optional-clauselet ...)) (then)
              (()
	       (parse-step-clause parameters (syntax init-value) #F))
              ((then next-value)
	       (parse-step-clause
                parameters (syntax init-value) (syntax next-value)))))))))))

(define-translator-syntax for (form) ()
  ((_ ((clause ...) ...) body-form ...)
   (bind-methods 

     ((parse-clauses 
          (clauses
           loop-variables element-variable-bindings finally-variables
           collection-variable-bindings 
           initial-variable-bindings initial-arguments
           next-variable-bindings next-arguments
           finally-arguments finished-tests end-test)

        (if (empty? clauses)
            (values loop-variables element-variable-bindings finally-variables 
                    collection-variable-bindings 
                    initial-variable-bindings initial-arguments
                    next-variable-bindings next-arguments 
                    finally-arguments finished-tests end-test)

	    (bind ((new-loop-variables new-element-variable-bindings
                    new-finally-variables new-collection-variable-bindings
                    new-initial-variable-bindings new-initial-arguments 
                    new-next-variable-bindings new-next-arguments 
                    new-finally-arguments new-finished-tests new-end-test
                    (parse-for-clause (first clauses))))

              (parse-clauses 
               (rest clauses)
               (concatenate new-loop-variables loop-variables)
               (concatenate 
                new-element-variable-bindings element-variable-bindings)
               (concatenate new-finally-variables finally-variables)
               (concatenate 
                new-collection-variable-bindings collection-variable-bindings)
               (concatenate 
                new-initial-variable-bindings initial-variable-bindings)
               (concatenate new-initial-arguments initial-arguments)
               (concatenate new-next-variable-bindings next-variable-bindings)
               (concatenate new-next-arguments next-arguments)
               (concatenate new-finally-arguments finally-arguments)
               (concatenate new-finished-tests finished-tests)
               (or new-end-test end-test)))))

      (parse-body-forms (body-forms real-body-forms finally-forms)
        (syntax-case body-forms (finally)
	  (() (values (reverse! real-body-forms) finally-forms))
	  ((finally finally-form ...)
	   (set! finally-forms (syntax (finally-form ...)))
	   (values (reverse! real-body-forms) finally-forms))
	  ((form rest-form ...)
	   (parse-body-forms 
	    (syntax (rest-form ...))
            (add! real-body-forms (syntax form)) '())))))
     
     (bind ((loop-variables element-variable-bindings finally-variables
             collection-variable-bindings
             initial-variable-bindings initial-arguments
             next-variable-bindings next-arguments finally-arguments
             finished-tests end-test 
             (parse-clauses 
              (syntax ((clause ...) ...)) 
              '() '() '() '() '() '() '() '() '() '() #F))
            (body-forms finally-forms
             (parse-body-forms (syntax (body-form ...)) '() '())))

       (with-syntax
           (((loop-variable ...) loop-variables)
            ((element-variable-binding ...) element-variable-bindings)
            ((finally-variable ...) finally-variables)
            ((finally-variable-name ...)
             (map variable-name finally-variables))
            ((initial-variable-binding ...) initial-variable-bindings)
            ((initial-argument ...) initial-arguments)
            ((next-variable-binding ...) next-variable-bindings)
            ((next-argument ...) next-arguments)
            ((finally-argument ...) finally-arguments)
            ((collection-variable-binding ...) collection-variable-bindings)
            ((finished-test ...) finished-tests)
            (?end-test end-test)
            ((finally-form ...)
             (if (empty? finally-forms) '(#F) finally-forms))
            ((body-form ...) body-forms))

         (syntax 
	  (bind (collection-variable-binding ...)
            (looping-bind-methods 
	      ((.finally. (finally-variable ...) 
                 finally-variable-name ... ;; poor substitute for ignore
                 finally-form ...)
               (.loop. (loop-variable ...)
                     (if (or finished-test ...)
                         (.finally. finally-argument ...)
                         (bind (element-variable-binding ...)
		           (if ?end-test
		               (.finally. finally-argument ...)
		               (begin
		                body-form ...
                                (bind (next-variable-binding ...)
		                  (.loop. next-argument ...))))))))

              (bind (initial-variable-binding ...)
                (.loop. initial-argument ...))))))))))

;;; EOF
