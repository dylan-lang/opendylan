module:    internal
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method constant? ((object <object>)) #F)
(define-method constant? ((object (singleton #T))) #T)
(define-method constant? ((object (singleton #F))) #T)
(define-method constant? ((object <number>)) #T)
(define-method constant? ((object <symbol>)) 
  (keyword? object))
(define-method constant? ((object <character>)) #T)
(define-method constant? ((object <string>)) #T)
(define-method constant? ((object <simple-object-vector>)) #T)
(define-method constant? ((object <list>)) (id? (first object) 'quote))
(define-method constant? ((object <empty-list>)) #T)

(define-method atom? ((object <object>)) #F)
(define-method atom? ((object (singleton #T))) #T)
(define-method atom? ((object (singleton #F))) #T)
(define-method atom? ((object <number>)) #T)
(define-method atom? ((object <symbol>)) #T)
(define-method atom? ((object <character>)) #T)
(define-method atom? ((object <string>)) #T)
(define-method atom? ((object <empty-list>)) #T)

;; (define-variable *macros* (make <table>))
;; (define-variable *special-forms* (make <table>))

;;;; <Mark> are items that we compare with id?.
;;;; They don't have any slots because all we care about is id-ness.

(define-fluid *internal-context* #f)

(define-variable (*next-mark-number* <integer>) 0)

(define-class <mark> (<object>)
  ;; !@#$ TEMPORARY FOR PRINTING
  (number type: <integer>))

;; !@#$ TEMPORARY FOR PRINTING
(define-method initialize ((mark <mark>) #rest all-keys)
  (next-method)
  (set! (number mark) *next-mark-number*)
  (set! *next-mark-number* (+ *next-mark-number* 1))
  mark)

(define-method print ((mark <mark>) #key (stream #T) (verbose? #F))
  (if #F ;; verbose? ;; !@#$ should be readable?
      (format stream "#.~S" '(make <mark>))
      ;; !@#$ TEMPORARY FOR PRINTING
      (format stream "~D" (number mark)))
      ;; (print-unreadable-object (mark stream))
  mark)

;;;; WRAPPERs are lists of marks
;;;; !@#$ should be its own class with source-code as well

(define $empty-wrapper '()) ;; !@#$ should convert all code to this

(define-method new-mark-wrapper ()
  (list (make <mark>)))


;;; JOIN-WRAPPERS
;;; xor wrappers -- used to determine what identifiers macro actually produced

(define-method join-wrappers ((w1 <list>) (w2 <list>))
  (cond
   ((empty? w1) w2)
   ((empty? w2) w1)
   (else:
    (bind-methods ((merge (w1 w2)
		    (bind ((x (head w1)) (w1 (tail w1)))
		      (if (empty? w1)
			  (if (id? x (head w2))
			      (tail w2)
			      (pair x w2))
			  (pair x (merge w1 w2))))))
      (merge w1 w2)))))

(define-method same-marks? (w1 w2)
  (and (= (size w1) (size w2))
       (every? id? w1 w2)))

;;;; <context>

(define-class <context> (<object>)
  ;; (bindings init-keyword: bindings: init-value: '() type: <list>)
  ;; (module init-keyword: module: type: <object>)
  )

(define-method make ((class (singleton <context>)) #key module)
  module)

#|
(define-method print ((context <context>) #key (stream #T) (verbose? #F))
  (if #F ;; verbose? ;; !@#$ *print-readably*
      (format stream "#.~S"
	      `(make <context>
		     bindings: ',(bindings context)
		     module: (find-module ,(debug-name (module context)))))
      (print-unreadable-object (context stream))))
|#

(define-fluid *default-context* #F)

;;  (make <context> bindings: '() module: (find-module "DYLAN"))

;;;; <syntax-object>

(define-class <syntax-object> (<object>)
  (expression init-keyword: expression:)
  (wrapper init-keyword: wrapper: init-value: '() type: <list>)
  ;; !@#$ type: (union context null (member :current))
  (context init-keyword: context: init-value: #F))

(define <identifier> <object>) ;; !@#$ (union <symbol> <syntax-object>)

(define-method print-wrapper ((object <list>) #key (stream #T))
  (format stream "[")
  (iterate grovel (((marks <list>) object) ((first? <boolean>) #T))
    (unless (empty? marks)	
      (unless first? (format stream " "))
      (format stream "~S" (head marks))
      (grovel (tail marks) #F)))
  (format stream "]")
  object)

(define-method print ((object <syntax-object>) #key (stream #T) (verbose? #F))
  (if #F ;; verbose? ;; !@#$ *print-readably*
      (format stream "#.~S"
	      `(make <syntax-object>
		     expression: ',(expression object)
		     wrapper: ',(wrapper object)
		     context: ',(bind ((the-context (context object)))
				  (if (id? the-context current:)
				      (error "current: context not fixed up?")
				      the-context))))
      (begin
       (format stream "`~S" (expression object))
       (unless (empty? (wrapper object))
	 (print-wrapper (wrapper object) stream: stream))
       #|
       (when (context object)
	 (format stream "{~S}" (context object)))
       |#
       object)))
	 
      #|
      (print-unreadable-object (object stream)
	(format stream "expression: ~S wrapper: ~S context: ~S"
		(expression object)
		(wrapper object)
		(context object)))
      |#

(define-method wrap ((expression <empty-list>) (wrapper <list>) #key context)
  expression)

(define-method wrap
    ((syntax-object <syntax-object>) (the-wrapper <list>)
     #key (context: the-context *default-context*))
  (make <syntax-object>
	expression: (expression syntax-object)
	wrapper:    (join-wrappers the-wrapper (wrapper syntax-object))
	context:    (or (context syntax-object) the-context)))

(define-method wrap 
    ((expression <object>) (wrapper <list>) #key (context *default-context*))
  (make <syntax-object>
	expression: expression
	wrapper:    wrapper
	context:    context))

(define-method unwrap ((expression <object>))
  expression)

(define-method unwrap ((object <syntax-object>))
  (unwrap* (expression object) (wrapper object) (context object)))

;;; !@#$ THESE SHOULD BE WRITTEN WITH BIND-METHODS

(define-method unwrap*
    ((expression <empty-list>) (wrapper <list>)
     (context (union <context> (singleton #F))))
  '())

(define-method unwrap*
    ((expression <list>) (wrapper <list>)
     (context (union <context> (singleton #F))))
  (pair (wrap (head expression) wrapper context: context)
	(wrap (tail expression) wrapper context: context)))

(define-method as ((class (singleton <symbol>)) (object <syntax-object>))
  (if (identifier? object)
      (strip object)
      (error "ATTEMPTING TO CAST NON-IDENTIFIER SYNTAX-OBJECT ~S INTO A SYMBOL"
	     object)))

(define-method as ((class (singleton <list>)) (object <syntax-object>))
  (bind ((unwrapped (unwrap object)))
    (when (instance? unwrapped <pair>)
      (set! (tail unwrapped) (as <list> (tail unwrapped))))
    unwrapped))
	
(define-method unwrap*
    ((expression <simple-object-vector>) (wrapper <list>)
     (context (union <context> (singleton #F))))
  (bind ((length (size expression))
	 (result (make <simple-object-vector> length)))
    (dotimes (i length)
      (set! (element result i)
	    (wrap (element expression i) wrap context: context)))
    result))

(define-method unwrap*
    ((expression <object>) (wrapper <list>)
     (context (union <context> (singleton #F))))
  expression)

(define-method strip ((expression <object>)) expression)

(define-method strip ((expression <empty-list>)) '())

(define-method strip ((expression <list>))
  (bind ((stripped-head (strip (head expression)))
	 (stripped-tail (strip (tail expression))))
    (if (and (id? stripped-head (head expression))
	     (id? stripped-tail (tail expression)))
	expression
	(pair stripped-head stripped-tail))))

(define-method strip ((expression <simple-object-vector>))
  (bind ((length (size expression)))
    (bind-exit (return)
      (iterate strip-each (((i <integer>) 0))
        (when (< i length)
  	  (bind ((raw (element expression i))
	         (stripped (strip raw)))
	    (unless (id? raw stripped)
	      (bind ((result (make <simple-object-vector> size: length)))
                (iterate replace! (((j <integer>) 0))
                  (when (< j i)
		    (set! (element result j) (element expression j))
	            (replace! (+ j 1))))
                (iterate replace! (((j <integer>) i))
                  (when (< j length)
 		    (set! (element result j) (strip (element expression j)))
	            (replace! (+ j 1))))
	        (return result))))))
      expression)))
		  
(define-method strip ((object <syntax-object>))
  (strip (expression object)))

;;;; Identifier manipulations.

;;; IDENTIFIER? -- interface.
;;;
;;; Return T iff ID is a valid identifier.
;;; Ignore whatever syntax objects might be scattered thoughout.
;;; 
(define-method identifier? ((identifier <object>)) #F)
(define-method identifier? ((identifier <symbol>)) 
  ;; !@#$ patch for pseudo system
  (case identifier
    ((#T #F) #F) 
    (else: (and (instance? identifier <symbol>) (not (keyword? identifier))))))
(define-method identifier? ((identifier <syntax-object>))
  (identifier? (expression identifier)))

;;; ID-NAME

(define-method id-name ((identifier <symbol>))
  identifier)

(define-method id-name ((identifier <syntax-object>))
  (expression identifier))

(define-method id-wrapper ((identifier <symbol>))
  $empty-wrapper)

(define-method id-wrapper ((identifier <syntax-object>))
  (wrapper identifier))

(define-method id-context ((identifier <symbol>))
  #F)

(define-method id-context ((identifier <syntax-object>))
  (context identifier))

(define-method id-module
    ((identifier <symbol>) #key (default-module (current-module)))
  default-module)

(define-method id-module 
    ((identifier <syntax-object>) #key default-module)
  (context identifier))

;;; ID-NAME-AND-MODULE -- interface.
;;;
;;; Extract the name (with all syntax objects removed) and the module for
;;; this id.  Note: it must be a valid identifier, as verified by IDENTIFIER?.
(define-method id-name-and-module
    ((identifier <symbol>) #key (default-module (current-module)))
  (values identifier default-module))

(define-method id-name-and-module 
    ((identifier <syntax-object>) #key (default-module (current-module)))
  (values (expression identifier) (or (context identifier) default-module)))

;;; ID-NAME-AND-CONTEXT -- interface.
;;;
;;; Extract the name (with all syntax objects removed) and the context for
;;; this id.  Note: it must be a valid identifier, as verified by IDENTIFIER?.
(define-method id-name-and-context ((identifier <symbol>))
  (values identifier #F))

(define-method id-name-and-context ((identifier <syntax-object>))
  (values (expression identifier) (context identifier)))

;;; ID-NAME-MARKS-AND-CONTEXT -- internal.
;;;
;;; Return the name, the marks, and the context for ID.
;;;
(define-method id-name-marks-and-context
    ((identifier <symbol>) (wrapper <list>))
  (values identifier wrapper #F))

(define-method id-name-marks-and-context
    ((identifier <syntax-object>) (the-wrapper <list>))
  (values (expression identifier)
	  (join-wrappers the-wrapper (wrapper identifier))
	  (context identifier)))

(define-method bound-identifier=? ((id-1 <object>) (id-2 <object>))
  (bind ((name-1 marks-1
	  (id-name-marks-and-context id-1 $empty-wrapper))
	 (name-2 marks-2
	  (id-name-marks-and-context id-2 $empty-wrapper)))
    (and (id? name-1 name-2)
	 (same-marks? marks-1 marks-2))))

(define-method = ((id-1 <syntax-object>) (id-2 <syntax-object>))
  (bound-identifier=? id-1 id-2))

(define-method = ((id-1 <syntax-object>) (id-2 <object>))
  (= (strip id-1) id-2))

(define-method = ((id-1 <object>) (id-2 <syntax-object>))
  (= (strip id-2) id-1))

(define-method = ((id-1 <syntax-object>) (id-2 <symbol>))
  (and (empty? (wrapper id-1)) (id? (expression id-1) id-2)))

(define-method = ((id-1 <symbol>) (id-2 <syntax-object>))
  (and (empty? (wrapper id-2)) (id? (expression id-2) id-1)))

;;;; use id? instead of FREE-IDENTIFIER=? 

(define-method implicit-identifier ((the-context <syntax-object>) (id <object>))
  (wrap id (wrapper the-context) context: (context the-context)))

(define-method implicit-identifier ((context <object>) (id <object>))
  id)

;;;
;;; GENERATE-TEMPORARY -- internal.
;;;
(define *temporary-counter* -1)

(define-method generate-temporary ((name <symbol>))
  (set! *temporary-counter* (+ *temporary-counter* 1))
  (as <symbol> (format #F "~A-~D" name *temporary-counter*)))
(define-method generate-temporary ((name <object>))
  (set! *temporary-counter* (+ *temporary-counter* 1))
  (as <symbol> (format #F "tmp-~D" *temporary-counter*)))
(define-method generate-temporary ((name <syntax-object>))
  (set! *temporary-counter* (+ *temporary-counter* 1))
  (if (identifier? name)
      (as <symbol> (format #F "~A-~D" (expression name) *temporary-counter*))
      (as <symbol> (format #F "tmp-~D" *temporary-counter*))))

(define-method gen-pair (head-form tail-form)
  (bind ((result
  (cond ((constant? tail-form)
	 (bind ((the-tail (eval tail-form)))
	   (cond ((constant? head-form)
		  `'(,(eval head-form) . ,the-tail))
		 ((id? the-tail '())
		  `(list ,head-form))
		 (else:
		  `(pair ,head-form ',the-tail)))))
	((or (atom? tail-form) (identifier? tail-form))
	 `(pair ,head-form ,tail-form))
	(else:
	 (case (head tail-form)
	   ((list)
	    `(list ,head-form ,@(tail tail-form)))
	   ((pair list*)
	    `(list* ,head-form ,@(tail tail-form)))
	   (else:
	    `(pair ,head-form ,tail-form))))))
	 )
     ;; (format #T "GEN-PAIR ~S ~S -> ~S~%" head-form tail-form result)
     result))

(define-method gen-concatenate (list-1 list-2)
  ;; (format #T "GEN-CONCATENATE ~S ~S " list-1 list-2)
  (bind ((result
  (cond ((instance? list-2 <empty-list>) list-1)
	((constant? list-2)
	 (if (constant? list-1)
	     `',(concatenate (eval list-1) (eval list-2))
	     `(concatenate ,list-1 ',(eval list-2))))
        ((or (atom? list-2) (identifier? list-2))
         `(concatenate ,list-1 ,list-2))
	(else:
	 (case (head list-2)
	   ((concatenate)
	    `(concatenate ,list-1 ,@(tail list-2)))
	   (else:
	    `(concatenate ,list-1 ,list-2)))))
	   ))
     ;; (format #T "-> ~S~%" result)
     result))

(define-method gen-map (expression map-env)
  ;;; !@#$ env head/tail
  (bind ((result
  (bind ((vars (map tail map-env))
	 (vals (map head map-env)))
    (cond ((id? expression (head vars))
	   (head vals))
	  (else:
	   `(map (method ,vars ,expression) ,@vals)))))
	 )
     ;; (format #T "GEN-MAP ~S ~S -> ~S~%" expression map-env result)
     result))


;;; !@#$ SHOULD GO AWAY BUT HARD TO REWRITE BELOW

(define-method member ((value <object>) (list <empty-list>) #KEY test)
  #F)

(define-method member ((value <object>) (list <pair>) #KEY (test id?))
  (iterate member ((list list))
    (if (test value (head list))
	list
	(bind ((remaining (tail list)))
	  (and (not (empty? remaining)) (member remaining))))))

(define-method gen-ref
    ((var <object>) (level <integer>) (maps <list>) (emitter <function>))
  (bind ((result
  (if (zero? level)
      (emitter var maps)
      (gen-ref
       var (- level 1) (tail maps)
       (method (outer-var outer-maps)
	 ;; !@#$ member? needs to return <boolean>
	 ;; !@#$ member doesn't really exist (only above)
	 (bind ((b (member outer-var (head maps)
			   test: (method (value item)
				   (id? value (head item))))))
	   (if b
	       (emitter (tail b) maps)
	       (bind ((inner-var (generate-temporary var)))
		 (emitter inner-var
			  (pair (pair (pair outer-var inner-var) (head maps))
				outer-maps)))))))))
	 )
     ;; (format #T "GEN-REF ~S ~S ~S -> ~S~%" var level maps result)
     result))

;;;;
;;;; EXTERNAL INTERFACE
;;;;

;;; GENERATE-TEMPORARIES -- Public.
;;;
;;; Generate a list of variable names to use as temporaries.
;;;
(define-method generate-temporaries ((list <list>))
  (map (method (x) (generate-temporary x)) list))

(define-method ellipsis? ((x <syntax-object>) (wrapper <list>))
  (ellipsis? (expression x) '()))
(define-method ellipsis? ((x (singleton '...)) (wrapper <list>)) #T)
(define-method ellipsis? ((x <object>) (wrapper <list>)) #F)

(define-method syntax-error (object #rest messages)
  (if (empty? messages)
      (error "Invalid syntax ~A" (strip object))
      (error "~A ~A" (apply concatenate messages) (strip object))))

(define-method syntax-object->datum (x)
  (strip x))

;;; BOOT-STRAPPED MATCHER

(define-variable *debug-matcher?* #F)

(define-method match-pattern (pattern expr keywords)
  (bind-exit (return)
    (bind-methods
	((punt ()
	   (when *debug-matcher?*
	     (format #T "PUNT~%"))
	   (return #F '()))
	 (match (pattern expr the-wrapper the-context results)
	   (when *debug-matcher?*
	     (format #T "MATCH P: ~S E: ~S W: ~S C: ~S R: ~S~%"
		     pattern expr the-wrapper the-context results))
	   (case (head pattern)
	     ((any)
	      (pair (wrap expr the-wrapper context: the-context) results))
	     ((literal)
	      (unless (and (identifier? expr)
			   (id? (strip (element keywords (tail pattern)))
				(strip expr)))
		(punt))
	      results)
	     (else:
	      (if (instance? expr <syntax-object>)
		  (match* pattern
			  (expression expr)
			  (join-wrappers the-wrapper (wrapper expr))
			  (or (context expr) the-context)
			  results)
		  (match* pattern expr the-wrapper the-context results)))))
	 (match* (pattern expr the-wrapper the-context results)
	   (when *debug-matcher?*
	     (format #T "MATCH* P: ~S E: ~S W: ~S C: ~S R: ~S~%"
		     pattern expr the-wrapper the-context results))
	   (case (head pattern)
	     ((each)
              (unless (instance? expr <list>) (punt))
	      (if (empty? expr)
		  (match-empty pattern results)
		  (iterate collect
			   ((list-o-lists
			     (match-each
			      (tail pattern) expr the-wrapper the-context)))
			   ;; (format #T "LIST-O-LISTS ~S~%" list-o-lists)
			   (if (empty? (head list-o-lists))
			       results
			       (pair (map head list-o-lists)
				     (collect (map tail list-o-lists)))))))
	     ((list)
	      (match-list
	       (tail pattern) expr the-wrapper the-context #F results))
	     ((list*)
	      (match-list
	       (tail pattern) expr the-wrapper the-context #T results))
	     ((vector)
	      (unless (instance? expr <simple-object-vector>) (punt))
	      (match
	       (tail pattern) (as <list> expr) the-wrapper the-context results))
	     ((atom)
	      (unless (= (tail pattern) expr) (punt))
	      results)
	     (else:
	      (error "Internal Match* Error"))))
	 (match-each (pattern exprs the-wrapper the-context)
	   (when *debug-matcher?*
	     (format #T "MATCH-EACH P: ~S E: ~S W: ~S C: ~S~%"
		     pattern exprs the-wrapper the-context))
	   (if (instance? exprs <syntax-object>)
	       (match-each* pattern
			    (expression exprs)
			    (join-wrappers the-wrapper (wrapper exprs))
			    (or (context exprs) the-context))
	       (match-each* pattern exprs the-wrapper the-context)))
	 (match-each* (pattern exprs the-wrapper the-context)
	   (when *debug-matcher?*
	     (format #T "MATCH-EACH* P: ~S E: ~S W: ~S C: ~S~%"
		     pattern expr the-wrapper the-context))
	   (unless (instance? exprs <list>) (punt))
	   (if (empty? exprs)
	       '()
	       (pair (match pattern (head exprs) the-wrapper the-context '())
		     (match-each pattern (tail exprs) the-wrapper the-context))))
	 (match-empty (pattern results)
	   (when *debug-matcher?*
	     (format #T "MATCH-EMPTY P: ~S R: ~S~%" pattern results))
	   (case (head pattern)
	     ((any) (pair '() results))
	     ((each) (match-empty (tail pattern) results))
	     ((list list* vector)
	      (iterate match-empty-list ((patterns (tail pattern)))
		       (if patterns
			   (match-empty (head patterns)
					(match-empty-list (tail patterns)))
			   results)))
	     ((literal atom) results)
	     (else:
	      (error "Internal Match-Empty Error"))))
	 (match-list (patterns exprs the-wrapper the-context dotted results)
	   (when *debug-matcher?*
	     (format #T "MATCH-LIST P: ~S E: ~S W: ~S C: ~S D: ~S R: ~S~%"
		     patterns exprs the-wrapper the-context dotted results))
	   (cond ((empty? patterns)
		  (unless (instance? exprs <empty-list>) (punt))
		  results)
		 ((and (empty? (tail patterns)) dotted)
		  (match (head patterns) exprs the-wrapper the-context results))
		 ((instance? exprs <syntax-object>)
		  (match-list* patterns
			       (expression exprs)
			       (join-wrappers the-wrapper (wrapper exprs))
			       (or (context exprs) the-context)
			       dotted
			       results))
		 (else:
		  (match-list*
		   patterns exprs the-wrapper the-context dotted results))))
	 (match-list* (patterns exprs the-wrapper the-context dotted results)
	   (when *debug-matcher?*
	     (format #T "MATCH-LIST* P: ~S E: ~S W: ~S C: ~S D: ~S R: ~S~%"
		     patterns exprs the-wrapper the-context dotted results))
	   (if (instance? exprs <list>)
	       (match (head patterns) (head exprs) the-wrapper the-context
		      (match-list (tail patterns) (tail exprs) the-wrapper
				  the-context dotted results))
	       (punt))))
      (values #T (match pattern expr '() #F '())))))

(define *boot-syntax* '())

(define-method %define-syntax 
    ((module-name <symbol>) (name <symbol>) (function <function>))
  (set! *boot-syntax* 
        (pair (list module-name name function) *boot-syntax*))
  (values))
