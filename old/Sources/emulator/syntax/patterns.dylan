;; Pattern matching and restructuring:

(define $key-pattern-default $false-syntax)

;; Pattern classes:

;; --

(define-class <pattern> (<object>))

(define-generic-function compile-pattern-expression (exp lookup 
                                                     #key template?))
(define-generic-function match-pattern (pat exp fail))
(define-generic-function generate-template-code (x))
(define-generic-function generate-template-expression (x y))

;; --

(define-class <literal-pattern> (<pattern>)
  (object-of required-init-keyword: object:))

;; --

(define-class <variable-pattern> (<pattern>)
  (name-of required-init-keyword: name:))

;; --

(define-class <empty-combination-pattern> (<pattern>))

;; --

(define-class <pair-pattern> (<pattern>)
  (head-pattern-of required-init-keyword: head-pattern:)
  (tail-pattern-of required-init-keyword: tail-pattern:))

;; --

(define-class <repeated-pattern> (<pattern>)
  (pattern-of      required-init-keyword: pattern:)
  (variables-of    required-init-keyword: variables:)
  (tail-pattern-of required-init-keyword: tail-pattern:)
  (tail-length-of  required-init-keyword: tail-length:))

;; --

(define-class <key-pattern> (<pattern>)
  (key-specs-of required-init-keyword: specs:))

;; --

(define-method compile-pattern-expression ((e <literal-constant>) depth
                                           #key (template? #f))
  (values (make <literal-pattern> object: e)
          '()))

(define-method compile-pattern-expression ((e <variable-name>) depth
                                           #key (template? #f))
  (if (pattern-variable-expression? e)
    (bind ((var (make <variable-pattern> name: (name-of e))))
      (values var
              (list (name-of e))))
    (values (make <literal-pattern> object: e)
            '())))

(define-method compile-pattern-expression ((e <combination>) depth
                                           #key (template? #f))
  (compile-pattern-expressions (expressions-of e) 
                               depth
                               template?: template?))

(define-method compile-pattern-expressions (pat depth
                                            #key (template? #f))
  (cond
    ((empty? pat)
      (make <empty-combination-pattern>))
    ((and (>= (size pat) 2)
          (ellipsis-expression? (second pat)))
      (bind ((term vars 
               (compile-pattern-expression (first pat) 
                                           (+ depth 1)
                                           template?: template?))
             (tail-length
               (size (tail (tail pat))))
             (tail-term tail-vars
               (compile-pattern-expressions (tail (tail pat))
                                            depth
                                            template?: template?)))
        (values
          (make <repeated-pattern>
                pattern:      term
                variables:    vars
                tail-pattern: tail-term
                tail-length:  tail-length)
          (concatenate vars
                       tail-vars))))
    ((and (not template?) (key-expression? (first pat)))
      (bind ((specs (make <deque>))
             (vars  (make <deque>)))
        (for-each ((spec (tail pat))) ()
          (bind ((key bind default
                   (parse-key-pattern-expression spec)))
            (when bind (push-last vars bind))
            (push-last specs (list key bind default))))
        (values
	  (make <key-pattern> specs: (as <list> specs))
          (as <list> vars))))
    (else:
      (bind ((head-pat head-vars
               (compile-pattern-expression (head pat) 
                                           depth
                                           template?: template?))
             (tail-pat tail-vars
               (compile-pattern-expressions (tail pat) 
                                            depth
                                            template?: template?)))
        (values
          (make <pair-pattern>
                head-pattern: head-pat
                tail-pattern: tail-pat)
          (concatenate head-vars
                       tail-vars))))))

(define-method parse-key-pattern-expression ((pat <variable-name>))
  (if (pattern-variable-expression? pat)
    (bind ((name (name-of pat)))
      (values
        (pattern-variable-symbol-keyword name)
	name
	$key-pattern-default))
    (values
      (as-keyword (name-of pat))
      #f 
      #f)))

(define-method parse-key-pattern-expression ((pat <combination>))
  (bind ((exps (combination-expressions pat))
	 (nexps (size exps)))
    (cond
     ((and (= nexps 2) (pattern-variable-expression? (first exps)))
      (bind ((name (name-of (first exps))))
	(values
	 (pattern-variable-symbol-keyword name)
	 name
	 (second exps))))
     ((= nexps 2)
      (values
       (strip-expression (first exps))
       (second exps)
       $key-pattern-default))
     (else:
      (values
       (strip-expression (first exps))
       (second exps)
       (third exps))))))

;; --

(define fudged-tail
  (method (l) (make <combination> expressions: l)))

;    (if (empty? l)
;      (make <literal-constant>
;            object: '())
;      (make <combination>
;            expressions: l))))

(define-method match-pattern ((pat <pattern>) (e <expression>) fail)
  (fail pat e))

(define-method match-pattern ((pat <variable-pattern>) (e <expression>) fail)
  (list e))

(define-method match-pattern ((pat <literal-pattern>) (e <expression>) fail)
  (if (similar-expressions? (object-of pat) e) 
    '()
    (fail pat e)))

(define-method match-pattern ((pat <empty-combination-pattern>)
                              (e <combination>)
                              fail)
  (if (zero? (size (expressions-of e))) '() (fail pat e)))

(define-method match-pattern ((pat <pair-pattern>) (e <combination>) fail)
  (bind ((es (expressions-of e)))
    (concatenate (match-pattern (head-pattern-of pat) 
                                (head es)
                                fail)
                 (match-pattern (tail-pattern-of pat)
				(fudged-tail (tail es))
                                fail))))

(define-method match-pattern ((pat <key-pattern>) (e <combination>) fail)
  (bind ((es (expressions-of e)))
    (unless (even? (size es))
      (fail pat e))
    (bind ((var-vals (make <deque>))
           (e-tab (make <table>)))
      (iterate walk ((l es))
        (unless (empty? l)
          (bind ((keyword-e (first l))
                 (value-e   (second l)))
            (unless (and (instance? keyword-e <literal-constant>)
                         (instance? (object-of keyword-e) <keyword>))
              (fail pat e))
            (bind ((keyword (object-of keyword-e)))
              (unless (element e-tab keyword default: #f)
                (set! (element e-tab keyword) value-e))))
          (walk (tail (tail l)))))
      (for-each ((spec (key-specs-of pat))) ()
        (bind ((key var default (apply values spec)))
          (when var
            (push-last var-vals
	               (element e-tab key default: default)))))
      (as <list> var-vals))))
      
(define-method match-pattern ((pat <repeated-pattern>) (e <combination>) fail)
  (bind-methods

    ((split (l n)
       (if (zero? n) (values '() l)
         (bind ((left right (split (tail l) (- n 1))))
           (values (pair (head l) left)
                   right)))))

    (bind ((e (expressions-of e))
           (repeated-length (- (size e) (tail-length-of pat))))
      (unless (>= repeated-length 0)
        (fail pat e))
      (bind ((repeated-e tail-e (split e repeated-length)))
        (concatenate
          (if (empty? repeated-e)
            (map (always '()) (variables-of pat))
            ;; morally unsound matrix transpose...
            (apply map
                   list
                   (map (curry (rcurry match-pattern fail) (pattern-of pat))
                        repeated-e)))
          (match-pattern (tail-pattern-of pat) 
			 (fudged-tail tail-e)
                         fail))))))

(define-method match-patterns (pats exps fail)
  (cond
    ((and (empty? pats) (empty? exps))
      '())
    ((or (empty? pats) (empty? exps))
      (fail pats exps))
    (else:
      (concatenate (match-pattern (head pats) (head exps) fail)
                   (match-patterns (tail pats) (tail exps) fail)))))

;; --

(define-method generate-template-expression ((pat <literal-pattern>)
                                             lookup)
  (object-of pat))

(define-method generate-template-expression ((pat <variable-pattern>)
                                             lookup)
  (bind ((exp (lookup (name-of pat))))
    (select exp instance?
      ((<expression>)
        exp)
      ((<sequence>)
        (error "pattern var ~s not deep enough in template" pat))
      (else:
        (error "invalid expression ~s for insertion in template" exp)))))

(define-method generate-template-expression ((pat <empty-combination-pattern>)
                                             lookup)
  (make <combination> expressions: '()))

(define-method generate-template-expression ((pat <pair-pattern>)
                                             lookup)
  (make <combination>
        expressions: 
          (pair (generate-template-expression (head-pattern-of pat)
                                              lookup)
                (bind ((exp (generate-template-expression (tail-pattern-of pat)
                                                          lookup)))
                  (if (null-literal-constant? exp) '()
                    (expressions-of exp))))))

(define-method generate-template-expression ((pat <repeated-pattern>)
                                             lookup)
  (bind ((vars (variables-of pat))
         (vals (map lookup vars)))
    (make <combination>
          expressions:
            (concatenate
              (apply map
                     (method (#rest one-set-of-vals)
                       (generate-template-expression (pattern-of pat) 
                                                     (rcurry lookup-in 
                                                             vars 
                                                             one-set-of-vals)))
                     vals)
              (bind ((exp (generate-template-expression (tail-pattern-of pat)
						        lookup)))
	        (if (null-literal-constant? exp) '()
	          (expressions-of exp)))))))

(define null-literal-constant?
  (method (x)
    (and (instance? x <literal-constant>)
         (id? (object-of x) '()))))

(define lookup-in
  (method (var vars vals)
    (bind-exit (return)

      (for-each ((var-name vars)
                 (var-val  vals))
                ()
        (when (id? var var-name)
          (return var-val))))))

(define compare
  (method (p e)
    (bind-exit (return)
      (bind ((term vars (compile-pattern-expression (build-expression p #f)
						    0)))
        (match-pattern term
                       (build-expression e #f)
                       (method (fail-pat fail-e)
                         (format #t "Failed on: ~s vs ~s~%"
                                 fail-pat fail-e)
                         (return #f)))))))

;; eof
