;; Expression classes plus translation between forms.

;; --

;; Parameters:

(define $default-variable-name   'temp)
(define $ellipsis-symbol         '...)
(define $wildcard-symbol         '?)
(define $key-symbol              '#key)
(define $pattern-variable-prefix #\?)

(define ellipsis-symbol?
  (method (sym)
    (id? sym $ellipsis-symbol)))

(define wildcard-symbol?
  (method (sym)
    (id? sym $wildcard-symbol)))

(define key-symbol?
  (method (sym)
    (id? sym $key-symbol)))

(define pattern-variable-symbol?
  (method (sym)
    (bind ((name (as <string> sym)))
      (and (> (size name) 0)
           (= (element name 0) $pattern-variable-prefix)))))

(define pattern-variable-symbol-keyword
  (method (sym)
    (as/keyword*string 
      (copy-sequence (as <string> sym) start: 1))))

;; --

;; Contexts:

(define-class <lexical-context> (<object>))

;; Expression classes:

;; We'll give all expressions a lexical context - you never know, it
;; might be useful.

(define-class <expression> (<object>)
  
  (code-of
    init-keyword: code:
    init-value:   (uninitialized)
    type:         <object>)

  (lexical-context-of
    init-keyword:  lexical-context:
    init-function: (curry make <lexical-context>)
    type:          <lexical-context>)

)

(define expression? (rcurry instance? <expression>))

(define-class <literal-constant> (<expression>)

  (object-of
    required-init-keyword: object:)

)

(define literal-constant? (rcurry instance? <literal-constant>))

(define-method print ((lc <literal-constant>)
                      #key
                      (stream #t)
                      (verbose? #f))
  (format stream "^~s" (object-of lc))
  lc)

(define-class <variable-name> (<expression>)

  (name-of
    type:         <symbol>
    init-keyword: name:
    init-value:   $default-variable-name)

)

(define variable-name? (rcurry instance? <variable-name>))

(define-method pattern-variable-expression? ((vn <variable-name>))
  (pattern-variable-symbol? (name-of vn)))

(define-method print ((vn <variable-name>)
                      #key
                      (stream #t)
                      (verbose? #f))
  (format stream "^~s" (name-of vn))
  vn)

(define-method initialize ((vn <variable-name>) 
                           #rest args
                           #key (context (unsupplied)))
  (bind ((lexical-context 
           (if (unsupplied? context)
             (make <lexical-context>)
             (begin
               (check-type context <variable-name>)
               (lexical-context-of context)))))
    (apply next-method
           vn
           lexical-context: lexical-context
           args)))

(define-method as ((class (singleton <symbol>)) (vn <variable-name>))
  (name-of vn))

(define-method as ((class (singleton <string>)) (vn <variable-name>))
  (as <string> (name-of vn)))

(define-class <combination> (<expression>)

  (expressions-of
    type:                  <sequence>
    required-init-keyword: expressions:)

)

(define-method print ((comb <combination>)
                      #key
                      (stream #t)
                      (verbose? #f))
  (bind ((exprs (expressions-of comb))
         (sz (size exprs)))
    (if (zero? sz)
      (format stream "[]")
      (begin
        (format stream "[~s" (first exprs))
        (dotimes (i (- sz 1))
          (format stream " ~s" (element exprs (+ i 1))))
        (format stream "]" (first exprs))))
    comb))

(define-method combination-expressions ((comb <combination>))
  (expressions-of comb))
  
;; Expression convertion:

(define-method build-expression ((o <object>) context)
  (make <literal-constant>
        code:   o
        object: o))

(define-method build-expression ((s <symbol>) context)
  (cond
    ((id? s '())
      (make <combination> expressions: '()))
    ((not (id? ((cl-function keywordp) s) '()))
      (make <literal-constant>
            code:   s
            object: s))
    (else:
      (make <variable-name>
	    code:            s
	    name:            s 
	    lexical-context: context))))

(define-method build-expression ((l <list>) context)
  (make <combination>
	code:        l
	expressions: (map (rcurry build-expression context) l)))

(define-method strip-expression ((e <variable-name>))
  (name-of e))

(define-method strip-expression ((e <literal-constant>))
  (object-of e))

(define-method strip-expression ((e <combination>))
  (map strip-expression (expressions-of e)))

(define-method ellipsis-expression? ((e <expression>))
  #f)

(define-method ellipsis-expression? ((vn <variable-name>))
  (ellipsis-symbol? (name-of vn)))

(define-method key-expression? ((e <expression>))
  #f)

(define-method key-expression? ((vn <variable-name>))
  (key-symbol? (name-of vn)))

(define-method similar-expressions? ((e1 <expression>) (e2 <expression>))
  #f)

(define-method similar-expressions? ((c1 <literal-constant>)
                                     (c2 <literal-constant>))
  (= (object-of c1) (object-of c2)))

(define-method similar-expressions? ((v1 <variable-name>)
                                     (v2 <variable-name>))
  (id? (name-of v1) (name-of v2)))

(define macro-function
  (method (transformer)
    (method (before)
      ;; (format #t "before: ~s~%" before)
      (bind ((after 
               (strip-expression (transformer (build-expression before #f)))))
        ;; (format #t "after: ~s~%" after)
        after))))

;; Exported utilities:

(define-generic-function do-expression (f e))

(define-method do-expression (f (e <expression>))
  (f e))

(define-method do-expression (f (e <combination>))
  (f e)
  (expression-case e
    ((?e ...)
      (do (curry do-expression f) ?e))))

;; Useful constants.

(define $true-syntax  (make <literal-constant> object: #t))
(define $false-syntax (make <literal-constant> object: #f))

;; eof
