;; Some optional "cute" apple-esque print methods:

(define <top-class>   (cl-class class))
(define <real-method> (cl-class method))

(define hack-function-debug-name
  (method (name)
    (if (instance? name <list>) 
      (case (first name)
        ((setter)
          (as <symbol> (concatenate (as <string> (second name))
                                    (as <string> '-setter))))
        (else:
          (second name)))
      (translate-cl-value-name name))))

(define-method print ((c <top-class>) 
                      #key (stream (standard-output)) (verbose? #f))
  (format stream "{class ~a}" 
          (translate-cl-class-name (class-debug-name c)))
  c)

(define-method print ((f <function>)
                      #key (stream (standard-output)) (verbose? #f))
  (if (null? (function-debug-name f))
    (format stream "{anonymous method}")
    (format stream "{method ~a}" 
	    (hack-function-debug-name (function-debug-name f))))
  f)

(define-method print ((f <generic-function>)
                      #key (stream (standard-output)) (verbose? #f))
  (if (null? (function-debug-name f))
    (format stream "{anonymous generic}")
    (format stream "{generic ~a}" 
            (hack-function-debug-name (function-debug-name f))))
  f)

(define-method print ((m <real-method>)
                      #key (stream (standard-output)) (verbose? #f))
  (format stream "{the method ~a}" '?)
  m)

(define-method print ((dq <deque>) 
                      #key 
                      (stream (standard-output))
                      (verbose? #f))
  (if (empty? dq)
    (format stream "{empty deque}")
    (format stream "{deque of ~s}" (as <list> dq)))
  dq)

(define-method print ((r <range>) 
	              #key
                      (stream   (standard-output))
                      (verbose? #f))
  (if (and (size r) (zero? (size r)))
    (format stream "{the empty range}")
    (bind ((last (range-last r)))
      (if last
        (format stream
                "{range ~s through ~s, by ~s}"
	        (range-from r)
	        last
	        (range-by r))
        (format stream
                "{range ~s onwards, by ~s}"
	        (range-from r)
	        (range-by r)))
      r)))

#|
(define-method print ((o <object>) 
                      #key 
                      (stream (standard-output))
		      (verbose? #f))
  (if (not (instance? (object-class o) <class>)) (next-method)
    (begin
      (format stream "{instance of ~s}" 
	      (class-debug-name (object-class o)))
      o)))
|#

(define-method print ((v <stretchy-vector>) 
                      #key 
                      (stream (standard-output))
		      (verbose? #f))
  (if (empty? v)
    (format stream "{empty stretchy vector}")
    (format 
      stream "{stretchy vector of ~s}" (as <list> (stretchy-vector-data v))))
  v)

(define $the-keyword-package ((cl-function find-package) "KEYWORD"))

(define-method print ((s <symbol>)
                      #key
                      (stream (standard-output))
                      (verbose? #f))
  (bind ((pkg ((cl-function symbol-package) s)))
    (if (not (id? pkg $the-keyword-package))
      (next-method)
      (format stream "~a:" (as <string> s)))
    s))
         
;; eof
