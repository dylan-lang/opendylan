;; Optimising methods:

;; We use the <cl-sequence> class here:

(define-method list? (o) #f)
(define-method list? ((o <list>)) #t)

(define-method cl-sequence? (o) #f)
(define-method cl-sequence?((o <cl-sequence>)) #t)

(define-method lisp-test (f)
  (method (a b) (if (f a b) #t '())))
;; (define-method lisp-test ((f (singleton id?)))
;;  (cl-function eql))

(define-method reversed-lisp-test (f)
   (method (a b) (if (f b a) #t '())))

(define-method lisp-test-1 (f)
  (method (a) (if (f a) #t '())))

(define-method size ((s <cl-sequence>))
  (size/sequence s))

(define-method element ((s <cl-sequence>) (i <integer>) 
                        #key (default (void)))
  (if (or (< i 0) (>= i (size/sequence s)))
    (if (void? default) 
      (error 
        "element: key ~s not found in sequence ~s and no default was supplied"
        i 
        s)
      default)
    (element/sequence*integer s i)))

(define-method element-setter (value (s <cl-sequence>) (i <integer>))
  (set-element/sequence*integer s i value))

(define-method first ((l <empty-list>) #key (default (unsupplied)))
  (if (supplied? default) default
    (error "No first element of ~s and no default supplied." l)))

(define-method second ((l <empty-list>) #key (default (unsupplied)))
  (if (supplied? default) default
    (error "No second element of ~s and no default supplied." l)))

(define-method third ((l <empty-list>) #key (default (unsupplied)))
  (if (supplied? default) default
    (error "No third element of ~s and no default supplied." l)))

(define-method first ((l <pair>) #key default)
  (head l))

(define-method second ((l <pair>) #key (default (unsupplied)))
  (bind ((from-second (tail l)))
    (if (id? from-second '())
      (if (supplied? default) default
        (error "No second element of ~s and no default supplied." l))
      (head from-second))))

(define-method third ((l <pair>) #key (default (unsupplied)))
  (bind ((from-third (tail (tail l))))
    (if (id? from-third '())
      (if (supplied? default) default
        (error "No third element of ~s and no default supplied." l))
      (head from-third))))

(define-method do (f (s <cl-sequence>) #rest rest)
  (if (empty? rest) (map-as/sequence '() f s) (next-method)))

(define-method map (f (s <list>) #rest rest)
  (if (empty? rest) (map-as/sequence 'list f s) (next-method)))

(define-method map (f (s <simple-object-vector>) #rest rest)
  (if (empty? rest) (map-as/sequence 'vector f s) (next-method)))

(define-method map (f (s <byte-string>) #rest rest)
  (if (empty? rest) (map-as/sequence 'string f s) (next-method)))

(define-method map-as ((class (singleton <list>)) f (source <cl-sequence>) #rest rest)
  (if (empty? rest) (map-as/sequence 'list f source) (next-method)))

(define-method map-as ((class (singleton <simple-object-vector>)) f (source <cl-sequence>) #rest rest)
  (if (empty? rest) (map-as/sequence 'vector f source) (next-method)))

(define-method map-as ((class (singleton <byte-string>)) f (source <cl-sequence>) #rest rest)
  (if (empty? rest) (map-as/sequence 'string f source) (next-method)))

(define-method map-into ((target <cl-sequence>) f (source <cl-sequence>) #rest rest)
  (if (empty? rest) (map-into/sequence target f source) (next-method)))

(define-method any? (ok? (s <cl-sequence>) #rest rest)
  (if (empty? rest) (any?/sequence ok? s) (next-method)))

(define-method every? (ok? (s <cl-sequence>) #rest rest)
  (if (empty? rest) (every?/sequence ok? s) (next-method)))

(define-method member? (obj (s <list>) #key (test id?))
  (not 
    (id? ((cl-function member) obj s :test (lisp-test test)) '())))

(define-method choose (ok? (s <cl-sequence>))
  ((cl-function remove-if-not) (lisp-test-1 ok?) s))

(define-method remove ((s <cl-sequence>) item #key test count)
  (if (and test (not (id? test id?)))
    (if count
      (remove/sequence item s count: count test: (reversed-lisp-test test))
      (remove/sequence item s test: (reversed-lisp-test test)))
    (if count 
      (remove/sequence item s count: count)
      (remove/sequence item s))))

(define-method remove! ((s <cl-sequence>) item #key test count)
  (if (and test (not (id? test id?)))
    (if count
      (remove!/sequence item s count: count test: (reversed-lisp-test test))
      (remove!/sequence item s test: (reversed-lisp-test test)))
    (if count 
      (remove!/sequence item s count: count)
      (remove!/sequence item s))))

(define-method = ((s1 <byte-string>) (s2 <byte-string>))
  (not (id? ((cl-function string=) s1 s2) '())))

(define-method < ((s1 <byte-string>) (s2 <byte-string>))
  (not (id? ((cl-function string<) s1 s2) '())))

(define-method as-uppercase ((s <byte-string>))
  ((cl-function string-upcase) s))

(define-method as-uppercase! ((s <byte-string>))
  ((cl-function nstring-upcase) s))

(define-method as-lowercase ((s <byte-string>))
  ((cl-function string-downcase) s))

(define-method as-lowercase! ((s <byte-string>))
  ((cl-function nstring-downcase) s))

(define-method union ((l1 <list>) (l2 <list>) #key (test id?))
  ((cl-function union) l1 l2 :test (lisp-test test)))

(define-method intersection ((l1 <list>) (l2 <list>) #key (test id?))
  ((cl-function intersection) l1 l2 :test (lisp-test test)))

(define-method remove-duplicates ((s <cl-sequence>) #key (test id?))
  ((cl-function remove-duplicates) s :test (lisp-test test)))

(define-method remove-duplicates! ((s <cl-sequence>) #key (test id?))
  ((cl-function delete-duplicates) s :test (lisp-test test)))

(define-method concatenate ((s1 <list>) #rest more)
  (cond
    ((empty? more) 
      s1)
    ((and (list? (head more)) (empty? (tail more)))
      ((cl-function append) s1 (head more)))
    ((every? list? more)
      (apply (cl-function append) s1 more))
    (else:
      (next-method))))

(define-method reverse ((l <cl-sequence>))
  ((cl-function reverse) l))

(define-method reverse! ((l <cl-sequence>))
  ((cl-function nreverse) l))

(define-method sort ((s <cl-sequence>) #key (test <) (stable #f))
  (if stable
    ((cl-function stable-sort) ((cl-function copy-seq) s) (lisp-test test))
    ((cl-function sort) ((cl-function copy-seq) s) (lisp-test test))))

(define-method sort! ((s <cl-sequence>) #key (test <) (stable #f))
  (if stable
    ((cl-function stable-sort) s (lisp-test test))
    ((cl-function sort) s (lisp-test test))))

(define-method shallow-copy ((s <cl-sequence>))
  ((cl-function copy-seq) s))

(define-method as ((class (singleton <list>)) (s <cl-sequence>))
  ((cl-function coerce) s 'list))

(define-method as ((class (singleton <list>)) (s <stretchy-vector>))
  (bind ((data (stretchy-vector-data s)))
    (if data
        ((cl-function coerce) (stretchy-vector-data s) 'list)
        '())))

(define-method reduce ((f <function>) initial-value (s <cl-sequence>))
  ((cl-function reduce) f s :initial-value initial-value))

(define-method reduce1 ((f <function>) (s <cl-sequence>))
  ((cl-function reduce) f s))

;; eof
