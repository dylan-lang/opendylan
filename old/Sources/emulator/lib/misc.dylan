;; Implements: Chapter 17, Pages 157-159

;; Miscellaneous higher-order functions:

(define compose
  (method (f #rest fns)
    (if (null? fns) f
      (bind ((rest (apply compose fns)))
        (cond 
          ((id? rest identity) f)
          ((id? f identity)    rest)
          (else:
            (method (#rest values)
              (f (apply rest values)))))))))

(define complement
  (method (ok?)
    (method (value)
      (not (ok? value)))))

(define curry
  (method (f #rest curried-args)
    (method (#rest args)
      (apply f (concatenate curried-args args)))))

(define rcurry
  (method (f #rest curried-args)
    (method (#rest args)
      (apply f (concatenate args curried-args)))))

(define disjoin
  (method (f #rest fns)
    (if (null? fns) f
      (bind ((composite (apply disjoin fns)))
        (method (#rest args)
          (or (apply f args) (apply composite args)))))))

(define conjoin
  (method (f #rest fns)
    (if (null? fns) f
      (bind ((composite (apply conjoin fns)))
        (method (#rest args)
          (and (apply f args) (apply composite args)))))))

(define always
  (method (value)
    (method (#rest junk) value)))

;; eof

