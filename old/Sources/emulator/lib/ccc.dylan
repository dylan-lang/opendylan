;; Comparison, Copying, Coercing.

(define-generic-function as             (class object))
(define-generic-function shallow-copy   (object))
(define-generic-function class-for-copy (object))
(define type-for-copy class-for-copy)

(define-method as (class object)
  (if (id? (object-class object) class) object
    (error "No coercion method on as for ~s and ~s" class object)))

(define-method class-for-copy ((x <object>))
  (object-class x))

(define not
  (method (x)
    (id? x #f)))

(define-generic-function < (x y))
(define-generic-function = (x y))
(define-generic-function =hash (x))

(define > 
  (method (x y)
    (< y x)))

(define <=
  (method (x y)
    (not (< y x))))

(define >=
  (method (x y)
    (not (< x y))))

(define /=
  (method (x y)
    (not (= x y))))

;; Infix.

(define == id?)

(define ~ not)

(define ~= /=)

(define ~== 
  (method (x y)
    (not (== x y))))

;; Default methods.

(define-method = ((x <object>) (y <object>))
  (id? x y))

;; Generic min, max

(define min
  (method (object #rest objects)
    (reduce (method (x y)
              (if (< x y) x y))
            object
            objects)))

(define max
  (method (object #rest objects)
    (reduce (method (x y)
              (if (> x y) x y))
            object
            objects)))


;; eof
