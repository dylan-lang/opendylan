; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package 'ccl)



(locally
  (declare (optimize (speed 3) (safety 0) (fixnum-safety 0) (debug 0)))
(defun typed-nfib(n)
  (declare (fixnum n))
  (if (<= n 1)
      1
      (+ (the fixnum (typed-nfib (- n 1))) (the fixnum (typed-nfib (- n 2))) 1)))
)

(defun untyped-nfib(n)
  (if (<= n 1)
      1
      (+ (untyped-nfib (- n 1)) (untyped-nfib (- n 2)) 1)))

(defmethod nfib-method((n integer))
  (if (<= n 1)
      1
      (+ (nfib-method (- n 1)) (nfib-method (- n 2)) 1)))

(defmethod nfib-method((n standard-object))
  (if (<= n 1)
      1
      (+ (nfib-method (- n 1)) (nfib-method (- n 2)) 1)))


(defmethod singleton-nfib-method((n (eql 0))) 1)
(defmethod singleton-nfib-method((n (eql 1))) 1)

(defmethod singleton-nfib-method((n integer))
  (+ (singleton-nfib-method (- n 1)) (singleton-nfib-method (- n 2)) 1))

(defun invoke-snm (n) (singleton-nfib-method n))
