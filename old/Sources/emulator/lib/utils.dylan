(define show
  (method (obj) (format #t "[~s]" obj) obj))

(define-class <placeholder> (<object>)
  (debug-name required-init-keyword: debug-name:))

(define unsupplied unsupplied? supplied?
  (bind ((val (make <placeholder> debug-name: 'unsupplied)))
    (values
      (method () val)
      (method (o) (id? o val))
      (method (o) (not (id? o val))))))

(define unfound unfound? found?
  (bind ((val (make <placeholder> debug-name: 'unfound)))
    (values
      (method () val)
      (method (o) (id? o val))
      (method (o) (not (id? o val))))))

(define uninitialized uninitialized? initialized?
  (bind ((val (make <placeholder> debug-name: 'uninitialized)))
    (values
      (method () val)
      (method (o) (id? o val))
      (method (o) (not (id? o val))))))

;; eof

