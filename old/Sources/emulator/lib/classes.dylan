(define-method all-superclasses ((c <object>)) ; should be <class>
  (sort (reduce union (list c) (map all-superclasses (direct-superclasses c)))
        test: subclass?))

(define-method all-subclasses ((c <object>)) ; should be <class>
  (reduce union (list c) (map all-subclasses (direct-subclasses c))))

(define-class <singleton> (<object>))

(define-method make ((class (singleton <singleton>)) #key object)
  `(eql ,object))

(define-method seal ((c <class>))
  (format #t "~&Warning: ignoring seal on ~s~%" c)
  c)
