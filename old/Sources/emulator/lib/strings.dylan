;; (define-class <string> (<vector>))

;; Creation:

(define-method make ((class (singleton <string>))
                     #key (size 0)
		          (fill #\space))
  (make <byte-string> size: size fill: fill))

(define-method make ((class (singleton <byte-string>))
                     #key (size 0)
		          (fill #\space))
  (make/string size initial-element: fill))

;; Iteration:

;; (Inherited from <vector> via our dodgy heirarchy)
 
;; Coercion:

(define-method as ((class (singleton <string>)) any)
  (as <byte-string> any))

(define-method as ((class (singleton <byte-string>)) (s <byte-string>))
  s)

(define-method as ((class (singleton <byte-string>)) (c <collection>))
  (bind ((len (size c))
         (new (make <byte-string> size: len)))
    (for ((state (initial-state c) (next-state c state))
          (i     0                 (+ i 1)))
         ((not state) new)
      (set! (element new i) (current-element c state)))))

(define-method as ((class (singleton <byte-string>)) (s <symbol>))
  (as <string> s))

;; --

(define-method < ((s1 <sequence>) (s2 <sequence>))
  (bind-exit (return)
    (for-each ((c1 s1) (c2 s2)) ()
      (cond
        ((< c1 c2) (return #t))
        ((> c1 c2) (return #f))))
    (< (size s1) (size s2))))
      
(define-method as-lowercase ((s <string>))
  (map as-lowercase s))

(define-method as-lowercase! ((s <string>))
  (map-into s as-lowercase s))

(define-method as-uppercase ((s <string>))
  (map as-uppercase s))

(define-method as-uppercase! ((s <string>))
  (map-into s as-uppercase s))

;; --

(define-class <unicode-string> (<string>))

(define-method make ((class (singleton <unicode-string>)) #key)
  (error "Creation of unicode strings is not supported in the emulator"))

;; eof
