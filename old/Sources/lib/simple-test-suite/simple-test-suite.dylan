module:    simple-test-suite
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;;; PRIMITIVE CALLS

;; (primitive-terminal-output ^#\A (%stream-handle *standard-output*))
;; (primitive-terminal-output ^#\newline (%stream-handle *standard-output*))

;;;;; LAMBDA CALLS

;; (for (((item <integer>) in '(65 66 67)))
;;   (primitive-terminal-output
;;    (primitive-small-integer-as-byte-character (%small-integer-data item))
;;    (stream-handle *standard-output*)))
;; 
;; (primitive-terminal-output
;;  (primitive-small-integer-as-byte-character ^10) 
;;  (stream-handle *standard-output*))

;;;;; DISPATCH PRINTING

(define trampoline-1 (method ((f <function>) argument)
  (f argument)))

(define $print-items
  '(#\B 1 -23 "HELLO WORLD"
    #(1) #(1 2) #((1)) #(#(1)) #(#(1) #(2)) #(#(1) #(2) 3)))

(iterate loop (((items <list>) $print-items))
  (unless (empty? items)
    (print (head items)) (output *standard-output* #\Newline)
    (loop (tail items))))

;;;;; MORE COMPLICATED PRINTING

(format *standard-output* "HELLO, ~D IS MY FAVORITE NUMBER~%" 3)

;;;; LITERAL TEST

(format *standard-output* "HOWDY LITERAL LIST ~S~%" '(1 2 3))

;; (format *standard-output* "HOWDY LITERAL IMPROPER LIST ~S~%" '(1 2 . 3))

(format *standard-output* "HOWDY LITERAL VECTOR ~S~%" #(1 2 3))

;;;;; METHOD DEFINITION

(define-method gf-simple ((a <pair>)) a)
(define-method gf-simple ((a <integer>)) a)

(define-variable ($gf-simple-tests <simple-object-vector>)
    (vector (pair 1 1) 85 0))

(format *standard-output* "GF-SIMPLE-PAIR ~S~%"
	(gf-simple (element $gf-simple-tests 0)))
(format *standard-output* "GF-SIMPLE-INTEGER ~S~%"
	(gf-simple (element $gf-simple-tests 1)))

(define-method gf-rest (a #rest r) (pair a r))

(define-variable ($gf-rest-test <simple-object-vector>) #(1 2 3))

(format *standard-output* "GF-REST ~S~%"
	(apply gf-rest $gf-rest-test))

(define-variable ($gf-key-tests <simple-object-vector>)
    #(#(1 key: 2) #(1)))

(define-method gf-key (a #key (key 666)) (pair a key))

(format *standard-output* "GF-KEY ~S~%"
	(apply gf-key (element $gf-key-tests 0)))

(format *standard-output* "GF-KEY-DEFAULT ~S~%"
	(apply gf-key (element $gf-key-tests 1)))

(define-method gf-key-rest (a #rest rest #key key) (pair (pair a key) rest))

(format *standard-output* "GF-KEY-REST ~S~%"
	(apply gf-key-rest (element $gf-key-tests 0)))

(format *standard-output* "GF-KEY-REST-DEFAULT ~S~%"
	(apply gf-key-rest (element $gf-key-tests 1)))

(define-method gf-singleton ((a <pair>)) a)
(define-method gf-singleton ((a <integer>)) a)
(define-method gf-singleton ((a (singleton 0))) 99)

(format *standard-output* "GF-SINGLETON-PAIR ~S~%"
	(gf-singleton (element $gf-simple-tests 0)))
(format *standard-output* "GF-SINGLETON-INTEGER ~S~%"
	(gf-singleton (element $gf-simple-tests 1)))
(format *standard-output* "GF-SINGLETON==0 ~S~%"
	(gf-singleton (element $gf-simple-tests 2)))

;;;; APPLY TESTS

(format *standard-output* "APPLY LIST ~S~%" (apply list 1 2 3 4 ()))

(format *standard-output* "APPLY LIST ~S~%" 
        (apply list 1 2 3 4 5 6 7 8 9 10 ()))

;;;; BIG DISPATCH -- TEST DEFAULT DISCRIMINATOR

(define-method gf-big (a1 a2 a3 a4 a5 a6 a7 (a8 <integer>))
  (format *standard-output* "GF-BIG ")
  (print a1) (print a2) (print a3) (print a4)
  (print a5) (print a6) (print a7) (print a8)
  (print #\Newline))

(apply gf-big #(1 2 3 4 5 6 7 8))

(define-method gf-big-rest (a1 a2 a3 a4 a5 a6 a7 (a8 <integer>) #rest rest)
  (format *standard-output* "GF-BIG-REST ")
  (print a1) (print a2) (print a3) (print a4)
  (print a5) (print a6) (print a7) (print a8)
  (print rest)
  (print #\Newline))

(apply gf-big-rest #(1 2 3 4 5 6 7 8 9 10))

#|
;;;; BIND-EXIT TESTS

(print (bind-exit (return) (+ (return 1) 2))) (print #\newline)

(print (bind-exit (return)
	   (bind-methods ((factorial ((k <integer>) (product <integer>))
			    (print product)
			    (if (= k 1)
				(return product)
				(factorial (- k 1) (* product k)))))
	     (factorial 10 1))))
(print #\newline)

(print (bind-exit (return) ((method () (return 1))) 2)) (print #\newline)

;; (print (bind-exit (return) ((curry return 1)) 2)) (print #\newline)

(print (bind-exit (return) (unwind-protect (+ (return 1) 2) (print 0) 3))) (print #\newline)

(print 
 (bind-exit (return)
  (unwind-protect
      (+ (return 1) 2)
    (print 9)
    (return 1)
    (print 0)
    2)))
(print #\newline)

(print 
 (bind-exit (return)
   (unwind-protect
	(unwind-protect
	     (unwind-protect
		  (return 3)
	       (print 0))
	  (print 1))
     (print 2))))
(print #\newline)

(print
 (bind-exit (outer)
  (unwind-protect
      (+ (outer 1) 2)
    (bind-exit (inner)
      (unwind-protect
          (+ (outer 1) 2)
        (print 9)
        (inner 1)
        (print 0)
        6))
    (print 3)
    (outer 5)
    (print 7)
    8)))
(print #\newline)
|#

;;;;; THAT'S ALL FOLKS

(format *standard-output* "THAT'S ALL FOLKS!~%")
