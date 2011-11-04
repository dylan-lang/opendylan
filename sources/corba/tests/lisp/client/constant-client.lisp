;; -*- Mode: Lisp; -*-

;; #<harlequin copyright marker>

(in-package "CL-USER")

(defmacro do-check (form1 form2)
  (with-unique-names (mess1 mess2 eq1 eq2)
  `(let ((,mess1 ',form1)
         (,mess2 ',form2)
         (,eq1 ,form1)
         (,eq2 ,form2))
     (unless (equalp ,eq1 ,eq2)
       (format t "*** Not equal : ~A ~A : ~A ~A~%"
               ,mess1 ,mess2 ,eq1 ,eq2)))))

(defun constant-tests ()

  ;; constant integer test

  (do-check omg.org/root:min_short -32768)
  (do-check omg.org/root:max_short 32767)
  (do-check omg.org/root:min_ushort 0)
  (do-check omg.org/root:max_ushort 65535)
  (do-check omg.org/root:min_long -536870911)
  (do-check omg.org/root:max_long 536870911)
  (do-check omg.org/root:min_ulong 0)
  (do-check omg.org/root:max_ulong 536870911)
  (do-check omg.org/root:octal_integer #o01234567)
  (do-check omg.org/root:hexadecimal_integer_1 #xABCDEF)
  (do-check omg.org/root:hexadecimal_integer_2 #xABCDEF)

  ;; constant character test

  (do-check omg.org/root:space-% #\Space)
  (do-check omg.org/root:underscore #\_)
  (do-check omg.org/root:digit0 #\0)
  (do-check omg.org/root:lettera #\A)
  (do-check omg.org/root:newline #\Newline)
  (do-check omg.org/root:horizontal_tab #\tab)
  (do-check omg.org/root:vertical_tab #\VT)
  (do-check omg.org/root:backspace #\backspace)
  (do-check omg.org/root:carriage_return #\Return)
  (do-check omg.org/root:form_feed #\FormFeed)
  (do-check omg.org/root:alert #\BEL)
  (do-check omg.org/root:question_mark #\?)
  (do-check omg.org/root:single_quote #\')
  (do-check omg.org/root:double_quote #\")
  (do-check omg.org/root:octal_number_1 (code-char 1))
  (do-check omg.org/root:octal_number_2 (code-char #o12))
  (do-check omg.org/root:octal_number_3 (code-char #o123))
  (do-check omg.org/root:hexadecimal_number_1 (code-char #x1))
  (do-check omg.org/root:hexadecimal_number_2 (code-char #x12))

  ;; boolean tests
  
  (do-check omg.org/root:my_true t)
  (do-check omg.org/root:my_false nil)

  ;; string tests

  (do-check omg.org/root:empty_string "")
  (do-check omg.org/root:name "Keith")
  (do-check omg.org/root:concatenated_strings "First part+Second part")
  (do-check omg.org/root:escape_sequences
            (coerce (list #\Newline #\space #\tab #\space 
                          (code-char #xb) #\space #\backspace #\space
                          #\return #\space #\formfeed #\space #\bel #\space
                          #\\ #\space #\? #\space #\' #\space #\" #\space
                          (code-char 1) #\space (code-char #xA) #\space
                          (code-char #x53) #\space (code-char 1) #\space (code-char #x12))
                    'string))

  ;; expression tests

  (do-check omg.org/root:unary_plus 2)
  (do-check omg.org/root:unary_minus -3)
  (do-check omg.org/root:binary_minus -1)
  (do-check omg.org/root:binary_division 14)
  (do-check omg.org/root:binary_mod 2)
  
  (do-check omg.org/root:binary_xor 1)
  (do-check omg.org/root:binary_or 3)
  (do-check omg.org/root:binary_and 2)
  (do-check omg.org/root:binary_lshift 40)
  (do-check omg.org/root:binary_rshift 25)

  (do-check omg.org/root:minus_one -1)
  (do-check omg.org/root:ice_cream 99)
  (do-check omg.org/root:secs_in_1_day 86400)
  (do-check omg.org/root:foo 48)

  (do-check omg.org/root:a_prime omg.org/root:a)

  (let ((a omg.org/root:a)
        (b omg.org/root:b)
        (c omg.org/root:c)
        (d omg.org/root:d))

    (do-check omg.org/root:distrib_1 (logxor (logior a b c) d))
    (do-check omg.org/root:distrib_2 (logior (logxor a d) (logxor b d) (logxor c d)))
    (do-check omg.org/root:distrib_3 (logand (logxor a b c) d))
    (do-check omg.org/root:distrib_4 (logxor (logand a d) (logand b d) (logand c d)))
    (do-check omg.org/root:distrib_5 (ash (logand a b c) (- d)))
    (do-check omg.org/root:distrib_6 (logand (ash a (- d)) (ash b (- d)) (ash c (- d))))
    (do-check omg.org/root:distrib_7 (ash (logand a b c) d))
    (do-check omg.org/root:distrib_8 (logand (ash a d) (ash b d) (ash c d)))
    (do-check omg.org/root:distrib_9 (+ (ash (ash a (- b)) (- c)) d))
    (do-check omg.org/root:distrib_10 (ash (ash (+ a d) (- (+ b d))) (- (+ c d))))
    (do-check omg.org/root:distrib_11 (- (ash (ash a b) c) d))
    (do-check omg.org/root:distrib_12 (ash (ash (- a d) (- b d)) (- c d)))
    (do-check omg.org/root:distrib_13 (truncate (+ a b c) d))
    (do-check omg.org/root:distrib_14 (+ (truncate a d) (truncate b d) (truncate c d)))
    (do-check omg.org/root:distrib_15 (* (- a b c) d))
    (do-check omg.org/root:distrib_16 (- (* a d) (* b d) (* c d)))
    (do-check omg.org/root:distrib_17 (mod (+ a b c) d))
    (do-check omg.org/root:distrib_18 (+ (mod a d) (mod b d) (mod c d)))

    (do-check omg.org/root:unary_1 (+ a (- b) (lognot c)))
    (do-check omg.org/root:unary_2 (+ a (- b) (lognot c))))
            
  )
