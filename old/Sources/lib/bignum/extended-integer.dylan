;;; Copyright (c) 1993, Jonathan Bachrach
;;; Copyright (c) 1993, Functional Objects, Inc.
;;; Copyright (c) 1993, IRCAM
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms,  with or without modifica-
;;; tion,  are not permitted  without the express prior written permission of the
;;; copyright holders.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY JONATHAN BACHRACH,  Functional Objects, Inc., 
;;; AND IRCAM ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO,  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PAR-
;;; TICULAR PURPOSE ARE  DISCLAIMED.   IN NO EVENT SHALL JONATHAN BACHRACH,   THE
;;; Functional Objects, Inc.,  OR  IRCAM BE LIABLE FOR ANY DIRECT,  INDIRECT,  IN-
;;; CIDENTAL, SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL DAMAGES  (INCLUDING,  BUT NOT 
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;   LOSS OF USE, DATA, 
;;; OR PROFITS;  OR BUSINESS INTERRUPTION)  HOWEVER CAUSED  AND  ON ANY THEORY OF 
;;; LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,  OR TORT (INCLUDING NEGLI-
;;; GENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  EVEN 
;;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-module "DYLAN")

(define ($digit-size <integer>) 16)
(define ($digit-size-1 <integer>) (- $digit-size 1))
(define ($digit-size+1 <integer>) (+ $digit-size 1))
(define ($digit-shifter <integer>) (expt 2 $digit-size))
(define ($digit-mask <integer>) (- $digit-shifter 1))
(define ($all-ones-digit <integer>) $digit-mask)

(define-class <extended-integer> (<stretchy-vector>))

(define-method initialize 
    ((integer <extended-integer>) #key (size: the-size 1) (fill 0) (bytes '()))
  (next-method integer size: (max the-size (size bytes)) fill: fill)
  (unless (empty? bytes)
    (replace-subsequence! integer bytes))
  integer)

(define-method print
    ((integer <extended-integer>) 
     #key (stream *standard-output*) (verbose? #F) (base 10))
  ;; (format #T "BASE: ~D~%" base)
  (set! base (as <extended-integer> base))
  (bind-methods ((sub-print ((integer <extended-integer>))
		  (bind (((quotient <extended-integer>)
                          (remainder <extended-integer>)
			  (big-truncate/ integer base)))
		    (set! remainder (as <integer> remainder))
		    (unless (binary= quotient $extended-zero)
		       ;; Recurse until you have all the digits pushed on stack
		       (sub-print quotient))
		     ;; Then as each recursive call unwinds, turn the digit (in
		     ;; remainder) into a character and output the character.
		     (print (as <character>
				(if (and (> remainder 9) (> base 10))
				    (+ (as <integer> #\A)
				       (- remainder 10))
				    (+ (as <integer> #\0) remainder)))
			    stream: stream
                            verbose?: #F))))
    (if (big-negative? integer)
	(begin
	 (print #\- stream: stream)
	 (sub-print (binary- $extended-zero integer)))
	(sub-print integer))
    integer))

(define-method big-print
    ((integer <extended-integer>) 
     #key (stream *standard-output*) (verbose? #F) (base 10))
  ;; should put base in use verbose? flag in <integer> print method
  (format stream "@[")
  (bind ((first? #T))
    (for-each ((byte (reverse integer))) ()
      (if first? (set! first? #F) (format stream ":"))
      (print byte stream: stream verbose?: verbose? base: 16)))
  (format stream "]")
  (bind ((number 0)
         (negative? (big-negative? integer)))
    (for-each ((digit (reverse integer))) ()
      (set! number (+ (* number $digit-shifter)
                      (if negative? (logand (lognot digit) $digit-mask) digit))))
    (print (if negative? (- -1 number) number) stream: stream))
  integer)

(define-method normalize 
    ((integer <extended-integer>) #: <extended-integer>)
  (bind ((normalized-size (size integer)))
    (unless (= normalized-size 1)
      (iterate scan ((next-digit (element integer (- normalized-size 2)))
                     (sign-digit (element integer (- normalized-size 1))))
        (when (or (and (= sign-digit 0)
                       (not (logbit? $digit-size-1 next-digit)))
                  (and (= sign-digit $digit-mask) 
                       (logbit? $digit-size-1 next-digit)))
          (set! normalized-size (- normalized-size 1))
          (unless (= normalized-size 1)
            (scan (element integer (- normalized-size 2)) next-digit)))))
    (unless (= normalized-size (size integer))
      (set! integer (copy-sequence integer end: normalized-size)))
    integer))

(define-method sign-digit ((integer <extended-integer>) #: <integer>)
  (if (logbit? $digit-size-1 (last integer)) -1 0))

(define-method shallow-copy ((integer <extended-integer>))
  (make <extended-integer> bytes: (copy-sequence integer)))

(define-method as ((class (singleton <extended-integer>)) (integer <integer>))
  ;; !@#$ patch for bigger than digit
  (make <extended-integer> bytes: (list (logand integer $digit-mask))))

(define-method as ((class (singleton <integer>)) (integer <extended-integer>))
  ;; !@#$ patch for extending sign bit
  (first integer))

;;;; !@#$ BOGUS FOR <EXTENDED-INTEGER>

(define-method precision ((class (singleton <extended-integer>))) 0)

(define-method most-positive ((class (singleton <extended-integer>))) 0)

(define-method most-negative ((class (singleton <extended-integer>))) 0)

(define-method precision ((integer <extended-integer>))
  (* (size integer) $digit-size))

;; !@#$ should be inherited
(define-method =hash ((integer <extended-integer>))
  (first integer))

(define-method binary= ((number-1 <extended-integer>) (number-2 <extended-integer>))
  (every? binary= number-1 number-2))

;; !@#$ big-positive should be positive

(define-method big-positive? ((number <extended-integer>))
  (and (= (sign-digit number) 0) (not (= number 0))))

(define-method big-negative? ((number <extended-integer>))
  (= (sign-digit number) -1))

(define-method binary< ((number-1 <extended-integer>) (number-2 <extended-integer>))
  (big-negative? (binary- number-1 number-2)))

(define-method add-with-carry
    ((byte-1 <integer>) (byte-2 <integer>) (carry <integer>) 
     #: (values <integer> <integer>))
  ;; (format #T "ADD-with-carry ~D ~D ~D~%" byte-1 byte-2 carry)
  (bind ((result (+ byte-1 byte-2 carry)))
    (if (zero? (logand (+ $digit-mask 1) result))
	(values result 0)
	(values (logand $digit-mask result) 1))))

(define-method binary+ ((number-1 <extended-integer>) (number-2 <extended-integer>))
  (bind (((carry <integer>) 0)
	 ((a-initial-size <integer>) (size number-1))
	 ((b-initial-size <integer>) (size number-2))
	 ((a <extended-integer>) (a-size <integer>)
	  (b <extended-integer>) (b-size <integer>)
	  (if (> a-initial-size b-initial-size)
	      (values number-1 a-initial-size number-2 b-initial-size)
	      (values number-2 b-initial-size number-1 a-initial-size)))
	 ((result-size <integer>) (+ a-size 1))
         ((result <extended-integer>) (make <extended-integer> size: result-size)))

    (bind-methods
      ((finish-add ((a <extended-integer>) (result <extended-integer>)
		    (carry <integer>) (b-sign-digit <integer>)
		    (start <integer>) (end <integer>))
         (format #T "finish-add~%")
	 (for ((i start (+ i 1)))
	     ((= i end)
	      (set! (element result end)
		    (add-with-carry (sign-digit a) b-sign-digit carry)))
	   (bind (((v <integer>) (k <integer>)
		   (add-with-carry (element a i) b-sign-digit carry)))
	     (format #T "RESULT ~D CARRY ~D~%" v k)
	     (set! (element result i) v)
	     (set! carry k)))))

      (dotimes (i b-size)
	(bind (((v <integer>) (k <integer>)
		(add-with-carry (element a i) (element b i) carry)))
	  (format #T "RESULT ~D CARRY ~D~%" v k)
	  (set! (element result i) v)
	  (set! carry k)))
      (if (/= a-size b-size)
	  (finish-add a result carry (sign-digit b) b-size a-size)
	  (set! (element result b-size)
		(add-with-carry (sign-digit a) (sign-digit b) carry)))
      (normalize result))))

(define-method subtract-with-borrow
    ((a-digit <integer>) (b-digit <integer>) (borrow <integer>) 
     #: (values <integer> <integer>))
  ;; (format #T "SUB-with-borrow ~D ~D ~D~%" a-digit b-digit borrow)
  (bind (((diff <integer>) (+ (- a-digit b-digit) borrow $digit-mask)))
    (values (logand diff $digit-mask) (ash diff (- $digit-size)))))

(define-method binary- ((a <extended-integer>) (b <extended-integer>))
  (bind (((a-size <integer>) (size a))
	 ((b-size <integer>) (size b))
	 ((result-size <integer>) (+ (max a-size b-size) 1))
	 ((result <extended-integer>) (make <extended-integer> size: result-size))
	 ((borrow <integer>) 1)
	 ((a-sign <integer>) (sign-digit a))
	 ((b-sign <integer>) (sign-digit b)))
    (dotimes (i result-size)
      (bind (((a-digit <integer>) (if (< i a-size) (element a i) a-sign))
	     ((b-digit <integer>) (if (< i b-size) (element b i) b-sign))
	     ((v <integer>) (k <integer>)
	      (subtract-with-borrow a-digit b-digit borrow)))
	;; (format #T "RESULT ~D BORROW ~D~%" v k)
	(set! (element result i) v)
	(set! borrow k)))
    (normalize result)))

(define-method multiply-and-add 
    ((x <integer>) (y <integer>) (previous <integer>) (carry-in <integer>))
  (bind ((big (* x y))
         (big-lo (+ (logand big $digit-mask) previous carry-in))
         (lo (logand big-lo $digit-mask))
         (carry (+ (ash big (- $digit-size)) (ash big-lo (- $digit-size)))))
    ;; (format #T "MULTIPLY-AND-ADD ~D ~D ~D ~D -> ~D ~D~%"
    ;;         x y previous carry-in carry lo)
    (values carry lo)))

(define $extended-zero (as <extended-integer> 0))         
  
(define-method binary* ((number-1 <extended-integer>) (number-2 <extended-integer>))
  (bind ((a-positive? (big-positive? number-1))
	 (b-positive? (big-positive? number-2))
	 (a (if a-positive? number-1 (binary- $extended-zero number-1)))
	 (b (if b-positive? number-2 (binary- $extended-zero number-2)))
	 (a-size (size a))
	 (b-size (size b))
	 (result-size (+ a-size b-size))
	 (result (make <extended-integer> size: result-size))
	 (result-negative? (not (id? a-positive? b-positive?))))
    (dotimes (i a-size)
      (bind ((carry-digit 0)
	     (x (element a i))
	     (k i))
	(dotimes (j b-size)
	  (bind ((big-carry result-digit
		  (multiply-and-add
                   x (element b j) (element result k) carry-digit)))
	    (set! (element result k) result-digit)
	    (set! carry-digit big-carry)
	    (set! k (+ k 1))))
	(set! (element result k) carry-digit)))
    (when result-negative?
      (set! result (binary- $extended-zero result)))
    (normalize result)))
    
(define-method operate 
    ((function <function>) (x <extended-integer>) (y <extended-integer>))
  (bind (((x <extended-integer>) (y <extended-integer>)
          (if (< (size x) (size y)) (values x y) (values y x)))
         ((x-size <integer>) (size x))
         ((y-size <integer>) (size y))
         ((result <extended-integer>)
          (make <extended-integer> size: y-size)))
    (dotimes (i x-size)
      (set! (element result i)
            (function (element x i) (element y i))))
    (for ((i x-size (+ i 1)))
        ((>= i y-size)
         (normalize result))
      (set! (element result i) (function 0 (element y i))))))

(define-method binary-logior ((x <extended-integer>) (y <extended-integer>))
  (operate binary-logior x y))

(define-method binary-logxor ((x <extended-integer>) (y <extended-integer>))
  (operate binary-logxor x y))

(define-method binary-logand ((x <extended-integer>) (y <extended-integer>))
  (operate binary-logior x y))

(define-method shift-right ((integer <extended-integer>) (count <integer>))
  (bind ((size (size integer))
         ((number-digits <integer>) (number-bits <integer>) 
          (truncate/ count $digit-size)))
    (if (>= number-digits size)
	(if (>= integer 0) (as <extended-integer> 0) (as <extended-integer> -1))
	(bind (((high-bits-in-first-digit <integer>) (- $digit-size number-bits))
	       ((result-size <integer>) (- size number-digits))
	       ((result-size-1 <integer>) (- result-size 1))
               ((result <extended-integer>)
                (make <extended-integer> size: result-size)))
          (for ((i number-digits i+1)
                (i+1 (+ number-digits 1) (+ i+1 1))
                (j 0 (+ j 1)))
              ((= j result-size-1)
               (set! (element result j)
                     (ash (element integer i) (- number-bits)))
               (normalize result))
            (set! (element result j)
                  (logand 
                   (logior (ash (element integer i) (- number-bits))
                           (ash (element integer i+1) 
                                high-bits-in-first-digit))
                   $digit-mask)))))))

(define-method shift-left  ((integer <extended-integer>) (count <integer>))
  (bind ((size (size integer))
         ((number-digits <integer>) (number-bits <integer>) 
          (truncate/ count $digit-size))
         ((remaining-bits <integer>) (- $digit-size number-bits))
         ((result-size <integer>) (+ size number-digits 1))
	 ((result-size-1 <integer>) (- result-size 1))
	 ((result <extended-integer>)
	  (make <extended-integer> size: result-size)))
    (for ((i 0 i+1)
	  (i+1 1 (+ i+1 1))
	  (j (+ number-digits 1) (+ j 1)))
	((= j result-size-1)
	 (set! (element result number-digits)
	       (logand (ash (element integer 0) number-bits) $digit-mask))
	 (set! (element result j)
	       (ash (element integer i) (- remaining-bits)))
         (normalize result))
      (set! (element result j)
	    (logand (logior (ash (element integer i) (- remaining-bits))
		            (ash (element integer i+1) number-bits))
                    $digit-mask)))))

;; !@#$ shift should be ash

(define-method big-ash ((integer <extended-integer>) (count <integer>))
  (if (< count 0)
      (shift-right integer (- count))
      (shift-left integer count)))

;;;; TRUNCATE.

;;; This is the original sketch of the algorithm from which I implemented this
;;; TRUNCATE, assuming both operands are bignums.  I should modify this to work
;;; with the documentation on my functions, as a general introduction.  I've
;;; left this here just in case someone needs it in the future.  Don't look
;;; at this unless reading the functions' comments leaves you at a loss.
;;; Remember this comes from Knuth, so the book might give you the right general
;;; overview.
;;; 
;;;
;;; (truncate x y):
;;;
;;; If X's magnitude is less than Y's, then result is 0 with remainder X.
;;;
;;; Make x and y positive, copying x if it is already positive.
;;;
;;; Shift y left until there's a 1 in the 30'th bit (most significant, non-sign
;;;       digit)
;;;    Just do most sig digit to determine how much to shift whole number.
;;; Shift x this much too.
;;; Remember this initial shift count.
;;;
;;; Allocate q to be len-x minus len-y quantity plus 1.
;;;
;;; i = last digit of x.
;;; k = last digit of q.
;;;
;;; LOOP
;;;
;;; j = last digit of y.
;;;
;;; compute guess.
;;; if x[i] = y[j] then g = #xFFFFFFFF
;;; else g = x[i]x[i-1]/y[j].
;;;
;;; check guess.
;;; %UNSIGNED-MULTIPLY returns b and c defined below.
;;;    a = x[i-1] - (logand (* g y[j]) #xFFFFFFFF).
;;;       Use %UNSIGNED-MULTIPLY taking low-order result.
;;;    b = (logand (ash (* g y[j-1]) -32) #xFFFFFFFF).
;;;    c = (logand (* g y[j-1]) #xFFFFFFFF).
;;; if a < b, okay.
;;; if a > b, guess is too high
;;;    g = g - 1; go back to "check guess".
;;; if a = b and c > x[i-2], guess is too high
;;;    g = g - 1; go back to "check guess".
;;; GUESS IS 32-BIT NUMBER, SO USE THING TO KEEP IN SPECIAL REGISTER
;;; SAME FOR A, B, AND C.
;;;
;;; Subtract g * y from x[i - len-y+1]..x[i].  See paper for doing this in step.
;;; If x[i] < 0, guess is fucked.
;;;    negative g, then add 1
;;;    zero or positive g, then subtract 1
;;; AND add y back into x[len-y+1..i].
;;;
;;; q[k] = g.
;;; i = i - 1.
;;; k = k - 1.
;;;
;;; If k>=0, goto LOOP.
;;;
;;;
;;; Now quotient is good, but remainder is not.
;;; Shift x right by saved initial left shifting count.
;;;
;;; Check quotient and remainder signs.
;;; x pos y pos --> q pos r pos
;;; x pos y neg --> q neg r pos
;;; x neg y pos --> q neg r neg
;;; x neg y neg --> q pos r neg
;;;
;;; Normalize quotient and remainder.  Cons result if necessary.
;;;

;;; BIGNUM-TRUNCATE -- Public.
;;;
;;; This divides x by y returning the quotient and remainder.  In the general
;;; case, we shift y to setup for the algorithm, and we use two buffers to save
;;; consing intermediate values.  X gets destructively modified to become the
;;; remainder, and we have to shift it to account for the initial Y shift.
;;; After we multiple bind q and r, we first fix up the signs and then return
;;; the normalized results.
;;;
(define-method big-truncate/ 
    ((x <extended-integer>) (y <extended-integer>) 
     #: (values <extended-integer> <extended-integer>))
  (bind ((x-positive? (not (big-negative? x)))
	 (y-positive? (not (big-negative? y)))
	 (x (if x-positive? x (binary- $extended-zero x)))
	 (y (if y-positive? y (binary- $extended-zero y)))
	 (x-size (size x))
	 (y-size (size y))
	 (q r
	  (cond ((< y-size 2)
                 (format #T "SINGLE-DIGIT CASE ~D~%" y-size)
	         (truncate-single-digit x y))
	        ((binary< x y)
                 (format #T "X<Y ~S < ~S~%" x y)
		 (values (as <extended-integer> 0) (copy-sequence x)))
	        (else:
	         (bind ((x-size+1 (+ x-size 1))
                        (y-shift (shift-y-for-truncate y))
                        (truncate-x (big-ash x y-shift))
                        (truncate-y (big-ash y y-shift)))
                   (format #T "Y-SHIFT ~D~%" y-shift)
                   (unless (= (size truncate-x) x-size+1)
                     (set! (size truncate-x) x-size+1)
                     (set! (element truncate-x x-size) 0)
                     ;; (set! truncate-x (big-ash truncate-x $digit-size))
                     )
		   (values (do-truncate truncate-x x-size+1 truncate-y y-size)
			   ;; DO-TRUNCATE must execute first.
			   (if (zero? y-shift)
                               (begin 
                                (set! (size truncate-x) y-size)
			        (normalize truncate-x))
			       (big-ash truncate-x (- y-shift)))))))))
    (values (normalize 
             (if (id? x-positive? y-positive?) q (binary- $extended-zero q)))
	    (normalize (if x-positive? r (binary- $extended-zero r))))))

;;; SHIFT-Y-FOR-TRUNCATE -- Internal.
;;;
;;; This returns the amount to shift y to place a one in the second highest
;;; bit.  Y must be positive.  If the last digit of y is zero, then y has a
;;; one in the previous digit's sign bit, so we know it will take one less
;;; than digit-size to get a one where we want.  Otherwise, we count how many
;;; right shifts it takes to get zero; subtracting this value from digit-size
;;; tells us how many high zeros there are which is one more than the shift
;;; amount sought.
;;;
;;; Note: This is exactly the same as one less than the integer-length of the
;;; last digit subtracted from the digit-size.
;;; 
;;; We shift y to make it sufficiently large that doing the 64-bit by 32-bit
;;; %FLOOR calls ensures the quotient and remainder fit in 32-bits.
;;;
(define-method shift-y-for-truncate ((y <extended-integer>) #: <integer>)
  ;; !@#$ length exist?
  (- $digit-size (length (last y)) 1))

;;; BIGNUM-TRUNCATE-SINGLE-DIGIT -- Internal.
;;;
;;; This divides x by y when y is a single bignum digit.  BIGNUM-TRUNCATE fixes
;;; up the quotient and remainder with respect to sign and normalization.
;;;
;;; We don't have to worry about shifting y to make its most significant digit
;;; sufficiently large for %FLOOR to return 32-bit quantities for the q-digit
;;; and r-digit.  If y is a single digit bignum, it is already large enough
;;; for %FLOOR.  That is, it has some bits on pretty high in the digit.
;;;
(define-method truncate-single-digit 
    ((x <extended-integer>) (y <extended-integer>)
     #: (values <extended-integer> <extended-integer>))
  (bind ((q (make <extended-integer> size: (size x)))
	 (r 0)
	 (y (element y 0)))
    (for ((i (- (size x) 1) (- i 1)))
	((negative? i))
      (bind (((q-digit <integer>) (r-digit <integer>)
              (small-floor r (element x i) y)))
	(set! (element q i) q-digit)
	(set! r r-digit)))
    (values q (as <extended-integer> r))))

;;; DO-TRUNCATE -- Internal.
;;;
;;; This divides *truncate-x* by *truncate-y*, and x-size and y-size tell us how
;;; much of the buffers we care about.  TRY-BIGNUM-TRUNCATE-GUESS modifies
;;; *truncate-x* on each interation, and this buffer becomes our remainder.
;;;
;;; *truncate-x* definitely has at least three digits, and it has one more than
;;; *truncate-y*.  This keeps i, i-1, i-2, and low-x-digit happy.  Thanks to
;;; SHIFT-AND-STORE-TRUNCATE-BUFFERS.
;;;
(define-method do-truncate 
    ((truncate-x <extended-integer>) (x-size <integer>)
     (truncate-y <extended-integer>) (y-size <integer>)
     #: <extended-integer>)
  (bind (((q-size <integer>) (- x-size y-size))
	 ((y1 <integer>) (element truncate-y (- y-size 1)))
	 ((y2 <integer>) (element truncate-y (- y-size 2)))
	 ;; Add one for extra sign digit in case high bit is on.
	 ((q <extended-integer>) (make <extended-integer> size: (+ q-size 1))))
    (format #T "DO-TRUNCATE TX ~S X-SIZE ~D TY ~S Y-SIZE ~D~%"
            truncate-x x-size truncate-y y-size)
    (iterate next (((k <integer>) (- q-size 1))
                   ((i <integer>) (- x-size 1))
	           ((i-1 <integer>) (- x-size 2))
	           ((i-2 <integer>) (- x-size 3))
	           ((low-x-digit <integer>) (- x-size y-size 1)))
      (set! (element q k)
	    (try-truncate-guess
	     ;; This modifies *truncate-x*.  Must access elements each pass.
	     (truncate-guess 
              y1 y2
	      (element truncate-x i)
	      (element truncate-x i-1)
	      (element truncate-x i-2))
             truncate-x low-x-digit
	     truncate-y y-size))
      (format #T "TRUNCATING ~D ~D~%" k (element q k))
      (unless (zero? k)
        (next (- k 1) i-1 i-2 (- i-2 1) (- low-x-digit 1))))
    q))


;;; BIGNUM-TRUNCATE-GUESS -- Internal.
;;;
;;; This returns a guess for the next division step.  Y1 is the highest y
;;; digit, and y2 is the second to highest y digit.  The x... variables are
;;; the three highest x digits for the next division step.
;;;
;;; From Knuth, our guess is either all ones or x-i and x-i-1 divided by y1,
;;; depending on whether x-i and y1 are the same.  We test this guess by
;;; determining whether guess*y2 is greater than the three high digits of x
;;; minus guess*y1 shifted left one digit:
;;;    ------------------------------
;;;   |    x-i    |   x-i-1  | x-i-2 |
;;;    ------------------------------
;;;    ------------------------------
;;; - | g*y1 high | g*y1 low |   0   |
;;;    ------------------------------
;;;                ...                   <   guess*y2     ???
;;; If guess*y2 is greater, then we decrement our guess by one and try again.
;;; This returns a guess that is either correct or one too large.
;;;
(define-method truncate-guess
    ((y1 <integer>) (y2 <integer>) 
     (x-i <integer>) (x-i-1 <integer>) (x-i-2 <integer>) #: <integer>)
  (format #T "TRUNCATE-GUESS Y1 ~D Y2 ~D X-I ~D X-I-1 ~D X-I-2 ~D~%"
          y1 y2 x-i x-i-1 x-i-2)
  (iterate next (((guess <integer>) 
                  (if (= x-i y1) $all-ones-digit (small-floor x-i x-i-1 y1))))
    (bind ((high-guess*y1 low-guess*y1 (split-multiply guess y1))
	   (high-guess*y2 low-guess*y2 (split-multiply guess y2))
	   (middle-digit borrow (subtract-with-borrow x-i-1 low-guess*y1 1))
	   ;; Supplying borrow of 1 means there was no borrow, and we know
	   ;; x-i-2 minus 0 requires no borrow.
	   ((high-digit <integer>)
	    (subtract-with-borrow x-i high-guess*y1 borrow)))
      (format #T "GUESSING ~D~%" guess)
      (if (and (= high-digit 0)
	       (or (> high-guess*y2 middle-digit)
		   (and (= middle-digit high-guess*y2)
			(> low-guess*y2 x-i-2))))
	  (next (subtract-with-borrow guess 1 1))
	  guess))))

;;; TRY-BIGNUM-TRUNCATE-GUESS -- Internal.
;;;
;;; This takes a digit guess, multiplies it by *truncate-y* for a result one
;;; greater in length than y-size, and subtracts this result from *truncate-x*.
;;; Low-x-digit is the first digit of x to start the subtraction, and we know x
;;; is long enough to subtract a y-size plus one length bignum from it.  Next we
;;; check the result of the subtraction, and if the high digit in x became
;;; negative, then our guess was one too big.  In this case, return one less
;;; than guess passed in, and add one value of y back into x to account for
;;; subtracting one too many.  Knuth shows that the guess is wrong on the order
;;; of 3/b, where b is the base (2 to the digit-size power) -- pretty rarely.
;;;
(define-method try-truncate-guess
    ((guess <integer>)
     (truncate-x <extended-integer>) (low-x-digit <integer>)
     (truncate-y <extended-integer>) (y-size <integer>))
  (bind (((carry-digit <integer>) 0)
	 ((borrow <integer>) 1)
	 ((i <integer>) low-x-digit))
    ;; Multiply guess and divisor, subtracting from dividend simultaneously.
    (dotimes (j y-size)
      (bind (((high-digit <integer>) (low-digit <integer>)
	      (multiply-and-add guess (element truncate-y j) carry-digit 0)))
	(set! carry-digit high-digit)
	(bind (((x <integer>) (temp-borrow <integer>)
		(subtract-with-borrow (element truncate-x i) low-digit borrow)))
	  (set! (element truncate-x i) x)
	  (set! borrow temp-borrow)))
      (set! i (+ i 1)))
    (set! (element truncate-x i)
	  (subtract-with-borrow (element truncate-x i) carry-digit borrow))
    ;; See if guess is off by one, adding one Y back in if necessary.
    (format #T "TRYING GUESS ~D " guess)
    (cond ((not (negative? (element truncate-x i)))
           (format #T "WORKED~%")
	   guess)
	  (else:
	   ;; If subtraction has negative result, add one divisor value back
	   ;; in.  The guess was one too large in magnitude.
	   (bind ((i low-x-digit)
		  (carry 0))
	     (dotimes (j y-size)
	       (bind (((v <integer>) (k <integer>)
		       (add-with-carry 
                        (element truncate-y j) (element truncate-x i) carry)))
		 (set! (element truncate-x i) v)
		 (set! carry k))
	       (set! i (+ i 1)))
	     (set! (element truncate-x i)
		   (add-with-carry (element truncate-x i) 0 carry)))
	   (format #T "PATCHING~%")
	   (subtract-with-borrow guess 1 1)))))

(define-method small-floor 
    ((numerator-hi <integer>) (numerator-lo <integer>) (denominator <integer>)
     #: (values <integer> <integer>))
  (floor/ (+ (* numerator-hi $digit-shifter) numerator-lo) denominator))

(define-method split-multiply 
    ((x <integer>) (y <integer>) #: (values <integer> <integer>))
  (bind (((result <integer>) (* x y)))
    (values (logand (ash result (- $digit-size)) $digit-mask)
            (logand result $digit-mask))))

