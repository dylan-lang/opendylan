Module:    infix-reader
Language:  prefix-dylan
Synopsis:  Approximate hand-written tokenizer for infix Dylan
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Reader stuff shared with Zimmerman.

(define-method token-values (type val)
  (values type val (format #f "~a" val)))

(define-method as-default-case-symbol ((s <string>))
  (as <symbol> (as-uppercase s)))

(define-method as-default-case-keyword ((s <string>))
  (as-keyword (as-uppercase s)))

;; Tokenizers:

(define-method tokenize-integer ((string <string>))
  (token-values '<literal> 
                (tokenize-integer-aux string)))

(define-method tokenize-integer-aux ((string <string>))
  (bind ((positive? base digit-start
           (case (first string)
	     ((#\+) (values #t 10 1))
	     ((#\-) (values #f 10 1))
	     ((#\#)
	      (values #t
		      (case (second string)
			((#\b #\B) 2)
			((#\o #\O) 8)
			((#\x #\X) 16))
		      2))
             (else:
	       (values #t 10 0))))
         (negative? (not positive?))
	 (number 0)
	 (position digit-start))
      (set! position
	    (bind-methods
		((loop ((position <integer>))
		   (if (>= position (size string))
		       position
		       (bind (((character <character>)
			        (element string position))
			      ((digit <object>) 
                                (digit? character radix: base)))
			 (if digit
			     (begin
			      (set! number (+ digit (* number base)))
			      (loop (+ position 1)))
			     position)))))
	      (loop position)))
      (unless (= position (size string))
	(error "Invalid integer ~A" string))
      (if negative? (- number) number)))

(define-variable *read-default-float-format* <single-float>)

(define-method tokenize-float ((string <sequence>))
  (bind ((negative? (= (element string 0) #\-))
	 (positive? (= (element string 0) #\+))
	 ((number <integer>) 0)
	 ((exponent <integer>) 0)
	 ((divisor <integer>) 1)
	 ((position <integer>) (if (or negative? positive?) 1 0)))
    (set! position
	  (iterate grovel (((position <integer>) position))
	    (if (>= position (size string))
		position
		(bind (((character <character>)
			(element string position))
		       ((digit <object>) (digit? character)))
		  (if digit
		      (begin
		       (set! number (+ digit (* number 10)))
		       (grovel (+ position 1)))
		      position)))))
    (when (= (element string position) #\.)
      (set! position
	    (iterate grovel (((position <integer>) (+ position 1)))
	      (if (>= position (size string))
		  position
		  (bind (((character <character>)
			  (element string position))
			 ((digit <object>) (digit? character)))
		    (if digit
			(begin
			 (set! number (+ digit (* number 10)))
			 (set! divisor (* divisor 10))
			 (grovel (+ position 1)))
			position))))))
    (cond ((>= position (size string))
	   (when (/= position (size string))
	     (error "Invalid float ~A" string))
	   (make-float-aux
	    negative? number divisor exponent *read-default-float-format*))
	  ((member? (as-uppercase (element string position)) '(#\E #\F #\D #\L))
	   (bind ((class (case (as-uppercase (element string position))
			  ((#\E) *read-default-float-format*)
			  ((#\F) <single-float>)
			  ((#\D) <double-float>)
			  ((#\L) <extended-float>))))
	     (set! position (+ position 1))
	     (bind ((negative-exponent? (= (element string position) #\-))
		    (positive-exponent? (= (element string position) #\+)))
	       (when (or negative-exponent? positive-exponent?)
		 (set! position (+ position 1)))
	       (set! position
		     (iterate grovel (((position <integer>) position))
		       (if (>= position (size string))
			   position
			   (bind (((character <character>)
				   (element string position))
				  ((digit <object>) (digit? character)))
			     (if digit
				 (begin
				  (set! exponent (+ digit (* exponent 10)))
				  (grovel (+ position 1)))
				 position)))))
	       (when (/= position (size string))
		 (error "Invalid float ~A" string))
	       (when negative-exponent?
		 (set! exponent (- exponent)))
	       (make-float-aux negative? number divisor exponent class))))
	  (else: (error "Invalid float ~A" string)))))

(define-method make-float-aux
    ((negative? <object>) (number <integer>) (divisor <integer>)
     (exponent <integer>) class)
  (bind ((num (as <single-float> (/ number divisor))))
    (unless (= exponent 0) (set! num (* num (expt 10 exponent))))
    (when negative? (set! num (- num)))
    (token-values '<literal> (as class num))))

(define-method read-infix-binary-number (reader stream)
  (read-infix-constituents reader stream)
  (token-values '<literal>
                (tokenize-integer-aux (as <string> (buffer reader)))))

(define-method read-infix-octal-number (reader stream)
  (read-infix-constituents reader stream)
  (token-values '<literal>
                (tokenize-integer-aux (as <string> (buffer reader)))))

(define-method read-infix-hexadecimal-number (reader stream)
  (read-infix-constituents reader stream)
  (token-values '<literal>
                (tokenize-integer-aux (as <string> (buffer reader)))))

(define-method tokenize-operator ((string <string>))
  (token-values '<binary-operator> (as-default-case-symbol string)))

(define-method tokenize-reserved ((string <string>))
  (bind ((key (as-default-case-keyword string)))
    (token-values key string)))

(define-method tokenize-symbol ((string <string>))
  (if (id? (last string) #\:)
    (token-values '<keyword> 
		  (as-default-case-keyword 
		   (copy-sequence string
				  end: (- (size string)
					  1))))
    (bind ((symbol (as-default-case-symbol string))
	   (reserved (as-reserved symbol)))
      (if reserved
        (token-values reserved string)
	(bind ((macro-type (as-infix-macro symbol)))
	  (if macro-type
            (token-values macro-type symbol)
	    (token-values '<symbol> symbol)))))))

(define $reserved-table
  (bind ((tab (make <table>)))
    (for-each ((name '(

                       define end 
                       let local 
                       method generic macro
                       handler
                       otherwise

                       ))) ()
      (set! (element tab name) #t))
    tab))

(define-method as-reserved ((s <symbol>))
  (bind ((well? (element $reserved-table s default: #f)))
    (if well? (as-keyword s) #f)))

(define $infix-macro-table (make <table>))

;; Word classes.

(define-method register-infix-macro-word (name class)
  (set! (element $infix-macro-table name) class))

(define-method register-details-begin-word (name)
  (set! (element $infix-macro-table name) '<details-begin-word>))

(define-method register-expr-begin-word (name)
  (set! (element $infix-macro-table name) '<expr-begin-word>))

(define-method register-simple-begin-word (name)
  (set! (element $infix-macro-table name) '<simple-begin-word>))

(define-method register-details-intermediate-word (name)
  (set! (element $infix-macro-table name) '<details-intermediate-word>))

(define-method register-expr-intermediate-word (name)
  (set! (element $infix-macro-table name) '<expr-intermediate-word>))

(define-method register-simple-intermediate-word (name)
  (set! (element $infix-macro-table name) '<simple-intermediate-word>))

;; Macro word classes.

(define-method register-defining-macro (name)
  (set! (element $infix-macro-table name) '<defining-macro-name>))

(define-method register-statement-0-macro (name)
  (set! (element $infix-macro-table name) '<statement-0-macro-name>))

(define-method register-statement-1-macro (name)
  (set! (element $infix-macro-table name) '<statement-1-macro-name>))

(define-method register-statement-2-macro (name)
  (set! (element $infix-macro-table name) '<statement-2-macro-name>))

(define-method as-infix-macro ((s <symbol>))
  (element $infix-macro-table s default: #f))

;; Standard words.

(for ((word in '(begin case))) 
  (register-simple-begin-word word))

(for ((word in '(if unless until while select)))
  (register-expr-begin-word word))

(for ((word in '(block for)))
  (register-details-begin-word word))

(for ((word in '(cleanup else finally)))
  (register-simple-intermediate-word word))

(for ((word in '(elseif)))
  (register-expr-intermediate-word word))

(for ((word in '(exception)))
  (register-details-intermediate-word word))

(for ((word in '(constant variable class module library)))
  (register-infix-macro-word word '<defining-word>))
  
;; Utils:

(define-method read-infix-constituents ((reader <reader>) (stream <stream>))
  (for ((char (input stream) (input stream)))
       ((not (id? (char-kind reader char) constituent:))
	(undo-input stream))
    (output (buffer reader) (as-lowercase char)))
  (values))

;; Literals:

(define-method read-infix-string ((reader <reader>) (stream <stream>))
  (reset (buffer reader))
  (input stream) ;; lose opening double quote
  (for ((char (input stream) (input stream)))
       ((or (= char #\") (id? char eof:))
	(when (id? char eof:)
	  (reader-error stream "String terminated with eof")))
    (when (id? char #\\)
      (set! char (read-infix-escape-character reader stream))
      (when (id? char eof:)
	(reader-error stream "String terminated with eof")))
    (output (buffer reader) char))
  (token-values '<string> (as <string> (buffer reader))))

(define-method read-infix-character ((reader <reader>) (stream <stream>))
  (reset (buffer reader))
  (input stream) ;; lose opening quote
  (bind ((char (input stream)))
    (when (id? char eof:)
      (reader-error stream "Character terminated with eof"))
    (when (id? char #\\)
      (set! char (read-infix-escape-character reader stream)))
    (unless (id? (input stream) #\')
      (reader-error stream "No terminator for character ~s" char))
    (token-values '<literal> char)))

(define-method read-infix-escape-character (reader stream)
  (bind ((next (input stream)))
    (case next
      ((#\a) #\Bell)
      ((#\b) #\Backspace)
      ;; ((#\e) ??)
      ((#\f) #\Formfeed)
      ((#\n) #\Newline)
      ((#\r) #\Return)
      ((#\t) #\Tab)
      ((#\0) #\Null)
      (else: next))))

;; Comments:

(define-method read-infix-line-comment ((reader <reader>) (stream <stream>))
  (for ((char (input stream) (input stream)))
       ((or (id? char eof:) (= char #\newline))
	 (values))))

(define-method read-infix-area-comment ((reader <reader>) (stream <stream>))
  (bind-exit (return)
    (for ((char (input stream) (input stream)))
          ((id? char eof:)
	     (reader-error stream "Area comment ended with eof"))
        (case char
          ((#\*)
            (if (= (input stream) #\/) (return)
              (undo-input stream)))
          ((#\/)
            (bind ((next (input stream)))
              (case next
                ((#\/)
                  (read-infix-line-comment reader stream))
                ((#\*)
                  (read-infix-area-comment reader stream))
                (else:
                  (undo-input stream)))))))))

;; ...

;; Punctuation:

;; Obvious, single character punctuation

(define-method read-infix-punctuation ((reader <reader>) (stream <stream>))
  (case (input stream)
    ((#\,)
      (token-values '<var-sep> ","))
    ((#\;)
      (token-values '<statement-sep> ";"))
    ((#\() 
      (token-values '<lbracket> "("))
    ((#\)) 
      (token-values '<rbracket> ")"))
    ((#\[)
      (token-values '<lsbracket> "["))
    ((#\])
      (token-values '<rsbracket> "]"))
    ((#\{)
      (token-values '<lcbracket> "{"))
    ((#\})
      (token-values '<rcbracket> "}"))
    ((#\.)
      (token-values '<dot> "."))
    ((#\~)
      (token-values '<not> "~"))
    ((#\-)
      (token-values '<minus> "-"))))

;; Symbols:

(define-method as-punctuation ((symbol <symbol>))
  (case symbol
    ((\:=)  '<becomes>)
    ((=)    '<binds>)
    ((\:\:) '<var-type-sep>)
    ((==)   '<var-singleton-sep>)
    ((=>)   '<implies>)
    ((\.)   '<dot>)

    ((\~)   '<not>)
    ((\-)   '<minus>)
    (else:  #f)))

(define-method number-constituent? ((reader <reader>) (char <character>))
  (case (secondary-char-kind reader char)
    ((digit: decimal-point: slash:) #t)
    (else:                          #f)))

(define-method operator-constituent? ((reader <reader>) (char <character>))
  (case (secondary-char-kind reader char)
    ((operator-constituent:) #t)
    (else:                   #f)))

(define-method classify-infix-atomic-token ((reader <reader>) 
                                            (stream <stream>)
                                            (string <string>))
  (bind ((symbol (as <symbol> string)))
    (bind ((punc (as-punctuation symbol)))
      (if punc 
        (values punc symbol)
	(bind ((number? #t)
	       (operator? #t)
	       (sz (size string))
	       (count sz))
	  (for-each ((char string)) 
	      ((and (not number?) (not operator?)))
	    (when (and number? 
		       (not (or (number-constituent? reader char)
				(and (= count sz)
				     (member? char '(#\+ #\-))
				     (> sz 1)))))
	      (set! number? #f))
	    (when (and operator? 
		       (not (operator-constituent? reader char)))
	      (set! operator? #f))
	    (set! count (- count 1)))
	  (cond
	    (number?
              (bind ((next (input stream)))
                (case next
                  ((#\.)
                    (reset (buffer reader))
		    (read-infix-constituents reader stream)
                    (tokenize-float
                      (concatenate string "." (as <string> (buffer reader)))))
                  (else:
                    (undo-input stream)
	            (tokenize-integer string)))))
	    (operator? 
	      (tokenize-operator string))
	    (else: 
	      (tokenize-symbol string))))))))

(define-method read-infix-symbol-or-operator ((reader <reader>)
                                              (stream <stream>))
  (reset (buffer reader))
  (read-infix-constituents reader stream)
  (bind ((symbol (as-default-case-symbol (as <string> (buffer reader)))))
    (unless (= (input stream) #\")
      (reader-error stream "badly terminated symbolic constant ~s" symbol))
    (token-values '<literal> symbol)))

(define-method read-infix-escaped-symbol ((reader <reader>)
				          (stream <stream>))
  (reset (buffer reader))
  (input stream)
  (read-infix-constituents reader stream)
  (bind ((symbol (as-default-case-symbol (as <string> (buffer reader)))))
    (token-values '<symbol> symbol)))

(define-method read-infix-token ((reader <reader>) (stream <stream>))
  (reset (buffer reader))
  (bind ((char (input stream)))
    (case char 
      ((#\/)
        (bind ((char (input stream)))
          (case char
            ((#\/)
              (read-infix-line-comment reader stream))
            ((#\*)
              (read-infix-area-comment reader stream))
            (else:
              (undo-input stream)
              (read-infix-constituents reader stream)
              (classify-infix-atomic-token
	        reader
                stream
                (concatenate-as 
                  <string> (list #\/) (as <string> (buffer reader))))))))
      ((#\~)
        (bind ((char (input stream)))
          (if (operator-constituent? reader char)
            (begin
              (undo-input stream)
              (read-infix-constituents reader stream)
              (classify-infix-atomic-token
	        reader
                stream
	        (concatenate-as 
	          <string> (list #\~) (as <string> (buffer reader)))))
            (begin
              (undo-input stream)
              (token-values '<not> "~")))))
      ((#\-)
        (bind ((char (input stream)))
          (if (digit? char base: 10)
            (begin
              (undo-input stream)
              (read-infix-constituents reader stream)
              (classify-infix-atomic-token
	        reader
                stream
	        (concatenate-as 
	          <string> (list #\-) (as <string> (buffer reader)))))
            (begin
              (undo-input stream)
              (token-values '<minus> "-")))))
      ((#\?)
        (bind ((char (input stream)))
          (cond
            ((id? char #\?)
              (token-values '<query-query> "??"))
            ((id? char #\=)
              (token-values '<query-equal> "?="))
            (else:
              (undo-input stream)
              (token-values '<query> "?")))))
      ((#\.)
        (bind ((char (input stream)))
          (if (id? char #\.)
            (begin
              (input stream)
              (token-values '<ellipsis> "..."))
            (begin
              (undo-input stream)
              (token-values '<dot> ".")))))
      (else:
        (undo-input stream)
        (read-infix-constituents reader stream)
        (classify-infix-atomic-token 
           reader
           stream
           (as <string> (buffer reader)))))))

(define-method read-infix-extended-token ((reader <reader>) (stream <stream>))
  (reset (buffer reader))
  (output (buffer reader) (as-lowercase (input stream)))
  (bind ((char (as-lowercase (input stream))))
    (when (id? char eof:)
      (reader-error stream "READ-EXTENDED-TOKEN ended in eof"))
    (output (buffer reader) char)
    (case char
      ((#\n) 
        (read-remaining-infix-extended-token reader stream "#next")
        (token-values &next: "#next"))
      ((#\r) 
        (read-remaining-infix-extended-token reader stream "#rest") 
        (token-values &rest: "#rest"))
      ((#\k) 
        (read-remaining-infix-extended-token reader stream "#key")
        (token-values &key: "#key"))
      ((#\a) 
        (read-remaining-infix-extended-token reader stream "#all-keys")
        (token-values &all-keys: "#all-keys"))
      ((#\t) 
        (read-remaining-infix-extended-token reader stream "#t")
        (token-values '<literal> #t))
      ((#\f) 
        (read-remaining-infix-extended-token reader stream "#f")
        (token-values '<literal> #f))
      ((#\b) 
        (read-infix-binary-number reader stream))
      ((#\o) 
        (read-infix-octal-number reader stream))
      ((#\x) 
        (read-infix-hexadecimal-number reader stream))
      ((#\")
        (undo-input stream)
        (bind ((class value (read-infix-string reader stream)))
          (token-values '<literal> 
                        (as-default-case-symbol value))))
      ((#\()
        (token-values '<list-open> "#("))
      ((#\[)
        (token-values '<vector-open> "#["))
      ((#\{)
        (token-values '<syntax-open> "#{"))
      ((#\#)
        (token-values '<splice> "##"))
      (else: 
        (reader-error stream "Unrecognized extended token")))))

(define-method read-remaining-infix-extended-token
    ((reader <reader>) (stream <stream>) (token-string <string>))
  (read-infix-constituents reader stream)
  (unless (= (buffer reader) token-string)
    (reader-error stream "Unrecognized extended token ~A"
		  (as <string> (buffer reader))))
  (values))

(define-class <infix-reader> (<reader>))

(define-method initialize ((reader <infix-reader>) #rest all-keys)
  (next-method)
  (fill! (char-actions reader) read-infix-token)

  (do (method (c)
        (set! (secondary-char-kind reader c) operator-constituent:))
      "!&*+/<=>?|^$%@-~:")

  (set! (secondary-char-kind reader #\.) decimal-point:)

  (set! (char-kind reader #\\) action:)
  (set! (char-kind reader #\") action:)
  (set! (char-kind reader #\') action:)
  (set! (char-kind reader #\`) action:)
  (set! (char-kind reader #\,) action:)
  (set! (char-kind reader #\() action:)
  (set! (char-kind reader #\)) action:)
  (set! (char-kind reader #\[) action:)
  (set! (char-kind reader #\]) action:)
  (set! (char-kind reader #\{) action:)
  (set! (char-kind reader #\}) action:)
  (set! (char-kind reader #\;) action:)
  (set! (char-kind reader #\#) action:)
  (set! (char-kind reader #\\) action:)
  (set! (char-kind reader #\.) dot:)

  (set! (action reader #\,) read-infix-punctuation)
  (set! (action reader #\;) read-infix-punctuation)
  (set! (action reader #\() read-infix-punctuation)
  (set! (action reader #\)) read-infix-punctuation)
  (set! (action reader #\[) read-infix-punctuation)
  (set! (action reader #\]) read-infix-punctuation)
  (set! (action reader #\{) read-infix-punctuation)
  (set! (action reader #\}) read-infix-punctuation)

  (set! (action reader #\") read-infix-string)
  (set! (action reader #\') read-infix-character)
  (set! (action reader #\\) read-infix-escaped-symbol)
  
  (set! (action reader #\#) read-infix-extended-token)

  (values))

(define *infix-reader* (make <infix-reader>))

(define-method tokenize-infix ((stream <stream>))
  (for ((char (input stream) (input stream)))
        ((or (id? char eof:) (not (id? (char-kind *infix-reader* char) 
				       white-space:)))
	 (if (id? char eof:)
	   eof:
	   (begin
             (undo-input stream)
	     (bind ((#rest results
                      ((action *infix-reader* char)
		         *infix-reader*
			 stream)))
	       (if (empty? results)
                 (tokenize-infix stream)
	         (apply values results))))))))

;; eof
