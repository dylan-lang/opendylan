; Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
;               All rights reserved.
; License:      Functional Objects Library Public License Version 1.0
; Dual-license: GNU Lesser General Public License
; Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method in-module (#rest junk)
  (format #t "Warning: the in-module form is obsolete.~%")
  #f)

(define-method export (#rest junk)
  (format #t "Warning: the export form is obsolete.~%")
  #f)

(define $no-value (list 'no-value))
(define $not-found (list 'not-found))
(define $local-not-found (list 'local-not-found))

(define $unbound-value (list 'unbound-value))
(define-method unbound () $unbound-value)
(define-method unbound? (x) (id? x $unbound-value))

(define-class <not-found-error> (<simple-error>))
(define-class <key-not-found-error> (<not-found-error>))
(define-class <value-not-found-error> (<not-found-error>))
(define-class <invalid-index-error> (<not-found-error>))
(define-class <non-integer-index-error> (<invalid-index-error> <type-error>))
(define-class <subscript-out-of-bounds-error> (<invalid-index-error>))

(define append concatenate)

(define cons pair)
(define car head)
(define cdr tail)

(define rest tail)

(define find-module
  (method (name)
    `(the-module ,name)))

(define-method proper? ((list <list>))
  (iterate loop ((ptr list))
    (if (instance? ptr <list>)
	(if (empty? ptr) #T (loop (cdr ptr)))
	#F)))

(define list*
  (method (val #rest vals)
    (cond
      ((empty? vals)
        val)
      (else:
        (pair val (apply list* vals))))))

(define-method difference
    ((sequence-1 <sequence>) (sequence-2 <sequence>) #key (test id?))
  (choose (method (item) (not (member? item sequence-2 test: test)))
	  sequence-1))

(define position 
  (method (seq obj #key (test id?))
    (find-key seq (method (x) (test x obj)))))

(define-method concatenate! ((s <sequence>) #rest more)
  (apply concatenate s more))

(define-method clear! ((tab <lisp-object-table>))
  (clear tab))

(define-method remove-all-keys! ((tab <lisp-object-table>))
  (clear tab))

(define-method power-of-two-ceiling ((value <integer>))
  (iterate search (((power-of-two <integer>) 1))
    (if (>= power-of-two value)
        power-of-two
      (search (+ power-of-two power-of-two)))))

(define-method find-value
    ((collection <collection>) (procedure <function>)
     #key (skip 0) (failure #F))
  (bind ((key (find-key collection procedure skip: skip failure: $not-found)))
    (if (id? key $not-found)
        failure
      (element collection key default: failure))))

(define-class <rectangular-complex> (<object>))

(import-cl-functions

  (open as: lisp/open)
  close
  (read-line as: input-line)

  ((dylan dylan-packaged-read) as: lisp/read)

)

(define force-output list)

(define fresh-line
  (method (#rest junk)
    (format #t "~&")))

(define $eof (list 'eof))

(define-method open (name #rest args #key (direction input:))
  (apply lisp/open
         name 
         direction: direction
         if-exists: supersede:
         args))

(define-method read 
    (#key (stream (standard-input)) (features) (eof-value $eof))
  (if features
    (lisp/read stream: stream features: features eof-value: eof-value)
    (lisp/read stream: stream eof-value: eof-value)))

(define-method constant? (x)
  (select x instance?
    ((<number> <string>) #t)
    (else:               #f)))

(define *dylan-suffixes* '(".dylan" ".dyl"))

(define-method existing-dylan-filename ((filename <string>))
  (bind-exit (return)
    (for-each ((suffix *dylan-suffixes*)) ()
      (bind ((full-filename (concatenate filename suffix)))
	(when (probe-file full-filename)
	  (return full-filename))))
    #F))

;; Generic streams.

(import-cl-functions
  (read-char as: lisp/read-char)
  (unread-char as: lisp/unread-char)
  (write-char as: lisp/write-char)
  (clear-input as: lisp/clear-input))

(import-cl-classes
  ((conditions simple-reader-error) as: <simple-reader-error>)
  ((conditions end-of-file)         as: <end-of-file>)
)

;; Seriously naff!!!!!!!!

(define *last-read-by-input* #f)

;(define-method input ((s <stream>))
;  (set! *last-read-by-input* (lisp/read-char s nil eof:)))

(define-method input ((s <stream>))
  (set! *last-read-by-input* (lisp/read-char s)))

(define-method undo-input ((s <stream>))
  (lisp/unread-char *last-read-by-input* s))

(define-method output ((s <stream>) (char <character>))
  (lisp/write-char char s))

(define-method clear-input ((s <stream>))
  ;; (lisp/clear-input s) screws backtracking in listeners
  s)

;; Readers and buffers

(define-class <reader-error> (<simple-error> <simple-reader-error>)
  (stream init-keyword: stream:))

(define-method reader-error ((stream <stream>) (string <string>) #rest args)
  (clear-input stream)
  (error (make <reader-error> 
               stream: stream
               format-string:  string
               format-arguments: args)))

(define-method reader-error (stream (string <string>) #rest args)
  (error (make <reader-error> 
               stream: stream
               format-string:  string
               format-arguments: args)))

;;;;;
;;;;; READER-BUFFER
;;;;;

(define-class <reader-buffer> (<sequence>)
  (data type: <deque>
        init-function: (curry make <deque>)))

(define-method output ((b <reader-buffer>) (c <character>))
  (push-last (data b) c)
  c)

(define-method print ((buffer <reader-buffer>) #key (stream #T) (verbose? #F))
  (format stream "string: ~S"
	  (as <string> (data buffer))))

(define-method empty? ((buffer <reader-buffer>))
  (empty? (data buffer)))

(define-method size ((buffer <reader-buffer>))
  (size (data buffer)))

(define-method element
    ((buffer <reader-buffer>) (index <integer>) #key (default $no-value))
  (element (data buffer) index))

(define-method = ((buffer <reader-buffer>) (string <string>))
  ;; case insensitive
  (and (= (size string) (size buffer))
       (for ((i 0 (+ i 1)))
	    ((or (>= i (size string))
		 (and (not (= (element buffer i) (element string i)))
		      (not (= (as-lowercase (element buffer i))
			      (as-lowercase (element string i))))))
	     (>= i (size string))))))

(define-method as ((class (singleton <string>)) (buffer <reader-buffer>))
  (as <string> (data buffer)))

(define-method reset ((buffer <reader-buffer>))
  (set! (size (data buffer)) 0)
  buffer)


;;;;;
;;;;; READER
;;;;;

(define-class <reader> (<object>)
  (char-kinds    init-function: 
                 (method () (make <vector> size: 128 fill: constituent:))
		 type: <simple-object-vector>)
  (char-kinds-2  init-function: 
                 (method () (make <vector> size: 128 fill: alphabetic:))
		 type: <simple-object-vector>)
  (char-actions  init-function: (method () (make <vector> size: 128))
		 type: <simple-object-vector>)
  (buffer        init-function: (method () (make <reader-buffer>))
		 type: <reader-buffer>))

(define-method print ((reader <reader>) #key (stream #T) (verbose? #F))
  (format stream "{the reader - buffer: ~S}" (buffer reader)))

(define-method char-kind ((reader <reader>) (char <character>))
  (element (char-kinds reader) (as <integer> char)))

(define-method char-kind ((reader <reader>) (value (singleton eof:))) eof:)

(define-method char-kind-setter
    ((new-value <object>) (reader <reader>) (char <character>))
  (set! (element (char-kinds reader) (as <integer> char)) new-value))

(define-method action ((reader <reader>) (char <character>))
  (element (char-actions reader) (as <integer> char)))

(define-method action-setter
    ((new-value <object>) (reader <reader>) (char <character>))
  (set! (element (char-actions reader) (as <integer> char)) new-value))

(define-method secondary-char-kind ((reader <reader>) (char <character>))
  (element (char-kinds-2 reader) (as <integer> char)))

(define-method secondary-char-kind
    ((reader <reader>) (value (singleton eof:)))
  eof:)

(define-method secondary-char-kind-setter
    ((new-value <object>) (reader <reader>) (char <character>))
  (set! (element (char-kinds-2 reader) (as <integer> char)) new-value))

(define-method flush-white-space ((reader <reader>) (stream <stream>))
  (for ((char (input stream) (input stream)))
       ((not (id? (char-kind reader char) white-space:)) char)))

(define-method initialize ((reader <reader>) #rest all-keys)
  (next-method)

  (set! (char-kind reader #\tab)      white-space:)
  (set! (char-kind reader #\linefeed) white-space:)  
  (set! (char-kind reader #\space)    white-space:)
  (set! (char-kind reader #\page)     white-space:)
  (set! (char-kind reader #\return)   white-space:)
  (set! (char-kind reader #\rubout)   white-space:)

  (set! (secondary-char-kind reader #\0) digit:)
  (set! (secondary-char-kind reader #\1) digit:)
  (set! (secondary-char-kind reader #\2) digit:)
  (set! (secondary-char-kind reader #\3) digit:)
  (set! (secondary-char-kind reader #\4) digit:)
  (set! (secondary-char-kind reader #\5) digit:)
  (set! (secondary-char-kind reader #\6) digit:)
  (set! (secondary-char-kind reader #\7) digit:)
  (set! (secondary-char-kind reader #\8) digit:)
  (set! (secondary-char-kind reader #\9) digit:)

  (set! (secondary-char-kind reader #\+) plus-sign:)
  (set! (secondary-char-kind reader #\-) minus-sign:)
  (set! (secondary-char-kind reader #\.) decimal-point:)

  (values))

;;; Characters

(define lower-case-a-code (as <integer> #\a))
(define upper-case-a-code (as <integer> #\A))
(define lower-case-z-code (as <integer> #\z))
(define upper-case-z-code (as <integer> #\Z))
(define zero-code (as <integer> #\0))
(define nine-code (as <integer> #\9))
(define space-code (as <integer> #\Space))
(define rubout-code (as <integer> #\Rubout))

(define-method standard? ((character <character>))
  #T)

(define-method graphic? ((character <character>))
  (bind (((code <integer>) (as <integer> character)))
    (and (>= code space-code) (< code rubout-code))))

(define-method alpha? ((character <character>))
  (bind (((code <integer>) (as <integer> character)))
    (or (and (>= code lower-case-a-code) (<= code lower-case-z-code))
	(and (>= code upper-case-a-code) (<= code upper-case-z-code)))))

(define-method digit? ((character <character>) #key (radix 10))
  (bind (((code <integer>) (as <integer> character)))
    (or (and (>= code zero-code) (<= code nine-code)
	     (- code zero-code))
	(and (> radix 10) (< radix 36)
	     (or (and (>= code upper-case-a-code)
		      (< (- code upper-case-a-code) (- radix 10))
		      (+ (- code upper-case-a-code) 10))
		 (and (>= code lower-case-a-code)
		      (< (- code lower-case-a-code) (- radix 10))
		      (+ (- code lower-case-a-code) 10)))))))

(define-method alphanumeric? ((character <character>))
  (or (alpha? character) (digit? character)))

(define-method lowercase? ((character <character>))
  (bind (((code <integer>) (as <integer> character)))
    (not (and (>= code upper-case-a-code) (<= code upper-case-z-code)))))

(define-method uppercase? ((character <character>))
  (bind (((code <integer>) (as <integer> character)))
    (not (and (>= code lower-case-a-code) (<= code lower-case-z-code)))))

(define-method both-case? ((character <character>))
  (not (alpha? character)))

(define-method whitespace? ((character <character>))
  (member? character #(#\space #\newline #\tab)))

;;

(define-method debug-name (x)
  #f)

(define-method debug-name ((x <class>))
  (class-debug-name x))

(define-method debug-name ((x <function>))
  (function-debug-name x))

(define-method debug-name-setter (x y) x)

(define old-nil '())

;; eof
