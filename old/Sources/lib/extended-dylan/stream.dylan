module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;; EXTERNAL PROTOCOL SUPPORTED BY ALL STREAMS

(define-generic-function close (stream #key abort))

(define-method close ((stream <stream>) #key abort)
  (set! (state stream) 'closed))

;;; Must be implemented by the element type streams.

(define-generic-function element-type (stream))

;;;; INTERNAL PROTOCOL SUPPORTED BY ALL STREAMS

;;; Prevents access to a stream if it is not open

(define-method ensure-open ((stream <stream>))
  (unless (id? (state stream) 'open)
    (error "Attempt to use stream ~A which is ~A" stream (state stream))))

;;; bytes-per-element must be implemented by element type streams
;;; returns length of on element, in 8-bit bytes

(define-generic-function bytes-per-element (stream))

;;; storage-unit-size must be implemented by device streams

(define-generic-function storage-unit-size (stream))

;;; THE CLASS INPUT-STREAM AND ITS METHODS

(define-class <input-stream> (<stream>))

;;;; EXTERNAL PROTOCOL SUPPORTED BY ALL STREAMS

;;; INPUT reads one character from stream.  It returns either an object,
;;; or the keyword 'eof if the stream is at end-of-file.  Every subclass of
;;; <input-stream> must define a method for this function.

(define-generic-function input (input-stream))

;;; This default method on stream is overridden by input streams.
;;; It is defined simply to give a comprehensible error message when this
;;; situation occurs, and to make it unnecessary for all external functions
;;; to check the stream argument type.

(define-method input ((stream <input-stream>))
  (error "Cannot get input from stream ~A of type ~A."
	 stream (object-class stream)))

;;; UNDO-INPUT undoes the last call to input.  Returns input-stream.
;;; Every subclass of <input-stream> must define a method for this function.
;;; Only one element may be returned to the builtin stream classes.

(define-generic-function undo-input (input-stream))

;;; INPUT-NO-HANG returns either the next element or #F if no input is currently
;;; available, or 'eof if end-of-file is reached.  The default method provided
;;; by <input-stream> simply calls input; this is sufficient for file streams,
;;; but interactive streams should define their own method.

(define-generic-function input-no-hang (input-stream))

(define-method input-no-hang ((stream <input-stream>))
  (input stream))

;;; PEEK returns either the next element or 'eof.
;;; The default method calls input and undo-input.

(define-generic-function peek (input-stream))

(define-method peek ((stream <input-stream>))
  (bind ((next (input stream)))
    (unless (id? next eof:)
      (undo-input stream))
    next))

;;; CLEAR-INPUT clears any buffered input associated with input-stream.  It is
;;; primarily useful for clearing type-ahead from keyboards when some kind of
;;; asynchronous error has occurred.  If this operation doesn't make sense for
;;; the stream involved, then clear-input does nother.  It returns the
;;; input-stream.

(define-generic-function clear-input (input-stream))

(define-method clear-input ((stream <input-stream>)) stream)
  
;;; LISTEN returns #T or #F depending on whether there is an object available.
;;; The default method uses input-no-hang and undo-input.  Most streams should
;;; define their own method since it will usually be trivial and will always be
;;; more efficient than the default method.

(define-generic-function listen (input-stream))

(define-method listen ((stream <input-stream>))
  (bind ((next (input-no-hang stream)))
    (if (or (not next) (id? next eof:))
	#F
	(begin (undo-input stream) #T))))

;;;;
;;;; THE CLASS OUTPUT-STREAM AND ITS METHODS
;;;;

;;; This basic class must be included in all output streams.

(define-class <output-stream> (<stream>))

;;;; EXTERNAL PROTOCOL SUPPORTED BY ALL OUTPUT STREAMS

;;; OUTPUT writes an object to stream and returns object.
;;; Every subclass of <output-stream> must have a method defined for this
;;; function.

(define-generic-function output (output-stream element))

;;; This default method on stream is overridden by output streams.
;;; It is defined simply to give a comprehensible error message when this
;;; situation occurs, and to make it unnecessary for all external functions
;;; to check the stream argument type.

(define-method output ((stream <output-stream>) (element <object>))
  (error "Cannot do output to stream ~A of type ~A."
	 stream (object-class stream)))

;;; OUTPUT-SEQUENCE writes vector of objects to stream, optionally delimited by
;;; start and end, which default to 0 and (size sequence).  The output-stream
;;; argument is returned.  The default method provided by <output-stream> uses
;;; repeated calls to output.

(define-generic-function output-sequence
    (output-stream sequence #key start end))

(define-method output-sequence
    ((stream <output-stream>) (sequence <sequence>)
     #key (start 0) (end (size sequence)))
  (for (((index <integer>) from start below end))
     (output stream (element sequence index)))
  stream)

;;; FINISH-OUTPUT attempts to ensure that all output sent to output-stream has
;;; reached its destination and only then returns the output-stream.

(define-generic-function finish-output (output-stream))

;;; FORCE-OUTPUT initiates the emptying of any internal buffers but returns the
;;; output-stream without waiting for completion or acknowledgement.

(define-generic-function force-output (output-stream))

(define-method force-output ((stream <output-stream>))
  (finish-output stream))

;;; CLEAR-OUTPUT attempts to abort any outstanding output operation in progress
;;; in order to allow as little output as possible to continue to the
;;; destination.  This is useful, for example when an asynchronous error occurs.
;;; clear-output returns output-stream.

(define-generic-function clear-output (output-stream))

;;; THE CLASS BIDIRECTIONAL-STREAM

;;; This class inherits all needed methods, and supplies none of its own.

(define-class <bidirectional-stream> (<input-stream> <output-stream>))

;;;;;
;;;;; DEVICE STREAMS
;;;;;

;;;;;
;;;;; <TERMINAL-STREAM> -- USED ONLY FOR <CHARACTER>'s
;;;;;

(define-class <terminal-stream> (<stream>))

;;;;
;;;; THE CLASS <TERMINAL-INPUT-STREAM>
;;;;

(define-class <terminal-input-stream> (<terminal-stream>)
  (input-terminal-path-name init-keyword: input-terminal-path-name:)
  (input-file-handle        init-value: #F)
  (input-element-number     init-value: 0 type: <integer>)
  (input-element-buffer     init-function: (method () (make <stretchy-vector>))
			    type: <stretchy-vector>))

;;; PATH-NAMES NOT IMPLEMENTED YET
(define-method name-string (path-name) path-name)

(define-method initialize
    ((stream <terminal-input-stream>) #key input-terminal-path-name)
  (next-method)
  ;; !@#$ hack
  ;; (set! (input-file-handle stream) ...)
  (set! (%slot-value stream ^2) 
	(primitive-open-input-terminal 
	 (if (= input-terminal-path-name "input")
	     ;; !@#$ data incorrect (data (input-terminal-path-name stream))
	     ^"input"
	     (%string-as-native-string input-terminal-path-name))))
  ;; Store #\Newline in first position of buffer for sentinel for undo-input
  (set! (input-element-buffer stream)
	(add! (input-element-buffer stream) #\Newline))
  (set! (input-element-number stream) 1)
  stream)

(define-method print
    ((object <terminal-input-stream>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "NAME: ~S" (input-terminal-path-name object))))

(define-method input ((stream <terminal-input-stream>))
  (when (= (input-element-number stream) (size (input-element-buffer stream)))
    (clear-input stream)
    (iterate input-line-into 
             (((stream <terminal-input-stream>) stream) 
              ((vector <stretchy-vector>) (input-element-buffer stream)))
      (bind ((raw-character
	      (primitive-terminal-input (input-file-handle stream)))
	     (character
	      (if (primitive-true?
		   (primitive-terminal-eof? (input-file-handle stream)))
		  eof:
		  (as <character>
		      (%integer
		       (primitive-character-as-integer raw-character))))))
	(set! vector (add! vector character))
	(if (or (id? character #\newline) (id? character eof:))
	    stream
	    (input-line-into stream vector))))
    (set! (input-element-number stream) 1))
  (bind ((character
	  (element (input-element-buffer stream)
		   (input-element-number stream))))
    (set! (input-element-number stream) (+ (input-element-number stream) 1))
    character))

(define-method input-no-hang ((stream <terminal-input-stream>))
  ;; !@#$ NOT IMPLEMENTED YET
  ;; !@#$ COMPLICATED INTERACTION WITH BUFFERING
  (input stream))

(define-method undo-input ((stream <terminal-input-stream>))
  (unless (zero? (input-element-number stream))
    (set! (input-element-number stream) (- (input-element-number stream) 1)))
  stream)

(define-method clear-input ((stream <terminal-input-stream>))
  (set! (input-element-number stream) 1)
  (set! (size (input-element-buffer stream)) 1)
  stream)

(define-method close ((stream <terminal-input-stream>) #key abort)
  (unless (id? stream *standard-input*)
    (next-method)
    (primitive-close-terminal (input-file-handle stream)))
  (values))

;;;;
;;;; THE CLASS <TERMINAL-OUTPUT-STREAM>
;;;;

(define-class <terminal-output-stream> (<terminal-stream>)
  (output-element-buffer init-function: (method () (make <stretchy-vector>))
			 type: <stretchy-vector>)
  (column-width init-keyword: column-width: init-value: 80 type: <integer>)
  ;; !@#$ MAYBE LINE-COLUMN SHOULD BE A <TERMINAL-STREAM> SLOT
  (line-column init-value: 0 type: <integer>)
  ;; line-column of first element in buffer
  (saved-line-column init-value: 0 type: <integer>) 
  (output-terminal-path-name init-keyword: output-terminal-path-name:)
  (output-file-handle init-value: #F))

(define-method initialize
    ((stream <terminal-output-stream>) #key output-terminal-path-name)
  (next-method)
  ;;; !@#$ hack
  ;; (set! (output-file-handle stream) ...)
  (set! (%slot-value stream ^6)
	(primitive-open-output-terminal 
	 (if (= output-terminal-path-name "output")
	     ;; !@#$ data incorrect (data (output-terminal-path-name stream))
	     ^"output"
	     (%string-as-native-string output-terminal-path-name))))
  stream)

;; !@#$ used for pseudo system

#-compiler:
(define-lambda print-with-raw-stream (stream object)
  (set! (output-file-handle *standard-output*) stream)
  (print object stream: *standard-output* verbose?: #T)
  (force-output *standard-output*)
  (values))

(define-method print
    ((object <terminal-output-stream>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "NAME: ~S" (output-terminal-path-name object))))

(define-method output
    ((stream <terminal-output-stream>) (character <character>))
  (ensure-open stream)
  (set! (output-element-buffer stream)
	(add! (output-element-buffer stream) character))
  (case character
    ((#\Newline #\Page)
     (force-output stream)
     (set! (line-column stream) 0))
    ((#\Tab)
     (set! (line-column stream) (modulo (+ (line-column stream) 8) 8)))
    ((#\Backspace)
     (set! (line-column stream) (- (line-column stream) 1)))
    (else:
     (set! (line-column stream) (+ (line-column stream) 1))))
  stream)
     
(define-method finish-output ((stream <terminal-output-stream>))
  (ensure-open stream)
  (for (((character <character>) in (output-element-buffer stream)))
    (primitive-terminal-output 
     (primitive-integer-as-character
      (%integer-data (%character-code character)))
     (output-file-handle stream)))
  (set! (saved-line-column stream) (line-column stream))
  (set! (size (output-element-buffer stream)) 0)
  (primitive-force-output (output-file-handle stream))
  stream)

(define-method clear-output ((stream <terminal-output-stream>))
  (set! (line-column stream) (saved-line-column stream))
  (set! (size (output-element-buffer stream)) 0)
  stream)

(define-method close ((stream <terminal-output-stream>) #key abort)
  (unless abort (finish-output stream))
  (unless (id? stream *standard-output*)
    (next-method)
    (primitive-close-terminal (output-file-handle stream)))
  (values))

;;;;
;;;; THE CLASS <TERMINAL-BIDIRECTIONAL-STREAM>
;;;;

(define-class <terminal-bidirectional-stream>
    (<terminal-input-stream> <terminal-output-stream> <bidirectional-stream>))

(define-method print
    ((object <terminal-bidirectional-stream>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "INPUT-NAME: ~S OUTPUT-NAME: ~S"
	      (input-terminal-path-name object)
	      (output-terminal-path-name object)
	      )))

;;;;;
;;;;; <CONCATENATED-STREAM>
;;;;;

(define-class <concatenated-stream> (<stream>)
  (streams required-init-keyword: streams: type: <list>)
  (cursor init-value: #F type: <list>))

(define-method initialize ((stream <concatenated-stream>) #key streams)
  (next-method)
  (do ensure-open streams)
  (set! (state stream) 'open)
  (set! (cursor stream) (initial-state streams))
  stream)

(define-method print
    ((object <concatenated-stream>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "STREAMS: ~S" (streams object))))

(define-method close ((stream <concatenated-stream>) #key abort)
  (do (method (stream) (close stream abort: abort)) (streams stream))
  (next-method)
  (values))

(define-method input ((stream <concatenated-stream>))
  (cond ((cursor stream)
	 (bind ((value
		 (input (current-element (streams stream) (cursor stream)))))
	   (cond ((id? value eof:)
		  (bind ((next-state
			  (next-state (streams stream) (cursor stream))))
		    (cond (next-state
			   (set! (cursor stream) next-state)
			   (input stream))
			  (else: eof:))))
		 (else: value))))
	(else: eof:)))

(define-method input-no-hang ((stream <concatenated-stream>))
  (input (current-element (cursor stream))))

(define-method undo-input ((stream <concatenated-stream>))
  (when (cursor stream)
    (undo-input (current-element (cursor stream))))
  stream)

(define-method peek ((stream <concatenated-stream>))
  (if (cursor stream)
      (peek (current-element (cursor stream)))
      eof:))

(define-method listen ((stream <concatenated-stream>))
  (if (cursor stream)
      (listen (current-element (cursor stream)))
      eof:))

(define-method clear-input ((stream <concatenated-stream>))
  (when (cursor stream)
    (clear-input (current-element (cursor stream))))
  stream)

;;;;;
;;;;; <BROADCAST-STREAM> 
;;;;;

(define-class <broadcast-stream> (<stream>)
  (streams init-keyword: streams: type: <list>))

(define-method initialize ((stream <broadcast-stream>) #key streams)
  (next-method)
  (do ensure-open streams)
  (set! (state stream) 'open)
  stream)

(define-method print
    ((object <broadcast-stream>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "STREAMS: ~S" (streams object))))

(define-method close ((stream <broadcast-stream>) #key abort)
  (do (method (stream) (close stream abort: abort)) (streams stream))
  (next-method)
  (values))

(define-method output ((stream <broadcast-stream>) (element <object>))
  (do (method (stream) (output stream element)) (streams stream))
  stream)
     
(define-method output-sequence
    ((stream <broadcast-stream>) (sequence <sequence>)
     #key (start 0) (end (size sequence)))
  (do (method (stream) (output-sequence stream sequence start: start end: end))
      (streams stream))
  stream)
     
(define-method finish-output ((stream <broadcast-stream>))
  (do finish-output (streams stream))
  stream)

(define-method force-output ((stream <broadcast-stream>))
  (do force-output (streams stream))
  stream)

(define-method clear-output ((stream <broadcast-stream>))
  (do clear-output (streams stream))
  stream)

;;;;;
;;;;; <TWO-WAY-STREAM>
;;;;;

(define-class <two-way-stream> (<stream>)
  (input-stream required-init-keyword: input-stream: type: <stream>)
  (output-stream required-init-keyword: output-stream: type: <stream>))

(define-method initialize
    ((stream <two-way-stream>) #key input-stream output-stream)
  (next-method)
  (when input-stream
    (ensure-open input-stream))
  (when output-stream
    (ensure-open output-stream))
  (set! (state stream) 'open)
  stream)

(define-method print
    ((object <two-way-stream>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "INPUT-STREAM: ~S OUTPUT-STREAM: ~S"
	      (input-stream object) (output-stream object))))

(define-method close ((stream <two-way-stream>) #key abort)
  (close (input-stream stream))
  (close (output-stream stream))
  (next-method)
  (values))

(define-method input ((stream <two-way-stream>))
  (input (input-stream stream)))

(define-method input-no-hang ((stream <two-way-stream>))
  (input-no-hang (input-stream stream)))

(define-method undo-input ((stream <two-way-stream>))
  (undo-input (input-stream stream)))

(define-method peek ((stream <two-way-stream>))
  (peek (input-stream stream)))

(define-method listen ((stream <two-way-stream>))
  (listen (input-stream stream)))

(define-method clear-input ((stream <two-way-stream>))
  (clear-input (input-stream stream)))

(define-method output ((stream <two-way-stream>) (element <object>))
  (output (output-stream stream) element)
  stream)
     
(define-method output-sequence
    ((stream <two-way-stream>) (sequence <sequence>)
     #key (start 0) (end (size sequence)))
  (output-sequence (output-stream stream) sequence start: start end: end)
  stream)
     
(define-method finish-output ((stream <two-way-stream>))
  (finish-output (output-stream stream))
  stream)

(define-method force-output ((stream <two-way-stream>))
  (force-output (output-stream stream))
  stream)

(define-method clear-output ((stream <two-way-stream>))
  (clear-output (output-stream stream))
  stream)

;;;;;
;;;;; <ECHO-STREAM>
;;;;;

(define-class <echo-stream> (<two-way-stream>))

(define-method input ((stream <echo-stream>))
  (bind ((value (input (input-stream stream))))
    (unless (id? value eof:)
      (output stream value))
    value))

(define-method input-no-hang ((stream <echo-stream>))
  (bind ((value (input-no-hang (input-stream stream))))
    (unless (member? value '(eof: #F))
      (output stream value))
    value))

;;;;;
;;;;; <SEQUENCE-STREAM>
;;;;;

(define-class <sequence-stream> (<stream>)
  (index init-value: 0 type: <integer>)
  (sequence init-keyword: sequence: type: <sequence>))

(define-method initialize ((stream <sequence-stream>) #rest all-keys)
  (next-method)
  (set! (state stream) 'open)
  stream)

(define-method print
    ((object <sequence-stream>) #key (stream #T) (verbose? #F))
  (print-unreadable-object (object stream)
      (format stream "SEQUENCE: ~S" (sequence object))))

(define-method location ((stream <sequence-stream>))
  (if (> (index stream) (size (sequence stream)))
      eof:
      (index stream)))

(define-method location-setter
    ((new-position <integer>) (stream <sequence-stream>))
  (if (or (negative? new-position) (> new-position (size (sequence stream))))
      eof:
      (set! (index stream) new-position)))

;;;;;
;;;;; <SEQUENCE-INPUT-STREAM>
;;;;;

(define-class <sequence-input-stream> (<sequence-stream> <input-stream>)
  (sequence required-init-keyword: sequence: type: <sequence>))

(define-method close ((stream <sequence-input-stream>) #key abort)
  (next-method)
  (values))

(define-method input ((stream <sequence-input-stream>))
  (if (>= (index stream) (size (sequence stream)))
      eof:
      (bind ((result (element (sequence stream) (index stream))))
	(set! (index stream) (+ (index stream) 1))
	result)))

(define-method undo-input ((stream <sequence-input-stream>))
  (unless (< (index stream) 0)
    (set! (index stream) (- (index stream) 1)))
  stream)

;;;;;
;;;;; <SEQUENCE-OUTPUT-STREAM>
;;;;;

(define-class <sequence-output-stream> (<sequence-stream>)
  (sequence init-keyword: sequence: type: <stretchy-vector>))

(define-method initialize ((stream <sequence-output-stream>) #key sequence)
  (next-method)
  (unless sequence
    (set! (sequence stream) (make <stretchy-vector>)))
  stream)

(define-method close ((stream <sequence-output-stream>) #key abort)
  (next-method)
  (values))

(define-method output ((stream <sequence-output-stream>) (object <object>))
  (if (< (index stream) (size (sequence stream)))
      (set! (element (sequence stream) (index stream)) object)
      (set! (sequence stream) (add! (sequence stream) object)))
  (set! (index stream) (+ (index stream) 1))
  stream)
     
(define-method finish-output ((stream <sequence-output-stream>)) stream)

(define-method clear-output ((stream <sequence-output-stream>)) stream)

;;;;;
;;;;; <BIDIRECTIONAL-SEQUENCE-STREAM>
;;;;;

(define-class <sequence-bidirectional-stream>
    (<sequence-input-stream> <sequence-output-stream> <bidirectional-stream>))

;;;;;
;;;;; DEFINING CHARACTERS STREAMS
;;;;;

;;;; THE CLASS <CHARACTER-STREAM>

(define-class <character-stream> (<stream>))

(define-method bytes-per-element ((stream <character-stream>)) 1)
(define-method element-type ((stream <character-stream>)) <character>)

;;;; THE DIRECTIONAL <CHARACTER-STREAM> CLASSES

(define-class <character-input-stream> (<character-stream> <input-stream>))

;;;; EXTERNAL PROTOCOL SUPPORTED BY <CHARACTER-INPUT-STREAM>

;;; INPUT-LINE returns a string as the first value.  The second value is true
;;; if the string was terminated by end-of-file instead of the end of a line.
;;; The default method uses repeated calls to read-char.

(define-generic-function input-line (input-stream))

(define-method input-line ((stream <character-input-stream>))
  (bind (((string <stretchy-vector>) (make <stretchy-vector> size: 0)))
    (bind-methods ((input-characters ()
	             (bind ((character (input stream)))
		       (case character
			 ((eof:) (values (as <string> string) #T))
			 ((#\Newline) (values (as <string> string) #F))
			 (else: (add! string character) (input-characters))))))
      (input-characters))))

(define-class <character-output-stream> (<character-stream> <output-stream>))

;;;; EXTERNAL PROTOCOL SUPPORTED BY <CHARACTER-OUTPUT-STREAM>

;;; LINE-COLUMN returns the column number where the next character will be 
;;; written, or #F if that is not meaningful for stream.  The first column on a
;;; line is numbered 0.  This function is used in the implementation of the
;;; pretty printer and the format ~T directive.  For every character output
;;; stream class that is defined, a method must be defined for this function,
;;; although it is permissible for it to always return #F.

(define-generic-function line-column (output-stream))

(define-method line-column ((stream <character-output-stream>)) #F)
  
;;; OUTPUT-WIDTH returns an integer width of stream or #F.  The default method
;;; returns #F for all streams.

(define-generic-function output-width (output-stream))

(define-method output-width ((stream <output-stream>)) #F)

;;; START-LINE? is a predicate which returns #T if stream is positioned at the
;;; beginning of a line, else returns #F.  It is permissible to always return
;;; #F.  This is used in the implementation of fresh-line.  Note that while a
;;; value of 0 from line-column also indicates the beginning of a line, there
;;; are cases where start-line? can be meaningfully implemented although
;;; line-column can't be.  For example, for a window using variable-width
;;; characters, the column number isn't very meaningful, but the beginning of
;;; the line does have a clear meaning.  The default method for start-line? on
;;; class <character-output-stream> uses line-column, so if that is defined to
;;; return #F, then a method should be provided for either start-line? or
;;; fresh-line.

(define-generic-function start-line? (output-stream))

(define-method start-line? ((stream <character-output-stream>))
  (bind ((column (line-column stream)))
    (if column
	(= column 0)
	#F)))

;;; ADVANCE-TO-COLUMN writes enough blank space so that the next character will
;;; be written at the specified column.  Returns #T if the operation is
;;; successful, or #F if it is not supported for output-stream.  This is
;;; intended for use by the pretty printer and format ~T.  The default method
;;; uses line-column and repeated calls to output with #\space character; it
;;; returns #F if line-column returns #F.

(define-generic-function advance-to-column (output-stream column))

(define-method advance-to-column
    ((stream <character-output-stream>) (column <integer>))
  (bind ((current-column (line-column stream)))
    (if (and current-column (>= column current-column))
	(for ((i from 0 below (- column current-column)))
	  (output stream #\Space)
	  finally #T)
	#F)))

;;; FRESH-LINE outputs a newline only if the stream is not already at the start
;;; of a line.  The default method uses start-line? and terpri.

(define-generic-function fresh-line (output-stream))

(define-method fresh-line ((stream <character-output-stream>))
  (unless (start-line? stream)
    (terpri stream))
  stream)

;;; TERPRI writes an end of line.  Returns output-stream.  The default method
;;; does (output output-stream #\Newline).

(define-generic-function terpri (output-stream))

(define-method terpri ((stream <character-output-stream>))
  (output stream #\Newline))

;;;; THE <CHARACTER-BIDIRECTIONAL-STREAM> CLASS

(define-class <character-bidirectional-stream>
    (<character-input-stream> <character-output-stream> <bidirectional-stream>))

;;;;;
;;;;; INSTANTIABLE STREAMS
;;;;;

;;;; An instantiable stream needs three components, indicating the element type,
;;;; the direction, and the device type.

;;;; !@#$ THIS WILL BE REPLACED BY PARAMETERIZED CLASSES

;;;; INSTANTIABLE CHARACTER TERMINAL STREAMS

(define-class <character-terminal-input-stream>
    (<terminal-input-stream> <character-input-stream>))

(define-class <character-terminal-output-stream>
    (<terminal-output-stream> <character-output-stream>))

(define-class <character-terminal-bidirectional-stream>
    (<terminal-bidirectional-stream> <character-bidirectional-stream>))

;;;; INSTANTIABLE CHARACTER BROADCAST STREAMS

(define-class <character-broadcast-stream>
    (<broadcast-stream> <character-output-stream>))

;;;; INSTANTIABLE CHARACTER CONCATENATED STREAMS

(define-class <character-concatenated-stream>
    (<concatenated-stream> <character-input-stream>))

;;;; INSTANTIABLE CHARACTER TWO-WAY STREAMS

(define-class <character-two-way-stream>
    (<two-way-stream> <character-bidirectional-stream>))

;;;; INSTANTIABLE CHARACTER SEQUENCE STREAMS

(define-class <character-sequence-input-stream>
    (<sequence-input-stream> <character-input-stream>))

(define-class <character-sequence-output-stream>
    (<sequence-output-stream> <character-output-stream>))

(define-class <character-sequence-bidirectional-stream>
    (<sequence-bidirectional-stream> <character-bidirectional-stream>))

;;;;
;;;; !@#$ TEMPORARY OPEN PROTOCOL
;;;;

(define-method open 
    ((filename <string>) 
     #key (direction input:)
          (if-exists? new-version:) 
          ;; (if-does-not-exist? (if (id? direction input:) error: create:))
          )
  (bind ((stream
	  (case direction
	    ((input:)
	     (make <character-terminal-input-stream>
		   input-terminal-path-name: filename))
	    ((output:)
	     (make <character-terminal-output-stream>
		   output-terminal-path-name: filename)))))
    (set! (state stream) 'open)
    stream))

(define-variable *standard-input* #F)
(define-variable *standard-output* #F)

(define-lambda initialize-standard-io ()
  (set! *standard-input* (open "input" direction: input:))
  (set! *standard-output* (open "output" direction: output:))
  (values))

(initialize-standard-io)

(define-method format ((boolean <boolean>) (string <byte-string>) #more args)
  (if boolean ;; #T
      (apply format *standard-output* string args)
      (bind ((stream (make <sequence-output-stream>)))
	(apply format stream string args)
	(as <byte-string> (sequence stream)))))

;;; !@#$ temporary
(define-method format
    ((vector <stretchy-vector>) (string <byte-string>) #more args)
  (bind ((stream (make <sequence-output-stream> sequence: vector)))
    (set! (index stream) (size vector))
    (apply format stream string args)
    (sequence stream)))

(define-method format ((stream <stream>) (string <byte-string>) #more args)
  (bind ((found-tilde? #F)
	 (arg-index 0))
    (for ((i = 0 then (+ i 1))
	  (until (= i (size string))))
      (bind ((character (element string i)))
	(cond ((id? character #\~)
	       (set! found-tilde? #T))
	      (found-tilde?
	       (case (as-uppercase character)
		 ((#\X #\D #\A)
		  (bind ((arg (element args arg-index)))
		    (set! arg-index (+ arg-index 1))
		    (print arg stream: stream verbose?: #F)))
		 ((#\S)
		  (bind ((arg (element args arg-index)))
		    (set! arg-index (+ arg-index 1))
		    (print arg stream: stream verbose?: #T)))
		 ((#\%) (print #\newline stream: stream verbose?: #F))
		 ((#\~) (print #\~ stream: stream verbose?: #F))
		 (else: (error "Invalid ~~ command")))
	       (set! found-tilde? #F))
	      (else:
	       (print character stream: stream verbose?: #F))))))
  (values))
