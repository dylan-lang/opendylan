(in-package dylan)

;; Synopsis:  Port to lisp of the lexer inner loop for speed in the emulator.
;; Author:    Keith Playford
;; Copyright: 1996 Functional Objects, Inc. All rights reserved.

(declaim (optimize (speed 3) (safety 0) (fixnum-safety 0) (debug 0)))

;; First, we convert the state machine to Lisp.

(defstruct state () name result transitions)

(defun convert-state (dylan-state seen)
  (if (eq dylan-state *dylan-canonical-false*) nil
    (let ((converted (gethash dylan-state seen)))
      (or converted
          (let ((converted (setf (gethash dylan-state seen) (make-state))))
	    (setf (state-name converted)
		  (dylan+dylan/dfmc-common::name dylan-state))
	    (setf (state-result converted)
                  (let ((result 
                          (dylan+dylan/dfmc-reader::result dylan-state)))
                    (if (eq result *dylan-canonical-false*)
                      nil
                      result)))
	    (setf (state-transitions converted)
		  (let ((trans
                          (dylan+dylan/dfmc-reader::transitions dylan-state)))
                      (if (eq trans *dylan-canonical-false*) nil
                        (map 'vector #'(lambda (dylan-state)
                                         (convert-state dylan-state seen))
			     trans))))
            converted)))))

(defparameter $initial-state 
  (convert-state 
     dylan+dylan/dfmc-reader::$initial-state (make-hash-table :test #'eq)))

;; The inner state transition loop.

#|
(defmacro byte-ref (vector index)
  `(system:svref-no-check$signed-i-vector-8$fixnum
      (the (simple-array (signed-byte 8) (*)) ,vector)
      (the fixnum ,index)))
|#
(defmacro byte-ref (vector index)
  `(system:svref-no-check$i-vector-8$fixnum
      (the (simple-array (unsigned-byte 8) (*)) ,vector)
      (the fixnum ,index)))

;; (defmacro byte-ref (vector index)
;;  `(aref ,vector ,index))

(defun internal-get-token (lexer)
  (let* ((contents (dylan+dylan/byte-vector::bytes
                     (dylan+dylan/dfmc-reader::contents
                       (dylan+dylan/dfmc-reader::source lexer))))
         (length (length contents))
         (result-kind nil)
         (result-start (dylan+dylan/dfmc-reader::posn lexer))
         (result-end nil)
         (state $initial-state)  
         (unexpected-eof nil)
         (saved-line nil)
         (saved-line-start nil)
         (posn (dylan+dylan/dfmc-reader::posn lexer)))
    (declare (type fixnum length posn) 
             (type vector contents)
             (type state state))

    (tagbody start
      (let ((result (state-result state)))
        (when result
	  ;; It is an accepting state, so record the result and where
	  ;; it ended.
	  ;; (format t "Accept: ~s~%" (state-result state))
	  (setq result-kind result)
	  (setq result-end posn)))
      ;;
      ;; Try advancing the state machine once more if possible.
      ;; 
      (when (< posn length)
	(let ((table (state-transitions state)))
	  (when table
	    (let* ((char (the fixnum (byte-ref contents posn)))
		   (new-state (aref (the simple-vector table) char)))
	      (when new-state
		(setq state new-state)
		(incf posn)
		(go start))))))
      ;; Hit a dead end - now what?
      (when (and result-kind (symbolp result-kind))
	;;
	;; The result-kind is a symbol if this is one of the magic
	;; accepting states.  Instead of returning some token, we do
	;; some special processing depending on exactly what symbol
	;; it is, and then start the state machine over at the
	;; initial state.
	;;
	(case result-kind
	  ((whitespace))
	  ((newline)
	    (dylan+dylan/dfmc-reader::line-setter
	      (+ (dylan+dylan/dfmc-reader::line lexer) 1)
	      lexer)
	    (dylan+dylan/dfmc-reader::line-start-setter
	      result-end
	      lexer))
	  ((end-of-line-comment)
	    (loop for i of-type fixnum 
		    from (the fixnum result-end) below length
		  until (eq (the fixnum (byte-ref contents i)) 10)
		  finally (setq result-end i)))
	  ((multi-line-comment)
            (setq saved-line
                  (dylan+dylan/dfmc-reader::line lexer))
            (setq saved-line-start 
                  (dylan+dylan/dfmc-reader::line-start lexer))
            (let* ((end (dylan+dylan/dfmc-reader::skip-multi-line-comment
			  lexer result-end)))
	      (setq result-end 
                (if (eq end *dylan-canonical-false*) 
                    (progn
                      (setq unexpected-eof t)
                      nil)
                  end)))))
	(setq result-kind nil)
	(when result-end
	  (setq result-start result-end)
	  (setq result-end nil)
	  (setq state $initial-state
		posn result-start)
          (setq saved-line nil)
          (setq saved-line-start nil)
          (go start))))
    
  (when (not result-kind)
    ;;
    ;; If result-kind is #f, that means we didn't find an accepting
    ;; state.  Check to see if that means we are at the end or hit
    ;; an error.
    ;; 
    (cond
      ;; If we started at the end, it's just eof.
      ((eq result-start length)
         (setq result-kind
               dylan+dylan/dfmc-reader::<eof-marker>)
         (setq result-end result-start))
      ;; If we're at the end and didn't start there, we've hit a premature
      ;; eof.
      ((or unexpected-eof (= posn length))
         (setq result-kind nil)
         (setq result-end length)
	 (setq unexpected-eof t))
      ;; Otherwise it's just a malformed token.
      (t
         (setq result-kind nil)
         (setq result-end (+ result-start 1)))))

  ;;
  ;; Save the current token's end position so that the next token
  ;; starts here.
  ;;
  (dylan+dylan/dfmc-reader::posn-setter result-end lexer)
  ;;
  ;; Make a source location for the current token.
  ;; 
  (let((source-location
         (dylan+dylan/dfmc-reader::make-lexer-source-location
            lexer
            (dylan+dylan/dfmc-reader::source lexer)
	    result-start 
              (or saved-line (dylan+dylan/dfmc-reader::line lexer))
              (- result-start 
                 (or saved-line-start
                     (dylan+dylan/dfmc-reader::line-start lexer)))
	    result-end
              (dylan+dylan/dfmc-reader::line lexer)
              (- result-end (dylan+dylan/dfmc-reader::line-start lexer)))))
  ;;
  ;; And finally, make and return the actual token.
  ;; 
  (cond
    ((null result-kind)
       (if unexpected-eof
         (dylan+dylan/dfmc-reader::invalid-end-of-input source-location)
         (dylan+dylan/dfmc-reader::invalid-token source-location)))
    ((functionp result-kind)
       (do-process-token result-kind lexer source-location))
    (t
      (clos::dylan-make result-kind 
	 :record (dylan+dylan/dfmc-reader::source-location-record source-location)
	 :source-position (dylan+dylan/dfmc-reader::source-location-source-position source-location)))))))

(defun do-process-token (f lexer source-location)
  (funcall f lexer source-location))

;; Install it.

(defun dylan+dylan/dfmc-reader::get-token (lexer)
  (internal-get-token lexer))

;;;; Further assorted speed hacks.

;; This function is no longer used internally, so avoid usurping other
;; specializers.
;; (defun dylan+dylan/source-records::make-source-location
;;     (record start-char start-line start-col end-char end-line end-col)
;;   (let ((object 
;;           (allocate-instance 
;;              dylan+dylan/dfmc-reader::<range-source-location>)))
;;     (clos::fast-set-standard-instance-access object 2 record)
;;     (clos::fast-set-standard-instance-access object 1
;;       (dylan+dylan/source-records::make-source-offset
;;          start-char start-line start-col))
;;     (clos::fast-set-standard-instance-access object 0
;;       (dylan+dylan/source-records::make-source-offset
;;          end-char end-line end-col))
;;     object))

;; eof
