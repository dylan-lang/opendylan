;; File header reader.

; Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
;               All rights reserved.
; License:      Functional Objects Library Public License Version 1.0
; Dual-license: GNU Lesser General Public License
; Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-generic-function maybe-read-file-header (stream))
(define-generic-function read-file-header (stream))

;; Parameters.

(define $non-file-header-characters #(#\# #\; #\())
(define $unique-header-keywords     #(module: language:))

;; Implementation.

(define-method maybe-read-file-header ((s <stream>))
  (for ((char = (input s) then (input s))
	(until (not (whitespace? char))))
    finally
    (undo-input s)
    (if (member? char $non-file-header-characters)
	(bind ((tab (make <table>)))
	  (set! (element tab language:) '("prefix-dylan"))
	  tab)
	(begin
	 (read-file-header s)))))

(define-class <duplicate-header-key-restart> (<restart>))

(define-method read-file-header ((s <stream>))
  (bind ((keys (make <table>)))
    (bind-methods 
      ((register-key (key value)
         (bind ((strings (element keys key default: '())))
           (when (and (member? key $unique-header-keywords)
		      (not (empty? strings)))
	     (handler-case 
               (error "Duplicated header keyword ~s" key)
               ((<duplicate-header-key-restart>
                  description:
		   (method (s)
		     (format s "Use new definition ~s" value)))
                 (set! strings '()))
               ((<duplicate-header-key-restart>
                  description:
                    (method (s)
                      (format s "Use existing definition ~s" strings)))
                 (set! value '()))
	       ((<duplicate-header-key-restart>
                  description:
                    (method (s)
                      (format s "Merge definitions"))))))
	   (set! (element keys key)
		 (concatenate strings value)))))
      (bind-exit (return)
        (while #t
          (bind ((key strings (read-file-header-component s)))
	    (if (not key) (return keys)
	      (register-key key strings))))))))

(define-method read-file-header-component ((s <stream>))
  (bind-exit (return)
    (bind ((key-line 
             (handler-case (input-line s)
               ((<end-of-file>) ""))))
      (when (header-end-marker-line? key-line)
        (return #f #f))
      (bind ((text-strings (make <deque>))
             (key text (parse-header-keyword-line key-line)))
        (push-last text-strings text)
        (handler-case
            (for ((char = (input s) then (input s))
		  (until (or (not (whitespace? char)) (id? char #\newline))))
	      (bind ((continuation-line (input-line s)))
		(if (header-end-marker-line? continuation-line)
		    (return key (as <list> text-strings))
		    (push-last 
		     text-strings 
		     (parse-header-continuation-line continuation-line))))
	      finally
	      (undo-input s))
          ((<end-of-file>)))
        (return key (as <list> text-strings))))))

(define-method parse-header-keyword-line ((line <string>))
  (bind ((colon (find-key line (curry id? #\:) failure: #f)))
    (unless colon
      (error "Badly formed file header line ~s" line))
    (values
      (as-keyword (copy-sequence line end: colon))
      (trim (copy-sequence line start: (+ colon 1)) whitespace?))))

(define-method parse-header-continuation-line ((line <string>))
  (trim line whitespace?))

(define-method header-end-marker-line? ((line <string>))
  (every? whitespace? line))

;; Utilities.

(define-method trim (seq lose?)
  (bind ((win?  (complement lose?))
         (start (find-key seq win?))
         (end   (- (size seq)
                   (find-key (reverse seq) win?)))) ;; find-key ... from:
    (copy-sequence seq start: start end: end)))

;; eof
