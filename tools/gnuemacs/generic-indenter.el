;; -*- Emacs-Lisp -*-
;; (The previous line puts Emacs into emacs-lisp-mode on loading.)

;; $HopeName:   $
;; File:	generic-indenter.el
;; Author:	Hugh Greene <hughg>
;; Created:	1996/07/29
;;
;; Description:
;; A generic indenter in emacs-lisp.
;;
;; Details:
;; This is an initial, prototype implementation.  Data structures
;; which would normally be user-provided are hard-coded (for the Dylan
;; language).
;;
;; Entry points:
;; (gi-indent-line point)
;; This is the main entry point to this module.  The single argument,
;; "point", is an emacs cursor position, which lies on the line to be
;; indented.  It may be used interactively (i.e., simply bound to some
;; key) and it should respect read-only buffers.
;;
;; (gi-compile-items)
;; This 0-argument function should be called whenever a new structure
;; is assigned to gi-items (with "setq"), i.e., whenever the item
;; descriptions are updated.  Until it is called, gi-indent-line may
;; not function correctly (it may be looking for REs which match the
;; patterns from before the update).
;;
;; Global variables (for configuration):
;; gi-debug
;; When set to "t" (the default), some lower-level functions will
;; check their arguments for validity.  Since the only possible
;; top-level bug is to give a buffer position which does not lie
;; within the buffer, which these checks do not look for, most errors
;; caught by these checks will be internal.  Therefore, if the
;; indenter is running smoothly for long enough, we may wish to turn
;; off these checks to speed things up, if needed.
;;
;; gi-default-indent
;; This integer value (default 2) is the default indentation for
;; ordinary lines within a block (i.e., between a /start/ and /end/,
;; except for /inter/ lines).  This may be overridden by each /start/
;; and /inter/ specification (without changing the value of
;; gi-default-indent) and may be used to compute other offsets.  It
;; can be freely changed at any time but this change will likely have
;; no effect until a new value is given to gi-items and
;; gi-compile-items is called again.
;;
;; gi-items
;; This global "defvar" holds the patterns which gi-indent-line looks
;; for and the indentation values it applies.  The format of the data
;; structure it should hold is given below and its intended meaning is
;; described in indenter-notes.txt.  In this prototype implementation,
;; quick constructors are provided to make it easy to hard-code
;; example items.  Later implementations will probably construct these
;; automatically in some way.
;;
;; As noted above, whenever gi-items is updated, using "setq", the
;; function gi-compile-items must be called before using
;; gi-indent-line.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; IMPLEMENTATION DEPENDENCIES
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The "cl" package emulates some CL facilities in ELisp.  It is part
;; of Emacs 19, apparently.
(require 'cl)
(require 'cl-19)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; DATA STRUCTURE DEFINITIONS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Hard-coded data structures.

;; Items for pattern-matching.
;;
;; Format of gi-items overall:
;; {all:{start-regexp end-regexp} (*)
;;  items:{
;;    item:{
;;      all-inters-regexp (*)
;;      start:{regexp offset2}
;;      inters:{inter:{regexp offset1 offset2} ...}
;;      num-inters (*)
;;      end:{regexp offset1}
;;    } ...
;;  }
;;  num-items (*)
;;  compiled (*)
;; }
;; (* These values are set by gi-compile-items.  Users should NOT
;; provide a value for them when setting gi-items.)
;;
;; See indenter-notes.txt for the meaning of these structures.
;;
;; Note: The odd "quick constructors" here are to aid me in creating a
;; "hard-coded" gi-items for testing.  Eventually some other piece of
;; code will construct gi-items (e.g., a parser of the spec language)
;; and that will use standard constructors.  The "i-" prefix is short
;; for "item[s]".

(defstruct
  (all-t
   (:constructor i-all (&optional (start-re nil) (end-re nil))))
  start-re end-re)

(defstruct
  (start-t
   (:constructor i-start (re &optional (offset2 gi-default-indent))))
  re offset2)

(defstruct
  (inters-t
   (:constructor i-inters (&optional inters)))
  (all-re nil) (inters nil))

(defstruct
  (inter-t
   (:constructor i-inter (re offset1 &optional (offset2 nil))))
   re offset1 offset2)

(defstruct
  (end-t
   (:constructor i-end (re &optional (offset1 0))))
  re offset1)

(defstruct
  (item-t
   (:constructor i-item (start end &optional (inters (i-inters)))))
  (all-inters-re nil) start inters num-inters end)

(defstruct items-t all items num-items compiled)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; GLOBAL VARIABLES
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following controls internal error checking.  Turning it off
;; might speed things up.
(defvar gi-debug t

"*Flag for internal error-checking.  Set it to non-nil for increased
robustness or nil for (perhaps) increased speed.")

;; Default indent offset within a block.
(defvar gi-default-indent 2

"*Default indentation offset.  Changes to this will only take effect
once a new value has been assigned to gi-items and gi-compile-items
has been called.")

;; The items (i.e., language constructs) to be recognised by the
;; indenter.  This structure has to be pre-processed after its ;;
;; definition by calling "(gi-compile-items)".  This will overwrite
;; the REs it contains, so if modifications are to be made go
;; gi-items, the "setq" below must be re-evaluated and
;; "(gi-compile-items)" must be called again.
(defvar gi-items nil

"*Data structure providing regexps used to recognise text structure
for indenting, as well as the indentation to be used in different
structures.  See the source code or documentation for details of its
structure.

After changing the value of this variable, always call the function
gi-compile-items before using gi-indent-line.")

(setq gi-items
  (make-items-t
   :all (i-all)
   :items
   (vector
    ;; "define [xxx]* (macro|class|method)", and "[local] method" all
    ;; indent as standard, i.e., contained statements are indented by
    ;; gi-default-indent.
    (i-item (i-start "define[ \t]+macro")
	    (i-end "end;?"))
    (i-item (i-start "define[ \t]+\\([^ \t\n]+[ \t]+\\)*class")
	    (i-end "end;?"))
    (i-item (i-start "define[ \t]+\\([^ \t\n]+[ \t]+\\)*method")
	    (i-end "end;?"))
    (i-item (i-start "\\(local[ \t]+\\)?method")
	    (i-end "end;?"))
    ;; "block" is much the same, except that lines with "(afterwards|
    ;; cleanup|exception)" are outdented to align with "block".
    (i-item (i-start "block")
	    (i-end "end;?")
	    (i-inters (vector
		       (i-inter "\\(afterwards\\|cleanup\\|exception\\)" 0))))
    ;; "case" is odd: the body is indented by 3 more than usual,
    ;; except for lines containing "=>" or beginning with "otherwise".
    (i-item (i-start "case" (+ 3 gi-default-indent))
	    (i-end "end;?")
	    (i-inters (vector
		       (i-inter "\\(\\(.*[ \t]+\\)?=>\\|otherwise\\)"
				gi-default-indent (+ 3 gi-default-indent)))))
    ;; This is supposed to be for parentheses, "(...)", but I'm not
    ;; sure it works.
    (i-item (i-start "\\(\\`\\|[^\\\\]\\)\\(\\\\\\\\\\)*\"" 0)
	    (i-end "\\(\\`\\|[^\\\\]\\)\\(\\\\\\\\\\)*\"" 0))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; FUNCTIONS
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;
;;;; Main indentation functions.
;;;;;;;;

(defun gi-calculate-indent (base-pos)

  "Given a character position BASE-POS in the current buffer, returns
the indent value for that line, based on the information in gi-items."

  (let*
      (
       ;; start-indent will be set to the indentation of the line
       ;; containing the "dominating" /start/ RE.
       start-indent

       ;; curr-item will refer to the item to which the /start/ belongs.
       curr-item

       ;; line-{start,end} are the obvious positions from the line on
       ;; which the base-pos lies.
       (line-start (progn (beginning-of-line) (point)))
       (line-end (progn (end-of-line) (point))))

    ;; Now calculate the column to which the line containing base-pos
    ;; should be indented.

    (multiple-value-bind
     ;; Find the enclosing /start/.
	(item-pos item-idx)
	(gi-find-indent-determiner base-pos 'start)
      (if (null item-pos)
	  ;; If there isn't one, we're at top level, so 0 indent.
	  0
	;; Otherwise, find the indentation of that /start/ as a base.
	(setq curr-item (aref (items-t-items gi-items) item-idx)
	      start-indent (progn (goto-char item-pos)
				  (current-column)))
	(+ start-indent
	   ;; Now calculate this line's indentation relative to it.
	   (if (gi-match-re-to-region
		(end-t-re (item-t-end curr-item))
		line-start line-end)
	       ;; If this line has an /end/ on its own line, use its
	       ;; /offset1/ value.
	       (end-t-offset1 (item-t-end curr-item))
	     ;; Otherwise, look back for any /inter/ matching the
	     ;; /start/ we found.
	     (multiple-value-bind
		 (inter-pos inter-idx)
		 (gi-find-indent-determiner
		  base-pos 'inter item-pos item-idx)
	       (cond
		;; If there's no /inter/, use the /start/'s /offset2/.
		((null inter-pos)
		 (start-t-offset2 (item-t-start curr-item)))
		;; If we're on a line with an /inter/, use it's
		;; /offset1/.
		((gi-match-re-to-region
		  (inter-t-re
		   (aref (inters-t-inters (item-t-inters curr-item))
			 inter-idx))
		  line-start line-end)
		 (inter-t-offset1
		  (aref
		   (inters-t-inters (item-t-inters curr-item))
		   inter-idx)))
		;; Otherwise we're between /inter/s, so use the
		;; /offset2/ of the preceding one, or the /offset2/ of
		;; the /start/, if there's no preceding /inter/.
		(t
		 (or
		  (inter-t-offset2
		   (aref
		    (inters-t-inters (item-t-inters curr-item))
		    inter-idx))
		  (start-t-offset2 (item-t-start curr-item))))))
	     )))))
  )

(defun gi-perform-indent (base-pos indent)

  "Given a character position BASE-POS in the current buffer and an
indentation value INDENT, alters the initial whitespace of BASE-POS'
line to have the first non-whitespace character at column INDENT.
Leaves the point at a \"sensible\" position."

  ;;; And now do the indentation itself.
  (let*
      (;; line-start is the first character pos on the indented line.
       (line-start (progn (goto-char base-pos)
			  (beginning-of-line)
			  (point)))
       ;; line-end will be the last character pos, AFTER indentation.
       line-end
       ;; old-indent is the indentation before we change it.
       (old-indent (current-indentation))
       ;; new-base-pos is where the point should be moved to, if it
       ;; lay on base-pos initially (if not, gi-indent-line will
       ;; override us at a higher level).
       (new-base-pos (+ (- base-pos old-indent) indent)))
    ;; Don't let the point move onto a previous line.
    (when (< new-base-pos line-start)
      (setq new-base-pos line-start))

    ;; Perform the indentation.
    (delete-horizontal-space)
    (indent-to-column indent)

    ;; A bit more work on where to leave the cursor.
    (setq line-end (progn (end-of-line) (point)))
    (when (> new-base-pos line-end)
      (setq new-base-pos line-end))

    ;; Lastly, position the cursor "sensibly", as described in the
    ;; pseudocode.
    (if (<= (- new-base-pos line-start) indent)
	(progn
	  (goto-char line-start)
	  (search-forward-regexp "[ \t]*"))
      (goto-char new-base-pos))
    ))

(defun gi-indent-line (base-pos)

  "Indents the line on which the character position BASE-POS lies,
using the regexps and indent values stored in gi-items.  (BASE-POS
takes the value of the point in interactive use.)

Always call (gi-compile-items) after updating gi-items."

  (interactive "*d")

  (let
      ((saved-pos
	;; Save our previous position then go to the indent position.
	;; (Done this way to avoid "(let ... (save-excursion ... (let ...".)
	(if (= (point) base-pos)
	    nil
	  (prog1 (point) (goto-char base-pos))))

       ;; Calculate the indentation required.
       (indent (gi-calculate-indent base-pos)))

    ;; Now we can change the text and position the cursor "sensibly".
    (gi-perform-indent base-pos indent)

    ;; For some reason, at least in Emacs 19.30.1, save-excursion
    ;; around gi-perform-indent leaves us at the start of the indented
    ;; line, not wherever we previously were.  So, we must restore the
    ;; cursor position manually, if we were indenting "remotely".
    (when saved-pos (goto-char saved-pos))))

;; This defun is an implementation-specific addition.
(defun gi-find-indent-determiner-check-params
  (base-pos target-kind item-pos item-idx)

  "Signals an error if the parameters (intended for
gi-find-indent-determiner) do not pass certain tests."

  (if (eq target-kind 'inter)
      (progn
	(when (not (and item-pos item-idx))
	  (error
	   (concat "Internal error (in gi-find-indent-determiner): "
		   "item-pos and item-idx must be supplied when "
		   "target-kind is 'inter.")))
	(when (or (< item-idx 0)
		  (>= item-idx (items-t-num-items gi-items)))
	  (error
	   (concat "Internal error (in gi-find-indent-determiner): "
		   "item-idx out of range for gi-items.")))
	)
    (unless (eq target-kind 'start)
      (error
       (concat "Internal error (in gi-find-indent-determiner): "
	       "target-kind must be one of '(start inter).")))))

(defun gi-find-indent-determiner
  (base-pos target-kind &optional item-pos item-idx)

  "Searches backward from the cursor position for some text matching a
regexp from gi-items.  Which regexp is matched determines the status,
in some textual structure, of BASE-POS' line, in terms of
indentation.

Returns the position of the first character of the matched pattern,
plus the index of the /start/ or /inter/ pattern it matches.

See the pseudocode, source code and/or documentation for detailed
explanation of the parameters, return values and method."

  ;;; Provide default values for item-pos and item-idx.
  (when (eq target-kind 'start)
    (setq item-pos nil item-idx 'any))

  ;;; Parameter validity checking.
  (when gi-debug
    (gi-find-indent-determiner-check-params
     base-pos target-kind item-pos item-idx))

  ;;; Search back, using a depth marker to make the process
  ;;; iterative instead of tail-recursive, to save emacs' stack.
  (goto-char base-pos)
  (loop with depth = 1 
	;; Decide on stop point:
	with stop-pos = (if item-pos (1+ item-pos) 0)
	for line-start = (progn (beginning-of-line) (point))
	for line-end = (progn (end-of-line) (point))
	for curr-pos =
	  ;; If we're on a /start/ or /end/ line, don't include it in
	  ;; the search; otherwise include the whole line.
	  (if (gi-match-re-to-region
	       (apply 'concat
		      "\\(" (all-t-start-re (items-t-all gi-items))
		      "\\|" (all-t-end-re (items-t-all gi-items))
		      "\\)" '())
	       line-start line-end)
	      line-start line-end)
	for (target-pos target-idx) = 
	  (if (= depth 1)
	      (gi-seek-item-back curr-pos stop-pos item-idx target-kind)
	    (gi-seek-item-back curr-pos stop-pos 'any 'start))
	for (end-pos end-idx) =
	  (gi-seek-item-back curr-pos stop-pos 'any 'end)
	do
	  (if target-pos
	      (if (and end-pos (> end-pos target-pos))
		  ;; There's an /end/ between us and our target, so
		  ;; go down a level, to later come back up at
		  ;; the corresponding /start/.
		  (progn
		    (setq depth (1+ depth))
		    (goto-char end-pos))
		;; We found either a /start/ matching a previously
		;; found /end/, or the top-level target (depending
		;; on depth); either way, come up a level.
		(setq depth (1- depth))
		(goto-char target-pos))
	    (setq depth
		  (if (= depth 1)
		      ;; Failed to find top-level target, so
		      ;; we're done.
		      0
		    ;; Failed to find a /start/ to match some
		    ;; /end/ which appeared to be a item between
		    ;; us and the top-level target (implying bad
		    ;; syntax) but we'll ignore it and go back to
		    ;; looking for the top-level target (since
		    ;; there can be no more intervening items).
		    1))
	    ;; Go back to where we were (before this failed seeking
	    ;; at a depth >1) in case gi-seek-item-back left us
	    ;; somewhere else.
	    (goto-char curr-pos))
	until (= depth 0)

        ;;; Return character position and index in gi-items of target.
	finally return (values target-pos target-idx))
  )

;;;;;;;;
;;;; Specialised regexp functions.
;;;;;;;;

;; This defun is an implementation-specific addition.
(defun gi-build-disjunct-re (kind items-or-item)

  "Auxilliary to compile-all-item-regexps.  Returns a disjunct
combination of regexps, i.e., \"\\(RE1\\|...\\|REn\\)\".  Depending on
KIND and ITEMS-OR-ITEM, the regexps are for either all /inter/s for
one item, or all /start/s or /end/s for all items."

  (let* ((extract-re
	  ;; A small function to pull out the appropriate RE.
	  (case kind
	    (start (lambda (itm) (start-t-re (item-t-start itm))))
	    (inter (lambda (oth) (inter-t-re oth)))
	    (end (lambda (itm) (end-t-re (item-t-end itm))))))
	 (source-vector
	  ;; If kind is 'inter, then items-or-item is an item, whose
	  ;; /inters/ vector we want to use; otherwise it is gi-items
	  ;; and we want to look at the overall items vector.
	  (if (eq kind 'inter)
	      (inters-t-inters (item-t-inters items-or-item))
	    (items-t-items items-or-item)))
	 (unique-res
	  ;; Remove duplicates to avoid, e.g., "end|end|end|..."
	  ;; (since several items may have the same /end/ RE).
	  (remove-duplicates
	   (map 'vector extract-re source-vector)
	   :test 'string=)))
    ;; Either return "" if there are no patterns, or add the "(|)"s.
    (if (equal [] unique-res)
	""
      (concat
       (reduce 'concat (subseq unique-res 1)
	       :initial-value (concat "\\(" (aref unique-res 0))
	       :key (lambda (s) (concat "\\|" s)))
       "\\)")))
  )

(defun gi-compile-items ()

  ;; Note: In addition to "compiling" the REs, this implementation
  ;; precomputes the number of items and the number of /inter/s for
  ;; each item, since these are needed several times and are
  ;; potentially expensive to compute in elisp.

  "*Manipulates the regexps in gi-items (and caches certain internal
values for later use).

Always call this once after setting a new value for gi-items (multiple
calls will have no effect until gi-items is changed again)."

  ;; In case the user changes gi-items manually, make this function
  ;; interactive, as it will have to be called before gi-items is
  ;; used.
  (interactive)

  (block nil ; to allow (return)

    ;;; Stop now if the items have not been updated (resetting the
    ;;; "compiled" flag) since the last compile.
    (when (items-t-compiled gi-items)
      (return))

    ;;; Sanity check, that we do have some items.  (This is needed even if
    ;;; not in debug mode, as 0 items is legal, if pointless.)
    (when (equal [] (items-t-items gi-items))
      (setf (all-t-start-re (items-t-all gi-items)) "")
      (setf (all-t-end-re (items-t-all gi-items)) "")
      (return))

    (let ((re-start-bol "^[ \t]*")
	  (re-inter-bol "^[ \t]*")
	  (re-end-bol "^[ \t]*\\(.*[ \t]+\\)?")
	  (re-eol "\\([ \t].*\\|$\\)"))

      ;;; Compile regexps for all-at-once /start/s and /end/s.
      ;; First the /start/s
      (setf (all-t-start-re (items-t-all gi-items))
	    (apply 'concat
		   re-start-bol
		   (gi-build-disjunct-re 'start gi-items)
		   re-eol '()))
      ;; Second the /end/s
      (setf (all-t-end-re (items-t-all gi-items))
	    (apply 'concat
		   re-end-bol
		   (gi-build-disjunct-re 'end gi-items)
		   re-eol '()))
      ;; And lastly the /inter/s
      (map nil
	   (lambda (curr-item)
	     (setf (inters-t-all-re (item-t-inters curr-item))
		   (if (or
			(equal [] (inters-t-inters (item-t-inters
						    curr-item)))
			(null (inters-t-inters (item-t-inters
						curr-item))))
		       ""
		     (apply 'concat
			    re-inter-bol
			    (gi-build-disjunct-re 'inter curr-item)
			    re-eol '())
		     )))
	   (items-t-items gi-items))

      ;;; Record the number of items, for later range-checking.
      (setf (items-t-num-items gi-items)
	    (length (items-t-items gi-items)))

      ;;; Now compile regexps for individual /start/s, /end/s and
      ;;; /inter/s.  This must be done after the all-at-once REs,
      ;;; since those require the unmodified versions of the
      ;;; individual REs.
      (map nil
	   (lambda (curr-item)
	     (setf (start-t-re (item-t-start curr-item))
		   (apply 'concat
			  re-start-bol
			  (start-t-re (item-t-start curr-item))
			  re-eol '())
		   (end-t-re (item-t-end curr-item))
		   (apply 'concat
			  re-end-bol
			  (end-t-re (item-t-end curr-item))
			  re-eol '()))
	     (map nil
		  (lambda (curr-inter)
		    (setf (inter-t-re curr-inter)
			  (apply 'concat
				 re-inter-bol
				 (inter-t-re curr-inter)
				 re-eol '())))
		  (inters-t-inters (item-t-inters curr-item)))

	     ;; Record the number of /inter/s in this item, for later
	     ;; range-checking.
	     (setf (item-t-num-inters curr-item)
		   (length (inters-t-inters (item-t-inters curr-item)))))
	   (items-t-items gi-items))
      ))

  (setf (items-t-compiled gi-items) t))

;; This defun is an implementation-specific addition.
(defun gi-match-re-to-region (re start-pos end-pos)

  "Returns non-nil if the regexp RE exactly fits all the characters in
the region bounded by START-POS and END-POS, nil otherwise.

Will alter the values returned by match-beginning, match-end etc., but
will leave the point where it was."

  (let ((saved-pos (point)))
    (goto-char start-pos)
    (prog1
	(if (re-search-forward re end-pos t)
	    (and (eq (match-beginning 0) start-pos)
		 (eq (match-end 0) end-pos))
	  nil)
      (goto-char saved-pos)))
  )

(defun gi-extract-match-re (item-idx kind)

  "Retrieve the compiled regexp corresponding to ITEM-IDX and KIND
(i.e., the original regexp with extras added to the start/end of it)."

  ;;; No parameter validity checking: rely on caller to get it right.

  (if (eq item-idx 'any)
      (case kind
	(start (all-t-start-re (items-t-all gi-items)))
	(end (all-t-end-re (items-t-all gi-items))))
    (let ((curr-item (aref (items-t-items gi-items) item-idx)))
      (case kind
	(start (start-t-re (item-t-start curr-item)))
	(inter (inters-t-all-re (item-t-inters curr-item)))
	(end (end-t-re (item-t-end curr-item))))
      ))
  )

;; This defun is an implementation-specific addition.
(defun gi-seek-item-back-check-params
  (base-pos stop-pos item-idx target-kind)

  "Signals an error if the parameters (intended for
gi-seek-item-back-check-params) do not pass certain tests."

  ;; Check parameter validity.
  (when (> stop-pos base-pos) (setq stop-pos 0))
  (unless (member target-kind '(start inter end))
    (error
     (concat "Internal error (in gi-seek-item-back): "
	     "target-kind must be one of '(start inter end).")))
  (cond
   ((natnump item-idx)
    (when (or (< item-idx 0)
	      (>= item-idx (items-t-num-items gi-items)))
      (error
       (concat "Internal error (in gi-seek-item-back): "
	       "item-idx out of range for gi-items."))))
   ((eq item-idx 'any) ; and (not (natnump item-idx))
    (unless (member target-kind '(start end))
      (error
       (concat "Internal error (in gi-seek-item-back): "
	       "item-idx may be 'any only if target-kind is "
	       "one of '(start end)."))))
   (t ; i.e. (not (or (natnump item-idx) (eq item-idx 'any)))
    (error
     (concat "Internal error (in gi-seek-item-back): "
	     "item-idx must be a non-negative integer or 'any.")))))

(defun gi-seek-item-back (base-pos stop-pos item-idx target-kind)

  "Search backwards from BASE-POS until a match is found for one of
the regexps in the list corresponding to ITEM-IDX and TARGET-KIND.
Ignore the match if it lies before STOP-POS.

The position of the point afterwards is undefined."

  (block nil ; to allow (return ...)
    (when gi-debug
      (gi-seek-item-back-check-params
       base-pos stop-pos item-idx target-kind))

    ;;; First check for special case, which seems odd but is quite
    ;;; common: we're looking for any /inter/ for an item which has
    ;;; no /inter/s specified.
    (when (and (eq target-kind 'inter)
	       (= 0
		  (item-t-num-inters
		   (aref (items-t-items gi-items) item-idx))))
      (return (list nil 'none)))

    (let (match-start-pos
	  match-end-pos
	  match-start-pos-past-ws
	  match-idx
	  ;;; Retrieve the RE to search for and add extras to it.
	  (match-re (gi-extract-match-re item-idx target-kind)))
	
      ;;; Move to the search position and search backwards.
      (goto-char base-pos)
      (setq match-start-pos (re-search-backward match-re stop-pos t)
	    match-end-pos (match-end 0)
	    match-start-pos-past-ws (re-search-forward "[ \t]*"))

      ;;; Process search results.

      ;; Check whether match failed (in which case, return now).
      (when (null match-start-pos) (return (list nil 'none)))

      (if (natnump item-idx)
	  (progn
	    (if (member target-kind '(start end))

		;; Simple: item-idx is number, target-kind is 'start/'end.
		(return
		 (list match-start-pos-past-ws item-idx))

	      ;; Found an /inter/ for item item-idx, now have to find
	      ;; which one.
	      (setq match-idx
		    (position-if
		     (lambda (curr-inter)
		       (gi-match-re-to-region
			(inter-t-re curr-inter)
			match-start-pos match-end-pos))
		     (inters-t-inters
		      (item-t-inters
		       (aref (items-t-items gi-items) item-idx))))
		    )))

	;; Otherwise, item-idx was 'any, so work out which idx
	;; we really got.
	(setq match-idx
	      (let ((extract-re
		     (if (eq target-kind 'start)
			 (lambda (itm) (start-t-re (item-t-start itm)))
		       (lambda (itm) (end-t-re (item-t-end itm))))))
		(position-if
		 (lambda (curr-item)
		   (gi-match-re-to-region
		    (apply extract-re (list curr-item))
		    match-start-pos match-end-pos))
		 (items-t-items gi-items)))))

      ;; Return the position of the match and the item index, unless we
      ;; failed to find one, which is an error.
      (if match-idx
	  (list match-start-pos-past-ws match-idx)
	(error
	 (format (concat "Internal error (in seek-item-back): "
			 "match was found at %d for 'any item but "
			 "could not determine index of match.")
		 match-start-pos)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Automatically re-compile the items' regexps when this file is
;;;; evaluated or compiled.
(gi-compile-items)
