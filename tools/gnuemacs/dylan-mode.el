;;; dylan-mode.el Implements indentation and basic support for Dylan (tm)
;;; programs.

;;; Copyright (C) 1994, 1995, 1996  Carnegie Mellon University
;;;
;;; Bug reports, questions, comments, and suggestions should be sent by
;;; E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
;;;
;;; Author: Robert Stockton (rgs@cs.cmu.edu)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to "gwydion-bugs@cs.cmu.edu")
;;; or from the Free Software Foundation, Inc., 675 Mass Ave,
;;; Cambridge, MA 02139, USA.

;;; User modifiable variables
(defvar dylan-indent 2
  "* Number of spaces to indent each sub-block.")
(defvar dylan-outdent-arrows t
  "* Should '=>' in function definitions be treated specially?")
(defvar dylan-highlight-function-calls nil
  "* Should font-lock mode try to distinguish all normal function calls?")
(defvar dylan-highlight-defsites t
  "* Should font-lock mode try to distinguish all variable bindings?")
(defvar dylan-no-highlights-in-header (and (boundp 'emacs-minor-version)
					   (> emacs-minor-version 30))
  "* Should font-lock ignore keywords in the header.  (Experimental -- may
  not work on all EMACSen.")

;;; Version 1.16
;;; History:
;;;   version 0.1: Quick one day hack -- appears to be useful
;;;   version 0.2: Added font lock support
;;;   version 0.3: Added misc features to work towards "industrial strength"
;;;     Detects "continuation lines" and indents them specially
;;;     Basic comment support
;;;     Added "symbol character" support (with second syntax table for
;;;     indentation and font-lock)
;;;     Added indentation support for "elseif" and "exception" clauses
;;;     Cleaned up a number of old bugs
;;;   version 0.4: Brought into compliance with new "post-DN22" syntax
;;;     new comment syntax
;;;     new "return types" syntax
;;;     accepts sealed, open, concrete, and abstract in class definitions
;;;     fixed bug in comment indentation
;;;     fine tune font-lock-regexps for "define ..."
;;;   version 0.5:
;;;     Added "dylan-insert-block-end" function.
;;;     Fixed bug in short circuiting indentation outside top level forms.
;;;   version 1.0:
;;;     Major code reorganization
;;;     Added full case statement support
;;;     Fixed "continuations" at top level
;;;     Added "beginning-of-form" and "end-of-form" commands
;;;     Fixed support for character literals and for "quoted" quote chars
;;;   version 1.1:
;;;     The "font-lock-mode" support no longer interferes with other language
;;;     modes.   (Thanks to emg@hip.atr.co.jp)
;;;   version 1.2:
;;;     Fixes for various bugs (thanks to wlott@cs.cmu.edu):
;;;       "foo-end;" was mistaken for the end of a compound statement
;;;       syntax tables sometimes ended in an odd state after errors
;;;       indentation sometimes failed if parens weren't balanced
;;;   version 1.3:
;;;     Added font lock support for "sealed", "open", etc.
;;;   version 1.4:
;;;     Added special-case support for generic function "continuations" and
;;;     for outdenting "=>" in function definitions.
;;;   version 1.5:
;;;     Adjusted regexps to accept "primary" and "free" adjectives
;;;     Mentioned dylan-outdent-arrows in the documentation
;;;     Added a space to comment-start
;;;   version 1.6:
;;;     Fixed bug in generic function continuations from 1.4.
;;;   version 1.7:
;;;     Merged changes from Joseph Wilson (jnw@cis.ufl.edu) to facilitate use 
;;;     within more general modes.
;;;   version 1.8:
;;;     Font lock fix for XEmacs from John Shen <jshen@cas.org>.
;;;   version 1.9:
;;;     Fixed bug in indentation for expressions in square and curly braces.
;;;     Generalized modifier-word handling for definitions.
;;;     Generalized 'define words' lists for easier extension.
;;;     Fixed "exceeded max nesting" bug with long lists of items.
;;;     Added switches for font-lock highlighting of functions and definition 
;;;     sites.
;;;   version 1.10:
;;;     Fixed bug in "," reindent code.  It couldn't deal with commas
;;;     in the middle of strings.
;;;   version 1.11 12/13/95 by David N. Gray <gray@harlequin.com>:
;;;     Add C-M-a and C-M-e for beginning and end of definition.
;;;     Fix font lock syntax table for XEmacs.
;;;   version 1.12:
;;;     Added support for "define function"
;;;     Fixed to ignore keywords in the file header.
;;;     Do not require ";" after return value (contributed by
;;;     gray@harlequin.com)
;;;     fixed various bugs resulting from overzealous acceptance of newlines:
;;;       the word "end" at the end of a comment line caused bad indentation
;;;       empty module and library definitions did strange things.
;;;   version 1.13:
;;;     Fixed dylan-insert-block-end to handle "define function" properly.
;;;     (Hopefully) fixed bug in indenting function result declarations,
;;;     which was introduced by the previous round of fixes.
;;;   version 1.14:
;;;     Modified to use font-lock-syntax-table if it is defined.  This
;;;     eliminates the need to use unportable constructs to modify the
;;;     behavior of font-lock mode -- thus, fontification should now be
;;;     reliable on modern EMACSen.
;;;   version 1.15
;;;     Fixed syntax table bugs which primarily affected gnu-emacs
;;;     19.[23] users.  
;;;     Optimized "beyond-dylan-header".  
;;;     Removed new-lines from various font-lock regexps so that
;;;     adjacent declarations aren't glommed together.
;;;   version 1.16
;;;     Made symbols properly fontify as strings.
;;;     Added the dylan-no-highlights-in-header variable (enabled by
;;;     default) which keeps keywords in headers from being treated
;;;     specially.
;;;   adjusted 12/6/96 by David N. Gray to set dylan-no-highlights-in-header
;;;     only for Emacs 19.31 or later
;;;   Modified 7/19/98 by David N. Gray to indent bodies of "with-..." macros.

;;; Known limitations:
;;;   Limited support for block (i.e. "/*") comments
;;;     TAB indentation doesn't work within "/*" comments
;;;   Magic => support doesn't work at end of buffer

;;; Desired features:
;;;   Copy indentation from first statement in body
;;;   Delete-backward-expanding-tabs
;;;   More consistency in font-lock highlighting
;;;   Better support for "/*" comments

;;; Private definitions.  Extensible by using add-dylan-keyword in
;;; your dylan-mode-hook.

(defun add-dylan-keyword (variable keyword)
  (add-to-list variable keyword)
  (set-dylan-patterns)
  (if (fboundp 'font-lock-mode)
      (setq font-lock-keywords dylan-font-lock-keywords)))

(defvar dyl-unnamed-definition-words
  '("interface")
  "Words which introduce unnamed definitions like 'define interface'.")

(defvar dyl-named-definition-words
  '("module" "library" "macro")
  "Words which introduce simple named definitions like 'define library'.")

(defvar dyl-parameterized-definition-words
  '("method" "class" "function")
  "Words which introduce trickier definitions like 'define method'.  These
require special definitions to be added to 'dyl-start-expressions'.")

(defvar dyl-simple-definition-words
  '("constant" "variable" "generic")
  "Words which introduce simple definitions (without implicit bodies).");

(defvar dyl-statement-words
  '("if" "block" "begin" "method" "case" "for" "select" "when" "unless"
    "until" "while" "%call-c-function")
  "Words which begin statements with implicit bodies.")

(defvar common-dyl-macro-words
  '("iterate" "collecting")
  "Names of common Dylan macros with implicit bodies.")

;; Names beginning "with-" are commonly used as statement macros.
(defvar dyl-statement-prefixes "\\|\\bwith-[-_a-zA-Z?!*@<>$%]+")

(defvar dyl-separator-keywords
  '("finally" "exception" "cleanup" "afterwards" "else" "elseif")
  "Patterns act as separators in compound statements.  This may include any
general pattern which must be indented specially.")

(defvar dyl-other-keywords
  '("above" "below" "by" "from"
    "handler" "in" "instance" "let" "local" "otherwise"
    "slot" "subclass" "then" "to" "virtual")
  "Keywords which do not require special indentation handling, but which
should be highlighted if this capability exists.")

(defun dylan-mode-commands (map)
  (define-key map ";" 'dylan-insert-and-indent)
  (define-key map "," 'dylan-insert-and-indent)
  (define-key map ">" 'dylan-arrow-insert)
  (define-key map "\n" 'dylan-newline-and-indent)
  (define-key map "\t" 'dylan-indent-line)
  (define-key map "\ea" 'dylan-beginning-of-form)
  (define-key map "\ee" 'dylan-end-of-form)
  (define-key map "\e)" 'dylan-insert-block-end)
  (define-key map "\e\C-a" 'dylan-beginning-of-defun)
  (define-key map "\e\C-e" 'dylan-end-of-defun)
  (define-key map "\e\C-h" 'mark-dylan-function)
)


(defvar dylan-mode-map ()
  "Keymap used in dylan mode.")
(if (not dylan-mode-map)
    (progn
      (setq dylan-mode-map (make-sparse-keymap))
      (dylan-mode-commands dylan-mode-map)))

(defvar dylan-mode-abbrev-table nil
  "Abbrev table in use in dylan-mode buffers.  Provides 'hooked' 
abbreviations to reindent lines containing 'separator' keywords.")
(if (not dylan-mode-abbrev-table)
    (progn
      (define-abbrev-table 'dylan-mode-abbrev-table ())
      (define-abbrev dylan-mode-abbrev-table "end" "end" 'reindent-line)
      (let ((list dyl-separator-keywords))
	(while list
	  (define-abbrev dylan-mode-abbrev-table
	    (car list) (car list) 'reindent-line)
	  (setq list (cdr list))))))

(defvar dylan-mode-syntax-table nil
  "User level syntax table.  Provides support for forward-word, etc.")
(defvar dylan-indent-syntax-table nil
  "Special syntax table which is used by the indent and font-lock code 
for finding keywords and the like.  This is necessary because there is
no equivalent to '\b' for identifiers.")

(if (not dylan-mode-syntax-table)
    (progn
      (setq dylan-mode-syntax-table (make-syntax-table))
      (modify-syntax-entry ?_ "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?- "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?< "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?> "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?? "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?! "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?= "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?: "_" dylan-mode-syntax-table)
      (modify-syntax-entry ?' "\"" dylan-mode-syntax-table)
      (modify-syntax-entry ?\f " " dylan-mode-syntax-table)
      (setq dylan-indent-syntax-table
	    (copy-syntax-table dylan-mode-syntax-table))
      (modify-syntax-entry ?_ "w" dylan-indent-syntax-table)
      (modify-syntax-entry ?- "w" dylan-indent-syntax-table)
      (modify-syntax-entry ?< "w" dylan-indent-syntax-table)
      (modify-syntax-entry ?> "w" dylan-indent-syntax-table)
      (modify-syntax-entry ?? "w" dylan-indent-syntax-table)
      (modify-syntax-entry ?! "w" dylan-indent-syntax-table)
      (modify-syntax-entry ?= "w" dylan-indent-syntax-table)
      (modify-syntax-entry ?: "w" dylan-indent-syntax-table)
      ; different emacs version handle comments differently
      (cond ((or (and (boundp 'running-lemacs) running-lemacs)
		 (string-match "XEmacs" emacs-version))
	     (modify-syntax-entry ?\n "> b" dylan-indent-syntax-table)
	     (modify-syntax-entry ?/ "w 1456" dylan-indent-syntax-table)
	     (modify-syntax-entry ?\* "w 23" dylan-indent-syntax-table)
	     (modify-syntax-entry ?\n "> b" dylan-mode-syntax-table)
	     (modify-syntax-entry ?/ "_ 1456" dylan-mode-syntax-table)
	     (modify-syntax-entry ?\* "_ 23" dylan-mode-syntax-table))
	    ((string-lessp emacs-version "19")
	     ; emacs 18 doesn't have sufficient support to grok "//" comments
	     ; so we must (regretfully) leave them out
	     (modify-syntax-entry ?/ "_ 14" dylan-mode-syntax-table)
	     (modify-syntax-entry ?\* "_ 23" dylan-mode-syntax-table)
	     (modify-syntax-entry ?/ "w 14" dylan-indent-syntax-table)
	     (modify-syntax-entry ?\* "w 23" dylan-indent-syntax-table))
	    (t
	     (modify-syntax-entry ?\n ">" dylan-mode-syntax-table)
	     (modify-syntax-entry ?/ "_ 124" dylan-mode-syntax-table)
	     (modify-syntax-entry ?\* "_ 23b" dylan-mode-syntax-table)
	     (modify-syntax-entry ?\n ">" dylan-indent-syntax-table)
	     (modify-syntax-entry ?/ "w 124" dylan-indent-syntax-table)
	     (modify-syntax-entry ?\* "w 23b" dylan-indent-syntax-table)))))

;;; Ugly code which you don't want to look at.
(defvar dylan-comment-pattern "//.*$"
  "Internal pattern for finding comments in dylan code.  Currently only
handles end-of-line comments.")

(defun make-pattern (start &rest list)
  "Builds a search pattern that matches any of the patterns passed to it.
Makes sure that it doesn't match partial words."
  (let ((str (concat "\\b" start "\\b")))
    (while list
      (setq str (concat str "\\|\\b" (car list) "\\b"))
      (setq list (cdr list)))
    str))

(defvar dyl-start-expressions '()
  "Patterns which match that portion of a 'compound statement' which precedes
the 'body'.  This is used to determine where the first statement 
begins for indentation purposes.  

Contains a list of patterns, each of which is either a regular 
expression or a list of regular expressions.  A set of balanced 
parens will be matched between each list element.")

(defvar dylan-font-lock-keywords nil
  "Value to which font-lock-keywords should be set when in dylan-mode")

(defvar dyl-definition-words nil)
(defvar dyl-definition-pattern nil)
(defvar dyl-named-definition-pattern nil)
(defvar dyl-unnamed-definition-pattern nil)
(defvar dyl-parameterized-definition-pattern nil)
(defvar dyl-end-keyword-pattern nil)
(defvar separator-word-pattern nil)
(defvar dyl-simple-definition-pattern nil)
(defvar dyl-other-pattern nil)
(defvar dyl-keyword-pattern nil)
(defvar find-keyword-pattern nil)
(defvar dylan-beginning-of-form-pattern nil)

(defun set-dylan-patterns ()
  (setq dyl-definition-words 
	(append dyl-unnamed-definition-words dyl-named-definition-words
		dyl-parameterized-definition-words))
  (setq dyl-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-definition-words) "\\)"))
  (setq dyl-named-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-named-definition-words) "\\)"))
  (setq dyl-unnamed-definition-pattern
	(concat "\\("
		(apply 'make-pattern dyl-unnamed-definition-words)
		"\\)"))
  (setq dyl-parameterized-definition-pattern
	(concat "\\("
		(apply 'make-pattern dyl-parameterized-definition-words)
		"\\)"))
  (setq dyl-keyword-pattern
	;; we disallow newlines in "define foo" patterns because it
	;; allows the actual keword to be confused for a qualifier if
	;; another definition follows closely.
	(concat
	 (apply 'make-pattern
	       (concat "define\\([ \t]+\\w+\\)*[ \t]+"
		       dyl-definition-pattern)
	       (append dyl-statement-words common-dyl-macro-words))
	 dyl-statement-prefixes))
  (setq dyl-end-keyword-pattern
	; we intentionally disallow newlines in "end foo" constructs,
	; because doing so makes it very difficult to deal with the
	; keyword "end" in comments.
	(concat "\\bend\\b[ \t]*\\("	
		(apply 'make-pattern
		       (append dyl-definition-words
			       dyl-statement-words
			       common-dyl-macro-words))
		dyl-statement-prefixes
		"\\)?"))
  (setq separator-word-pattern (apply 'make-pattern dyl-separator-keywords))
  (setq dyl-simple-definition-pattern
	(concat "\\(" (apply 'make-pattern dyl-simple-definition-words) "\\)"))
  (setq dyl-other-pattern
	(apply 'make-pattern
	       (concat "define\\([ \t\n]+\\w+\\)*[ \t\n]+"
		       dyl-simple-definition-pattern)
	       dyl-other-keywords))
  (setq dyl-start-expressions
	(list '("if[ \t\n]*" "")
	      '("block[ \t\n]*" "")
	      '("for[ \t\n]*" "")
	      '("select[ \t\n]*" "")
	      '("when[ \t\n]*" "")
	      '("unless[ \t\n]*" "")
	      '("until[ \t\n]*" "")
	      '("while[ \t\n]*" "")
	      '("iterate[ \t\n]*" "")
	      '("collecting[ \t\n]*" "")
	      ;; special patterns for "define method" which is funky
	      '("\\(define\\([ \t]+\\w+\\)*[ \t]+\\)?\\(method\\|function\\)[ \t\n]+[^\( ]*[ \t\n]*" "[ \t\n]*=>[^;)]+;?")
	      '("\\(define\\([ \t]+\\w+\\)*[ \t]+\\)?\\(method\\|function\\)[ \t\n]+[^\( ]*[ \t\n]*" "[ \t\n]*;")
	      (concat "define[ \t]+" dyl-named-definition-pattern
		      "[ \t\n]+[^ \t\n]+")
	      (concat "define[ \t]+" dyl-unnamed-definition-pattern)
	      (list (concat "\\(define\\([ \t]+\\w+\\)*[ \t]+\\)?"
			    dyl-parameterized-definition-pattern
			    "[ \t\n]+[^\( ]*[ \t\n]*")
		    "")
	      "begin" "case"
	      ;; Since we don't know the syntax of all the "with-" macros,
	      ;; just assume that the user has already split the line at
	      ;; the end of the header.
	      "with-[^\n]*"
	      "[[({]"))
  (setq find-keyword-pattern (concat "[][)(}{\"']\\|\\bdefine\\b\\|"
				     dyl-end-keyword-pattern 
				     "\\|" dyl-keyword-pattern))
  (setq dylan-beginning-of-form-pattern (concat "[;,]\\|=>\\|"
						find-keyword-pattern
						"\\|" separator-word-pattern))

  (if (fboundp 'font-lock-mode)
      (progn
	;; See font-lock-mode for details.  It's ugly, but it works.
	(setq dylan-font-lock-keywords
	      (list dyl-end-keyword-pattern
		    dyl-keyword-pattern
		    separator-word-pattern
		    "[-_a-zA-Z?!*@<>$%]+:"
		    (list "#\"[^\"]*\"?" 0 'font-lock-string-face t)
		    "#rest\\|#key\\|#all-keys\\|#next"
		    dyl-other-pattern
		    (list (concat "\\b\\(define\\([ \t]+\\w+\\)*[ \t]+"
				  dyl-simple-definition-pattern
				  "\\)\\b[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
			  4 'font-lock-function-name-face)
		    (list (concat "\\b\\(define\\([ \t]+\\w+\\)*[ \t]+"
				  dyl-definition-pattern "\\)")
			  1 'font-lock-keyword-face t)
		    (list (concat "\\b\\(define\\([ \t]+\\w+\\)*[ \t]+"
				  dyl-definition-pattern
				  "\\)\\b[ \t]+\\(\\(\\s_\\|\\w\\)+\\)")
			  4 'font-lock-function-name-face)
		    '("method[ \t\n]+\\(\\w+\\)" 1 font-lock-function-name-face)
		    '("\\bend[ \t]+\\w*\\b[ \t]+\\(\\(\\s_\\|\\w\\)+\\)" 1
		      font-lock-function-name-face)))
	(if dylan-no-highlights-in-header
	    (setq dylan-font-lock-keywords
		  (append dylan-font-lock-keywords
			  (list (list 'fontify-dylan-header 0 nil t)))))
	(if dylan-highlight-function-calls
	    (setq dylan-font-lock-keywords
		  (cons 
		   '("\\b\\(\\(\\s_\\|\\w\\)+\\)(" 1 font-lock-function-name-face)
		   dylan-font-lock-keywords)))
	(if dylan-highlight-defsites
	    (setq dylan-font-lock-keywords
		  (append
		   dylan-font-lock-keywords
		   (list
		    '("slot[ \t\n]+\\(\\w+\\)" 1 font-lock-function-name-face)
		    '("block[ \t\n]+(\\([^)]+\\)"
		      1 font-lock-function-name-face)
		    '("let[ \t\n]+\\(\\w+\\)" 1 font-lock-function-name-face)
		    '("let[ \t\n]+(\\([^)]+\\)"
		      1 font-lock-function-name-face))))))))

(defun look-back (regexp)
  "Attempts to find a match for \"regexp\" immediately preceding the current
point.  In order for this to work properly, the search string must end with
'$'.  Also note that this will only work within the current line."
  (save-excursion
    (save-restriction
      (let ((dot (point)))
	(beginning-of-line)
	(narrow-to-region dot (point))
	(re-search-forward regexp nil t)))))

(defvar find-keyword-pattern nil
  "A pattern which matches the beginnings and ends of various 'blocks',
including parenthesized expressions.")

(defvar dylan-beginning-of-form-pattern nil
  "Like 'find-keyword-pattern' but matches statement terminators as well.")

;(defun fontify-dylan-header (limit)
;  (if (>= (point) (- limit 1))
;      nil
;    (if (= limit (point-max))
;	(re-search-forward "\\`\\([-a-zA-Z]+:.*\n\\([ \t]+.*\n\\)*\\)*\\([-a-zA-Z]+:.*\\|[ \t]+.*\\)" limit t)
;      (goto-char 1)
;      (let ((result (re-search-forward
;		     "\\`\\([-a-zA-Z]+:.*\n\\([ \t]+.*\n\\)*\\)*\\([-a-zA-Z]+:.*\\|[ \t]+.*\\)"
;		     limit t)))
;	(and result (= result (- limit 1)) result)))))

(defun fontify-dylan-header (limit)
  (let ((end (dylan-header-end)))
    (and (> end (point))
	 (re-search-forward ".+" end t))))

(defun dylan-header-end ()
  (save-excursion
    (goto-char 1)
    (or
     (and (re-search-forward "\\`\\([-a-zA-Z]+:.*\n\\([ \t]+.*\n\\)*\\)*\n+"
			     nil t)
	  ;; in Emacs 18, the search just returns `t', not the point.
	  (point))
     0)))

;; The next two routines are organized bletcherously because gnu-emacs
;; does no tail call optimization.  We used to recursively call
;; dylan-find-keyword if we found a spurious endpoint (i.e. a comma,
;; semicolon, etc.), but this exceeded the maximum emacs stack depth
;; (which is clearly pretty low).
;;
(defun dylan-find-keyword (&optional match-statement-end in-case no-commas
				     start)
  "Moves the point backward to the beginning of the innermost enclosing
'compound statement' or set of parentheses.  Returns t on success and
nil otherwise."
  (let ((result 'not-found))
    (while (eq result 'not-found)
      (setq
       result
       (if (re-search-backward (if match-statement-end
				   dylan-beginning-of-form-pattern
				 find-keyword-pattern) nil t)
	   (cond ((look-back dylan-comment-pattern)
		  (goto-char (match-beginning 0))
		  'not-found)
		 ((looking-at "[])}'\"]")
		  (condition-case nil
		      (progn 
			(forward-char 1)
			(backward-sexp 1)
			'not-found)
		    (error nil)))
		 ((and (looking-at "define")	; non-nesting top level form
		       (not (looking-at dyl-keyword-pattern)))
		  nil)
		 ((or (looking-at "end")
		      (and (look-back "\\bend[ \t]+$") (backward-word 1)))
		  (dylan-find-keyword)
		  (if (or (and (looking-at "method")
			       (look-back "define\\([ \t\n]+\\w+\\)*[ \t]+$"))
			  (looking-at "define"))
		      nil
		    'not-found))
	; hack for overloaded uses of "while" and "until" reserved words
		 ((or (looking-at "until") (looking-at "while"))
		  (if (save-excursion
			(condition-case nil
			    (progn 
			      (backward-up-list 1)
			      (backward-sexp 1)
			      (looking-at "for\\b")) (error nil)))
		      (backward-up-list 1))
		  t)
		 ((and (looking-at separator-word-pattern)
		       (not match-statement-end))
		  'not-found)
		 ((and (looking-at ";") (not match-statement-end))
		  'not-found)
		 ((and (looking-at ",")
		       (or (not match-statement-end) no-commas))
		  'not-found)
		 ((and (looking-at "=>")
		       (not (and match-statement-end in-case)))
		  'not-found)
		 (t t))
	 (goto-char (point-min))
	 nil)))
    (and result (>= (point) (dylan-header-end)))))

(defun dylan-find-end (&optional match-statement-end in-case no-commas)
  "Moves the point forward to the end of the innermost enclosing
'compound statement' or set of parentheses.  Returns t on success and
nil otherwise."
  (let ((result 'not-found))
    (while (eq result 'not-found)
      (setq
       result
       (if (re-search-forward (if match-statement-end
				  dylan-beginning-of-form-pattern
				find-keyword-pattern) nil t)
	   (let ((match-start (match-beginning 0)))
	     (cond ((look-back dylan-comment-pattern)
		    (forward-line)
		    'not-found)
		   ((look-back "[[({'\"]$")
		    (condition-case nil
			(progn 
			  (backward-char 1)
			  (forward-sexp 1)
			  'not-found)
		      (error nil)))
		   ((look-back "[])}]$") t)
		   ((look-back "define$") ; special case for top-level forms
		    (dylan-find-end t nil nil)
		    nil)
		   ((look-back "\\bend\\([ \t]+\\w+\\)?$")
		    (if (and (not (looking-at "[ \t]+\\(end\\|=>\\)\\b"))
			     (looking-at "[ \t]+\\w+"))
			(goto-char (match-end 0)))
		    t)
		   ; hack for overloaded uses of "while" and "until"
		   ; reserved words
		   ((look-back "until$\\|while$")
		    (if (save-excursion
			  (condition-case nil
			      (progn 
				(backward-up-list 1)
				(backward-sexp 1)
				(looking-at "for\\b")) (error nil)))
			(up-list 1))
		    t)
		   ((save-excursion (goto-char match-start)
				    (looking-at separator-word-pattern))
		    t)
		   ((look-back ";$")
		    (if (not match-statement-end)
			'not-found
		      t))
		   ((look-back ",$")
		    (if (or (not match-statement-end) no-commas)
			'not-found
		      t))
		   ((look-back "=>$")
		    (if (not (and match-statement-end in-case))
			'not-found
		      t))
		   (t				; start compound statement
		    (if (save-excursion (goto-char match-start)
					(looking-at "define"))
			(progn (dylan-find-end) nil)
		      (dylan-find-end)
		      'not-found))))
	 (goto-char (point-max))
	 nil)))
    result))

(defun dylan-skip-star-comment-backward ()
  "Utility function for 'dylan-skip-whitespace-backward'.  Finds beginning
of enclosing '/*' comment.  Deals properly with nested '/*' and with '//'."
  (re-search-backward "/\\*\\|\\*/")
  (while (cond ((look-back dylan-comment-pattern)
		(goto-char (match-beginning 0)))
	       ((looking-at "\\*/")
		(dylan-skip-star-comment-backward))
	       (t nil))
    (re-search-backward "/\\*\\|\\*/"))
  t)

(defun dylan-skip-star-comment-forward ()
  "Utility function for 'dylan-skip-whitespace-forward'.  Finds end
of enclosing '/*' comment.  Deals properly with nested '/*' and with '//'."
  (re-search-forward "/\\*\\|\\*/")
  (while (cond ((look-back dylan-comment-pattern)
		(end-of-line))
	       ((look-back "/\\*$")
		(dylan-skip-star-comment-forward))
	       (t nil))
    (re-search-forward "/\\*\\|\\*/"))
  t)

(defvar non-whitespace-string "\\s_\\|\\s(\\|\\s\"\\|\\s$\\|\\s<\\|\\s/\\|\\sw\\|\\s.\\|\\s)\\|\\s'\\|\\s\\"
  "A magic search string which matches everything but 'whitespace'.  Used
because old version of emacs don't have 'skip-syntax-backward'.")

(defun dylan-skip-whitespace-backward ()
  "Skips over both varieties of comments and other whitespace characters."
  ; skip syntactic whitespace
  (if (re-search-backward non-whitespace-string nil t)
      (forward-char)
    (goto-char 0))
  ; skip comments
  (while (cond ((look-back dylan-comment-pattern)
		(goto-char (match-beginning 0)))
	       ((look-back "\\*/$")
		(goto-char (match-beginning 0))
		(dylan-skip-star-comment-backward))
	       (t nil))
    (if (re-search-backward non-whitespace-string nil t)
	(forward-char)
      (goto-char 0))))

(defun dylan-skip-whitespace-forward ()
  "Skips over both varieties of comments and other whitespace characters."
  ; skip syntactic whitespace
  (re-search-forward "\\(\\s \\|\\s>\\)*")
  ; skip comments
  (while (cond ((looking-at dylan-comment-pattern)
		(goto-char (match-end 0))
		t)
	       ((looking-at "/\\*")
		(goto-char (match-end 0))
		(dylan-skip-star-comment-forward))
	       (t nil))
    (re-search-forward "\\(\\s \\|\\s>\\)*")))

(defun aux-find-body-start (clauses)
  "Helper function for 'find-body-start'"
  (save-excursion
    (cond ((null clauses) (point))
	  ((looking-at (car clauses))
	   (if (null (cdr clauses))
	       (match-end 0)
	     (goto-char (match-end 0))
	     (and (looking-at "[[({]")
		  (condition-case nil (forward-list) (error nil))
		  (aux-find-body-start (cdr clauses))))))))

(defun find-body-start (exprs)
  "When passed 'dyl-start-expressions', processes it to find the beginning
of the first statment in the compound statement which starts at the 
current point."
  (cond ((null exprs) (point-max))
	((listp (car exprs))
	 (or (aux-find-body-start (car exprs)) (find-body-start (cdr exprs))))
	(t (if (looking-at (car exprs))
	       (match-end 0)
	     (find-body-start (cdr exprs))))))

(defun backward-dylan-statement (&optional in-case no-commas)
  "Moves the cursor to some undefined point between the previous 'statement'
and the current one.  If we are already between statements, move back one 
more."
  (unwind-protect
      ;; Because "\b" doesn't work with "symbol-chars" we temporarily
      ;; install a new syntax table and restore the old one when done
      (progn
	(set-syntax-table dylan-indent-syntax-table)
	(dylan-skip-whitespace-backward)
	(let* ((dot (point)))
	  ;; skip over "separator words"
	  (if (save-excursion
		(and (re-search-backward separator-word-pattern nil t)
		     (if (not (looking-at "exception\\|elseif"))
			 (forward-word 1)
		       (goto-char (match-end 0))
		       (condition-case nil (forward-list 1)
			 (error nil))
		       t)
		     (>= (point) dot)))
	      (progn (re-search-backward separator-word-pattern nil t)
		     (dylan-skip-whitespace-backward)))
	  (if (look-back "[,;]$\\|=>$")
	      (backward-char))
	  (cond ((not (dylan-find-keyword t in-case no-commas))
		 (if (look-back "\\(define\\|local\\)[ \t]+")	; hack
		     (goto-char (match-beginning 0))))
		((looking-at separator-word-pattern)
		 (let ((start (point)))
		   (cond ((looking-at "\\(exception\\|elseif\\)[ \t\n]*(")
			  (goto-char (match-end 1))
			  (condition-case nil (forward-list 1)
			    (error nil)))
			 (t (forward-word 1)))
		   (if (>= (point) dot)
		       (progn (goto-char start)
			      (backward-dylan-statement in-case no-commas)))))
		((looking-at "[;,]\\|=>")
		 (goto-char (match-end 0)))
		(t
		 ;; check whether we were already at the first "form" in an
		 ;; enclosing block
		 (let ((first (find-body-start dyl-start-expressions)))
		   (if (< first dot)
		       (goto-char first)
		     (if (look-back "\\(define\\|local\\)[ \t]+")	; hack
			 (goto-char (match-beginning 0)))))))))
    (set-syntax-table dylan-mode-syntax-table)))

(defun dylan-beginning-of-form ()
  "Finds the beginning of the innermost 'statement' which contains or
terminates at the current point."
  (interactive)
  (backward-dylan-statement)
  (dylan-skip-whitespace-forward))

(defun forward-dylan-statement (&optional in-case no-commas)
  "Moves the cursor to some undefined point between the current 'statement'
and the next one.  If we are already between statements, move forward one 
more."
  (unwind-protect
      ;; Because "\b" doesn't work with "symbol-chars" we temporarily
      ;; install a new syntax table and restore the old one when done
      (progn
	(set-syntax-table dylan-indent-syntax-table)
	(dylan-skip-whitespace-forward)
	(let* ((dot (point)))
	  ;; skip over "separator words"
	  (if (looking-at separator-word-pattern)
	      (if (not (looking-at "exception\\|elseif"))
			 (forward-word 1)
		       (goto-char (match-end 0))
		       (condition-case nil (forward-list 1)
			 (error nil))))
	  (cond ((not (dylan-find-end t in-case no-commas))
		 (if (look-back "\\(define\\|local\\)[ \t]+")	; hack
		     (goto-char (match-beginning 0))))
		(t)))
	(cond ((looking-at "[,;]$") (forward-char))
	      ((looking-at "=>") (forward-word 1))))
    (set-syntax-table dylan-mode-syntax-table)))

(defun dylan-end-of-form ()
  "Finds the end of the innermost 'statement' which contains or begins
at the current point."
  (interactive)
  (forward-dylan-statement))

(defun indent-if-continuation (term-char line-start block-start
					 &optional in-case in-paren)
  (save-excursion
    (goto-char line-start)
    (let ((arrow (and dylan-outdent-arrows (looking-at "=>"))))
      (dylan-skip-whitespace-backward)
      (if (look-back "finally$")	; special case -- this one is tricky
	  0				; because "for" can have empty bodies
	(let ((real-start (point)))
	  (backward-dylan-statement in-case)
	  (dylan-skip-whitespace-forward)
	  (cond ((and (= block-start 0) (not (looking-at "define")))
		 0)			; special case for beginning of file
		((= real-start block-start) 0)
		((< (point) block-start)
		 (+ dylan-indent (if (and arrow (not in-case)) -3 0)))
		((< (save-excursion
		      (forward-dylan-statement in-case
					       (equal term-char ";"))
		      (point)) line-start)
		 0)
		;; Give continuations of generic functions extra
		;; indentation to match what happens with method
		;; declarations.  This is an odd special case, but some
		;; folks like it.  If you don't, comment out the next 3
		;; lines.
		((looking-at
		  "define\\([ \t\n]+\\w+\\)*[ \t]+generic")
		 (+ dylan-indent dylan-indent (if arrow -3 0)))
		(t dylan-indent)))))))

(defun dylan-indent-line (&optional ignore-case extra-indent)
  "Indents a line of dylan code according to its nesting."
  ;; The "ignore-case" and "extra-indent" vars are used for recursive
  ;; calls so that the special code for handling case statements won't
  ;; recurse infinitely.
  (interactive)
  (setq extra-indent (or extra-indent 0))
  (unwind-protect
      (save-excursion
	;; Because "\b" doesn't work with "symbol-chars" we temporarily
	;; install a new syntax table and restore the old one when done
	(set-syntax-table dylan-indent-syntax-table)
	(beginning-of-line)
	(delete-horizontal-space)
	(let* ((body-start)		; beginning of "body" of enclosing
					; compound statement
	       (was-paren)		; t if in parenthesized expr.
	       (in-case)		; t if in "case" or "select" stmt
	       (block-indent		; indentation of enclosing comp. stmt
		(save-excursion
		  (if (not (dylan-find-keyword))
		      nil
		    (and (looking-at "method")
			 (look-back "define\\([ \t\n]+\\w+\\)*[ \t]+$")
			 (goto-char (match-beginning 0)))
		    (and (looking-at "[[({]") (setq was-paren t))
		    (and (looking-at "select\\|case") (setq in-case t))
		    (setq body-start (find-body-start dyl-start-expressions))
		    (+ (current-column) extra-indent))))
	       (indent			; correct indentation for this line
		(cond ((not block-indent)
		       (indent-if-continuation ";" (point) 0))
		      ;; some keywords line up with start of comp. stmt 
		      ((looking-at separator-word-pattern) block-indent)
		      ;; end keywords line up with start of comp. stmt 
		      ((looking-at dyl-end-keyword-pattern) block-indent)
		      ;; parenthesized expressions (separated by commas)
		      (in-case
		       ; if the line is blank, we pick an arbitrary
		       ; indentation for now.  We judge the "proper"
		       ; indentation by how the statement is punctuated once
		       ; it is finished
		       (cond ((looking-at "^$")
			      (if (save-excursion
				    ; Look for end of prev statement.  This
				    ; is hairier than it should be because
				    ; we may be at the end of the buffer
				    (let ((dot (point)))
				      (forward-dylan-statement t)
				      (dylan-skip-whitespace-backward)
				      (if (> (point) dot)
					  (backward-dylan-statement t))
				      (look-back ";$\\|=>$")))
				  (+ block-indent dylan-indent dylan-indent
				     (indent-if-continuation "," (point)
							     body-start t))
				(+ block-indent dylan-indent 
				   (indent-if-continuation "," (point)
							   body-start t))))
			     ((save-excursion
				(forward-dylan-statement t)
				(look-back ",$\\|=>$"))
			      (+ block-indent dylan-indent 
				 (indent-if-continuation "," (point)
							 body-start t)))
			     (t (+ block-indent dylan-indent dylan-indent
				   (indent-if-continuation "," (point)
							   body-start t)))))
		      (was-paren (+ block-indent 1
				    (indent-if-continuation "," (point)
							    body-start)))
		      ;; statements (separated by semi-colons)
		      (t (+ block-indent dylan-indent
			    (indent-if-continuation ";" (point)
						    body-start))))))
	  (indent-to-column indent)))
    ;; put the cursor where the user is likely to want it.
    (and (= (current-column) 0) (skip-chars-forward " \t"))
    (set-syntax-table dylan-mode-syntax-table)))

(defun in-case ()
  "Checks to see whether we are immediately nested in a 'case' or 'select'
statement.  Is used to provide special re-indentation for ',', ';', and '=>'."
  (save-excursion
    (dylan-find-keyword)
    (looking-at "case\\|select")))

(defun reindent-line ()
  (interactive)
  (save-excursion (funcall indent-line-function)))

(defun dylan-newline-and-indent ()
  (interactive)
  (expand-abbrev)
  (newline-and-indent))

(if (and (boundp 'running-lemacs) running-lemacs)
    (defun this-command-chars ()
      (events-to-keys (this-command-keys)))
  (defun this-command-chars ()
    (this-command-keys)))

(defun dylan-insert-and-indent ()
  "Make ';' and ',' do re-indentation for case statements."
  (interactive)
  (self-insert-command 1)
  (save-excursion
    ;; These things are finicky around EOF, so use "point-marker" instead
    ;; of "point" so that re-indentations won't yield infinite loops
    (let ((dot (point-marker)))
      (beginning-of-line)
      (if (in-case)
	  (progn
	    (backward-dylan-statement t)
	    (dylan-skip-whitespace-forward)
	    (while (< (point) (marker-position dot))
	      (funcall indent-line-function)
	      (forward-line 1)))))))

(defun dylan-arrow-insert ()
  "Make '=>' do re-indentation for case statements and function declarations."
  (interactive)
  (if (not (= (preceding-char) ?=))
      (self-insert-command 1)
    (self-insert-command 1)
    (save-excursion
      (if (in-case)
	  (let ((dot (point-marker)))
	    (backward-dylan-statement t)
	    (dylan-skip-whitespace-forward)
	    (while (< (point) (marker-position dot))
	      (funcall indent-line-function)
	      (forward-line 1)))
	(funcall indent-line-function)))))


;;; This intensely DWIMish function tries to insert whatever text is needed to
;;; finish off the enclosing indentation context.
(defun dylan-insert-block-end ()
  "Insert whatever text is needed to finish off the enclosing indentation
context (i.e. \"block\").  Makes educated guesses about whether newlines
and closing punctuation are needed."
  (interactive)
  (let* ((here (point))
	 (terminator)
	 (need-newline)
	 (str
	  (unwind-protect
	      (save-excursion
		;; Because "\b" doesn't work with "symbol-chars" we temporarily
		;; install a new syntax table and restore the old one when done
		(set-syntax-table dylan-indent-syntax-table)
		(if (not (dylan-find-keyword))
		    (error "No nesting block."))
		; need newline if multi-line block and not "("
		(setq need-newline (not (or (looking-at "[[({]")
					    (save-excursion (end-of-line)
							    (>= (point) here)))))
		(setq terminator
		      (save-excursion
			(cond ((not (dylan-find-keyword)) ";")
			      ((looking-at "[[({]") "")
			      (t ";"))))
		; We intentionally fail to accept newlines in "define
		; foo" because it can cause undue confusion.
		(if (looking-at
		     (concat "define\\([ \t]+\\w+\\)*[ \t]*"
			     dyl-definition-pattern))	; find the actual word
		    (goto-char (match-beginning 2)))
		(cond ((looking-at "begin") (concat " end" terminator))
		      ((looking-at "\\[") "]")
		      ((looking-at "(") ")")
		      ((looking-at "{") "}")
		      ((or (looking-at "\\(method\\|function\\|class\\)\\([ \t]+\\w+\\)?")
			   (looking-at "\\(library\\|module\\)[ \t]+\\w+")
			   (looking-at "\\w+"))
		       (concat " end "
			       (buffer-substring (match-beginning 0)
						 (match-end 0))
			       terminator))
		      (t (concat " end" terminator))))
	    (set-syntax-table dylan-mode-syntax-table))))
    (if need-newline
	(progn
	  (beginning-of-line)
	  (if (looking-at "[ \t]*$")
	      (delete-horizontal-space)
	    (end-of-line)
	    (newline))))
    (let* ((start (point))
	   (end (progn (insert str) (point))))
      (goto-char start)
      (while (re-search-forward "[ \t\n]+" end t)
	(replace-match " "))
      (goto-char end)
      (reindent-line))))


;; Regular expression for finding the beginning of a definition.
(defvar dylan-defun-regexp)
(setq dylan-defun-regexp
      "^ *\\(define \\|module:\\|interface \\)")

(defvar dylan-defun-end-regexp) ; end of a routine
(setq dylan-defun-end-regexp "^ *end\\s-")

(defun dylan-beginning-of-defun (&optional arg)
  "Move backward to next beginning of definition.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (and arg (< arg 0) (forward-char 1))
  (and (or (re-search-backward dylan-defun-regexp nil t (or arg 1))
	   ;(re-search-backward
	   ;  "^\\(type\\|const\\|var\\|%include\\)\\s-"
	   ;  nil 'move (or arg 1))
	   )
       (progn (beginning-of-line) t)))

(defun dylan-end-of-defun (&optional arg)
  "Move forward to next end of function."
  (interactive "p")
  (let ((old (dot)))
    (dylan-skip-whitespace-forward)
    (end-of-line)
    (let ((next-begin
	   (and (re-search-forward dylan-defun-regexp nil 'move 1)
		;; in Emacs 18, the search just returns `t', not the point.
		(point))))
      (if next-begin
	  (progn
	    (beginning-of-line)
	    (let ((last-end
		   (and (re-search-backward dylan-defun-end-regexp nil 'move 1)
			(point))))
	      (if (and last-end
		       (< last-end next-begin)
		       (> last-end old))
		  (progn
		    (goto-char last-end)
		    (end-of-line)
		    (forward-char)
		    )
		(progn
		  (goto-char next-begin)
		  (beginning-of-line)
		  (backward-char)
		  )
		))
	    (if (and arg (> arg 1))
		(dylan-end-of-defun (1- arg)))
	    t
	    )))))

(defun mark-dylan-function ()
  "Put mark at end of Dylan function, point at beginning."
  (interactive)
  (beginning-of-line)
  (dylan-end-of-defun)
  (push-mark (point) t t)
  (dylan-beginning-of-defun)
  )

(defun dylan-mode-variables ()
  ;; Use value appropriate for font-lock-mode now.  Reset after running hooks.
  (if (fboundp 'font-lock-mode)
      (setq font-lock-keywords dylan-font-lock-keywords))
  (if (not (boundp 'font-lock-syntax-table))
      (set-syntax-table dylan-indent-syntax-table)
    (make-variable-buffer-local 'font-lock-syntax-table)
    (setq font-lock-syntax-table dylan-indent-syntax-table))
  (set-syntax-table dylan-indent-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'dylan-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "//+[ \t]*\\|/\\*[ \t]*")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq local-abbrev-table dylan-mode-abbrev-table)
  (make-local-variable 'after-change-function)
  (setq after-change-function nil)
  (run-hooks 'dylan-mode-hook)
  ;; This is the table the user should always see, even though the indent and
  ;; font lock code both reset it temporarily.
  (set-syntax-table dylan-mode-syntax-table))

(defun dylan-mode ()
  "Major mode for editing dylan programs.

Tab and newline do dylan specific indentation.
'//' comments are handled completely and '/*' comments marginally.
Supports font-lock-mode under emacs 19 and lucid emacs.

The following bindings are available traversing and editing dylan code:
  \\[dylan-beginning-of-form]
	Moves to the beginning of the current 'statement'.
  \\[dylan-end-of-form]
	Moves to the end of the current 'statement'.
  \\[dylan-beginning-of-defun]
	Moves to the beginning of the current definition.
  \\[dylan-end-of-defun]
	Moves to the end of the current definition.
  \\[dylan-insert-block-end]
	Insert the appropriate text to close the current 'block'.

The default indentation level is controlled by the 'dylan-indent' variable.
The default is 2 spaces.

By default, the mode uses a special indentation level for function return 
declarations which lines up parameter declarations with return type 
declarations.  This special feature may be turned off by setting 
'dylan-outdent-arrows' to nil.
\\{dylan-mode-map}"
  (interactive)
  (abbrev-mode 1)
  (use-local-map dylan-mode-map)
  (setq major-mode 'dylan-mode)
  (setq mode-name "dylan")
  (setq local-abbrev-table dylan-mode-abbrev-table)
  (dylan-mode-variables))

(set-dylan-patterns)

;; We must use the "indentation" syntax table when doing font-lock
;; processing.  In modern EMACSen (xemacs 19.14, FSF 19.30), the
;; font-lock-syntax-table variable handles this for us.  In older
;; EMACSen, we need some pretty ugly hacks -- the only thing to be said
;; in their favor is that they often work.
(if (and (fboundp 'font-lock-mode)
	 (require 'font-lock)		; force load to test version
	 (not (boundp 'font-lock-syntax-table)))
    (progn
      (defvar old-after-change-function nil
	"Used to modify the behavior of font-lock-mode.")
      (defun dm-after-change-function (&rest args)
	(let ((old-syntax-table (syntax-table)))
	  (unwind-protect
	      (progn
		(set-syntax-table dylan-indent-syntax-table)
		(apply old-after-change-function args))
	    (set-syntax-table old-syntax-table))))
      ;; More hacks to magically switch syntax tables as necessary
      (if (boundp 'font-lock-after-fontify-buffer-hook)
	  (progn ; new way (in XEmacs 19.13)
	    (add-hook
	     'font-lock-mode-hook
	     '(lambda ()
		(if (not (eq major-mode 'dylan-mode))
		    nil
		  (setq font-lock-keywords dylan-font-lock-keywords)
		  ;; This is to handle fontification updates while editing:
		  (make-variable-buffer-local 'old-after-change-function)
		  (setq old-after-change-function
			'font-lock-after-change-function)
		  (make-variable-buffer-local 'after-change-function)
		  (setq after-change-function 'dm-after-change-function)
		  ;; And this is for the initial processing of the file:
		  (set-syntax-table dylan-indent-syntax-table)
		  )))
	    (add-hook
	     'font-lock-after-fontify-buffer-hook
	     '(lambda ()
		(if (eq major-mode 'dylan-mode) ; restore normal syntax
		    (set-syntax-table dylan-mode-syntax-table)))
	     t)
	    )
	;; else older version of font-lock-mode
	;; (but this doesn't seem to work)
	(add-hook
	 'font-lock-mode-hook
	 '(lambda ()
	    (if (not (eq major-mode 'dylan-mode))
		nil
	      (setq font-lock-keywords dylan-font-lock-keywords)
	      (make-variable-buffer-local 'old-after-change-function)
	      (setq old-after-change-function 'font-lock-after-change-function)
	      (make-variable-buffer-local 'after-change-function)
	      (setq after-change-function 'dm-after-change-function)))))))

(provide 'dylan-mode)
