;;; dylan-params v. 0.02
;;; Nick Kramer (nkramer@cs.cmu.edu)

;;; Copyright (C) 1994  Carnegie Mellon University

;;; A quick hack to provide a parameter summary for methods in the
;;; Dylan Interim Reference Manual (DIRM). Comes in two flavors:
;;; dylan-show-method-params, which is meant to be called explicitly,
;;; and auto-display-method-params, which is meant to be called
;;; automatically.

;;; dylan-show-method-params gets the method name from the cursor
;;; position. If an identifier can't be found near the cursor or if a
;;; prefix argument was supplied, this function asks the user for a
;;; method name. It displays a parameter list in the echo area, or
;;; will open up a new window if the parameter list is long.
;;; (Currently the only "long" param lists are for,
;;; forward-iteration-protocol, and table-protocol. Ok, for isn't
;;; actually a method, but it's handy to have in there) If the method
;;; name is unknown, it will complain.

;;; auto-display-method-params is meant to be bound to the open paren
;;; key '('. If there is no space between a method name and the open
;;; paren, it will automatically display the parameter list in the
;;; echo area. This function is meant to be unobtrusive; if a method
;;; name is not found or if the method name is unknown, nothing is
;;; done. Similarly, it will *always* display in the echo area, never
;;; popping up another window, even if the parameter list is long.

;;; To use, put these lines in your .emacs file:

;;; (autoload 'dylan-show-method-params "dylan-params"
;;; 	  "Provide a Dylan method parameter summary." t)
;;; 
;;; (autoload 'auto-display-method-params "dylan-params"
;;; "When bound to '(', automatically provides Dylan method param summaries.")
;;; 
;;; (setq dylan-mode-hook '(lambda ()
;;; 			 (local-set-key "("    'auto-display-method-params)))

;;; (Remove the ;;;) This automatically loads both functions whenever
;;; you need them, and binds the open paren key to
;;; auto-display-method-params. This sample .emacs code assumes that
;;; you're using Robert Stockton's Dylan mode; if you aren't, you
;;; should be.

;;; This file contains parameter summaries of every method in the
;;; DIRM, as well as a description of for.  One can extend the summary
;;; list by appending to dylan-method-list; see end of file for a big
;;; example.  In the future, something less brain dead might be used,
;;; but in the mean time, that's what we have.

;;; Questions, comments, bugs, flames to nkramer@cs.cmu.edu.
;;; Disclaimer follows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; program's author (send electronic mail to nkramer@cs.cmu.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dylan-id-chars "-a-zA-Z0-9!&*<>=|^$%@_+~?/")
;; Derived from Bill Chiles' patterns

;;;;;;;;;;;;;;;;;;;;;;;;

(defun dylan-show-method-params (prefix)
"Provide a Dylan method parameter summary.
Finds the method name under the cursor, and tells about its arguments.
If an identifier is not found or if a prefix argument is given, it asks 
the user for a method name. If the param list is long, it'll display 
in a separate buffer like Help does. Otherwise, it'll display in the 
echo area."
  (interactive "p")
  (save-excursion
    (skip-chars-backward dylan-id-chars)
    (let* ((start (point))
	   (execute-this (skip-chars-forward dylan-id-chars))
	   (end (point))
	   (id (if (or (= start end)
		       (/= prefix 1))
		   (read-string "Method name: ")
		 (downcase (buffer-substring start end)))))
      (dylan-display-method-arg-string id)
)))

;;;;;;;;;;;;;;;;;;;;;;;;

(defun auto-display-method-params ()
  "When bound to '(', automatically provides Dylan method param summaries.
Is silent if a method name can't be found or if the method name is unknown;
never opens up additional windows."
  (interactive)
  (save-excursion
    (skip-chars-backward dylan-id-chars)
    (let* ((start (point))
	   (execute-this (skip-chars-forward dylan-id-chars))
	   (end (point))
	   (id (downcase (buffer-substring start end))))
      (dylan-display-method-arg-string id t t)))
  (insert "(")
)

;;;;;;;;;;;;;;;;;;;;;;;;

(defun dylan-display-method-arg-string (id &optional disallow-long-doc-strings 
					suppress-errors)
  (let* ((assoc-answer    (assoc id dylan-method-list))
	 (method-name     (car assoc-answer))
	 (arg-string      (car (cdr assoc-answer)))
	 (result-string   (car (cdr (cdr assoc-answer))))
	 (long-doc-string (car (cdr (cdr (cdr assoc-answer))))))

    (cond ((null assoc-answer)
	   (if (not suppress-errors)
	       (error "I don't know about %s" id)
	     nil))
	  ((and long-doc-string (not disallow-long-doc-strings))
	   (with-output-to-temp-buffer "*Method Description*"
	     (print long-doc-string)
	     (print-help-return-message)))
	  (result-string (message "%s"     
				  (concat "method " method-name "(" 
					  arg-string ") => " result-string)))
	  (t (message "%s" (concat "method " method-name "(" arg-string ")")))
)))

;;;;;;;;;;;;;;;;;;;;;;;;

;; Methods, as they appear in the interim book..
(defvar dylan-method-list '(
     ("for" "..." nil
      "for ( clauses  [{until | while} end-test] )
  body
  [finally result-body]
end [for]    => values

variable = init-value then next-value
variable [keyed-by key] in collection
variable from start [{to | above | below} bound] [by increment]")
     ("generic-function-methods" "generic-function" "sequence")
     ("add-method" "generic-function, method" "new-method, old-method")
     ("generic-function-mandatory-keywords" "generic-function" 
      "collection-of-keywords-or-#f")
     ("function-specializers" "function" 
      "num-required-args, rest-boolean, kwd-sequence-or-#f")
     ("applicable-method?" "function, #rest sample-args" "boolean")
     ("sorted-applicable-methods" "generic-function, #rest sample-args"
      "sequence1, sequence2")
     ("find-method" "generic-function, specializer-list" "method-or-#f")
     ("remove-method" "generic-function, method" "method")
     ("make" "class, #key #all-keys" "instance")
     ("initialize" "instance, #key #all-keys")
     ("slot-initialized?" "instance, getter" "boolean")
     ("instance?" "object, type" "boolean")
     ("subtype?" "type1, type2" "boolean")
     ("object-class" "object" "class")
     ("all-superclasses" "class" "sequence")
     ("direct-superclasses" "class" "sequence")
     ("direct-subclasses" "class" "sequence")
     ("as" "class, object" "instance")
     ("shallow-copy" "object" "new-object")
     ("class-for-copy" "object" "class")
     ("size" "collection" "integer-or-#f")
     ("empty?" "collection" "boolean")
     ("do" "procedure, collection, #rest more-collections" "#f")
     ("map" "procedure, collection, #rest more-collections" "new-collection")
     ("map-as" "class, procedure, collection, #rest more-collections"
      "new-collection")
     ("map-into" 
      "mutable-collection, procedure, collection, #rest more-collections"
      "mutable-collection")
     ("any?" "procedure, collection, #rest more-collections" "value")
     ("every?" "procedure, collection, #rest more-collections" "boolean")
     ("reduce" "procedure, initial-value, collection" "value")
     ("reduce1" "procedure, collection" "value")
     ("member?" "value, collection, #key test:" "boolean")
     ("find-key" "collection, procedure, #key skip:, failure:" "key")
     ("replace-elements!" 
      "mutable-collection, predicate, new-value-fn, #key count:"
      "mutable-collection")
     ("fill!" "mutable-collection, value, #key start:, end:")
     ("key-test" "collection" "test-function")
     ("size-setter" "n, stretchy-sequence" "n")
     ("add" "sequence, new-element" "new-sequence")
     ("add!" "sequence1, new-element" "sequence2")
     ("add-new" "sequence, new-element, #key test:" "new-sequence")
     ("add-new!" "sequence, new-element, #key test:" "new-sequence")
     ("remove" "sequence, value, #key test:, count:" "new-sequence")
     ("remove!" "sequence, value, #key test:, count:" "new-sequence")
     ("choose" "predicate, sequence" "new-sequence")
     ("choose-by" "predicate, test-sequence, value-sequence"
      "new-sequence")
     ("intersection" "sequence1, sequence2, #key test:" "new-sequence")
     ("union" "sequence1, sequence2, #key test:" "new-sequence")
     ("remove-duplicates" "sequence, #key test:" "new-sequence")
     ("remove-duplicates!" "sequence, #key test:" "new-sequence")
     ("copy-sequence" "source, #key start:, end:" "new-sequence")
     ("concatenate-as" "class, sequence1, #rest #rest more-sequences"
      "new-sequence")
     ("concatenate" "sequence1, #rest more-sequences" "new-sequence")
     ("replace-subsequence!" "sequence, insert-sequence, #key start:, end:"
      "result-sequence")
     ("reverse" "sequence" "new-sequence")
     ("reverse!" "sequence1" "sequence2")
     ("sort" "sequence, #key test:, stable:" "new-sequence")
     ("sort!" "sequence1, #key test:, stable:" "sequence2")
     ("first" "sequence, #key default:" "value")
     ("second" "sequence, #key default:" "value")
     ("third" "sequence, #key default:" "value")
     ("first-setter" "new-value, sequence" "new-value")
     ("second-setter" "new-value, sequence" "new-value")
     ("third-setter" "new-value, sequence" "new-value")
     ("last" "sequence, #key default:" "value")
     ("last-setter" "new-value, mutable-sequence" "new-value")
     ("subsequence-position" "big-sequence, pattern, #key test:, count:"
      "index")
     ("dimensions" "array" "sequence")
     ("rank" "array" "rank")
     ("row-major-index" "array, #rest subscripts" "index")
     ("aref" "array, #rest indices" "element")
     ("aref-setter" "new-value, array, #rest indices" "new-value")
     ("dimension" "array, axis" "dimension")
     ("push" "deque, new-value" "deque")
     ("pop" "deque" "first-element")
     ("push-last" "deque, new-value" "deque")
     ("pop-last" "deque" "first-element")
     ("pair" "head, tail" "pair")
     ("list" "#rest args" "list")
     ("head" "list" "object")
     ("tail" "list" "object")
     ("head-setter" "object, pair" "object")
     ("tail-setter" "object, pair" "object")
     ("range" "#key from:, to:, above:, below:, by:, size:)" "range")
     ("as-lowercase" "string-or-character" "new-string-or-character")
     ("as-lowercase!" "string" "string")
     ("as-uppercase" "string-or-character" "new-string-or-character")
     ("as-uppercase!" "string" "string")
     ("element" "collection, key, #key default:" "value")
     ("element-setter" "value, mutable-collection, key" "value")
     ("remove-key!" "table, key" "table")
     ("vector" "#rest args" "vector")
     ("forward-iteration-protocol" "collection" 
      "init-state, limit, next, finished?, key, elt, elt-setter, copy-state"
      "forward-iteration-protocol(collection) =>
       init-state      :: <object>
       limit           :: <object>
       next-state(coll, state) => new-state
       finished-state?(coll, state, limit) => boolean
       current-key(coll, state) => key
       current-element(coll, state) => element
       current-elt-setter(value, coll, state) => value
       copy-state(coll, state) => new-state")
     ("backward-iteration-protocol" "collection" 
      "final-state, limit, previous, finished?, key, elt, elt-setter, copy-state"
      "backward-iteration-protocol(collection) =>
       final-state      :: <object>
       limit           :: <object>
       previous-state(coll, state) => new-state
       finished-state?(coll, state, limit) => boolean
       current-key(coll, state) => key
       current-element(coll, state) => element
       current-elt-setter(value, coll, state) => value
       copy-state(coll, state) => new-state")
     ("table-protocol" "table" "test-function, hash-function"
      "table-protocol(table) =>
       test-function(key1, key2) => boolean
       hash-function(key) => (id, state)")
     ("merge-hash-codes" "id1, state1, id2, state2, #key ordered:"
      "merged-id, merged-state")
     ("odd?" "integer" "boolean")
     ("even?" "integer" "boolean")
     ("zero?" "integer" "boolean")
     ("positive?" "integer" "boolean")
     ("negative?" "integer" "boolean")
     ("integral?" "integer" "boolean")
     ("negative" "number" "number")
     ("floor" "real" "integer, real")
     ("ceiling" "real" "integer, real")
     ("round" "real" "integer, real")
     ("truncate" "real" "integer, real")
     ("floor/" "real1, real2" "integer, real")
     ("ceiling/" "real1, real2" "integer, real")
     ("round/" "real1, real2" "integer, real")
     ("truncate/" "real1, real2" "integer, real")
     ("modulo" "real1, real2" "real")
     ("remainder" "real1, real2" "real")
     ("abs" "number" "number")
     ("logior" "#rest integers" "integer")
     ("logxor" "#rest integers" "integer")
     ("logand" "#rest integers" "integer")
     ("lognot" "#rest integers" "integer")
     ("logbit?" "index, integer" "integer")
     ("ash" "integer, count" "integer")
     ("rationalize" "number" "number")
     ("denominator" "number" "number")
     ("lcm" "integer1, integer2" "integer")
     ("gcd" "integer1, integer2" "integer")
     ("min" "real, #rest more-reals" "real")
     ("max" "real, #rest more-reals" "real")
     ("compose" "function1, #rest more-functions" "function")
     ("complement" "predicate" "function")
     ("disjoin" "predicate1, #rest more-predicates" "function")
     ("conjoin" "predicate1, #rest more-predicates" "function")
     ("curry" "function, #rest curried-args" "new-function")
     ("rcurry" "function, #rest curried-args" "new-function")
     ("always" "object" "function")
     ("apply" "function, #rest args" "values")
     ("identity" "object" "object")
     ("signal" "condition" "values")
     ("error" "condition" "(will never return)")
     ("cerror" "restart-description, condition" "#f")
     ("break" "condition" "#f")
     ("check-type" "value, type" "value-or-signals-error")
     ("abort" "")
     ("default-handler" "condition" "values")
     ("restart-query" "restart")
     ("return-query" "condition")
     ("do-handlers" "funarg")
     ("return-allowed?" "condition" "boolean")
     ("return-description" "condition" "description")
))
     
