module:    functional-collection
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;;
;;;; <INVERTABLE-FUNCTIONAL-COLLECTION>
;;;;

(define-class <invertable-functional-collection> (<explicit-key-collection>)
  (function         init-keyword: function:          type: <function>)
  (inverse-function init-keyword: inverse-function:  type: <function>)
  (contents         required-init-keyword: contents: type: <collection>))

(define-method size ((collection <invertable-functional-collection>))
  (size (contents collection)))

(define-method element 
    ((collection <invertable-functional-collection>) (key <object>)
     #key (default (unsupplied)))
  ;; !@#$ assumes $not-found mechanism through-out
  (bind ((mapped-key ((function collection) key default: (not-found))))
    (when (not-found? mapped-key)
      (error "element outside of domain: ~S ~S" collection key))
    (bind ((value
	    (element (contents collection) mapped-key default: (not-found))))
      (if (not-found? value)
          (error "element outside of range: ~S ~S->~S"
		 collection key mapped-key)
          value))))

(define-method element-setter 
    ((new-value <object>)
     (collection <invertable-functional-collection>) (key <object>))
  (bind ((mapped-key ((function collection) key default: (not-found))))
    (when (not-found? mapped-key)
      (error "element-setter outside of range: ~S ~S" collection key))
    (set! (element (contents collection) mapped-key) new-value)))

(define-method initial-state ((collection <invertable-functional-collection>))
  (initial-state (key-sequence (contents collection))))

(define-method next-state
    ((collection <invertable-functional-collection>) (state <object>))
  (next-state (key-sequence (contents collection)) state))

(define-method current-element
    ((collection <invertable-functional-collection>) (state <object>))
  (element (contents collection) 
           (current-element (current-key collection state))))

(define-method current-element-setter 
    ((new-value <object>)
     (collection <invertable-functional-collection>) (state <object>))
  (set! (element (contents collection) 
                 (current-element (current-key collection state)))
        new-value))

(define-method current-key
    ((collection <invertable-functional-collection>) (state <object>))
  ((inverse-function collection)
   (current-element (key-sequence collection) state)))

;;;;
;;;; <FUNCTIONAL-COLLECTION>
;;;;

(define-class <functional-collection> (<explicit-key-collection>)
  ;; @@@@ domain and function are required
  (domain   init-keyword:          domain:   type: <collection>)
  (function init-keyword:          function:  type: <function>)
  (contents required-init-keyword: contents: type: <collection>))

(define-method size ((collection <functional-collection>))
  (size (domain collection)))

(define-method element 
    ((collection <functional-collection>) (key <object>)
     #key (default (unsupplied)))
  ;; !@#$ assumes $not-found mechanism through-out
  (bind ((mapped-key ((function collection) key default: (not-found))))
    (when (not-found? mapped-key)
      (error "element outside of domain: ~S ~S" collection key))
    (bind ((value (element (contents collection) mapped-key default: default)))
      (if (id? value default)
          (if (unsupplied? default)
              (error "element outside of range: ~S ~S->~S" 
                     collection key mapped-key)
              default)
          value))))

(define-method element-setter 
    ((new-value <object>) (collection <functional-collection>) (key <object>))
  (bind ((mapped-key ((function collection) key default: (not-found))))
    (when (not-found? mapped-key)
      (error "element-setter outside of range: ~S ~S" collection index))
    (set! (element (contents collection) mapped-key) new-value)))

(define-method initial-state ((collection <functional-collection>))
  (initial-state (domain collection)))

(define-method next-state
    ((collection <functional-collection>) (state <object>))
  (next-state (domain collection) state))

(define-method current-element
    ((collection <functional-collection>) (state <object>))
  (element (contents collection) 
           ((function collection) (current-key collection state))))

(define-method current-element-setter 
    ((new-value <object>) (collection <functional-collection>) (state <object>))
  (set! (element 
         (contents collection) 
	 ((function collection) (current-key collection state)))
        new-value))

(define-method current-key
    ((collection <functional-collection>) (state <object>))
  (current-element (domain collection) state))

;;;;
;;;; <MAPPED-COLLECTION>
;;;;

(define-class <mapped-collection> (<functional-collection>))

(define-method initialize 
    ((collection <mapped-collection>) #key mapping)
  (unless mapping
    (error "<mapped-collection> must have a mapping"))
  (set! (function collection)
        (method (key #key (default (unsupplied)))
          (bind ((value (element mapping default: default)))
            (if (unsupplied? value)
                (error "Element outside of range: ~S ~S" collection key)
                value))))
  (set! (domain collection) (key-sequence mapping))
  collection)
