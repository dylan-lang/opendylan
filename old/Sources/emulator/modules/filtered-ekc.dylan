Module:    internal
Language:  prefix-dylan
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;; Filtered explicit key collections.

;; Simply allows you to stick together a key filter and a collection
;; to give you a new collection whose elements are those of the
;; orginal collection with their keys mapped as specified by the
;; filter.
;;
;; Example:
;;
;;   (make <filtered-explicit-key-collection>
;;         collection: name-table
;;         filter:     (make <filter> ...))

(define-class <filtered-explicit-key-collection> (<explicit-key-collection>)
  (filtered-collection 
    required-init-keyword: collection:)
  (filter
    required-init-keyword: filter:))

;; Minimal protocol.

(define-method initial-state ((c <filtered-explicit-key-collection>))
  (bind ((ks (filtered-key-sequence (filtered-collection c) (filter c))))
    (if (empty? ks) #f ks)))

(define-method next-state ((c <filtered-explicit-key-collection>)
                           (state <list>))
  (bind ((next (tail state)))
    (if (empty? next) #f next)))

(define-method copy-state ((c <filtered-explicit-key-collection>)
                           (state <list>))
  state)

(define-method current-element ((c <filtered-explicit-key-collection>)
                                (state <list>))
  (filtered-element (filtered-collection c) (filter c) (first state)))

(define-method current-key ((c <filtered-explicit-key-collection>)
			    (state <list>))
  (first state))

;; Optimisations:

(define-method element ((c <filtered-explicit-key-collection>)
                        key
                        #key
                        (default (unsupplied)))
  (if (supplied? default)
    (filtered-element (filtered-collection c) (filter c) key default: default)
    (filtered-element (filtered-collection c) (filter c) key)))

(define-method element-setter (value
                               (c <filtered-explicit-key-collection>)
                               key)
  (set! (filtered-element (filtered-collection c) (filter c) key) value))

;; eof
