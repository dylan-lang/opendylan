Module:    internal
Language:  prefix-dylan
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;; Filters.

;; This file implements the class <filter> whose instances encapsulate 
;; some combination of importation, exclusion and renaming directives 
;; to be applied to the key sequence of some collection. The directives 
;; supported are compatible with those supported by modules and libraries
;; for controlling their respective namespaces.
;;
;; Example:
;;
;;   (define name-filter
;;     (make <filter>
;;           exclude: '(joe)
;;           rename:  '((jim => sue) (sue => jim))
;;           prefix:  "person/"))
;;
;; Any explicit-key-collection can be accessed via such a filter through
;; the filtered-xxx versions of the key collection access functions.
;;
;; Example:
;;
;;   (filtered-element name-table name-filter 'sue) -> <<jim's data>>

;; Filters are implemented using function composition to construct 
;; two mapping functions from keys to keys, one in each direction.
;; The complexity of these functions is related to the complexity of
;; the directives specified. If no directive is specified, both these
;; functions become the identity and no computation is performed
;; when mapping keys.
;;
;; The functions need to be "recompiled" should the directives ever 
;; be destructively modified.

(define-generic-function
  filtered-key-sequence (collection filter))
(define-generic-function 
  filtered-element (collection filter key #key default))
(define-generic-function 
  filtered-element-setter (value collection filter key))

(define-class <filter> (<object>)
  (import-spec 
    init-keyword: import:
    init-value:   'all)
  (exclude-spec
    init-keyword: exclude:
    init-value:   '())
  (prefix-spec
    init-keyword: prefix:
    init-value:   "")
  (rename-spec
    init-keyword: rename:
    init-value:   '())
  (reveal-spec
    init-keyword: reveal:
    init-value:   'all)
  (filtered-key->key)
  (key->filtered-key))

(define-method initialize ((f <filter>) #rest init-args)
  (next-method)
  (ensure-valid-filter f)
  (compute-filter-maps f)
  f)

(define-method ensure-valid-filter ((f <filter>))
  (when (and (not (empty? (exclude-spec f)))
             (not (id? (import-spec f) 'all)))
    (error "Exclude clause ~s specified while import /= all"
           (exclude-spec f)))
  (bind ((conflicts 
           (intersection (map first (rename-spec f)) (exclude-spec f) 
              test: id?)))
    (when (not (empty? conflicts))
      (error "The renaming ~s conflicts with the exclusion of ~s"
             conflicts (map first conflicts))))
  (bind ((renames imports
           (bind ((spec (import-spec f)))
             (if (id? spec 'all) (values '() spec)
               (values
                 (choose (rcurry instance? <list>) spec)
                 (choose (rcurry instance? <symbol>) spec))))))
    (set! (import-spec f) imports)
    (set! (rename-spec f) (concatenate (rename-spec f) renames)))
  (for-each ((rename (rename-spec f))) ()
    (unless (and (= (size rename) 3) (id? (second rename) '=>))
      (error "The rename clause ~s is malformed in ~s" rename f)))
  (unless (id? (import-spec f) 'all)
    (set! (import-spec f)
          (remove-duplicates 
            (concatenate (import-spec f) (map first (rename-spec f)))))))

(define-method compute-filter-maps ((f <filter>))
  (bind ((fk->k identity)
         (k->fk identity))
    (bind ((imported (import-spec f)))
      (unless (id? imported 'all)
        (bind ((filter (method (keys)
                         (choose (rcurry member? imported) keys))))
          (set! k->fk (compose filter k->fk))
          (set! fk->k (compose filter fk->k)))))
    (bind ((excluded (exclude-spec f)))
      (unless (empty? excluded)
        (bind ((filter (method (keys)
                         (choose 
                           (complement (rcurry member? excluded)) keys))))
          (set! k->fk (compose filter k->fk))
          (set! fk->k (compose filter fk->k)))))
    (bind ((renamed (rename-spec f))
           (prefix (as <string> (as <symbol> (prefix-spec f)))))
      (unless (and (empty? renamed) (= prefix ""))
        (set! k->fk 
              (compose 
                (renaming-expander renamed prefix)
		k->fk))
        (set! fk->k 
              (compose 
                fk->k
                (unrenaming-expander renamed prefix (exclude-spec f))))))
    (bind ((revealed (reveal-spec f)))
      (unless (id? revealed 'all)
        (bind ((filter (method (keys)
                         (choose (rcurry member? revealed) keys))))
          (set! k->fk (compose filter k->fk))
          (set! fk->k (compose filter fk->k)))))
    (set! (filtered-key->key f) (compose fk->k list))
    (set! (key->filtered-key f) (compose k->fk list))
    (values)))

(define-method renaming-expander (renaming prefix)
  (method (keys)
    (bind ((aliases '()))
      (for-each ((key keys)) ()
        (bind ((local-aliases '()))
          (for-each ((rename renaming)) ()
	    (when (id? key (first rename))
	      (set! local-aliases (pair (third rename) local-aliases))))
          (when (empty? local-aliases)
	    (set! local-aliases 
                  (pair (as <symbol>
	                  (concatenate prefix (as <string> key)))
                        local-aliases)))
          (set! aliases (concatenate local-aliases aliases))))
      aliases)))

(define-method unrenaming-expander (renaming prefix excluded)
  (method (keys)
    (bind ((unaliases '()))
      (for-each ((key keys)) ()
        (bind ((local-unaliases '()))
          (for-each ((rename renaming)) ()
	    (when (id? key (third rename))
	      (set! local-unaliases (pair (first rename) local-unaliases))))
          (when (empty? local-unaliases)
            (bind ((string (as <string> key))
	           (i (subsequence-position string prefix)))
	      (when (and i (= i 0))
                (bind ((symbol
	                 (as <symbol>
	                   (copy-sequence string start: (size prefix)))))
                  (unless (or (member? symbol excluded)
                              (member? symbol renaming
                                       test: (method (symbol rename)
                                               (id? symbol (first rename)))))
                    (set! local-unaliases (pair symbol local-unaliases)))))))
          (set! unaliases (concatenate local-unaliases unaliases))))
      unaliases)))

(define-method filtered-key-sequence ((c <explicit-key-collection>)
                                      (f <filter>))
  (bind ((k->fk (key->filtered-key f))
         (filtered-keys '()))
    (for-each ((k (key-sequence c))) ()
      (bind ((fks (k->fk k)))
        (set! filtered-keys (concatenate fks filtered-keys))))
    (reverse! filtered-keys)))

(define $filter-no-value (list 'filter-no-value))

(define-method filtered-element ((c <explicit-key-collection>)
                                 (f <filter>)
                                 key
                                 #key
                                 (default (unsupplied)))
  (bind ((use-default
           (method ()
             (if (supplied? default) default
               (error "filtered-element - no map for key ~s and no default specified"
                      key))))
         (keys ((filtered-key->key f) key)))
    (if (empty? keys) (use-default)
      (if (supplied? default)
        (element c (first keys) default: default)
        (element c (first keys))))))

(define-method filtered-element-setter (value
                                        (c <explicit-key-collection>)
                                        (f <filter>)
                                        key)
  (bind ((keys ((filtered-key->key f) key)))
    (when (empty? keys)
      (error "filtered-element-setter - no map for key ~s"
             key))
    (set! (element c (first keys)) value)))

;;; Refinements.

(define-class <cached-filter> (<filter>)
  (key-cache init-function: (curry make <table>)))

(define-method flush ((f <cached-filter>))
  (set! (key-cache f) (make <table>)))

(define-method compute-filter-maps ((f <cached-filter>))
  (next-method)
  (bind ((mapper (filtered-key->key f)))
    (set! (filtered-key->key f)
          (method (key)
            (or (element (key-cache f) key default: #f)
                (set! (element (key-cache f) key) (mapper key)))))))

;; eof
