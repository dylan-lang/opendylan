Module:   internal
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;;; Modules for the translator.

;; Interactions:

(import-cl-functions
  ((dylan make-library-module-package))
  ((dylan find-translator-module))
  ((dylan canonicalize-module-variable))

  ((dylan module-package))
  ((dylan module-name))

  ((system do-symbols-func) as: do-package-symbols)
  ((cl unintern) as: unintern)
  ((cl makunbound))
  ((cl fmakunbound))
)

(define-class <translator-module> (<namespace>)
  (module-name
    init-keyword: name:
    init-value:   'anonymous)
  (module-package
    init-keyword: package:))

(define-method debug-name ((mod <translator-module>))
  (module-name mod))

(define-method initialize ((mod <translator-module>) #rest args #key name)
  (next-method)
  (set! (module-package mod) (make-library-module-package 'dylan name))
  mod)

(define-method reinitialize ((mod <translator-module>) #rest args)
  (next-method)
  (check-module-consistency mod)
  mod)

(define-method check-module-consistency ((mod <translator-module>))
#|
  (format #t "Module redefined - doing consistency check...")
  (bind ((names '())
         (symbols '())
         (pkg (module-package mod)))
    (do-package-symbols pkg
      (method (sym)
        (bind ((name (as <symbol> (as <string> sym))))
          (when (internal-element mod name default: #f)
            (set! names (pair name names))
            (set! symbols (pair sym symbols))))))
    (format #t "done.~%")
    (unless (empty? names)
      (format #t "~%")
      (format #t "The following were locally resolved before but are now imported: ~%")
      (do (method (name) (format #t "~s " name)) names)
      (do (method (sym) 
            (makunbound sym)
            (fmakunbound sym)
            (unintern sym pkg)) 
          symbols)
      (format #t "~%~%")))
|#
)

;; Magic!

(define-method namespace-export-representation ((space <translator-module>) name)
  (pair name space))

(define-class <magic-translator-module> (<translator-module>))

(define-method element ((mod <magic-translator-module>) key #key default)
  (or (element (local-key-collection mod) key default: #f)
      (set! (element (local-key-collection mod) key) (pair key mod))))

;; Promise everything explicitly exposed by the filter.

(define-method filtered-key-sequence 
    ((c <magic-translator-module>) (f <filter>))
  (if (instance? (reveal-spec f) <list>) (reveal-spec f) '()))

(define-method canonicalize-module-variable ((mod <translator-module>) name)
  (bind ((pair (internal-element mod name default: #f)))
    (if pair (values (tail pair) (head pair)) (values mod name))))

;; The space of modules in the translator.

(define $translator-modules (make <table>))

(define-method find-translator-module (name #key (default (unsupplied)))
  (or (element $translator-modules name default: #f)
      (if (supplied? default) default
        (error "No such module ~s" name))))

(define-method find-translator-module-setter (module name)
  (when (element $translator-modules name default: #f)
    (format #t "Redefining module ~s~%" name))
  (set! (element $translator-modules name) module))

(define-method %define-module (name #key uses creates exports)
  (bind ((uses
           (map (method (use) 
		  (pair (find-translator-module (head use)) (tail use)))
		uses))
         (args
           (list
	     name: name
	     uses:       uses
	     re-exports: (re-exports-from-uses uses)
	     creates:    creates
	     exports:    (concatenate exports creates)
	     package:    (make-library-module-package 'dylan name)))
         (module
           (find-translator-module name default: #f)))
    (if (not module)
      (set! (find-translator-module name) (apply make <translator-module> args))
      (apply reinitialize module args))))

(define-method re-exports-from-uses (uses)
  (reduce 
    (method (re-exports use) 
      (bind ((args (apply re-export-arguments-from-use (tail use))))
        (if args (pair (pair (head use) args) re-exports) re-exports)))
    '()
    uses))

(define-method re-export-arguments-from-use (#rest clauses #key export)
  (if (not export) #f
    (pair reveal: (pair export clauses))))

(define-method %define-module-alias (alias name)
  (set! (find-translator-module alias) (find-translator-module name))
  (values))

;; The boot module.

(set! (find-translator-module 'internal) 
      (make <magic-translator-module> name: 'internal))

;; eof
