; Module:    extended-dylan
; Author:    Keith Playford
; Copyright: Copyright (c) 1996-2000 Functional Objects, Inc. All rights reserved.

;;;; Namespaces.

;; Namespaces are a shared abstract above modules and libraries that
;; deal in the management of inherited namespaces together with a 
;; privileged local name table that can be extended, and a limited set
;; of exports visible to the outside world.
;;
;; The <namespace> class itself is positioned under 
;; <explicit-key-collection> and it is the exported variables of the 
;; namespace that are accessible through the standard collection 
;; interface.

(define-generic-function
  namespace-export-representation (space name))

(define-generic-function
  internal-element (space name #key default))
(define-generic-function
  internal-element-setter (value space name))

(define-generic-function 
  add-namespace-user (space user))
(define-generic-function
  remove-namespace-user (space user))

(define-class <namespace> (<explicit-key-collection>)

  (debug-name
    init-keyword: debug-name:
    init-value:   #f)

  use-spec
  create-spec
  export-spec

  (imported-key-collection
    init-value: #f)
  local-key-collection

  internal-cache

  (exported-key-collection
    init-value: #f)

  (users
    init-value:   '()
    init-keyword: users:)

)

(define-method add-namespace-user ((space <namespace>) (user <namespace>))
  (set! (users space) (add-new! (users space) user)))

(define-method remove-namespace-user ((space <namespace>) (user <namespace>))
  (set! (users space) (remove! (users space) user)))

(define-method initialize ((space <namespace>) #rest initargs)
  (next-method)
  (set! (local-key-collection space) (make <table>))
  (set! (internal-cache space) (make <table>))
  (apply compute-namespace space initargs)
  (verify-namespace space)
  (clear-caches space)
  space)

(define-method compute-namespace 
    ((space <namespace>) 
       #key (uses '()) (re-exports '()) (creates '()) (exports '()))
  (set! (use-spec space) uses)
  (set! (create-spec space) creates)
  (set! (export-spec space) exports)
  (bind ((imported
           (map (curry compute-imported-collection space) uses)))
    (set! (imported-key-collection space)
          (make <union-collection> members: imported)))
  (bind ((re-exported 
           (map (curry compute-imported-collection space) re-exports))
         (exported
           (compute-imported-collection 
             space
	     (list (local-key-collection space) import: exports))))
    (set! (exported-key-collection space)
          (make <union-collection> members: (pair exported re-exported))))
  (for ((name in exports))
    (set! (element (local-key-collection space) name)
          (namespace-export-representation space name)))
  space)

(define-method compute-imported-collection ((space <namespace>) use-spec)
  (bind ((used-collection (head use-spec))
         (filter-spec (tail use-spec))
         (filter (apply make <filter> filter-spec)))
    (make <filtered-explicit-key-collection>
	  filter:     filter
	  collection: used-collection)))

;;; Verification.

;; Still doesn't check clashes within a single use clause.

(define-class <namespace-import-collision> (<error>)
  (namespace 
    required-init-keyword: namespace:)
  (collisions
    required-init-keyword: collisions:))

(define-method print ((error <namespace-import-collision>) #key (stream #t))
  (format stream "Namespace collisions over ~a in module ~a~%~%"
          (map head (collisions error)) (debug-name (namespace error)))
  (for ((clash in (collisions error)))
    (for ((option in (tail clash)))
      (bind ((use (head option))
             (val (tail option)))
        (format stream "Is ~a the ~a from ~a (via use of ~a)~%" 
                (head clash) (head val) (debug-name (tail val))
                (debug-name (filtered-collection use)))))))

(define-method collect-duplicate-symbols (s)
  ((cl-function (dylan collect-duplicate-symbols)) s))

(define-method verify-namespace ((space <namespace>))
  (bind ((imports (imported-key-collection space))
         (imported-names
           (key-sequence imports))
         (potential-clashes
           (collect-duplicate-symbols imported-names))
         (clashes '()))
    (for ((name in potential-clashes))
      (bind ((views '()))
        (for ((collection in (members imports)))
          (bind ((val (element collection name default: (unfound))))
            (when (found? val)
              (set! views (pair (pair collection val) views)))))
        (bind ((clash-set
                 (remove-duplicates views
                    test: (method (v1 v2)
                            (id? (tail v1) (tail v2))))))
          (unless (= (size clash-set) 1)
            (set! clashes (pair (pair name views) clashes))))))
    (unless (empty? clashes)
      (error (make <namespace-import-collision>
                   namespace: space
                   collisions: clashes)))
    (bind ((clashes (intersection (export-spec space) imported-names)))
      (unless (empty? clashes)
        (error
          "Export clause collisions over ~a in module ~a - exporting imports"
	  clashes (debug-name space))))))

;;; Iteration protocol.

;; We set ourselves up as a collection with keys being the exported names
;; of the this module. This is a simple delegation to the exported 
;; key collection's value.

(define-method initial-state ((space <namespace>))
  (initial-state (exported-key-collection space)))

(define-method next-state ((space <namespace>) state)
  (next-state (exported-key-collection space) state))

(define-method copy-state ((space <namespace>) state)
  (copy-state (exported-key-collection space) state))

(define-method current-element ((space <namespace>) state)
  (current-element (exported-key-collection space) state))

(define-method current-key ((space <namespace>) state)
  (current-key (exported-key-collection space) state))

(define-method element ((space <namespace>) key #key default)
  (element (exported-key-collection space) key default: default))

;; Internal lookup.

(define-method internal-element ((space <namespace>) key 
                                 #key (default (unsupplied)))
  (bind ((value (element (internal-cache space) key default: (unfound))))
    (if (found? value) value
      (bind ((local (element (local-key-collection space) key 
			     default: (unfound))))
	(if (found? local) 
	  (set! (element (internal-cache space) key) local)
	  (bind ((inherited (element (imported-key-collection space) key 
				     default: (unfound))))
	    (if (found? inherited)
	      (set! (element (internal-cache space) key) inherited)
	      (if (supplied? default) default
		(error
		  "No element with key ~s in ~s and no default specified"
		  key space)))))))))

(define-method internal-element-setter (value (space <namespace>) key)
  (set! (element (local-key-collection space) key) value))

;; Iteration support.

(define-method do-users (f (space <namespace>))
  (do-users-aux f space (make <table>)))

(define-method do-users-aux (f (space <namespace>) visited)
  (unless (element visited space default: #f)
    (set! (element visited space) #t)
    (f space)
    (for ((using-space in (users space)))
      (do-users-aux f using-space visited))))

;; Redefinition support.

(define-method clear-caches ((space <namespace>))
  (for ((use in (use-spec space)))
    (add-namespace-user (first use) space))
  (do-users do-clear-caches space))

(define-method do-clear-caches ((space <namespace>))
  (set! (internal-cache space) (make <table>)))

(define-method reinitialize ((space <namespace>) #rest args)
  ;; Deregister ourselves as clients of the modules we used to use
  (for ((use in (use-spec space)))
    (remove-namespace-user (first use) space))
  (apply initialize space args))

;;; Print method.

(define-method print ((space <namespace>) #key (stream #t))
  (if (debug-name space)
    (format stream "{the ~s ~s}"
            (debug-name (object-class space))
            (debug-name space))
    (format stream "{an anonymous ~s}"
            (debug-name (object-class space))))
  space)

;; eof
