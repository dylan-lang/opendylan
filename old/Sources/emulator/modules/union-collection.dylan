module:    internal
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-class <union-collection> (<explicit-key-collection>)
  ;; @@@@ should be required-init-keyword
  (members init-keyword: members: init-value: '() type: <collection>))

(define-method make ((class (singleton <union-collection>)) #rest all-keys)
  (next-method))

(define-class <cached-union-collection> (<union-collection>))

(define-class <id?-cached-union-collection> (<cached-union-collection>)
  (cache init-function: (method () (make <table>)) type: <table>))

(define-class <=-cached-union-collection> (<cached-union-collection>)
  (cache init-function: (method () (make <table> test: =)) type: <table>))

(define-method make
    ((class (singleton <cached-union-collection>)) #rest all-keys)
  (apply make <id?-cached-union-collection> all-keys))

(define-class <union-state> (<object>)
  (state         required-init-keyword: state:         type: <object>)
  (members-state required-init-keyword: members-state: type: <object>))

(define-method next-valid-state 
    ((collection <union-collection>) (union-state <union-state>))
  (when (members-state union-state)
    (if (state union-state)
	union-state
	(bind ((the-members-state
                (next-state (members collection) (members-state union-state))))
	 (set! (members-state union-state) the-members-state)
	 (when the-members-state
	   (set! (state union-state)
	         (initial-state 
                  (current-element (members collection) the-members-state)))
	   (next-valid-state collection union-state))))))

(define-method initial-state ((collection <union-collection>))
  (bind ((members-state (initial-state (members collection)))) 
    (when members-state
      (next-valid-state
       collection
       (make <union-state> 
	     state: (initial-state
		     (current-element (members collection) members-state))
	 members-state: members-state)))))

(define-method next-state
    ((collection <union-collection>) (union-state <union-state>))
  (set! (state union-state) 
        (next-state 
         (current-element (members collection) (members-state union-state))
	 (state union-state)))
  (next-valid-state collection union-state))

(define-method current-element 
    ((collection <union-collection>) (union-state <union-state>))
  (current-element 
   (current-element (members collection) (members-state union-state))
   (state union-state)))

(define-method current-element-setter
    ((new-value <object>) 
     (collection <union-collection>) (union-state <union-state>))
  (set! (current-element 
         (current-element (members collection) (members-state union-state))
         (state union-state))
        new-value))

(define-method current-key
    ((collection <union-collection>) (union-state <union-state>))
  (current-key 
   (current-element (members collection) (members-state union-state))
   (state union-state)))

(define-method copy-state 
    ((collection <union-collection>) (union-state <union-state>))
  (make <union-state> 
	members-state: (members-state union-state)
	state: (state union-state)))

(define-method remove-key! ((collection <union-collection>) key)
  (bind-exit (return)
    (for ((member in (members collection)))
      (bind ((value (element member key default: $local-not-found)))
        (unless (id? value $local-not-found)
          (remove-key! member key)
	  (return)))))
  collection)

(define-method element 
    ((collection <union-collection>) (key <object>) #key (default $not-found))
  (bind-exit (return)
    (for ((member in (members collection)))
      (bind ((value (element member key default: $local-not-found)))
	(unless (id? value $local-not-found)
	  (return value))))
    (if (id? default $not-found)
	(error "Element outside of range: ~S ~S" collection key)
	default)))

(define-method element-setter
    ((new-value <object>) (collection <union-collection>) (key <object>))
  (bind-exit (return)
    (for ((member in (members collection)))
      (when (element member key default: #F)
        (set! (element member key) new-value)
        (return)))
    (set! (element 
           (current-element 
            (members collection) (initial-state (members collection)))
           key)
          new-value))
  new-value)

;;;; CACHED

(define-method element 
    ((collection <cached-union-collection>) (key <object>)
     #key (default $not-found))
  (bind ((cached-value 
          (element (cache collection) key default: $local-not-found)))
    (if (id? cached-value $local-not-found)
        (set! (element (cache collection) key) (next-method))
        cached-value)))

(define-method element-setter
    ((new-value <object>) 
     (collection <cached-union-collection>) (key <object>))
  (next-method)
  (set! (element (cache collection) key) new-value)
  new-value)

(define-method remove-key! ((collection <cached-union-collection>) key)
  (remove-key! (cache collection) key)
  (next-method))

(define-method flush ((collection <cached-union-collection>))
  (clear! (cache collection))
  (values))
