Module:    mop
Language:  prefix-dylan
Synopsis:  DW-compatible MOP support
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; From CL:

(import-cl-functions

  (class-slots as: slot-descriptors)
  (find-slot-definition as: lisp/find-slot-definition)
  (symbol-value as: lisp/symbol-value)
  (slot-value as: lisp/slot-value)
  (initialize-instance as: lisp/initialize-instance)
  ((clos getter->name))
  ((clos setter->name))
  ((clos dylan-default-initialize))
)

(import-cl-classes

  (slot-definition as: <slot-descriptor>)

)

(define-method initialize-defaults (object #rest init-args)
  (dylan-default-initialize object init-args))

(define-method slot-descriptor (object (getter <function>))
  (handler-case
    (lisp/find-slot-definition (getter->name getter) (object-class object))
    ((<error>) #f)))

(define-method slot-getter ((desc <slot-descriptor>))
  (lisp/slot-value desc 'clos::getter))

(define-method slot-setter ((desc <slot-descriptor>))
  (lisp/slot-value desc 'clos::setter))

(define-method slot-value (object (desc <slot-descriptor>))
  ((slot-getter desc) object)) 

(define-method slot-value-setter (value object (desc <slot-descriptor>))
  ((slot-setter desc) value object))

;; Dummy definitions.

(define-method slot-type ((desc <slot-descriptor>)) 
  <object>)

(define-method slot-allocation ((desc <slot-descriptor>))
  (lisp/slot-value desc 'clos::dylan-allocation))

(define-method init-value ((desc <slot-descriptor>))
  (lisp/slot-value desc 'clos::init-value))

(define-method init-function ((desc <slot-descriptor>))
  (lisp/slot-value desc 'clos::init-function))

(define-method init-keyword ((desc <slot-descriptor>))
  (first (lisp/slot-value desc 'clos::initargs)
         default: #f))

(define allocate-instance allocate)

(define-method shallow-copy (object)
  (bind ((class (object-class object))
         (copy (allocate-instance class)))
    (for ((descriptor in (slot-descriptors class)))
      (bind ((getter (slot-getter descriptor)))
        (when (and (id? (slot-allocation descriptor) instance:)
                   (slot-initialized? object getter))
          ((slot-setter descriptor) (getter object) copy))))
    copy))

(define-method reinitialize (object #rest all-keys)
  (apply lisp/initialize-instance object all-keys)
  (apply initialize-defaults object all-keys)
  (apply initialize object all-keys))

(define specializers method-specializers)

;; eof
