Module:    alien
Language:  prefix-dylan
Synopsis:  Runtime emulation of the native DW ffi using the LW ffi
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; From CL:

(import-cl-functions
  ((ffi read-foreign-modules))
  ((ffi alien-type))
  ((ffi |set ALIEN-TYPE|) as: set-alien-type)
  ((ffi lisp-alien-address) as: %dylan-alien-address)
)

(import-cl-classes
  ((ffi alien) as: <alien-system-object>))

(define-method alien-type-setter (val alien)
  (set-alien-type alien val))

(define-method dylan-alien-address ((i <integer>))
  i)

(define-method dylan-alien-address ((a <alien-system-object>))
  (%dylan-alien-address a))
  
(define *lib-format-string*
  (cond
    ((probe-file "/usr/lib/libXm.a")
      "/usr/lib/lib~a.a")
    ((probe-file "/usr/local/motif/lib/libXm.a")
      "/usr/local/motif/lib/lib~a.a")
    (else:
      (format #t "Warning: couldn't find the Motif libraries.~%")
      "/usr/lib/lib~a.a")))

(define-method rfm ()
  (apply read-foreign-modules 
         (map (method (s) (format #f *lib-format-string* s))
              '("Xm" "Xaw" "Xmu" "Xt" "X11"))))

(define-method ensure-alien-modules (#rest libs)
  (apply read-foreign-modules
         (map (method (s)
                (format #f *lib-format-string* 
                        (copy-sequence s start: 2)))
              libs)))

(define-generic-function make-alien (class))
(define-generic-function make-alien-array (class #key size))
(define-generic-function push-pointer (alien inc))
(define-generic-function push-pointer-using-name (name alien inc))
(define-generic-function alien-element-using-name (name alien index))
(define-generic-function 
  alien-element-using-name-setter (val name alien index))

(define-generic-function do-cast-as (type alien))

;; The LW ffi behaves strangely.
;; 
;;  If you make an array of word-sized values, the accessor (type*-> o i)
;;  indexes the object and gives you a value of type type.
;;
;;  If you make an array of immediate structures, the accessor (struct*-> o i)
;;  effectively pushes the base pointer and gives you back something of type
;;  type*!
;;
;;  Hence the follwing odd code, using the same function in methods on both
;;  element and push-pointer.

(define-method element ((alien <alien-system-object>) index #key default)
  (alien-element-using-name (alien-type alien) alien index))

(define-method element-setter (val (alien <alien-system-object>) index)
  (set! (alien-element-using-name (alien-type alien) alien index) val))

(define-method push-pointer (alien inc)
  (push-pointer-using-name (alien-type alien) alien inc))

(define-method fudge-alien-class (maker array-maker aget aset name ref-name)
  (bind ((class (make <class>
                      debug-name: name
                      superclasses: (list <object>))))
    (when name
      (add-method do-cast-as
                  (method ((c (singleton class)) alien
                           #next next-method)
                    (set! (alien-type alien) name)
                    alien)))
    (when maker
      (add-method make-alien
                  (method ((c (singleton class)) #next next-method)
                    (maker))))
    (when array-maker
      (add-method make-alien-array
                  (method ((c (singleton class)) 
                           #next next-method
                           #key size)
                    (array-maker size))))
    (when aget
      (add-method push-pointer-using-name
                  (method ((name (singleton ref-name)) alien index
                           #next next-method)
                    (aget alien index)))
      (add-method alien-element-using-name
                  (method ((name (singleton ref-name)) alien index
                           #next next-method)
                    (aget alien index)))
      (add-method alien-element-using-name-setter
                  (method (val (name (singleton ref-name)) alien index
                           #next next-method)
                    (aset alien index val))))
    class))

(define-method genericize (object)
  (unless (id? ((cl-function type-of) object) 'ffi::alien)
    (error "Don't know how to genericize ~s" object))
  ((cl-function (ffi lisp-alien-address)) object))

(define-method genericize ((i <integer>))
  i)

(define-method genericize ((s <string>))
  (genericize ((cl-function (ffi string-to-char-string)) s)))

(define-alien-type <%memory> unsigned-byte:)

(define-alien-type <%unsigned-byte> unsigned-byte:)
(define-alien-type <%signed-byte>   signed-byte:)
(define-alien-type <%byte>          signed-byte:)

(define-alien-type <%unsigned-short> unsigned-short:)
(define-alien-type <%signed-short>   short:)
(define-alien-type <%short>          short:)

(define-alien-type <%unsigned-long> unsigned-long:)
(define-alien-type <%signed-long>   long:)
(define-alien-type <%long>          long:)

(define-alien-type <%integer> int:)

(define-alien-type <%character> char:)
(define-alien-type <%string> lisp-string:)
(define-alien-type <%pointer> (pointer-to: <%character>))

;; From c.dyl

(define-alien-type <C/short>   <%short>)
(define-alien-type <C/short*>  (pointer-to: <C/short>))
(define-alien-type <C/short**> (pointer-to: <C/short*>))

(define-alien-type <C/int> <%integer>)
(define-alien-type <C/int*> (pointer-to: <C/int>))
(define-alien-type <C/int**> (pointer-to: <C/int*>))

(define-alien-type <C/char> <%character>)
(define-alien-type <C/char*> (pointer-to: <C/char>))
(define-alien-type <C/char**> (pointer-to: <C/char*>))

(define-alien-type <C/string> <%string>)
(define-alien-type <C/string*> (pointer-to: <C/string>))
(define-alien-type <C/string**> (pointer-to: <C/string*>))

;; eof
