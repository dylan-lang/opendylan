; copyright: 1996 Functional Objects, Inc. All rights reserved.
(in-package dylan)

(defmacro def-low-level-accessors (dylan-name lisp-name)
  (let* ((c-type (ffi::get-c-type-descriptor lisp-name))
         (load-name (ffi::basic-c-type-load c-type))
         (store-name (ffi::basic-c-type-store c-type))
         (import-name (ffi::basic-c-type-import c-type))
         (export-name (ffi::basic-c-type-export c-type)))
    (let* ((getter-name
             (intern (concatenate 'string 
		       (symbol-name dylan-name)
		       (symbol-name '-at))))
           (setter-name
             (make-setter-name getter-name)))
      `(progn
         (defun ,getter-name (address)
           (,import-name
             (,load-name
               (sys::integer-raw-int address)
               0)))
         (defun ,setter-name (value address)
	   (,store-name
             (sys::integer-raw-int address)
             0
	     (,export-name value)))))))

(def-low-level-accessors signed-byte    :signed-byte)
(def-low-level-accessors unsigned-byte  :unsigned-byte)
(def-low-level-accessors signed-short   :short)
(def-low-level-accessors unsigned-short :unsigned-short)
(def-low-level-accessors signed-long    :long)
(def-low-level-accessors unsigned-long  :unsigned-long)
(def-low-level-accessors float          :single-float)
(def-low-level-accessors double         :double-float)
(def-low-level-accessors character      :char)

#|
(defun malloc (size)
  (sys::check-in-make-positive-bignum
    (sys::lisp-malloc size 8))) ;; double word align everything
|#

(ffi:define-foreign-function malloc ((size :unsigned-long))
  :result-type :unsigned-long)

;; Compile stage evaluation

(define-dylan-translator 
    at-compile-stage translate-at-compile-stage (form env)
  (destructuring-bind (_ &rest body) form
    (declare (ignore _))
    (let ((translation (translate-dylan-expr `(begin ,@body))))
      (eval translation))))

;; Hack to stop foreign code exiting lifted from the Capi

(ffi::foreign-callable exit
                       (:uinteger)
                       :result-type :as-is ;; important
                       )

(eval-when (compile eval)
  (unless (fboundp 'compiler::integer-to-lisp-address)
    (load "~lisp/ncomp/new-macros")))

(defun exit (int)
  (cerror "Exit LispWorks !" "Exit called [~a]" int)
  (compiler::call-address (compiler::lisp-to-raw-address
                           (compiler::integer-to-lisp-address *exit-address*))
                          (sys::fixnum-raw-int int)))

;; eof
