Module:   c-ffi
Language: prefix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(import-cl-functions
  ((dylan malloc))
  ((dylan signed-byte-at))
  ((dylan signed-byte-at-setter))
  ((dylan unsigned-byte-at))
  ((dylan unsigned-byte-at-setter))
  ((dylan signed-short-at))
  ((dylan signed-short-at-setter))
  ((dylan unsigned-short-at))
  ((dylan unsigned-short-at-setter))
  ((dylan signed-long-at))
  ((dylan signed-long-at-setter))
  ((dylan unsigned-long-at))
  ((dylan unsigned-long-at-setter))
  ((dylan float-at))
  ((dylan float-at-setter))
  ((dylan double-at))
  ((dylan double-at-setter))
)

;; Run at compile-stage

(define-method generate-lw-dff (d-name c-name args result)
  (bind ((l-name
           (as <symbol>
               (concatenate (as <string> d-name) (as <string> '-lw))))
         (val-args
           (choose (complement by-reference) args))
         (ref-args
           (choose by-reference args))
         (res-types
           `(,@(if result `(,(type-expression result)) '()) 
               ,@(map type-expression ref-args)))
         (res-names
           `(,@(if result `(the-result) '()) ,@(map name ref-args))))
    `(progn
      (ffi::define-foreign-function 
            (,((cl-function (dylan %runtime-resolve-name)) l-name)
              ,c-name source:)
         ,(map (method (arg)
                 (if (by-reference arg)
                   `(,(name arg) 
                       ,(lw-type (referenced-type (type arg))) reference:)
                   `(,(name arg)
                       ,(lw-type (type arg)))))
               args)
         ,@(if result
             `(result-type: ,(lw-type (type result)))
              '()))
      ,((cl-function (dylan translate-dylan-expr))
         `(define-method ,d-name ,(map name val-args)
            (bind ((,@(if (empty? res-names) '(.dummy.) res-names)
                    (,l-name 
                      ,@(map (method (arg)
		               `(bind ((map (class-for-map
                                              ,(type-expression arg))))
                                  (lw-export 
                                   (if map
                                       (export-value 
                                         ,(type-expression arg) ,(name arg))
                                     ,(name arg)))))
		             val-args))))
              (values
                ,@(map (method (name type)
                         `(bind ((map (class-for-map ,type)))
                            (if map
                                (import-value map ,type 
                                              (lw-import ,type ,name))
			      (lw-import ,type ,name))))
                       res-names res-types))))))))

(define-method generate-lw-fc (d-name c-name ref-name args result)
  (bind ((l-name
           (as <symbol>
               (concatenate (as <string> d-name) (as <string> '-lw))))
         (ref-args
           (choose by-reference args))
         (res-types
           `(,@(if result `(,(type result)) '()) ,@(map type ref-args)))
         (res-names
           `(,@(if result `(the-result) '()) ,@(map name ref-args)))
         (lw-type ;; hack
           (method (type) 'fixnum:)))
    `(progn
      (ffi::foreign-callable 
         ,((cl-function (dylan %runtime-resolve-name)) l-name)
         ,(map (method (arg)
		 (lw-type (type arg)))
               args)
         foreign-name: ',((cl-function make-symbol) c-name)
         ,@(if result
             `(result-type: ,(lw-type (type result)))
              '()))
      ,((cl-function (dylan translate-dylan-expr))
         `(define-method ,l-name ,(map name args)
            (bind ((,@res-names
                    (,l-name 
                      ,@(map (method (arg)
		               `(bind ((map (class-for-map ,(type arg))))
                                  (lw-export 
                                   (if map
                                       (export-value ,(type arg) ,(name arg))
                                     ,(name arg)))))
		             args))))
              (values
                ,@(map (method (name type)
                         `(bind ((map (class-for-map ,type)))
                            (if map
                                (import-value map ,type 
                                              (lw-import ,type ,name))
			      (lw-import ,type ,name))))
                       res-names res-types)))))
      (dylan-define ,ref-name
        (ffi::make-typed-alien
           :function
           (system::integer-raw-int
              (ffi::foreign-symbol-address
                 (ffi::lisp-name-to-foreign-name 
                   ',((cl-function (dylan %runtime-resolve-name)) l-name)
                  :encode :source))))))))

;; eof
