module:    extended-library
language:  prefix-dylan
author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(define-method constant? ((object <object>)) #F)
(define-method constant? ((object (singleton #T))) #T)
(define-method constant? ((object (singleton #F))) #T)
(define-method constant? ((object <number>)) #T)
(define-method constant? ((object <symbol>)) 
  #+HARLEQUIN-LISPWORKS-DYLAN-TRANSLATOR: ;; !@#$ could be a problem
  (keyword? object)
  #-HARLEQUIN-LISPWORKS-DYLAN-TRANSLATOR:
  #F)
(define-method constant? ((object <character>)) #T)
(define-method constant? ((object <string>)) #T)
(define-method constant? ((object <simple-object-vector>)) #T)
(define-method constant? ((object <list>)) (id? (first object) 'quote))
(define-method constant? ((object <empty-list>)) #T)

(define-method atom? ((object <object>)) #F)
(define-method atom? ((object (singleton #T))) #T)
(define-method atom? ((object (singleton #F))) #T)
(define-method atom? ((object <number>)) #T)
(define-method atom? ((object <symbol>)) #T)
(define-method atom? ((object <character>)) #T)
(define-method atom? ((object <string>)) #T)
(define-method atom? ((object <empty-list>)) #T)


