Module:    transcendentals
Language:  prefix-dylan
Synopsis:  Transcendentals in the translator
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;;; Primitives from CL

(import-cl-functions

   sin  cos  tan  asin  acos  atan  (atan as: atan2)
   sinh cosh tanh asinh acosh atanh
   exp  log  sqrt

)

;; eof
