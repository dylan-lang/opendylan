Module:    ratio
Language:  prefix-dylan
Synopsis:  Ratios in the translator
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

;; Primitives from CL

(import-cl-classes

  (ratio as: <ratio>)

)

(import-cl-functions

   rationalize
   numerator
   denominator

)

;; eof
