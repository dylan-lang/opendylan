module:    internal
language:  prefix-dylan
author:    Eliot Miranda and Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

(import-cl-functions ((system eq-hashfn) as: address-of))
(import-cl-functions 
 ((dylan current-gc-state) as: current-gc-state))


