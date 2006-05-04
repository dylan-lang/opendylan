module:    pentium-linux-rtg
Synopsis:  Initializer for the Dylan Pentium Linux runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Support for a dummy initialization of the base runtime (to a bit bucket). 
// This will be necessary in order to permit generation of a client 
// runtime without generating a base first.

dummy-generate-runtime(make(<pentium-freebsd-back-end>));
