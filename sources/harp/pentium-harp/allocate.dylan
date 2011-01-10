module:    pentium-harp
Synopsis:  Pentium allocate instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// For Dylan on the Pentium with a conservative GC, allocate-raw-area
// does nothing. Allocate-local-area allocates spill space for both 
// G and N type spills.


define pentium-template allocate-raw-area
  pattern (be)
    #f;
end pentium-template;
    

define pentium-template allocate-local-area
  pattern (be)
    let state = be.variables.vreg-state;
    let total-spills = state.raw-size + state.next-gc-spill;
    unless (zero?(total-spills))
      harp-out (be)
	sub(be, reg--stack, reg--stack, 4 * total-spills);
      end harp-out;
    end unless;
end pentium-template;

