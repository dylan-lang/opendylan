module:    powerpc-harp
Synopsis:  PowerPC stack allocation code
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// The (emulated) stack pointer points to the 'top' element, stack grows down.

/// don't do anything here, do it with allocate-local-area


define powerpc-template allocate-raw-area
  pattern (be)
    #f;
end powerpc-template;

define powerpc-template allocate-local-area
  pattern (be)
    let state = be.variables.vreg-state;
    let total-spills = state.raw-size + state.next-gc-spill;
    unless (zero?(total-spills))
      // Adjust the stack pointer
      emit-d(be, addic-op, reg--stack, reg--stack, -4 * total-spills);
    end unless;
end powerpc-template;

