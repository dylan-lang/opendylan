module:    linux-pentium-rtg
Synopsis:  Stack overflow handling
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define linux-runtime-primitive dylan-stack-overflow-handler
  // On entry:
  //   base-address  - the address of the page to re-protect for guarding
  //   page-size     - size of page to re-protect
  //   protection    - the protection setting
  //
  //   Calls a Dylan error reporting function within the context of an
  //   unwind-protect frame which will reset the stack guard.
  //   The cleanup code tail calls the leaf-case reset-page-guard primitive
  //   which is ultimately responsible for resetting the guard and continuing
  //   with the NLX.
  // On exit:
  //  Should never exit directly (only via a NLX)

  nreg base-addr, page-size, protection;

  local 
    method protected-op (be)
      op--call-iep(be, dylan-stack-overflow-error);
    end method,

    method cleanup-op (be)
    end method;

  op--c-load-arguments(be, base-addr, page-size, protection);
  op--unwind-protect(be, protected-op, cleanup-op);
  // Control should never get here - but code the tail anyway
  ins--rts(be);
end linux-runtime-primitive;



