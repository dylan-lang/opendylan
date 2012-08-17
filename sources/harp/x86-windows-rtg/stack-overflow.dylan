module:    harp-x86-windows-rtg
Synopsis:  Stack overflow handling
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define win-fun runtime-external win-VirtualProtect = "VirtualProtect",  data:  "16";



define leaf runtime-primitive reset-guard-page
  // On entry:  Stack assumes we've been called by an unwind-protect cleanup
  //            I.e. the top of stack holds:
  //                ret-addr to cleanup
  //                ret-addr to primitive-nlx
  //                ultimate-destination BE frame
  //   arg0   - base-address: the address of the page to re-protect for guarding
  //   mlist  - page-size:    size of page to re-protect
  //   tmp1   - protection :  the protection setting
  // On exit:
  //   Never exits.
  //   The protection of the memory page is reset, and 
  //   the unwind chain is re-invoked.

  arg0 arg0;
  arg0 base-address;
  mlist page-size;
  tmp1 protection;
  nreg new-stack, be-frame, old-prot;
  greg values-vec;
  stack stack;
  tag already-below;

  // First, find our ultimate NLX destination
  ins--add(be, stack, stack, 8);  // Don't care about the return addresses
  ins--pop(be, be-frame);         // the ultimate destination

  // Move the stack pointer BELOW the region we want to protect,
  // before setting the protection
  ins--sub(be, new-stack, base-address, 4);  // now below base-address
  ins--blo(be, already-below, stack, new-stack);
  ins--move(be, stack, new-stack);
  ins--tag(be, already-below);

  // Now call Windows to re-establish the protection
  ins--push(be, be-frame);       // remember the ultimate destination
  ins--sub(be, stack, stack, 4); // save a word on the stack for the old protection
  ins--move(be, old-prot, stack);
  op--stdcall-c(be, win-VirtualProtect, base-address, page-size, protection, old-prot);
  ins--add(be, stack, stack, 4); // get rid of old-prot
  ins--pop(be, be-frame);

  // The guard is now back in place - so continue with the unwind. 
  // To avoid recoding lots of the NLX code, we reset the MV area
  // from the BE frame itself, and re-invoke primitive-nlx
  ins--ld(be, values-vec, be-frame, BE-values-vector-offset);
  op--restore-multiple-values-from-vector(be, values-vec);
  ins--push(be, arg0);
  ins--move(be, arg0, be-frame);
  ins--call(be, primitive-nlx-ref, 1);
  
end runtime-primitive;




define win32-runtime-primitive dylan-stack-overflow-handler
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
      with-harp (be)
        arg0 arg0;
        mlist mlist;
        tmp1 tmp1;

        ins--move(be, arg0, base-addr);
        ins--move(be, mlist, page-size);
        ins--move(be, tmp1, protection);
        ins--call(be, primitive-reference(reset-guard-page), 1, mlist: #t);
      end with-harp;
    end method;

  op--c-load-arguments(be, base-addr, page-size, protection);
  op--unwind-protect(be, protected-op, cleanup-op);
  // Control should never get here - but code the tail anyway
  ins--rts(be);
end win32-runtime-primitive;



