module:    harp-x86-rtg
Synopsis:  Complex Primitives for the Dylan x86 runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method op--preserve-mlist-for-mep-apply
    (be :: <x86-back-end>, mlist :: <register>) => ()
  with-harp (be)
    stack stack;

    // Pentium note: we suffer some serious register pressure here.
    // This is worse than for XEPs with optionals because we have to 
    // preserve MList too. The ugly but workable solution I've adopted
    // is to preserve MList on the stack, just past all the optional arguments.
    // We rely on the code in the IEP to remove it afterwards, because we
    // frig the count to allow for it being on the stack.

    ins--add(be, stack, stack, 4); // remove 1 of the the 2 stack args
    ins--st(be, mlist, stack, 0);  // store the MList on the stack.
  end with-harp;

end method;

define sideways method op--restore-mlist-for-mep-apply
    (be :: <x86-back-end>, mlist :: <register>) => ()
  with-harp (be)
    stack stack;

    // Pentium frig: Now have to get the value back into the MList register,
    // and adjust the count to allow for the fact that the Mlist is on the stack
    let fixed-offset = op--shuffle-size-for-requireds(be, #"dynamic");
    let count-addr = make-n-register(be);
    let count = make-n-register(be);
    ins--asl(be, fixed-offset, fixed-offset, 2);  // get fixed size in bytes
    ins--add(be, count-addr, stack, fixed-offset);// this points at the rest val
    ins--add(be, count-addr, count-addr, 4);      // this points at the count
    ins--ld(be, count, count-addr, 0);            // this is the count
    ins--add(be, count, count, 4);                // frig it suitably
    ins--st(be, count, count-addr, 0);            // store it back
    ins--ld(be, mlist, stack, count);             // Mlist is at end of opts
  end with-harp;
end method;


define sideways method op--push-registers-for-remove-optionals
    (be :: <x86-back-end>) => (bytes-pushed :: <integer>)
  with-harp (be)
    arg0 arg0;
    arg-count argc;
    mlist mlist;
    function function;
  
    // NB arg0 & function are just not touched
    ins--push(be, argc);
    ins--push(be, mlist);

    2 * 4
  end with-harp;
end method;

define sideways method op--pop-registers-for-remove-optionals
    (be :: <x86-back-end>) => ()
  with-harp (be)
    arg0 arg0;
    arg-count argc;
    mlist mlist;
    function function;

    // NB arg0 & function are just not touched
    ins--pop(be, mlist);
    ins--pop(be, argc);
  end with-harp;
end method;
