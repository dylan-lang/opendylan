module:    dylan-rtg
Synopsis:  Non-local exit primitives for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// NLX primitives ...




/*

           Bind-exit and unwind-protect implementation notes:


1. Introduction

Bind-exit and unwind-protect are represented on the stack as frames which
contain information about how to invoke the revelevant continuation. 
Unwind-protect frames are also chained together, and the current environment 
of existing unwind-protects is available in the dynamic-environment slot
of the Thread Environment Block.

There are primitives to build each type of frame, and also to remove 
unwind-protect frames (bind-exit frames just have to be popped - so that is
done inline). The primitive which removes unwind-protect frames in the
fall-through case is also responsible for invoking the cleanup code (which
is called as a sub-function in the same function frame as its parent).

There are also primitives to do non-local exits (NLX). These are passed 
the address of the bind-exit frame for the destination, and also the
multiple values to be returned. As part of the NLX, any intervening 
unwind-protects are invoked and their frames are removed. Multiple-values
are saved around the unwind-protects in the bind-exit frame of the 
destination.


2. Bind-exit

A bind-exit frame looks as follows:

	52	continuation address
	48	frame pointer
	44	current unwind-protect frame
	4	space for a stack allocated vector for up to 8 MVs
	0	pointer to saved multiple-values as a vector

During an NLX, multiple-values will be saved in the frame if an intervening 
unwind-protect is active. The frame itself contains space for 4 values. If
more values are present, then they will be heap allocated.

The compiler compiles bind-exit as follows:

  let frame = primitive-build-bind-exit-frame(tag1);
  let closure = make-bind-exit-closure(frame);
  do-the-bind-exit-body-setting-results-as-for-a-return();
  tag1:
  



3. Unwind-protect

An unwind-protect frame looks as follows:

	8	address of start of cleanup code
	4	frame pointer
	0	previous unwind-protect frame


The compiler compiles unwind-protect as follows 
(but see op--unwind-protect below):

  let frame = primitive-build-unwind-protect-exit-frame(tag1);
  do-the-protected-forms-setting-results-as-for-a-return();
  primitive-unwind-protect-cleanup();
  goto(tag-finished);
  tag1:
  do-the-cleanup-forms();
  end-cleanup();  // inlined as a return instruction
  tag-finished:

*/


define constant BE-max-values = 8;  // chosen to support the iteration protocol
define constant BE-values-size = BE-max-values * 4;

define constant BE-values-vector-offset = 0;
define constant BE-values-space-offset = BE-values-vector-offset + 4;
define constant BE-first-value-offset = BE-values-space-offset + 8;
define constant BE-current-UP-offset = BE-first-value-offset + BE-values-size;
define constant BE-frame-pointer-offset = BE-current-UP-offset + 4;
define constant BE-code-offset = BE-frame-pointer-offset + 4;
define constant BE-frame-size = BE-code-offset + 4;


define leaf runtime-primitive build-bind-exit-frame
  // On entry:
  //    arg0 - address of code
  // On exit:
  //    result - address of bind-exit frame

  arg0 arg0;
  result result;
  stack stack;
  frame frame;
  arg0 current;
  tmp 1, ret-addr;

  if-return-address() ins--pop(be, ret-addr) end;
  // Stack allocate the bind-exit frame, and fill in the destination slots
  ins--sub(be, stack, stack, BE-frame-size);
  ins--st(be, arg0, stack, BE-code-offset);
  op--ld-current-unwind-protect-frame(be, current);
  ins--st(be, frame, stack, BE-frame-pointer-offset);
  ins--st(be, current, stack, BE-current-UP-offset);
  ins--move(be, result, stack); 
  if-return-address()
    ins--jmp(be, ret-addr, 0);
  else
    ins--rts(be);
  end;
end runtime-primitive;




define constant UP-previous-frame-offset = 0;
define constant UP-frame-pointer-offset = UP-previous-frame-offset + 4;
define constant UP-code-offset = UP-frame-pointer-offset + 4;
define constant UP-frame-size = UP-code-offset + 4;


define leaf runtime-primitive build-unwind-protect-frame
  // On entry:
  //    arg0 - address of code
  // On exit:
  //    result - address of unwind-protect frame

  arg0 arg0;
  result result;
  stack stack;
  frame frame;
  arg0 current;
  nreg ret-addr;

  if-return-address() ins--pop(be, ret-addr) end;
  // Stack allocate the unwind-protect frame, and fill in the destination slots
  ins--sub(be, stack, stack, UP-frame-size);
  ins--st(be, arg0, stack, UP-code-offset);
  op--ld-current-unwind-protect-frame(be, current);
  ins--st(be, frame, stack, UP-frame-pointer-offset);
  ins--st(be, current, stack, UP-previous-frame-offset);
  ins--move(be, result, stack); 
  op--st-current-unwind-protect-frame(be, stack);
  if-return-address()
    ins--jmp(be, ret-addr, 0);
  else
    ins--rts(be);
  end;
end runtime-primitive;


define leaf runtime-primitive unwind-protect-cleanup
  // On entry:
  //    No arguments passed - we get the current UP frame
  //    from the global variable.
  // On exit:
  //    The values of the cleanup code are returned
  // Purpose:
  //    This primitive is called for the fall-through case
  //    of an unwind-protect, to invoke the cleanup code.
  // Implementation:
  //    The unwind-protect frame is first unchained from the 
  //    global variable, and all current multiple values are
  //    saved. The cleanup code is then called as a function - 
  //    but run within the same frame as the caller of the 
  //    primitive. Finally, the multiple values are restored, 
  //    the unwind-protect frame is popped, and control is returned
  //    to the caller.

  stack stack;
  nreg tmp;
  tmp 1, ret-addr;
  nreg up-frame;
  
  // start by unchaining the UP frame
  op--ld-current-unwind-protect-frame(be, up-frame);
  ins--ld(be, tmp, up-frame, UP-previous-frame-offset);
  op--st-current-unwind-protect-frame(be, tmp);
  
  // Save the multiple values and current frame by pushing on the stack
  op--push-multiple-values(be);
  ins--push(be, up-frame);
  
  // Call the cleanup code
  ins--ld(be, tmp, up-frame, UP-code-offset);
  ins--call(be, tmp, 0);
  
  // Restore values, and exit
  ins--pop(be, up-frame);
  op--pop-multiple-values(be);
  if-return-address() ins--ld(be, ret-addr, stack, 0) end;
  ins--add(be, stack, up-frame, UP-frame-size);
  if-return-address()
    ins--jmp(be, ret-addr, 0);
  else
    ins--rts(be);
  end;
end runtime-primitive;

define open generic op--leaf-call
    (be :: <harp-back-end>, code :: <register>) => ();

define method op--leaf-call
    (be :: <harp-back-end>, code :: <register>) => ()
 ins--call(be, code, 0);
end method;


/*  Old interface
define leaf runtime-primitive nlx
  // On entry:
  //    arg0: target-bind-exit
  //    next argument: vector of multiple values
  // On exit:
  //    control is transferred to the bind-exit continuation

  frame frame;
  stack stack;
  arg0 arg0;
  result result;
  arg0 be-frame;
  nreg final-up-frame, global-up-frame, be-frame-n, values-vec;
  tag need-to-cleanup;
  
  op--ld-current-unwind-protect-frame(be, global-up-frame);
  ins--ld(be, final-up-frame, be-frame, BE-current-UP-offset);
  ins--move(be, be-frame-n, be-frame);
  ins--load-stack-arg-n(be, values-vec, 0);
  ins--bne(be, need-to-cleanup, global-up-frame, final-up-frame);
  
  // Case where there are no intervening unwinds
  // The multiple values must be restored from the vector
  op--restore-multiple-values-from-vector(be, values-vec);
  op--transfer-control-to-bind-exit-frame(be, be-frame-n);
  
  // Case where there are intervening unwinds.
  // Store the multiple values in the BE frame, before calling cleanup
  ins--tag(be, need-to-cleanup);
  op--store-bind-exit-values-from-vector(be, be-frame-n, 
                                         global-up-frame, values-vec);
  op--invoke-nlx-cleanup(be, be-frame-n, global-up-frame);
end runtime-primitive;
*/


define leaf runtime-primitive nlx
  // On entry:
  //    arg0: target-bind-exit
  //    next argument: first argument to continuation
  // On exit:
  //    control is transferred to the bind-exit continuation

  frame frame;
  stack stack;
  arg0 arg0;
  result result;
  arg0 be-frame;
  nreg final-up-frame, global-up-frame, be-frame-n, first-arg;
  tag need-to-cleanup;
  
  op--ld-current-unwind-protect-frame(be, global-up-frame);
  ins--ld(be, final-up-frame, be-frame, BE-current-UP-offset);
  ins--move(be, be-frame-n, be-frame);
  op--load-arguments(be, #f, first-arg);
  ins--bne(be, need-to-cleanup, global-up-frame, final-up-frame);
  
  // Case where there are no intervening unwinds
  // The multiple values are already set - but the result register 
  // has been clobbered by the bind-exit frame itself.
  ins--move(be, result, first-arg);
  op--transfer-control-to-bind-exit-frame(be, be-frame-n);
  
  // Case where there are intervening unwinds.
  // Store the multiple values in the BE frame, before calling cleanup
  ins--tag(be, need-to-cleanup);
  op--store-bind-exit-values-from-mv-area(be, be-frame-n, 
                                          global-up-frame, first-arg);
  op--invoke-nlx-cleanup(be, be-frame-n, global-up-frame);
end runtime-primitive;



/// OP--INVOKE-NLX-CLEANUP
/// Calls the cleanup code in up-frame in order to get to the 
/// ultimate bind-exit destination in be-frame. The bind-exit frame
/// should have been initialized with all the returning multiple values.
/// Iterates after calling each cleanup, and finally transfers control 
/// to the bind-exit.
///
/// NB. We know that there is at least one cleanup to be called - because
/// the NLX entry point handles the direct case.

define method op--invoke-nlx-cleanup 
    (be :: <harp-back-end>, 
     be-frame :: <register>, 
     up-frame :: <register>)

  with-harp (be)
    frame frame;
    stack stack;
    nreg tmp;
    result result;
    nreg up-code, final-up-frame;
    tag start-of-loop, uncaught-exit;
  
    // Start of loop. Two registers are live at this time:
    //  be-frame - ultimate bind-exit destination
    //  up-frame - current unwind-protect frame
    ins--tag(be, start-of-loop);
    ins--beq(be, uncaught-exit, up-frame, 0);

    op--pop-any-SEH-handlers(be, up-frame);
    ins--move(be, stack, up-frame);   // pop the stack back to the UP frame
  
    // start by unlinking this UP from the global chain
    ins--ld(be, tmp, stack, UP-previous-frame-offset);
    ins--ld(be, up-code, stack, UP-code-offset);
    op--st-current-unwind-protect-frame(be, tmp);
  
    // Now set the context and call the UP code
    ins--ld(be, frame, stack, UP-frame-pointer-offset);
    ins--push(be, be-frame);    // Remember the ultimate destination
    ins--call(be, up-code, 0);  // Do the call to the cleanup
    ins--pop(be, be-frame);     // Restore the ultimate destination
    
    // Find the next frame and see if its the final one
    op--ld-current-unwind-protect-frame(be, up-frame);
    ins--ld(be, final-up-frame, be-frame, BE-current-UP-offset);
    ins--bne(be, start-of-loop, final-up-frame, up-frame);
  
    // We've found all intervening unwind-protects - so take the jump
    let values-vec = make-g-register(be);
    ins--ld(be, values-vec, be-frame, BE-values-vector-offset);
    op--restore-multiple-values-from-vector(be, values-vec);
    op--transfer-control-to-bind-exit-frame(be, be-frame);
  
    // Error case where we've exhausted the UP chain:
    ins--tag(be, uncaught-exit);
    ins--jmp(be, primitive-error-ref, 0);  // !@#$ temporary error handler
  end with-harp;
end method;


/// OP--TRANSFER-CONTROL-TO-BIND-EXIT-FRAME
/// jumps to bind-exit continuation, in the appropriate stack-frame
/// environment. The multiple-values should have been initialized first.

define method op--transfer-control-to-bind-exit-frame
    (be :: <harp-back-end>, be-frame :: <register>)
  with-harp (be)
    frame frame;
    stack stack;
    nreg continuation;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    op--pop-any-SEH-handlers(be, be-frame);
    ins--ld(be, continuation, be-frame, BE-code-offset);
    ins--ld(be, frame, be-frame, BE-frame-pointer-offset);
    ins--add(be, stack, be-frame, BE-frame-size);
    ins--jmp(be, continuation, max-num-arg-regs);
  end with-harp;
end method;


/// OP--POP-ANY-SEH-HANDLERS
/// This is responsible for the minimal amount of interaction between the 
/// Dylan NLX implementation, and the Windows SEH mechanism. Just before
/// popping the stack, we look to see if it will make any of the registered
/// SEH handlers stale. If so, then they get popped first. In practice,
/// there will only be a need for this if an NLX is made past a callback.

define open generic op--pop-any-SEH-handlers
    (be :: <harp-back-end>, new-stack-ptr :: <register>);


define method op--store-bind-exit-values-from-mv-area
    (be :: <harp-back-end>, 
     be-frame :: <register>, 
     up-frame :: <register>, 
     first-value :: <register>)

  with-harp (be)
    nreg vec;
    tmp 1, mv-area;
    arg-count count;
    tag have-mvs, mvs-done, vector-ok;
  
    ins--add(be, vec, be-frame, BE-values-space-offset);
    ins--bmvset(be, have-mvs);
  
    //          Case where there is a single value
    ins--st(be, first-value, vec, 8);
    op--set-vector-size(be, 1, vec);
    ins--bra(be, mvs-done);
  
    //          Case where there are multiple values
    ins--tag(be, have-mvs);
    op--ld-mv-count(be, count);
    ins--ble(be, vector-ok, count, BE-max-values);
    // Case where the BE frame doesn't hold all the values
    // Do a heap allocation
    let new-vec = op--safe-allocate-vector(be, count, count, be-frame, up-frame);
    ins--move(be, vec, new-vec);
  
    // Now have a suitable vector
    ins--tag(be, vector-ok);
    let vec-data = make-n-register(be);
    op--set-vector-size(be, count, vec);
    ins--add(be, vec-data, vec, 8);
    op--ld-mv-area-address(be, mv-area);
    ins--copy-words-down-w(be, vec-data, mv-area, count);
  
    ins--tag(be, mvs-done);
    ins--st(be, vec, be-frame, BE-values-vector-offset);
  end with-harp;
end method;


define method op--store-bind-exit-values-from-vector
    (be :: <harp-back-end>, 
     be-frame :: <register>, 
     up-frame :: <register>, 
     values-vec :: <register>)

  with-harp (be)
    nreg vec;
    arg-count count;
    tag vector-ok;
  
    op--vector-size(be, count, values-vec);
    ins--add(be, vec, be-frame, BE-values-space-offset);
    ins--ble(be, vector-ok, count, BE-max-values);
    // Case where the BE frame doesn't hold all the values
    // Do a heap allocation
    let new-vec = op--safe-allocate-vector(be, count, 
                                           count, be-frame, up-frame, values-vec);
    ins--move(be, vec, new-vec);
  
    // Now have a suitable vector
    ins--tag(be, vector-ok);
    ins--add(be, count, count, 2); // allow for size and header too
    ins--push(be, vec); // preserve vec around copy (Pentium register pressure)
    ins--copy-words-down-w(be, vec, values-vec, count);
    ins--pop(be, vec);
    ins--st(be, vec, be-frame, BE-values-vector-offset);
  end with-harp;
end method;


define method op--restore-multiple-values-from-vector
    (be :: <harp-back-end>, vec-arg :: <register>)
  with-harp (be)
    result result;
    tmp 1, vec-data;
    nreg count, mv-area;
    tag single-value, multiple-values, no-values, done;

    let vec = op--duplicate(be, vec-arg);
    op--vector-size(be, count, vec);
    ins--bne(be, multiple-values, count, 1);
  
    // single value case
    ins--tag(be, single-value);
    ins--ld(be, result, vec, 8);
    ins--reset-values(be);
    ins--bra(be, done);
  
    // general multiple values case
    ins--tag(be, multiple-values);
    op--st-mv-count(be, count);
    ins--beq(be, no-values, count, 0);
  
    // actual multiple values case
    ins--add(be, vec-data, vec, 8);
    ins--ld(be, result, vec-data, 0);
    op--ld-mv-area-address(be, mv-area);
    ins--copy-words-down-w(be, mv-area, vec-data, count);
    ins--set-values(be);
    ins--bra(be, done);
  
    // zero values case
    ins--tag(be, no-values);
    ins--move(be, result, dylan-false);
    ins--set-values(be);
    ins--bra(be, done);
  
    ins--tag(be, done);
  end with-harp;
end method;



define method op--push-multiple-values (be :: <harp-back-end>)
  with-harp (be)
    result result;
    stack stack;
    nreg count;
    tmp 1, mv-area;

    // First push result
    ins--push(be, result);
    // Now all live MVs
    let have-mvs = make-tag(be);
    let mvs-done = make-tag(be);
    ins--bmvset(be, have-mvs);
    //          Case where there is a single value
    ins--push(be, 1);  // Use 1 to indicate special case
    ins--bra(be, mvs-done);
    //          Case where there are multiple values
    ins--tag(be, have-mvs);
    op--ld-mv-count(be, count);
    let count-in-bytes = op--multiply-by-4(be, count);
    ins--sub(be, stack, stack, count-in-bytes);
    op--ld-mv-area-address(be, mv-area);
    ins--copy-words-down-w(be, stack, mv-area, count);
    // Now push the count (in bytes)
    ins--push(be, count-in-bytes);
    ins--tag(be, mvs-done);
  end with-harp;
end method;

define method op--pop-multiple-values (be :: <harp-back-end>)
  with-harp (be)
    result result;
    stack stack;
    nreg count-in-bytes, mv-area;

    // First pop the MV count (in bytes)
    ins--pop(be, count-in-bytes);
    // Now pop all live MVs
    let have-mvs = make-tag(be);
    let mvs-done = make-tag(be);
    ins--bne(be, have-mvs, count-in-bytes, 1);  // check for special case
    //          Case where there is a single value
    ins--reset-values(be);
    ins--bra(be, mvs-done);
    //          Case where there are multiple values
    ins--tag(be, have-mvs);
    let count = op--divide-by-4(be, count-in-bytes);
    op--st-mv-count(be, count);
    op--ld-mv-area-address(be, mv-area);
    ins--copy-words-down-w(be, mv-area, stack, count);
    ins--add(be, stack, stack, count-in-bytes);
    ins--set-values(be);
    ins--tag(be, mvs-done);
    // Finally, pop result
    ins--pop(be, result);
  end with-harp;
end method;


define method op--safe-allocate-vector
    (be :: <harp-back-end>, size :: <register>, #rest preserved-regs)
    => (vec :: <register>);
  with-harp (be)
    result result;
    arg0 arg0;
    greg vec;

    for (reg in preserved-regs) ins--push(be, reg) end;
    ins--move(be, arg0, size);
    ins--call(be, primitive-allocate-vector-ref, 1);
    ins--move(be, vec, result);
    for (reg in reverse(preserved-regs)) ins--pop(be, reg) end;  
    vec;
  end with-harp;
end method;



/// OP--UNWIND-PROTECT
/// Implements invocations of unwind-protect for use in the runtime itself.


define method op--unwind-protect 
    (be :: <harp-back-end>, 
     protected-op :: <function>, 
     cleanup-op :: <function>)
  with-harp (be)
    arg0 arg0;
    tag cleanup-tag, finished-tag;

    ins--load-nlx-address(be, arg0, cleanup-tag);
    ins--call(be, primitive-build-unwind-protect-frame-ref, 1);
    protected-op(be);
    ins--call(be, primitive-unwind-protect-cleanup-ref, 1);
    ins--control-flow-link(be, cleanup-tag);
    ins--bra(be, finished-tag);
    ins--tag(be, cleanup-tag);
    cleanup-op(be);
    ins--end-cleanup(be, cleanup-tag);
    ins--tag(be, finished-tag);
  end with-harp;
end method;
