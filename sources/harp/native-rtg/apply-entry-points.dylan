module:    harp-native-rtg
Synopsis:  Apply entry point generation for the Dylan rtg
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




//// Apply entry points



/// Simple Apply XEPs

/// This is the most primitive form of apply via XEPs. 
/// On entry, the calling convention is as for a normal XEP call,
/// (with the destination function in the function register).
//
/// The compiler should call the appropriate apply-xep rather than the 
/// xep associated with the function object. The apply-xep will spread 
/// out the rest arguments before tail-calling the function xep.
/// Only the dynamic case will look at the arg count. By using
/// apply-xep-n, the compiler is making a guarantee that it is passing
/// n normal arguments before the vector argument. In the dynamic case, 
/// the arg-count should include the rest parameter itself - and hence
/// there should be a minimum value of 1.
///
/// In this apply-xep context, the "required" arguments are taken to be
/// all the arguments apart from the last. This is slightly loose 
/// terminology - and it implies nothing about the expectations of the 
/// destination function - which will do its own checks in the XEP.
/// 
/// WARNING
/// Don't try and tail call one of these if the vector was stack 
/// allocated in your frame! There is no support here to preserve
/// the count convention. In this sense, apply-xep is just like a normal
/// xep.


define entry-point apply-xep (be :: <harp-back-end>, num)
  if (special-case?(be, num))
    op--apply-xep-discriminating-special(be, num);
  else
    op--apply-xep-discriminating(be, num);
  end
end entry-point;


define method op--apply-xep-discriminating-special
    (be :: <harp-back-end>, required :: <integer>)
  // This is a special case, because the vector argument is in a register.
  with-harp (be)
    stack stack;
    function function;
    arg-count argc;
    nreg vec;
    tmp 1, vec-elt2, ret-addr;
    nreg size, size-in-bytes;
    nreg hole-start;
    tag vector-in-registers;
    arg0 arg0;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    let elements-in-regs = max-num-arg-regs - required;
    let argvec = be.registers.reg-machine-arguments[required];
  
    // First get hold of the vector arg
    ins--move(be, vec, argvec);
    op--vector-size(be, size, vec);
  
    // Now check its size, before deciding what to do.
    // If the size is <= elements-in-regs, the stack stays the same.
    // Otherwise we extend.
    ins--ble(be, vector-in-registers, size, elements-in-regs);
    
    // Extend the stack by size-in-bytes minus 4 * elements-in-regs

    // decrement size by elements in regs
    ins--sub(be, size, size, elements-in-regs);
    ins--asl(be, size-in-bytes, size, 2);
    // Get hold of the return address
    if-return-address() ins--ld(be, ret-addr, stack, 0) end;
    ins--sub(be, stack, stack, size-in-bytes);
    // Put back the return address
    if-return-address() ins--st(be, ret-addr, stack, 0) end;

    // Copy the vector data onto the stack
    if-return-address()
      // store to just above ret addr
      ins--add(be, hole-start, stack, 4);
    else
      ins--move(be, hole-start, stack);
    end;
    // load from (elements-in-regs + 1)th word of the data
    ins--add(be, vec-elt2, vec, 8 + 4 * elements-in-regs);
    op--copy-words-with-update(be, #f, hole-start, vec-elt2, size);

    // And do the tail call

    ins--add(be, argc, size, max-num-arg-regs);   // manipulate the arg-count
    for (i :: <integer> from required below max-num-arg-regs)
      let reg :: <real-register> = be.registers.reg-machine-arguments[i];
      // load the vector data
      ins--ld(be, reg, vec, 8 + 4 * (i - required));
    end;
    ins--jmp-indirect(be, function, be.function-xep-offset, max-num-arg-regs);
  
    // Handle the case where no arguments go on the stack.

    ins--tag(be, vector-in-registers);

    if (max-num-arg-regs = 1)
      ins--move(be, argc, size);
      // We load arg0 regardless of whether the data is valid.
      ins--ld(be, arg0, vec, 8);  // load the first word of data
    else

      ins--add(be, argc, size, required);
      ins--add(be, vec, vec, 8);

      op--copy-registers-with-update
	(be, #f, #f, vec, size, required, to?: #t);

    end if;

    ins--jmp-indirect(be, function, be.function-xep-offset, max-num-arg-regs);
  end with-harp;
end method;


define open generic op--apply-xep-discriminating 
    (be :: <harp-back-end>, required);

define method op--apply-xep-discriminating 
    (be :: <harp-back-end>, required)

  op--check-apply-special-case(be, required);
  
  with-harp (be)
    stack stack;
    function function;
    arg-count argc;
    tmp 1, tmp, vec;
    nreg size, dummy;
    tag zero-vector, one-vector;
  
    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    let top-size = op--shuffle-size-for-apply(be, required);
    let req-index = op--multiply-by-4(be, top-size);

    // First get hold of the vector arg
    ins--ld(be, vec, stack, req-index);   
    op--vector-size(be, size, vec);

    // Now check its size, before deciding what to do.
    // If the size is zero, we must shorten the stack. If it's 1
    // the stack stays the same. Otherwise we extend.
    ins--beq(be, zero-vector, size, 0);
    ins--beq(be, one-vector, size, 1);

    // Handle the extend-stack case
    op--extend-stack-for-apply(be, vec, size, required, req-index);

    // Handle the keep-stack-the-same case
    ins--tag(be, one-vector);
    ins--ld(be, dummy, vec, 8);    // load the only element in the vector
    ins--st(be, dummy, stack, req-index);
    // The arg count is the same as the supplied number
    op--calculate-supplied-number-for-apply(be, argc, required);
    ins--jmp-indirect(be, function, be.function-xep-offset, max-num-arg-regs);
  
    // Handle the shrink-stack case.
    // We actually do this by putting the return address where the
    // vector was, calling with one less arg, followed by a return.
    ins--tag(be, zero-vector);
    op--calculate-required-number-for-apply(be, argc, required);

    op--preserve-return-address-for-apply(be, req-index);
    ins--call-indirect(be, function, be.function-xep-offset, max-num-arg-regs);
    op--restore-return-address-for-apply(be);

    ins--rts(be);
  end with-harp;
end method;


define open generic op--preserve-return-address-for-apply
    (be :: <harp-back-end>, req-index);

define open generic op--restore-return-address-for-apply
    (be :: <harp-back-end>);

define open generic op--extend-stack-for-apply
    (be :: <harp-back-end>, vec, size, required, req-index);

define method op--extend-stack-for-apply
    (be :: <harp-back-end>, vec, size, required, req-index)
  with-harp (be)
    stack stack;
    function function;
    arg-count argc;
    nreg nsize, first-opt, hole-start;
  
    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    // Handle the extend-stack case
    ins--sub(be, size, size, 1);  // decrement the size to allow for vector arg
    let ntop-size = op--divide-by-4(be, req-index);
    op--shuffle-stack(be, first-opt, #f, ntop-size, 0, size);
    op--vector-size(be, nsize, vec);  // get back the vector size
    ins--add(be, vec, vec, 8);        // the start of the data
    // save the final arg count around the shuffle
    op--calculate-arg-count-for-apply(be, argc, required, req-index, nsize);
    ins--add(be, hole-start, stack, req-index);
    op--copy-words-with-update(be, #f, hole-start, vec, nsize);
    ins--jmp-indirect(be, function, be.function-xep-offset, max-num-arg-regs);
  end with-harp;
end method;


// OP--CALCULATE-ARG-COUNT-FOR-APPLY
// This gets the arg count from the req-index (may help relieve
// the register pressure in remembering the passed arg count);

define method op--calculate-arg-count-for-apply
    (be :: <harp-back-end>, dest :: <register>, required :: <integer>, req-index, vec-size)
  ins--add(be, dest, vec-size, required);
end method;

define method op--calculate-arg-count-for-apply
    (be :: <harp-back-end>, dest :: <register>, required == #"dynamic", req-index, vec-size)
  // req-num is the number of passed parameters on stack _apart_ from the vector
  let req-num = op--divide-by-4(be, req-index);
  ins--add(be, dest, vec-size, req-num);

  // Adjust arg-count by the shuffle size factored into req-num previously
  let shuffle-size = required-arguments-in-registers(be, required) - be.return-address-size;
  unless (shuffle-size == 0)
    ins--add(be, dest, dest, shuffle-size);
  end;
end method;


// OP--CALCULATE-REQUIRED-NUMBER-FOR-APPLY
// This is a DU op which sets the destination register with
// the number of required parameters passed to the apply primitive


define method op--calculate-required-number-for-apply
    (be :: <harp-back-end>, 
     dest :: <register>, 
     required :: <integer>)
  ins--move(be, dest, required);
end method;


define method op--calculate-required-number-for-apply
    (be :: <harp-back-end>, 
     dest :: <register>, 
     required == #"dynamic")
  // In the dynamic case, the arg count is one less than the
  // number of supplied parameters
  ins--sub(be, dest, be.registers.reg-arg-count, 1);
end method;


// OP--CALCULATE-SUPPLIED-NUMBER-FOR-APPLY
// This is a DU op which sets the destination register with
// the number of parameters passed to the apply primitive


define method op--calculate-supplied-number-for-apply
    (be :: <harp-back-end>, 
     dest :: <register>, 
     required :: <integer>)
  ins--move(be, dest, required + 1);
end method;


define method op--calculate-supplied-number-for-apply
    (be :: <harp-back-end>, 
     dest :: <register>, 
     required == #"dynamic")
  // In the dynamic case, the arg count is one less than the
  // number of supplied parameters
  ins--move(be, dest, be.registers.reg-arg-count);
end method;



// We want the dynamic case of apply to handle (max-num-arg-regs - 1) "required" args.
// Look for this case especially. (It happens when the arg-count is <= max-num-arg-regs)

define method op--check-apply-special-case
    (be :: <harp-back-end>, required);
end method;

define method op--check-apply-special-case
    (be :: <harp-back-end>, required == #"dynamic");
  with-harp (be)
    arg-count argc;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    for (i :: <integer> from 0 below max-num-arg-regs)
      let continue = make-tag(be);
      op--arg-count-check(be, continue, argc, i + 1, #f);
      ins--jmp(be,
	       ins--constant-ref(be,
				 entry-point-name(be, "apply-xep", i)),
	       i + 1);
      ins--tag(be, continue);
    end;
      

  end with-harp;
end method;



/// OP--SHUFFLE-SIZE-FOR-APPLY
/// This returns either an integer or a register containing the number
/// of words of data at the top of stack which must be shuffled to make
/// room for spreading the vector for an apply. The result is based on the
/// the number of arguments passed. Here "requireds" refers to the arguments
/// passed _apart_ from the last vector arg.

define method op--shuffle-size-for-apply
    (be :: <harp-back-end>, requireds :: <integer>) => (r :: <integer>);
  op--shuffle-size-for-requireds(be, requireds);
end method;


define method op--shuffle-size-for-apply
    (be :: <harp-back-end>, requireds == #"dynamic") 
    => (r :: <register>);
  with-harp (be)
    arg-count argc;
    nreg copy-count;
    let args-in-regs = required-arguments-in-registers(be, requireds);
    // allow for the return address
    let extras-on-stack = be.return-address-size;
    let shuffle-size = extras-on-stack - args-in-regs;
    // now calculate the amount to copy. Its supplied-required + shuffle-size
    ins--sub(be, copy-count, argc, 1);  // number of supplied requireds
    unless (shuffle-size == 0)
       ins--add(be, copy-count, copy-count, shuffle-size);  // amount to copy
    end unless;
    copy-count;
  end with-harp;
end method;


