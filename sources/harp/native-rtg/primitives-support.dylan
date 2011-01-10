module:    native-rtg
Synopsis:  Support for primitive generation for the Dylan runtime
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// with-preserved-arguments
//   The body is generated in a context in which the argument registers
//   and any other explicitly mentioned registers are preserved.

define macro with-preserved-arguments 
  { with-preserved-arguments (?registers:*) ?:body end }
    => { with-harp (?=be)
           let other-regs = vector(?registers);
	   for (reg in ?=be.registers.reg-machine-arguments)
	     ins--push(?=be, reg);
	   end for;
           for (reg in other-regs) ins--push(?=be, reg) end;
           ?body;
           for (reg in other-regs.reverse) ins--pop(?=be, reg) end;
	   for (reg in ?=be.registers.reg-machine-arguments.reverse)
	     ins--pop(?=be, reg);
	   end for;
         end with-harp }

registers:
  { } => { }
  { ?:name, ... } => { ?=be.registers. "reg-" ## ?name, ... }
end macro;



// op--select-entry-point
//   A DUU op which loads the destination register with the appropriate constant
//   reference of the specific entry point corresponding to the supplied set of refs,
//   given the required parameter number in U2


ignore(op--select-entry-point);

define method op--select-entry-point
    (be :: <harp-back-end>, dest :: <register>, required, refs :: <vector>) 
    => ()
  with-harp (be)
    tag done;

    // Check first for dynamic case
    let limit = refs.size - 1;
    ins--move(be, dest, refs[limit]);
    ins--bge(be, done, required, limit);

    // Now check for each static case
    for (i from 0 below limit)
      let inapplicable = make-tag(be);
      ins--bne(be, inapplicable, required, i);
      ins--move(be, dest, refs[i]);
      ins--bra(be, done);
      ins--tag(be, inapplicable);
    end for;

    ins--tag(be, done);
  end with-harp;
end method;



/// op--load-argument-n-leafcase
// This function loads an argument in the prolog of an IEP (i.e. while
// still without a frame).

ignore(op--load-argument-n-leafcase);

define method op--load-argument-n-leafcase
    (be :: <harp-back-end>, argument :: <register>, arg-num :: <integer>) 
    => ()
  let in-regs = be.registers.arguments-passed-in-registers;
  let last-in-reg = in-regs - 1; // the number of the last arg to be passed in a reg
  if (arg-num > last-in-reg)
    // The arg is on the stack
    ins--load-stack-arg-n(be, argument, arg-num - in-regs);
  else
    // The arg is in a register
    ins--move(be, argument, be.registers.reg-machine-arguments[arg-num]);
  end if;
end method;
  

// For efficiency, the <register> method for op--load-argument-n-leafcase
// takes the arg-num as a raw value already multiplied by 4 (to
// use for a byte offset). Callers can probably calculate this 
// value just as easily as the non-multiplied value.
  
define method op--load-argument-n-leafcase
    (be :: <harp-back-end>, argument :: <register>, arg-num :: <register>)
    => ()
  let in-regs = be.registers.arguments-passed-in-registers;
  // size of return address on stack
  let non-arg-size = be.return-address-size-in-bytes;
  let offset-for-arg-base = non-arg-size - (4 * in-regs);
  with-harp (be)
    stack stack;
    nreg stack-offset;

    if (offset-for-arg-base == 0) // true for some back-ends, e.g. Pentium
      ins--move(be, stack-offset, arg-num);
    else
      ins--add(be, stack-offset, arg-num, offset-for-arg-base);
    end if;
    ins--ld(be, argument, stack, stack-offset);
  end with-harp;
end method;



/// op--load-argument-n-via-frame
// This function loads an argument relative to the frame pointer.
// The caller must know that the frame has been built.

ignore(op--load-argument-n-via-frame);

define method op--load-argument-n-via-frame
    (be :: <harp-back-end>, argument :: <register>, arg-num :: <integer>) 
    => ()
  let in-regs = be.registers.arguments-passed-in-registers;
  let last-in-reg = in-regs - 1; // the number of the last arg to be passed in a reg
  if (arg-num > last-in-reg)
    // The arg is on the stack
    let stack-num = arg-num - in-regs;
    let frame-offset = stack-num + 2; // one for ret-addr, one for saved FP
    with-harp (be)
      frame frame;
      ins--ld(be, argument, frame, frame-offset * 4);
    end with-harp;
  else
    // The arg is in a register
    ins--move(be, argument, argument-register(arg-num));
  end if;
end method;
  

// For efficiency, the <register> method for op--load-argument-n-via-frame
// takes the arg-num as a raw value already multiplied by 4 (to
// use for a byte offset). Callers can probably calculate this 
// value just as easily as the non-multiplied value.
  
define method op--load-argument-n-via-frame
    (be :: <harp-back-end>, argument :: <register>, arg-num :: <register>)
    => ()
  let in-regs = be.registers.arguments-passed-in-registers;
  let non-arg-size = 8; // size of return address + saved FP
  let offset-for-arg-base = non-arg-size - (4 * in-regs);
  with-harp (be)
    frame frame;
    nreg frame-offset;

    ins--add(be, frame-offset, arg-num, offset-for-arg-base);
    ins--ld(be, argument, frame, frame-offset);
  end with-harp;
end method;


define method setup-args-for-dylan-call 
      (be :: <harp-back-end>, args) => (num-in-regs :: <integer>)
  let regs = be.registers;
  let argsize = args.size;
  let num-in-regs = min(argsize, regs.arguments-passed-in-registers);
  // Push the stack args in reverse order
  for (i from argsize - 1 to num-in-regs by -1)
    ins--push(be, args[i]);
  end for;
  // place the register args
  for (i from 0 below num-in-regs)
    ins--move(be, regs.reg-machine-arguments[i], args[i]);
  end for;
 num-in-regs;
end method;

define method setup-args-for-c-call 
      (be :: <harp-back-end>, args) => (num-in-regs :: <integer>)
  let regs = be.registers;
  let argsize = args.size;
  let num-in-regs = min(argsize, regs.c-arguments-passed-in-registers);
  // Push the stack args in reverse order
  for (i from argsize - 1 to num-in-regs by -1)
    ins--push(be, args[i]);
  end for;
  // place the register args
  for (i from 0 below num-in-regs)
    ins--move(be, regs.reg-c-machine-arguments[i], args[i]);
  end for;
 num-in-regs;
end method;

ignore(setup-args-for-c-call-from-memory);

define method setup-args-for-c-call-from-memory
    (be :: <harp-back-end>,
     base-of-args :: <register>, arg-size :: <register>)
 => ()
  with-harp (be)

  nreg size-in-bytes, new-args;
  stack stack;
  tag args-to-stack, done;

  op--copy-registers-with-update
    (be, base-of-args, arg-size, base-of-args, arg-size, 0,
     to?: #t, C?: #t,
     done-tag: done,
     continue-tag: args-to-stack);

  ins--tag(be, args-to-stack);
  ins--asl(be, size-in-bytes, arg-size, 2);
  ins--sub(be, stack, stack, size-in-bytes);
  ins--move(be, new-args, stack);
  ins--copy-words-down-w(be, new-args, base-of-args, arg-size);

  ins--tag(be, done);

  end with-harp;
end method;

define method op--call-iep
    (be :: <harp-back-end>, func, #rest args)
  let num-in-regs = setup-args-for-dylan-call(be, args);
  // Do the call  
  ins--call(be, func, num-in-regs);
end method;

define method op--call-xep
    (be :: <harp-back-end>, func, #rest args)
  let regs = be.registers;
  let num-in-regs = setup-args-for-dylan-call(be, args);
  // set the xep registers ...
  ins--move(be, regs.reg-function, func);
  ins--move(be, regs.reg-arg-count, args.size);
  // Do the call  
  ins--call-indirect(be, regs.reg-function, be.function-xep-offset, num-in-regs);
end method;

define method op--call-c 
    (be :: <harp-back-end>, name :: <byte-string>, #rest args)
  let name-ref = ins--constant-ref(be, c-mangle(be, name));
  apply(op--call-c, be, name-ref, args);
end method;

define method op--call-c 
    (be :: <harp-back-end>, name-ref :: <constant-reference>, #rest args)
  // First push any caller-reserved-space
  op--push-space-for-callee(be);

  let num-in-regs = setup-args-for-c-call(be, args);
  let num-on-stack = args.size - num-in-regs;

  // Do the call  
  ins--call-alien(be, name-ref, 0);
  // And pop the args
  unless (num-on-stack == 0)
    let stack = be.registers.reg-stack;
    ins--add(be, stack, stack, 4 * num-on-stack);
  end unless;

  // And pop the caller-reserved-space
  op--pop-space-for-callee(be);

end method;

define open generic op--push-space-for-callee
    (be :: <harp-back-end>);

define method op--push-space-for-callee
    (be :: <harp-back-end>)
end;

define open generic op--pop-space-for-callee
    (be :: <harp-back-end>);

define method op--pop-space-for-callee
    (be :: <harp-back-end>)
end;


ignore(op--unimplemented-primitive);

define method op--unimplemented-primitive (be :: <harp-back-end>)
  ins--call(be, primitive-error-ref, 0);
  ins--rts-and-drop(be, 0);
end method;


/*
define method op--runtime-error (be :: <harp-back-end>)
  ins--call(be, primitive-error-ref, 0);
  ins--rts-and-drop(be, 0);
end method;
*/


define inline method op--rts-dropping-n-args 
    (be :: <harp-back-end>, num-args :: <integer>)
  let reg-args = be.registers.arguments-passed-in-registers;
  let stack-args = max(num-args - reg-args, 0);
  ins--rts-and-drop(be, 4 * stack-args);
end method;


ignore(op--rts-dropping-n-requireds-and-optionals);

define method op--rts-dropping-n-requireds-and-optionals
    (be :: <harp-back-end>, req-num :: <integer>)
  let reg-args = be.registers.arguments-passed-in-registers;
  let stack-args
    = if (req-num > reg-args)
        req-num - reg-args + 1;
      elseif (req-num = reg-args)
        1;    // just the rest arg is on the stack
      else 0; // rest arg in a register
      end if;
  let count-reg-offset = stack-args; // count reg follows the stack args
  with-harp (be)
    nreg count-reg;
    ins--load-stack-arg-n(be, count-reg, count-reg-offset);
    ins--rts-and-drop(be, count-reg);
  end with-harp;
end method;


// For efficiency, the <register> method for 
// op--rts-dropping-n-requireds-and-optionals
// takes the req-num number as a raw value already multiplied by 4
// (to use for a byte offset). Callers can probably calculate this 
// value just as easily as the non-multiplied value.
// NB THIS MAY ONLY BE USED IF ALL ARGUMENT REGISTERS ARE KNOWN TO BE FULL


define method op--rts-dropping-n-requireds-and-optionals
    (be :: <harp-back-end>, req-num :: <register>)
  let reg-args = be.registers.arguments-passed-in-registers;
  // Allow for rest and ret-addr on stack
  // Amount to be added is bytes%(1 + ret-addr + number-required - reg-args)
  let adjust = (1 + be.return-address-size - reg-args) * 4;
  with-harp (be)
    stack stack;
    nreg count-reg, count-reg-offset;
    ins--add(be, count-reg-offset, req-num, adjust);
    ins--ld(be, count-reg, stack, count-reg-offset);
    ins--rts-and-drop(be, count-reg);
  end with-harp;
end method;


define method op--load-arguments 
    (be :: <harp-back-end>, #rest regs) => ()
  let args-in-regs =
    min(regs.size,
	be.registers.arguments-passed-in-registers);
  for (i :: <integer> from 0 below args-in-regs)
    let reg = regs[i];
    if (reg)
      ins--move(be, reg, be.registers.reg-machine-arguments[i]);
    end;
  end;
  for (i :: <integer> from args-in-regs below regs.size)
    let reg = regs[i];
    if (reg)
      ins--load-stack-arg-n(be, reg, i - args-in-regs);
    end;
  end for;
end method;

ignore(op--load-arguments-leafcase);

define method op--load-arguments-leafcase 
    (be :: <harp-back-end>, #rest regs) => ()
  let args-in-regs =
    min(regs.size,
	be.registers.arguments-passed-in-registers);
  for (i :: <integer> from 0 below args-in-regs)
    let reg = regs[i];
    if (reg)
      ins--move(be, reg, be.registers.reg-machine-arguments[i]);
    end;
  end;
  for (i :: <integer> from args-in-regs below regs.size)
    let reg = regs[i];
    if (reg)
      let stack = be.registers.reg-stack;
      ins--ld(be, reg, stack, 4 * (i - args-in-regs + be.return-address-size));
    end;
  end for;
end method;


define method op--c-load-arguments 
    (be :: <harp-back-end>, #rest regs) => ()
  let args-in-regs =
    min(regs.size,
	be.registers.c-arguments-passed-in-registers);
  for (i :: <integer> from 0 below args-in-regs)
    ins--move(be, regs[i], be.registers.reg-c-machine-arguments[i]);
  end;
  for (i :: <integer> from args-in-regs below regs.size)
    ins--load-stack-arg-n(be, regs[i], i - args-in-regs);
  end for;
end method;

ignore(op--ld-mv-count);

define method op--ld-mv-count
    (be :: <harp-back-end>, dest :: <register>) => ()
  ins--ld-teb(be, dest, be.teb-mv-count-offset);
end method;


ignore(op--st-mv-count);

define method op--st-mv-count
    (be :: <harp-back-end>, data) => ()
  ins--st-teb(be, data, be.teb-mv-count-offset);
end method;


ignore(op--ld-mv-area-address);

define method op--ld-mv-area-address
    (be :: <harp-back-end>, dest :: <register>) => ()
  ins--ld-teb-address(be, dest, be.teb-mv-area-offset);
end method;


ignore(op--ld-current-unwind-protect-frame);

define method op--ld-current-unwind-protect-frame 
    (be :: <harp-back-end>, dest :: <register>) => ()
  ins--ld-teb(be, dest, be.teb-dynamic-environment-offset);
end method;


ignore(op--st-current-unwind-protect-frame);

define method op--st-current-unwind-protect-frame 
    (be :: <harp-back-end>, data) => ()
  ins--st-teb(be, data, be.teb-dynamic-environment-offset);
end method;



// OP--COPY-WORDS-WITH-UPDATE
// A processor specific DUUU op, which does a copy-words, and loads
// dest with the location  immediately after the copy. This can 
// be implemented efficiently on most platforms.

define open generic op--copy-words-with-update
    (be :: <harp-back-end>, dest, to, from, copy-count);


