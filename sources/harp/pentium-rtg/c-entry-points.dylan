module:    pentium-rtg
Synopsis:  C entry point generation for the Dylan Pentium rtg
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




//// C entry points



/// C XEPs
/// These allow valid Dylan function objects to call into C. The C
/// function is referenced by the IEP. The XEP accepts the same
/// args as the C function - but adjusts for the C calling convention.

/// For the Pentium, the C calling convention differs as follows:
///
///                    Dylan                      C
//
//    Push on stack:   R to L                    R to L
//    Registers:       1st arg                   none
//    Result:          eax                       eax
//    Pop args:        callee                    caller
//    Preserved:       none                      esi, edi


define entry-point c-xep (be :: <pentium-back-end>, required)
  let check-args-on-entry = (required ~= #"dynamic");
  op--general-xep-support (be, required, #f, op--c-xep-internal, 
                           check-specializers: #f,
                           check-args?: check-args-on-entry);
end entry-point;



define method op--c-xep-internal 
    (be :: <pentium-back-end>, required == 0)
  // Special case. If there are no arguments, the C and Dylan
  // conventions are compatible. Hence we can just tail call the IEP
end method;


define method op--c-xep-internal 
    (be :: <pentium-back-end>, required :: <integer>)
  // Where we know the number of arguments:
  // 1. pop the return address into a C preserved register
  // 2. push the arg0 register
  // 3. call the iep
  // 4. pop the args on the stack
  // 5. Jump to the return address
  let regs = be.registers;
  let function = regs.reg-function;
  let stack = regs.reg-stack;
  let arg0 = regs.reg-arg0;
  let ret-addr = regs.c-preserved-register-vector[0];
  ins--ld(be, ret-addr, stack, 0);
  ins--st(be, arg0, stack, 0);
  ins--call-indirect(be, function, be.function-iep-offset, 1);
  ins--add(be, stack, stack, 4 * required);
  ins--jmp(be, ret-addr, 1);
end method;

define method op--c-xep-internal 
    (be :: <pentium-back-end>, required)
  // Where we don't know the number of arguments:
  // Test first to see if se got 0. If so, do tail call.
  // Otherwise, adjust stack and call, preserving the arg-count too.
  // On return, decide how many args to pop and the jump to return addr.
  let regs = be.registers;
  let function = regs.reg-function;
  let stack = regs.reg-stack;
  let arg0 = regs.reg-arg0;
  let argc = regs.reg-arg-count;
  let ret-addr = regs.c-preserved-register-vector[0];
  let save-argc = regs.c-preserved-register-vector[1];
  let non-zero-args = make-tag(be);
  let tail-jump-to-iep = make-tag(be);
  
  op--arg-count-check(be, non-zero-args, argc, 0, #f);
  ins--bra(be, tail-jump-to-iep);

  ins--tag(be, non-zero-args);
  ins--ld(be, ret-addr, stack, 0);
  ins--st(be, arg0, stack, 0);
  ins--move(be, save-argc, argc);
  ins--call-indirect(be, function, be.function-iep-offset, 1);
  ins--and(be, save-argc, save-argc, #xff);
  ins--asl(be, save-argc, save-argc, 2);
  ins--add(be, stack, stack, save-argc);
  ins--jmp(be, ret-addr, 1);

  ins--tag(be, tail-jump-to-iep);
end method;

