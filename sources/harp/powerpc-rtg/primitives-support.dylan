module:    powerpc-rtg
Synopsis:  Support for primitive generation for the PowerPC runtime
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// 6 words of space must be reserved at the top of the stack 
/// for 6 Table-Of-Contents, Link-register ....

define constant $callee-reserved-space = 6;

// Reserve some space on stack for callee to use for preserved registers

define sideways method op--push-space-for-callee
    (be :: <powerpc-back-end>)
  let stack = be.registers.reg-stack;
  ins--sub(be, stack, stack, 4 * $callee-reserved-space);
end;

// Reclaim the caller-reserved-space
define sideways method op--pop-space-for-callee
    (be :: <powerpc-back-end>)
  let stack = be.registers.reg-stack;
  ins--add(be, stack, stack, 4 * $callee-reserved-space);
end;
