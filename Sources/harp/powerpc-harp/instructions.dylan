module:    powerpc-harp
Synopsis:  The PowerPC HARP instruction set
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The PowerPC instruction set


define instruction-set <powerpc-instruction-set>
     (<instruction-set>, <powerpc-back-end>)
  
  create powerpc-instructions, inheriting: default-instructions;

  u   op set-control-register;

  d   op get-control-register;

  // t   op stack-check;
  t   op bsr;

  // u   op increment-call-count;

      op save-regs, restor-regs;

  u op save-return-address, restore-return-address;

  // duuu op machine-specific;

end;


define sealed inline method instructions (be :: <powerpc-back-end>)
 => (instruction-set :: <powerpc-instruction-set>)
  powerpc-instructions
end method;
