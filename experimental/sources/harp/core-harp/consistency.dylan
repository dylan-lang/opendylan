module:    main-harp
Synopsis:  Consistency checking of HARP registers and tags
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




/// It is an error to pass virtual registers or tags to HARP if they were
/// created within a different call to invoke-harp. The symptoms of the 
/// error can be subtle - so here is a consistency checker which may be used 
/// to give a more definite indication that something's wrong.




// The following method is a handy way of checking
// for registers and tags which were created during an earlier 
// invocation.

define open generic harp-consistency-check (backend :: <harp-back-end>) => ();

define method harp-consistency-check (backend :: <harp-back-end>) => ()
  for (operand in backend.variables.sv-instructions)
    check-consistent-operand(backend, operand);
  end for;
end method;


define open generic check-consistent-operand 
    (be :: <harp-back-end>, operand) => ();

define method check-consistent-operand 
    (be :: <harp-back-end>, operand) => ()
  #f;
end method;

define method check-consistent-operand 
    (be :: <harp-back-end>, operand :: <tag>) => ()
  unless (operand.tag-variables == be.variables)
    inconsistent-operand-error(be, operand);
  end unless;
end method;

define method check-consistent-operand 
    (be :: <harp-back-end>, operand :: <virtual-register>) => ()
  unless (operand.virtual-register-vreg-state == be.variables.vreg-state)
    inconsistent-operand-error(be, operand);
  end unless;
end method;

define method inconsistent-operand-error (be :: <harp-back-end>, operand) => ()
  harp-warning(be, "Inconsistent operand %=.\n", operand);
end method;
