module:    powerpc-harp
Synopsis:  PowerPC float transcendentals - not currently available
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define powerpc-template
  (facos, fasin, fatan, fatanh, fcos, fcosh, fetox, fetoxm1, fgetexp,
   fgetman, fint, flog10, flog2, floge, flogep1, fsin, fsinh,
   ftan, ftanh, f10tox, f2tox)

  options (self);

  pattern (be, i, d, s)                // just dummy instructions for now...
    harp-out(be) fmove(be, d, s) end;  // this tells us what src and dest were!

end powerpc-template;


define powerpc-template 
   (dacos, dasin, datan, datanh, dcos, dcosh, detox, detoxm1, dgetexp,
    dgetman, dint, dlog10, dlog2, dloge, dlogep1, dsin, dsinh,
    dtan, dtanh, d10tox, d2tox)

  options (self);

  pattern (be, i, d, s)                // just dummy instructions for now...
    harp-out(be) dmove(be, d, s) end;  // this tells us what src and dest were!

end powerpc-template;
     

