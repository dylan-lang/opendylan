module:    linux-rtg
Synopsis:  C call-in Primitives for the Dylan Linux runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define linux-runtime-primitive spy-exit-application
  // On exit: entire process is terminated
  op--call-c(be, ExitProcess-ref, 0);
  ins--rts(be);
end linux-runtime-primitive;

define linux-runtime-primitive spy-fixup-imported-dylan-data
  // On entry:
  //   fixup-start, fixup-end
  // On exit:
  //    no result - imported references in static data will have been fixed up

  ins--rts(be);

end linux-runtime-primitive;

define linux-runtime-primitive spy-fixup-unimported-dylan-data
  // On entry:
  //   fixup-start, fixup-end
  // On exit:
  //    no result - unimported references in static data will have been fixed up

  ins--rts(be);

end linux-runtime-primitive;
