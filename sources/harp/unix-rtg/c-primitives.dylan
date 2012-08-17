module:    harp-unix-rtg
Synopsis:  C call-in Primitives for the Dylan Unix runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define unix-runtime-primitive spy-exit-application
  // On exit: entire process is terminated
  op--call-c(be, ExitProcess-ref, 0);
  ins--rts(be);
end unix-runtime-primitive;

define unix-runtime-primitive spy-fixup-imported-dylan-data
  // On entry:
  //   fixup-start, fixup-end
  // On exit:
  //    no result - imported references in static data will have been fixed up

  ins--rts(be);

end unix-runtime-primitive;

define unix-runtime-primitive spy-fixup-unimported-dylan-data
  // On entry:
  //   fixup-start, fixup-end
  // On exit:
  //    no result - unimported references in static data will have been fixed up

  ins--rts(be);

end unix-runtime-primitive;
