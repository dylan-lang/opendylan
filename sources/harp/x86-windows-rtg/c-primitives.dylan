module:    harp-x86-windows-rtg
Synopsis:  C call-in Primitives for the Dylan X86 Windows runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define win32-runtime-primitive spy-exit-application
  // On exit: entire process is terminated
  op--stdcall-c(be, win-ExitProcess, 0);
  ins--rts(be);
end win32-runtime-primitive;

define win32-runtime-primitive spy-fixup-imported-dylan-data
  // On entry:
  //   fixup-start, fixup-end
  // On exit:
  //    no result - imported references in static data will have been fixed up


  nreg fixup-start, fixup-end;

  op--c-load-arguments(be, fixup-start, fixup-end);
  op--call-iep(be, primitive-reference(fixup-imported-dylan-data), 
	       fixup-start, fixup-end);
  ins--rts(be);

end win32-runtime-primitive;

define win32-runtime-primitive spy-fixup-unimported-dylan-data
  // On entry:
  //   fixup-start, fixup-end
  // On exit:
  //    no result - unimported references in static data will have been fixed up


  nreg fixup-start, fixup-end;

  op--c-load-arguments(be, fixup-start, fixup-end);
  op--call-iep(be, primitive-reference(fixup-unimported-dylan-data),
               fixup-start, fixup-end);
  ins--rts(be);

end win32-runtime-primitive;
