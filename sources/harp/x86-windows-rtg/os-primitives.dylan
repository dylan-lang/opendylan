module:    harp-x86-windows-rtg
Synopsis:  OS specific Primitives for the Dylan X86 Windows runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// OS-specific primitives


// First, the constants

// define win-fun runtime-external win-CreateProcess  = "CreateProcess",             data:  "40";
define win-fun runtime-external win-ExitProcess    = "ExitProcess",               data:  "4";


define used-by-client win32-runtime-primitive exit-application
  // On entry: raw-int-status
  //    
  // On exit: entire process is terminated
  arg0 status;
  op--stdcall-c(be, win-ExitProcess, status);
  ins--rts-and-drop(be, 0);
end win32-runtime-primitive;
