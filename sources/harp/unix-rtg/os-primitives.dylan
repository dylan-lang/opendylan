module:    harp-unix-rtg
Synopsis:  OS specific Primitives for the Dylan Unix runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define c-fun runtime-external ExitProcess-ref    = "exit";

define used-by-client unix-runtime-primitive exit-application
  // On entry: raw-int-status
  //    
  // On exit: entire process is terminated
  arg0 status;

  op--call-c(be, ExitProcess-ref, status);
  ins--rts-and-drop(be, 0);
end unix-runtime-primitive;
