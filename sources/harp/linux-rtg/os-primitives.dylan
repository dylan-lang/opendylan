module:    linux-rtg
Synopsis:  OS specific Primitives for the Dylan Linux runtime generator
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define c-fun runtime-external c-primitive-start-timer-ref = "c_primitive_start_timer";

define c-fun runtime-external c-primitive-stop-timer-ref = "c_primitive_stop_timer";


define unix-runtime-primitive start-timer

  op--call-c(be, c-primitive-start-timer-ref);
  ins--rts-and-drop(be, 0);

end unix-runtime-primitive;

define unix-runtime-primitive stop-timer
  // On entry:
  //    
  // On exit:
  //   An SOV containing the elapsed time as #[seconds, microseconds]

  op--call-c(be, c-primitive-stop-timer-ref);
  ins--rts-and-drop(be, 0);

end unix-runtime-primitive;


define c-fun runtime-external ExitProcess-ref    = "exit";

define used-by-client unix-runtime-primitive exit-application
  // On entry: raw-int-status
  //    
  // On exit: entire process is terminated
  arg0 status;

  op--call-c(be, ExitProcess-ref, status);
  ins--rts-and-drop(be, 0);
end unix-runtime-primitive;
