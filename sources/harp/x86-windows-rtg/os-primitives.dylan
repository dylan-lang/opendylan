module:    harp-x86-windows-rtg
Synopsis:  OS specific Primitives for the Dylan X86 Windows runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// OS-specific primitives


// First, the constants

define win-fun runtime-external win-QueryCounter   = "QueryPerformanceCounter",   data:  "4";
define win-fun runtime-external win-QueryFrequency = "QueryPerformanceFrequency", data:  "4";

// define win-fun runtime-external win-CreateProcess  = "CreateProcess",             data:  "40";
define win-fun runtime-external win-ExitProcess    = "ExitProcess",               data:  "4";


define runtime-variable counter-start-lo     = "%counter_start_lo";
define runtime-variable counter-start-hi     = "%counter_start_hi";
define runtime-variable counter-stop-lo      = "%counter_stop_lo";
define runtime-variable counter-stop-hi      = "%counter_stop_hi";
define runtime-variable counter-frequency-lo = "%counter_frequency_lo";
define runtime-variable counter-frequency-hi = "%counter_frequency_hi";


define win32-runtime-primitive start-timer
  // On entry:
  //    
  // On exit:
  //   

  result res;

  op--stdcall-c(be, win-QueryCounter, as-direct-ref(be, counter-start-lo));
  ins--rts-and-drop(be, 0);
end win32-runtime-primitive;


define win32-runtime-primitive stop-timer
  // On entry:
  //    
  // On exit:
  //   An SOV containing the elapsed time as #[seconds, microseconds]

  result res;
  arg0 arg0;
  nreg counts-lo, counts-hi, secs, mics, rem, mlo, mhi;
  greg sov;
  tag got-data, no-counter;

  op--stdcall-c(be, win-QueryCounter, as-direct-ref(be, counter-stop-lo));
  op--stdcall-c(be, win-QueryFrequency, as-direct-ref(be, counter-frequency-lo));
  ins--beq(be, no-counter, res, 0);

  op--sub64(be, counts-lo, counts-hi, 
            counter-stop-lo, counter-stop-hi, 
            counter-start-lo, counter-start-hi);
  // assume frequency-hi == 0
  ins--divuxx(be, secs, rem, counts-lo, counts-hi, counter-frequency-lo);
  ins--mulux(be, mlo, mhi, rem, 1000000);
  ins--divuxx(be, mics, #f, mlo, mhi, counter-frequency-lo);

  ins--tag(be, got-data);
  ins--move(be, arg0, 2); // Allocate a 2 word vector
  ins--call(be, primitive-allocate-vector-ref, 1);
  ins--move(be, sov, res);
  ins--st(be, tag-as-integer(be, secs), sov, 8);
  ins--st(be, tag-as-integer(be, mics), sov, 12);
  ins--move(be, res, sov);
  ins--rts-and-drop(be, 0);

  ins--tag(be, no-counter);
  ins--move(be, secs, 0);
  ins--move(be, mics, 0);
  ins--bra(be, got-data);
end win32-runtime-primitive;


define used-by-client win32-runtime-primitive exit-application
  // On entry: raw-int-status
  //    
  // On exit: entire process is terminated
  arg0 status;
  op--stdcall-c(be, win-ExitProcess, status);
  ins--rts-and-drop(be, 0);
end win32-runtime-primitive;
