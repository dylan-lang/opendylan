module:     devel-dbg-ui
synopsis:   The stop-reason processing callback.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <create-process-stop-reason>)
                 => (interested? :: <boolean>)
   
   if (*current-debugger-options*.show-debug-output?)
     debugger-message ("Application has started running.");
   end if;
   application.application-thread-table :=
       add! (application.application-thread-table,
             stop-reason.stop-reason-thread);
   #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <create-thread-stop-reason>)
                 => (interested? :: <boolean>)
  if (*current-debugger-options*.show-debug-output?)
    debugger-message ("Application created a thread.");
  end if;
  let thread = stop-reason.stop-reason-thread;
  application.application-thread-table
    := add!(application.application-thread-table, thread);
  maybe-profile-new-thread(application, thread);
  #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <exit-thread-stop-reason>)
                 => (interested? :: <boolean>)
  if (*current-debugger-options*.show-debug-output?)
    debugger-message ("Application destroyed a thread.");
  end if;
  application.application-thread-table :=
    remove! (application.application-thread-table,
	     stop-reason.stop-reason-thread);
  let profiler-run = application.application-profiler-run;
  if (profiler-run)
    let options = profiler-run.profiler-options;
    let threads = options.profiler-threads;
    if (member?(stop-reason.stop-reason-thread, threads))
      remove!(threads, stop-reason.stop-reason-thread);
    end if;
  end if;
  #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <exit-process-stop-reason>)
                 => (interested? :: <boolean>)

  if (*current-debugger-options*.show-debug-output?)
    debugger-message ("Application terminated with exit code %d.",
                      stop-reason-process-exit-code (stop-reason));
  end if;
  let profiler-run = application.application-profiler-run;
  if (profiler-run & ~profiler-run.profiler-results)
    stop-profiling(application);
    debugger-message("Switched off the profiler.");
    debugger-message("Processing results, please wait...");
    process-profile-data(application);
    debugger-message("Done processing results.");
    print-profile-results(application);
    application.application-profiler-run := #f;
  end if;
  #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <load-library-stop-reason>)
                 => (interested? :: <boolean>)

   local method strip-hqn-prefix (libname :: <string>) => (str :: <string>)
           if (subsequence-position(libname, "hqn-", test: \=) == 0)
             let stripped = make(<string>, size: libname.size - 4);
             for (i from 4 below libname.size)
               stripped[i - 4] := libname[i]
             end for;
             stripped
           else
             libname
           end if
         end method;

   if (*current-debugger-options*.show-debug-output?)
     let (majv, minv) = library-version(stop-reason.stop-reason-library);
     debugger-message ("Module loaded: %s [Version %d.%d]",
                       stop-reason.stop-reason-library.library-core-name,
                       majv, minv);
     debugger-message ("  From: %s",
                       stop-reason.stop-reason-library.library-image-name);
   end if;
   let library-name = 
     strip-hqn-prefix
       (as-lowercase(stop-reason.stop-reason-library.library-core-name));
   let plausible-module-names =
     vector(library-name,
            "internal",
            concatenate(library-name, "-implementation"),
            concatenate(library-name, "-internal"),
            concatenate(library-name, "-extensions"));
   for (module-name in plausible-module-names)
     let context = make(<dylan-name-context>, 
                        library: library-name, module: module-name);
     add!(application.application-likely-namespaces, context);
   end for;
   #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <unload-library-stop-reason>)
                 => (interested? :: <boolean>)
   if (*current-debugger-options*.show-debug-output?)
     debugger-message ("Module unloaded: %s",
                       stop-reason.stop-reason-library.library-core-name);
   end if;
   #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <RIP-stop-reason>)
                 => (interested? :: <boolean>)
   if (*current-debugger-options*.show-debug-output?)
     debugger-message ("The application died (RIP event received).");
   end if;
   #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <system-invoke-debugger-stop-reason>)
                 => (interested? :: <boolean>)

  debugger-message ("Application hit a hard-coded breakpoint.");
  *program-failed?* := #t;
  #t
end method;

define method react-to-stop-reason
    (application :: <application>,
     stop-reason :: <breakpoint-stop-reason>)
       => (interested? :: <boolean>)
  #t;
end method;

define method react-to-stop-reason
    (application :: <application>, 
     stop-reason :: <source-step-out-stop-reason>)
  => (interested? :: <boolean>)
  debugger-message("Thread finished STEP-OUT operation.");
  #t
end method;

define method react-to-stop-reason
    (application :: <application>, 
     stop-reason :: <source-step-over-stop-reason>)
  => (interested? :: <boolean>)
  debugger-message("Thread finished STEP-OVER operation.");
  #t
end method;

define method react-to-stop-reason
    (application :: <application>, 
     stop-reason :: <source-step-into-stop-reason>)
  => (interested? :: <boolean>)
  debugger-message("Thread finished STEP-INTO operation.");
  #t
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <system-initialized-stop-reason>)
                 => (interested? :: <boolean>)
  if (*current-debugger-options*.show-debug-output?)
    debugger-message ("The executable and its libraries are mapped.");
    debugger-message ("The application is now ready to continue.");
  end if;
  *current-debugger-options*.stop-at-system-initialization?
end method;

define method react-to-stop-reason
    (application :: <application>,
     stop-reason :: <dylan-invoke-debugger-stop-reason>)
       => (interested? :: <boolean>)
  let printed-condition =
    dylan-error-message-string(stop-reason);
  debugger-message("Condition: %s.", printed-condition);
  let restart-set =
    available-restarts-for-thread(application, stop-reason.stop-reason-thread);
  if (size(restart-set) == 0)
    debugger-message("No restarts.");
  else
    debugger-message("Restarts available:");
    for (i from 1 to size(restart-set))
      debugger-message("  %d : %s", 
                        i, 
                        remote-restart-description(restart-set[i - 1]));
    end for;
  end if;
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
    (application :: <application>,
     stop-reason :: <output-debug-string-stop-reason>)
      => (interested? :: <boolean>)
   if (*current-debugger-options*.show-application-messages?)
     debugger-message ("APPLICATION MESSAGE: %s",
                       stop-reason.stop-reason-debug-string);
   end if;
  #f;
end method;

define method react-to-stop-reason
    (application :: <application>,
     stop-reason :: <dylan-debug-message-stop-reason>)
       => (interested? :: <boolean>)
  if (*current-debugger-options*.show-dylan-debug-messages?)
    debugger-message ("DEBUG-MESSAGE: %s",
      dylan-debug-message-string(stop-reason));
  end if;
  #f;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <single-step-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message = "Exception";
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-debug-point-address);
  let exception-message =
    "single instruction step";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <access-violation-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "access violation";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  let op-message =
    select(stop-reason.stop-reason-access-violation-operation)
      $access-violation-on-read    => "read";
      $access-violation-on-write   => "write";
      $access-violation-on-execute => "execute";
      otherwise                    => "access";
    end select;
  let violation-address-message =
    formatted-hex-to-string(stop-reason.stop-reason-access-violation-address);
  debugger-message("The program tried to %s memory at address %s",
                   op-message, violation-address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <array-bounds-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "array bounds";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <illegal-instruction-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "illegal instruction";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <privileged-instruction-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "privileged instruction";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <denormal-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "floating point denormal";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
    (application :: <application>, 
     stop-reason :: <stack-overflow-exception-stop-reason>)
        => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "stack overflow";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
    (application :: <application>, 
     stop-reason :: <integer-overflow-exception-stop-reason>)
        => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "integer overflow";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <float-divide-by-zero-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "floating point division by zero";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <inexact-result-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "inexact result";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
   (application :: <application>,
    stop-reason :: <invalid-float-operation-exception-stop-reason>)
       => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "invalid float operation";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <float-overflow-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "floating point overflow";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <float-underflow-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "floating point underflow";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <float-stack-check-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "intel float stack check";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <integer-divide-by-zero-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "integer division by zero";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <unclassified-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "unclassified";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <noncontinuable-exception-stop-reason>)
                 => (interested? :: <boolean>)
  let chance-message =
    if (stop-reason.stop-reason-exception-first-chance?)
      "First chance exception"
    else
      "Second chance exception"
    end if;
  let address-message =
    formatted-hex-to-string(stop-reason.stop-reason-exception-address);
  let exception-message =
    "single instruction step";
  debugger-message ("%s (%s) at %s",
                     chance-message, exception-message, address-message);
  *program-failed?* := #t;
  #t;
end method;

///// Catch-all method....

define method react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <stop-reason>)
                 => (interested? :: <boolean>)

  debugger-message ("Application stopped. I'm buggered if I know why!");
  *program-failed?* := #t;
  #t;
end method;
