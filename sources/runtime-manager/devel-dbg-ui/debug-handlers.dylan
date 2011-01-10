module:       devel-dbg-ui
synopsis:     Functions breakpoints and tracepoints for devel
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/////
///// SIMPLE TRACE INFO PRINTING SUPPORT
/////

// TODO:
// This is naff. Tracing is absolute "bare bones", and could be vastly
// improved once working.
//
// -> thingy        .... should be ....
// -> thingy[Thread001] (x = #[1, 2, 3, 4], y = 4)    ... or something...


///// *TRACE-DEPTH*
//    Used for pretty-printing. This integer variable is incremented every
//    time a traced-function is entered, and decremented every time one
//    returns. By printing *trace-depth* number of spaces before the
//    tracing information, the display is more readable.

define variable *trace-depth* = 0;

define method print-trace-depth-spaces ()
  format-out ("%s", make (<string>, size: *trace-depth*, fill: ' '))
end method;

// Really simple trace functionality. Just show the function names as
// we go in and out.

define method print-trace-entry (f :: <string>, t :: <string>)
  print-trace-depth-spaces();
  format-out ("-> %s[%s]\n", f, t);
  *trace-depth* := *trace-depth* + 1;
end method;

define method print-break-entry (f :: <string>, t :: <string>)
  format-out (">>>> BREAK: %s[%s]\n", f, t);
end method;

define method print-trace-exit (f :: <string>, t :: <string>)
  *trace-depth* := *trace-depth* - 1;
  print-trace-depth-spaces();
  format-out ("<- %s[%s]\n", f, t);
end method;

define method print-source-break-hit 
    (filename :: <string>, line :: <integer>, threadname :: <string>) => ()
  format-out(">>>> BREAK: Line %d of source file %s [%s]\n",
             line, filename, threadname);
end method;


/////
///// BREAKPOINT TYPES USED BY THE DEVEL DEBUGGER
/////


///// <CONSOLE-DEBUGGER-BREAKPOINT>
//    An abstract description of any breakpoint in the console debugger.

define abstract class <console-debugger-breakpoint> (<breakpoint>)

  slot debugger-breakpoint-enabled? :: <boolean>,
    init-value: #t;

  slot debugger-breakpoint-hit-count :: <integer>,
    init-value: 0;

  slot debugger-breakpoint-ignore-count :: <integer>,
    init-value: 0;

  slot debugger-breakpoint-ignore? :: <boolean>,
    init-value: #f;

  slot debugger-breakpoint-always-ignore? :: <boolean>,
    init-value: #f;

  slot debugger-breakpoint-ignore-level :: <integer>,
    init-value: 0;

  constant slot debugger-breakpoint-cookie :: <integer>,
    init-function: method ()
                     let cookie-val = *current-bp-cookie*;
                     *current-bp-cookie* := *current-bp-cookie* + 1;
                     cookie-val;
                   end method;

end class;


///// <FUNCTION-ENTRY-BREAKPOINT>
//    Our debugger allows commands essentially of the form "break X",
//    which means break at address X (which can be a literal or
//    symbolic address), but all breakpoints are basically assumed to
//    be on entry into a function. 

define abstract class <function-entry-breakpoint> 
                         (<console-debugger-breakpoint>)

       constant slot function-symbol :: false-or (<string>),
            init-value: #f,
            init-keyword: symbol:;

end class;


///// <DYLAN-FUNCTION-ENTRY-BREAKPOINT>
//    A dylan-specific break on function entry. The <function> object
//    being "broken" is stored, along with its symbolic record if it
//    exists.

define abstract class <dylan-function-entry-breakpoint>
                           (<function-entry-breakpoint>)

       constant slot function-object :: false-or(<remote-value>),
            required-init-keyword: function-object:;

end class;


///// <C-FUNCTION-ENTRY-BREAKPOINT>
//    Just doesn't store a <function> object!

define class <C-function-entry-breakpoint>
                      (<function-entry-breakpoint>)
end class;


///// <DYLAN-METHOD-ENTRY-BREAKPOINT>
//    A breakpoint upon entry into the IEP of a method.

define class <dylan-method-entry-breakpoint>
                     (<dylan-function-entry-breakpoint>)

       constant slot specializers-list :: <string>,
            init-value: "()",
            init-keyword: specializers:;

end class;


///// <BREAK-DESCRIPTION>
//    Encapsulates a registered breakpoint and its address.

define class <break-description> (<object>)

       constant slot break-address :: <remote-value>,
            required-init-keyword: address:;

       constant slot registered-break :: <console-debugger-breakpoint>,
            required-init-keyword: break:;

end class;


///// FIND-ACTIVE-BREAK
//    Searches through active breakpoints in the application looking for
//    one at the given address. Returns the bp if it is found, else
//    returns #f

define method find-active-break
  (app :: <application>, addr :: <remote-value>)
     => (maybe-breakpoint :: false-or (<break-description>))

  let i = 0;
  let found = #f;
  while ((~found) & (i < size(app.active-breaks)))
    if (addr = app.active-breaks[i].break-address)
      found := app.active-breaks[i];
    else
      i := i + 1;
    end if
  end while;
  found;
end method;


///// ADD-ACTIVE-BREAK
//    Adds a breakpoint to the list of active breakpoints in the application.
//    Also performs registration.

define method add-active-break
  (app :: <application>, bp :: <break-description>) => ()

  app.active-breaks := add! (app.active-breaks, bp);
  register-debug-point (app, bp.registered-break);
end method;


///// REMOVE-ACTIVE-BREAK
//    Removes a breakpoint from the list of active breakpoints.
//    Also performs deregistration.

define method remove-active-break
  (app :: <application>, bp :: <break-description>) => ()

  app.active-breaks := remove! (app.active-breaks, bp);
  deregister-debug-point (app, bp.registered-break);
end method;


/////
///// FUNCTION BREAKPOINT HANDLERS
/////


///// BREAKPOINT-CALLBACK
//    Registered on all breakpoints set with the "break" function. Note
//    that there is no specialized handle-debug-point-event method - 
//    the default behaviour will just call the callback.

define method breakpoint-callback
  (app :: <application>, bp :: <function-entry-breakpoint>,
   thr :: <remote-thread>)
     => (interested? :: <boolean>)

  if (bp.debugger-breakpoint-enabled?)
    bp.debugger-breakpoint-hit-count := bp.debugger-breakpoint-hit-count + 1;
    let str = "{anonymous}";
    if (bp.function-symbol)
      str := bp.function-symbol
    end if;
    print-break-entry(str, thr.thread-name);
    if (bp.debugger-breakpoint-always-ignore?)
      #f
    elseif (bp.debugger-breakpoint-ignore?)
      if (bp.debugger-breakpoint-ignore-count == 
            bp.debugger-breakpoint-ignore-level)
        bp.debugger-breakpoint-ignore? := #f;
        bp.debugger-breakpoint-ignore-count := 0;
        bp.debugger-breakpoint-ignore-level := 0;
        #t;
      else
        bp.debugger-breakpoint-ignore-count := 
          bp.debugger-breakpoint-ignore-count + 1;
        #f;
      end if;
    else
      #t
    end if;
  else
    #f
  end if;
end method;

define method breakpoint-callback
  (app :: <application>, bp :: <dylan-method-entry-breakpoint>,
   thr :: <remote-thread>)
     => (interested? :: <boolean>)

  if (bp.debugger-breakpoint-enabled?)
    bp.debugger-breakpoint-hit-count := bp.debugger-breakpoint-hit-count + 1;
    let str = "{anonymous dylan method}";
    if (bp.function-symbol)
       str := bp.function-symbol
    end if;
    str := concatenate (str, bp.specializers-list);
    print-break-entry(str, thr.thread-name);
    if (bp.debugger-breakpoint-always-ignore?)
      #f
    elseif (bp.debugger-breakpoint-ignore?)
      if (bp.debugger-breakpoint-ignore-count ==
            bp.debugger-breakpoint-ignore-level)
        bp.debugger-breakpoint-ignore? := #f;
        bp.debugger-breakpoint-ignore-count := 0;
        bp.debugger-breakpoint-ignore-level := 0;
        #t;
      else
        bp.debugger-breakpoint-ignore-count := 
          bp.debugger-breakpoint-ignore-count + 1;
        #f; 
      end if
    else
      #t
    end if
  else
    #f
  end if;
end method;


/////
///// TRACEPOINT TYPES USED BY THE DEVEL DEBUGGER
/////


///// <PROFILE-ON-TRACEPOINT>
//    A special kind of entry tracepoint. We use this to support switching
//    on the profiler during the invocation of a function, and then switching
//    it off when we return. The behaviour of this entry tracepoint is that
//    it deregisters itself when encountered, switches on the profiler, and
//    returns #f.

define class <profile-on-tracepoint> (<entry-tracepoint>)
  constant slot profile-on-options :: <profiler-options>,
    required-init-keyword: profiler-options:;
end class;


///// <PROFILE-OFF-TRACEPOINT>
//    Paired with the <profile-on-tracepoint>. When encountered, switches
//    off profiling.

define class <profile-off-tracepoint> (<return-tracepoint>)
end class;


///// <FUNCTION-ENTRY-TRACE>
//    All tracepoints trace functions. This is the root class for function
//    entry tracing.

define abstract class <function-entry-trace> (<entry-tracepoint>)

       constant slot traced-symbol :: false-or (<string>),
            init-value: #f,
            init-keyword: symbol:;

end class;


///// <FUNCTION-RETURN-TRACE>
//    The root class for function exit tracing.

define abstract class <function-return-trace>
                        (<return-tracepoint>)

       constant slot traced-symbol :: false-or (<string>),
            init-value: #f,
            init-keyword: symbol:;

end class;


///// <DYLAN-FUNCTION-ENTRY-TRACE>
//    Dylan-specific function entry tracing. Stores the function object
//    being traced, which may be a generic function.

define abstract class <dylan-function-entry-trace>
                        (<function-entry-trace>)

       constant slot function-object :: false-or(<remote-value>),
            required-init-keyword: function-object:;

end class;


///// <DYLAN-FUNCTION-RETURN-TRACE>
//    Dylan-specific function exit tracing.

define abstract class <dylan-function-return-trace>
                        (<function-return-trace>)

       constant slot function-object :: false-or(<remote-value>),
            required-init-keyword: function-object:;

end class;


///// <DYLAN-METHOD-ENTRY-TRACE>
//    Tracing the IEP of a method. This may store some representation
//    of the argument types so that more useful output can be given.

define class <dylan-method-entry-trace>
               (<dylan-function-entry-trace>)

       constant slot specializers-list :: <string>,
            init-value: "()",
            init-keyword: specializers:;

end class;


///// <DYLAN-METHOD-RETURN-TRACE>
//    Corresponding exit tracepoint.

define class <dylan-method-return-trace>
               (<dylan-function-return-trace>)

       constant slot specializers-list :: <string>,
            init-value: "()",
            init-keyword: specializers:;

end class;


///// <C-FUNCTION-ENTRY-TRACE>
//    Traces entry into foreign (C, in our case) functions.

define class <C-function-entry-trace>
               (<function-entry-trace>)
end class;


///// <C-FUNCTION-RETURN-TRACE>
//    Corresponding exit tracepoint.

define class <C-function-return-trace>
               (<function-return-trace>)
end class;


///// <TRACE-DESCRIPTION>
//    Encapsulates a registered tracepoint and its address.

define class <trace-description> (<object>)

       constant slot trace-address :: <remote-value>,
            required-init-keyword: address:;

       constant slot registered-trace :: <function-entry-trace>,
            required-init-keyword: trace:;

end class;


///// FIND-ACTIVE-TRACE
//    Searches through active tracepoints in the application looking for
//    one at the given address. Returns the bp if it is found, else
//    returns #f

define method find-active-trace
  (app :: <application>, addr :: <remote-value>)
     => (maybe-tracepoint :: false-or (<trace-description>))

  let i = 0;
  let found = #f;
  while ((~found) & (i < size(app.active-traces)))
    if (addr = app.active-traces[i].trace-address)
      found := app.active-traces[i];
    else
      i := i + 1;
    end if
  end while;
  found;
end method;


///// ADD-ACTIVE-TRACE
//    Adds a tracepoint to the list of active tracepoints in the application.
//    Also performs registration.

define method add-active-trace
  (app :: <application>, bp :: <trace-description>) => ()

  app.active-traces := add! (app.active-traces, bp);
  register-debug-point (app, bp.registered-trace);
end method;


///// REMOVE-ACTIVE-TRACE
//    Removes a tracepoint from the list of active tracepoints.
//    Also performs deregistration.

define method remove-active-trace
  (app :: <application>, bp :: <trace-description>) => ()

  app.active-traces := remove! (app.active-traces, bp);
  deregister-debug-point (app, bp.registered-trace);
end method;



/////
///// TRACEPOINT HANDLERS
/////


///// ENTRY-TRACEPOINT-CALLBACK
//    Registered on all breakpoints set with the "trace" command.

define method entry-tracepoint-callback
  (app :: <application>, tp :: <function-entry-trace>,
   thr :: <remote-thread>)
     => (interested? :: <boolean>)

  let str = "{anonymous function}";
  if (tp.traced-symbol)
     str := tp.traced-symbol
  end if;
  print-trace-entry(str, thr.thread-name);
  #f;
end method;

define method entry-tracepoint-callback
  (app :: <application>, tp :: <dylan-method-entry-trace>,
   thr :: <remote-thread>)
    => (interested? :: <boolean>)

  let str = "{anonymous dylan method}";
  if (tp.traced-symbol)
     str := tp.traced-symbol
  end if;
  let obj = tp.function-object;
  if (obj)
    let (sig, addr, spec) = remote-method-inspect(app, obj);
    let (req, restv, keys) = dylan-trace-entry-arguments(app, thr, sig);
    let pred = "";
    str := concatenate(str, "(");
    for (arg in req)
      str := concatenate(str, pred, debugger-print-object(app, arg));
      pred := ", ";
    end for;
    if (keys)
      for (arg in keys)
        str := concatenate(str, pred, 
                           debugger-print-object
                             (app, head(arg), decorate?: #f),
                           ": ",
                           debugger-print-object(app, tail(arg)));
        pred := ", ";
      end for;
    end if;
    if (restv)
      str := concatenate
               (str, pred, "#rest ",
                debugger-print-object(app, restv, length: 10));
    end if;
    str := concatenate(str, ")");
  end if;
  print-trace-entry(str, thr.thread-name);
  #f;
end method;


///// RETURN-TRACEPOINT-CALLBACK
//    Registered on all breakpoints set by the DM upon encountering
//    an already-registered <function-entry-trace>

define method return-tracepoint-callback
  (app :: <application>, tp :: <function-return-trace>,
   thr :: <remote-thread>)
     => (interested? :: <boolean>)

  let str = "{anonymous}";
  if (tp.traced-symbol)
     str := tp.traced-symbol
  end if;
  print-trace-exit(str, thr.thread-name);
  #f;
end method;

define method return-tracepoint-callback
  (app :: <application>, tp :: <dylan-method-return-trace>,
   thr :: <remote-thread>)
    => (interested? :: <boolean>)

  let str = "{anonymous dylan method}";
  if (tp.traced-symbol)
     str := tp.traced-symbol
  end if;
  let retvals = dylan-trace-return-values(app, thr);
  let pred = "(";
  for (retval in retvals)
    str := concatenate(str, pred, debugger-print-object(app, retval));
    pred := ", ";
  end for;
  str := concatenate(str, ")");
  print-trace-exit(str, thr.thread-name);
  #f;
end method;


///// MAKE-RETURN-TRACEPOINT
//    Given a <function-entry-trace>, returns a <function-return-trace>
//    There is no special behaviour defined on INITIALIZE-RETURN-TRACEPOINT

define method make-return-tracepoint
  (app :: <application>, tp :: <C-function-entry-trace>,
   thr :: <remote-thread>, #rest keys, #key, #all-keys)
     => (return-tracepoint :: <function-return-trace>)

  apply (make, <C-function-return-trace>,
         symbol: tp.traced-symbol,
         keys)
end method;

define method make-return-tracepoint
  (app :: <application>, tp :: <dylan-function-entry-trace>,
   thr :: <remote-thread>, #rest keys, #key, #all-keys)
     => (return-tracepoint :: <function-return-trace>)

  apply (make, <dylan-function-return-trace>,
         symbol: tp.traced-symbol,
         function-object: tp.function-object,
         keys)
end method;

define method make-return-tracepoint
  (app :: <application>, tp :: <dylan-method-entry-trace>,
   thr :: <remote-thread>, #rest keys, #key, #all-keys)
     => (return-tracepoint :: <function-return-trace>)

  apply (make, <dylan-method-return-trace>,
         symbol: tp.traced-symbol,
         function-object: tp.function-object,
         specializers: tp.specializers-list,
         keys)
end method;

define method make-return-tracepoint
  (application :: <application>, tp :: <profile-on-tracepoint>,
   thread :: <remote-thread>, #rest keys, #key, #all-keys)
      => (return-tracepoint :: <profile-off-tracepoint>)
  apply (make,
         <profile-off-tracepoint>,
         keys);
end method;


///// PROFILE-ON-CALLBACK
//    Switches on the profiler and deregisters itself.
//    Why the deregistration? Well, we want profiling to stay on for the
//    duration of this function activation. Recursive activations won't
//    bother us if we deregister the tracepoint.

define method profile-on-callback
    (application :: <application>, bp :: <profile-on-tracepoint>,
     thread :: <remote-thread>)
       => (definitely-false :: <boolean>)
  debugger-message("*** Automatic profiler activation");
  debugger-message("    Removing unused breakpoints before continuing...");
  for (entry-breakpoint in application.profiler-entry-breakpoints)
    deregister-debug-point(application, bp);
  end for;
  if (application.application-profiler-run)
    debugger-message("*** Ignoring this activation: already profiling!!");
  else
    start-profiling-with-options(application, bp.profile-on-options);
    debugger-message("Switched on the profiler.");
  end if;
  #f;
end method;


///// PROFILE-OFF-CALLBACK
//    Switches off the profiler. The DM will already have deregistered the
//    breakpoint if this callback is invoked.

define method profile-off-callback
    (application :: <application>, bp :: <profile-off-tracepoint>,
     thread :: <remote-thread>)
       => (definitely-true :: <boolean>)
  debugger-message("*** Automatic profiler deactivation. Results coming up.");
  if (application.application-profiler-run)
    stop-profiling(application);
    debugger-message("Switched off the profiler.");
    debugger-message("Processing results, please wait...");
    process-profile-data(application);
    debugger-message("Done processing results.");
    print-profile-results(application);
    application.application-profiler-run := #f;
  else
    debugger-message("*** Ignoring this deactivation. Not profiling!!");
  end if;
  #t;
end method;


/////
///// SOURCE LOCATION BREAKPOINTS
/////


define class <source-breakpoint> (<console-debugger-breakpoint>)

  constant slot source-breakpoint-filename :: <string>,
    required-init-keyword: filename:;

  constant slot source-breakpoint-linenumber :: <integer>,
    required-init-keyword: linenumber:;
/*
  constant slot source-breakpoint-column :: <integer>,
    init-value: 0,
    init-keyword: column:;
*/
end class;

define class <source-break-description> (<break-description>)
end class;


///// FIND-ACTIVE-SOURCE-BREAK
//    Searches through active breakpoints in the application looking for
//    one at the given address. Returns the bp if it is found, else
//    returns #f

define method find-active-source-break
  (app :: <application>, addr :: <remote-value>)
     => (maybe-breakpoint :: false-or (<source-break-description>))

  let i = 0;
  let found = #f;
  while ((~found) & (i < size(app.active-source-breaks)))
    if (addr = app.active-source-breaks[i].break-address)
      found := app.active-source-breaks[i];
    else
      i := i + 1;
    end if
  end while;
  found;
end method;


///// ADD-ACTIVE-SOURCE-BREAK
//    Adds a breakpoint to the list of active breakpoints in the application.
//    Also performs registration.

define method add-active-source-break
  (app :: <application>, bp :: <source-break-description>) => ()

  app.active-source-breaks := add! (app.active-source-breaks, bp);
  register-debug-point (app, bp.registered-break);
end method;


///// REMOVE-ACTIVE-SOURCE-BREAK
//    Removes a breakpoint from the list of active breakpoints.
//    Also performs deregistration.

define method remove-active-source-break
  (app :: <application>, bp :: <source-break-description>) => ()

  app.active-source-breaks := remove! (app.active-source-breaks, bp);
  deregister-debug-point (app, bp.registered-break);
end method;


///// SOURCE-BREAKPOINT-CALLBACK
//    Registered on all breakpoints set with the "break line x of file"
//    command. Just prints the occurrence of the breakpoint, and signals
//    that the application should stop for commands.

define method source-breakpoint-callback
    (application :: <application>, bp :: <source-breakpoint>,
     thread :: <remote-thread>)
       => (interested? :: <boolean>)
  if (bp.debugger-breakpoint-enabled?)
    bp.debugger-breakpoint-hit-count := bp.debugger-breakpoint-hit-count + 1;
    print-source-break-hit(bp.source-breakpoint-filename,
                           bp.source-breakpoint-linenumber,
                           thread.thread-name);
    if (bp.debugger-breakpoint-always-ignore?)
      #f
    elseif (bp.debugger-breakpoint-ignore?)
      if (bp.debugger-breakpoint-ignore-count ==
            bp.debugger-breakpoint-ignore-level)
        bp.debugger-breakpoint-ignore? := #f;
        bp.debugger-breakpoint-ignore-count := 0;
        bp.debugger-breakpoint-ignore-level := 0;
        #t;
      else
        bp.debugger-breakpoint-ignore-count := 
          bp.debugger-breakpoint-ignore-count + 1;
        #f;
      end if;
    else
      #t
    end if
  else
    #f
  end if;
end method;


///// FIND-DEBUGGER-BREAKPOINT
//    Given an integer cookie, attempts to locate the breakpoint that has
//    been assigned that cookie. Returns the breakpoint object itself,
//    or #f if it is not found.

define method find-debugger-breakpoint
    (application :: <application>, cookie :: <integer>)
       => (maybe-bp :: false-or(<console-debugger-breakpoint>),
           maybe-desc :: false-or(<break-description>))

  local method find-in-set(breakpoints :: <sequence>)
                   => (bp :: false-or(<console-debugger-breakpoint>),
                       d :: false-or(<break-description>));
    let sz = size(breakpoints);
    let i = 0;
    let found = #f;
    let description = #f;
    while ((~found) & (i < sz))
      if (breakpoints[i].registered-break.debugger-breakpoint-cookie == cookie)
        found := breakpoints[i].registered-break;
        description := breakpoints[i];
      else
        i := i + 1;
      end if
    end while;
    values(found, description);
  end method;

  let (registered-breakpoint, description)
    = find-in-set(application.active-breaks);

  if (registered-breakpoint)
    values(registered-breakpoint, description)
  else
    find-in-set(application.active-source-breaks);
  end if;
end method;


///// HANDLE-INTERACTOR-RETURN
//    The method for the console debugger.

define method handle-interactor-return
    (application :: <application>, thread :: <remote-thread>, id :: <object>,
     #rest return-vals)
       => (stop? :: <boolean>)
  debugger-message("Interactive Evaluation Returned:");
  if (return-vals.size == 0)
    debugger-message("No values.");
  else
    for (remote-val in return-vals)
      let hist-index = add-history-value(remote-val);
      let hex-object = 
        remote-value-as-string
           (application.debug-target-access-path, remote-val, 16);
      format-out("    0x%s  $%d : %s\n",
                 hex-object,
                 hist-index,
                 debugger-print-object(application, remote-val,
                                       length: 5, level: 2));
    end for
  end if;
  format-out("\n");
  #t
end method;


///// HANDLE-LIBRARY-INITIALIZATION-PHASE
//    The method for the console debugger

define method handle-library-initialization-phase
    (application :: <application>, thread :: <remote-thread>,
     lib :: <remote-library>, phase :: <library-initialization-phase>,
     top-level? :: <boolean>)
  => (interested? :: <boolean>)
  let opts = *current-debugger-options*;
  let extra = if (top-level?) "top-level" else "" end if;
  let libname = as-uppercase(lib.library-core-name);
  if (phase == #"start")
    if (opts.stop-at-library-initializations?)
      debugger-message("Running initializations for %s library %s...",
                       extra, libname);
    end if;
    if (top-level? & opts.stop-for-interaction?)
      debugger-message
        ("Stopped for pre-initialization debugging of component %s",
          libname);
      #t
    else
      opts.stop-at-library-initializations?;
    end if;
  else
    if (opts.stop-at-library-initializations?)
      debugger-message("Finished running initializations for %s library %s",
                       extra, libname);
    end if;
    if (top-level? & opts.stop-for-interaction?)
      debugger-message
        ("Stopped for post-initialization debugging of component %s",
         libname);
      #t
    else
      #f
    end if;
  end if;
end method;
