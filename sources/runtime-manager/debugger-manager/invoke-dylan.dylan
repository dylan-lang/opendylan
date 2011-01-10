module:          dm-internals
synopsis:        For the console debugger's evaluator mechanism.
                 Invokes the XEP of dylan functions via remote calls and
                 breakpoints.
author:          Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// <DYLAN-RETURN-BREAKPOINT>
//    A <breakpoint> that is triggered when a remote dylan call returns.

define class <dylan-return-breakpoint> (<breakpoint>)

  constant slot used-thread :: <remote-thread>,
    required-init-keyword: thread:;

  constant slot used-frame :: <remote-value>,
    required-init-keyword: frame:;

  constant slot calling-frame :: false-or(<remote-value>),
    init-value: #f,
    init-keyword: calling-frame:;

  constant slot stored-context :: <object>,
    required-init-keyword: context:;

  constant slot interactor-cookie :: <integer>,
    init-value: 0,
    init-keyword: cookie:;

  slot dylan-function-result :: <remote-value>;

  constant slot saved-mv-vector :: <sequence>,
    init-value: #[],
    init-keyword: saved-mv-vector:;

end class;


// Need to remember the application state on code entry
// for interactions on suspended threads, as these happen
// immediately behind the back of a running application

define open generic interaction-request-application-state
    (interaction-transaction-id) => (application-state);

define open generic interaction-request-application-state-setter
    (application-state, interaction-transaction-id) => (application-state);


///// <INTERACTOR-RETURN-BREAKPOINT>

define class <interactor-return-breakpoint> (<dylan-return-breakpoint>)

  constant slot interactor-result-spec :: <symbol>,
    required-init-keyword: result-spec:;

  // Also remember what the value of APPLICATION-STATE was when the
  // interactive code was entered.

  slot interaction-request-application-state = #"stopped",
    required-init-keyword: application-state:;

end class;


///// HANDLE-DEBUG-POINT-EVENT <DYLAN-RETURN-BREAKPOINT>
//    If we are in the correct thread and frame-pointer context,
//    restore the state from the remote call, and grab the function
//    result, and call next-method to invoke the registered callback.
//    If we are not in the correct context, just ignore the trigger.

define method handle-debug-point-event
    (application :: <debug-target>, bp :: <dylan-return-breakpoint>,
     thread :: <remote-thread>)
        => (interested? :: <boolean>)
  if (thread == bp.used-thread)
    let top-frame-now =
      initialize-stack-trace(application.debug-target-access-path, thread);
    let calling-frame-now =
      previous-frame(application.debug-target-access-path, top-frame-now);
    let top-frame-pointer =
      frame-pointer(application.debug-target-access-path, top-frame-now);
    let calling-frame-pointer =
      if (calling-frame-now)
        frame-pointer(application.debug-target-access-path, calling-frame-now)
      else
        #f
      end if;
    if ((top-frame-pointer = bp.used-frame) |
        ((bp.calling-frame) & (calling-frame-pointer) &
         (bp.calling-frame = calling-frame-pointer)))
      bp.dylan-function-result := 
        remote-call-result(application.debug-target-access-path, thread);
      remote-restore-context(application.debug-target-access-path, thread,
                             bp.stored-context);
      thread-set-mv-vector(application, thread, bp.saved-mv-vector);
      deregister-debug-point(application, bp);
      application.current-interactor-level := bp.interactor-cookie;
      next-method();
    else
      #f
    end if
  else
    #f
  end if;
end method;

define method handle-debug-point-event
    (application :: <debug-target>, bp :: <interactor-return-breakpoint>,
     thread :: <remote-thread>)
        => (interested? :: <boolean>)
  if (thread == bp.used-thread)
    let top-frame-now =
      initialize-stack-trace(application.debug-target-access-path, thread);
    let calling-frame-now =
      previous-frame(application.debug-target-access-path, top-frame-now);
    let top-frame-pointer =
      frame-pointer(application.debug-target-access-path, top-frame-now);
    let calling-frame-pointer =
      if (calling-frame-now)
        frame-pointer(application.debug-target-access-path, calling-frame-now)
      else
        #f
      end if;

    debugger-message("handle-debug-point-event\n"
		     "\nTHREAD:%= TOPF:%= TOPFP:%=\n"
		     "\nCALLINGF:%= CALLINGFP:%=\n"
		     "\nBPCALLINGF:%= BPUSEF:%=",
		     thread, top-frame-now, top-frame-pointer,
		     calling-frame-now, calling-frame-pointer,
		     bp.calling-frame, bp.used-frame);

    if ((top-frame-pointer = bp.used-frame) |
        ((bp.calling-frame) & (calling-frame-pointer) &
         (bp.calling-frame = calling-frame-pointer)))
      bp.dylan-function-result := 
        remote-call-result(application.debug-target-access-path, thread);
      remote-restore-context(application.debug-target-access-path, thread,
                             bp.stored-context);
      thread-set-mv-vector(application, thread, bp.saved-mv-vector);
      deregister-debug-point(application, bp);

      application.current-interactor-level := bp.interactor-cookie;
      let ret-seq =
        select (bp.interactor-result-spec)
          #"single-value" => 
             vector(bp.dylan-function-result);
          #"multiple-value" => 
             canonicalize-sequence(application, bp.dylan-function-result);
        end select;
      apply(handle-interactor-return,
            application,
            thread,
            bp,
            ret-seq);
    else
      #f
    end if
  else
    #f
  end if;
end method;


///// EMPTY-CALLBACK

define method empty-callback
    (application :: <debug-target>, bp :: <debug-point>,
     thread :: <remote-thread>)
        => (nothing :: <boolean>)
  #f
end method;


///// INVOKE-DYLAN
//    Sets up the thread to run a dylan function with a given sequence
//    of arguments. Sets a breakpoint, so requires a callback.

define method invoke-dylan
   (application :: <debug-target>, thread :: <remote-thread>,
    dylan-function :: <remote-value>, return-callback :: <function>,
    #rest argument-list)
       => (succeeded? :: <boolean>)

  let dylan-library = application.application-dylan-library;
  let dylan-runtime = application.application-dylan-runtime-library;

  let restart-trampoline =
    if (dylan-library)
      resolve-dylan-name(application, "spy-invoke-dylan-under-coded-restart",
                         $dylan-internal,
                         indirect?: #t,
                         library: dylan-library);
    end if;

  let invoker = 
    if (dylan-runtime)
      find-symbol(application.debug-target-access-path,
                  "call_dylan_function_returning_all_values",
                  library: dylan-runtime);
    end if;

  if (invoker)
    let top-frame = 
      initialize-stack-trace(application.debug-target-access-path, thread);
    let top-frame-pointer =
      frame-pointer(application.debug-target-access-path, top-frame);
    let calling-frame-now =
      previous-frame(application.debug-target-access-path, top-frame);
    let calling-frame-pointer =
      if (calling-frame-now)
        frame-pointer(application.debug-target-access-path, calling-frame-now)
      else
        #f
      end if;
    let (return-address, context) =
      if (restart-trampoline)
        call-debugger-function(
              application, 
              apply, 
              remote-call,
              application.debug-target-access-path,
              thread,
              invoker.remote-symbol-address,
              restart-trampoline,
              as-remote-value(size(argument-list) + 2),
              integer-as-tagged-remote-value
                (application.current-interactor-level),
              dylan-function,
              argument-list);
      else
        call-debugger-function(
              application,
              apply,
              remote-call,
              application.debug-target-access-path,
              thread,
              invoker.remote-symbol-address,
              dylan-function,
              as-remote-value(size(argument-list)),
              argument-list);
      end if;

    let debug-point = 
      make(<dylan-return-breakpoint>,
           address: return-address,
           callback: return-callback,
           context: context,
           thread: thread,
           frame: top-frame-pointer,
           calling-frame: calling-frame-pointer,
           cookie: application.current-interactor-level,
           saved-mv-vector: thread-current-mv-vector(application, thread));

    application.current-interactor-level := 
      application.current-interactor-level + 1;

    register-debug-point(application, debug-point);
    #t;
  else
    #f;
  end if;
end method;


///// HANDLE-INTERACTOR-RETURN
//    An open generic function to allow the DM client to process the
//    return from the evaluation.

define open generic handle-interactor-return
    (application :: <debug-target>, thread :: <remote-thread>,
     transaction-id :: <object>, #rest return-values)
       => (stop? :: <boolean>);

define method handle-interactor-return
    (application :: <debug-target>, thread :: <remote-thread>,
     transaction-id :: <object>,
     #rest return-values)
       => (stop? :: <boolean>)
  let transaction-id 
  = element(application.interactor-deferred-id-table,
	    transaction-id,
	    default: #f)
    | transaction-id;

  application.application-just-interacted? := #t;
  application.application-running-on-code-entry?
    := select (interaction-request-application-state(transaction-id))
	 #"running" => #t;
	 otherwise  => #f;
       end;
  debugger-message("handle-interactor-return %= %=",
		   application.application-just-interacted?,
		   application.application-running-on-code-entry?);
  #f
end method;


///// SETUP-INTERACTOR
//    A more general API to begin running an interactive form, either in 
//    Dylan or C. This returns an object to be considered a "unique ID"
//    for the evaluation.
//    Arguments:
//      application - A <debug-target> object.
//      thread - A <remote-thread> object: the thread to run the code.
//      interactive-function - The symbolic name of a C-callable function
//                             in the runtime which will be called.
//      dll - #f, or a string naming the DLL in which 'interactive-function'
//            should be located.
//      return-spec - #"single-value" or #"multiple-value".
//      arguments - <remote-value>s to be passed to the C function.

define method setup-interactor
    (application :: <debug-target>, thread :: <remote-thread>,
     symbolic-C-entry-point :: <string>, symbolic-dll :: false-or(<string>),
     return-spec :: <symbol>, #rest args)
       => (transaction-id :: <object>)

  debugger-message("setup-interactor %= running on %=", thread, current-thread().thread-name-internal);

  let recovery-manager =
      find-symbol(application.debug-target-access-path,
                  "spy_call_interactive_function",
                  library: application.application-dylan-runtime-library);

  let invoker = 
    if (symbolic-dll)
      symbol-table-find-symbol
                 (application.debug-target-symbol-table,
                  symbolic-C-entry-point,
                  library: find-library-called(application, symbolic-dll));
    else
      symbol-table-find-symbol
                 (application.debug-target-symbol-table,
                  symbolic-C-entry-point);
    end if;

  let debug-point = #f;

  if (invoker)

    // We have our C-callable entry point. Now read as much stack
    // context information as we need.

    let top-frame = 
      initialize-stack-trace(application.debug-target-access-path, thread);
    let top-frame-pointer =
      frame-pointer(application.debug-target-access-path, top-frame);
    let calling-frame-now =
      previous-frame(application.debug-target-access-path, top-frame);
    let calling-frame-pointer =
      if (calling-frame-now)
        frame-pointer(application.debug-target-access-path, calling-frame-now)
      else
        #f
      end if;

    // And actually set up the remote function call using an access-path
    // API.

    let (return-address, context) =
        if (recovery-manager)
          call-debugger-function(
                application,
                apply,
                remote-call,
                application.debug-target-access-path,
                thread,
                recovery-manager.remote-symbol-address,
                invoker.remote-symbol-address,
                args);
        else
          call-debugger-function(
                application,
                apply,
                remote-call,
                application.debug-target-access-path,
                thread,
                invoker.remote-symbol-address,
                args);
	end if;

    // This will tell us the address to register our breakpoint. Construct the
    // breakpoint object, caching all important information.

    debug-point :=
      make(<interactor-return-breakpoint>,
           address: return-address,
           callback: empty-callback,
           result-spec: return-spec,
           context: context,
           thread: thread,
           frame: top-frame-pointer,
           calling-frame: calling-frame-pointer,
           cookie: application.current-interactor-level,
           saved-mv-vector: thread-current-mv-vector(application, thread));

    application.current-interactor-level := 
      application.current-interactor-level + 1;

    // Perform the breakpoint registration.
    register-debug-point(application, debug-point);
  end if;

  // Actually use the registered debug point as the transaction ID, since it
  // is guaranteed to be unique.

  debug-point;
end method;


// Running interactive C forms

define method C-setup-interactor
    (breakpoint-class :: subclass(<interactor-return-breakpoint>),
     application :: <debug-target>, thread :: <remote-thread>,
     invoker :: <C-spy-function-descriptor>, #rest args)
       => (transaction-id :: <object>)

  debugger-message("C-setup-interactor %= running on %=",
		   thread, current-thread().thread-name-internal);

  locate-C-spy-function(application, invoker);

  let debug-point = #f;

  if (invoker.runtime-entry-point)

    // We have our C-callable entry point. Now read as much stack
    // context information as we need.

    let top-frame = 
      initialize-stack-trace(application.debug-target-access-path, thread);
    let top-frame-pointer =
      frame-pointer(application.debug-target-access-path, top-frame);
    let calling-frame-now =
      previous-frame(application.debug-target-access-path, top-frame);
    let calling-frame-pointer =
      if (calling-frame-now)
        frame-pointer(application.debug-target-access-path, calling-frame-now)
      else
        #f
      end if;

    // And actually set up the remote function call using an access-path
    // API.

    let (return-address, context) =
      call-debugger-function
      (application,
       apply,
       remote-call,
       application.debug-target-access-path,
       thread,
       invoker.runtime-entry-point,
       args);

    // This will tell us the address to register our breakpoint. Construct the
    // breakpoint object, caching all important information.

    debug-point :=
      make(breakpoint-class,
           address: return-address,
           callback: empty-callback,
           result-spec: #"single-value",
           context: context,
           thread: thread,
           frame: top-frame-pointer,
           calling-frame: calling-frame-pointer,
           cookie: application.current-interactor-level,
           saved-mv-vector: thread-current-mv-vector(application, thread));

    // Perform the breakpoint registration.
    register-debug-point(application, debug-point);

  else
    error("Could not locate C Spy function");
  end if;

  // Actually use the registered debug point as the transaction ID, since it
  // is guaranteed to be unique.

  debug-point;
end method;

