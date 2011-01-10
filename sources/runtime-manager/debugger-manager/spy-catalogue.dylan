module:       dm-internals
synopsis:     Describing and locating spy functions in the runtime.
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <INTERPRETED-RUNTIME-ENTRY-POINT>
//    An object that describes the location of a function in the
//    runtime.

define abstract class <abstract-runtime-entry-point> (<object>)
end class;

define abstract class <place-holding-runtime-entry-point> 
                 (<abstract-runtime-entry-point>)

  constant slot runtime-name :: <string>,
    required-init-keyword: runtime-name:;

  constant slot runtime-component :: false-or(<string>),
    init-value: #f,
    init-keyword: runtime-component:;

end class;

define abstract class <interpreted-runtime-entry-point> 
                         (<place-holding-runtime-entry-point>)

  slot runtime-symbol :: false-or(<remote-symbol>),
    init-keyword: symbol:,
    init-value: #f;

  slot attempted-to-locate? :: <boolean>,
    init-value: #f;

end class;


///// <SPY-FUNCTION-DESCRIPTOR>
//    One particular use of an interpreted runtime entry point

define abstract class <spy-function-descriptor>
                        (<interpreted-runtime-entry-point>)
end class;


///// <C-SPY-FUNCTION-DESCRIPTOR>
//    Describes a spy function that must be called with C calling
//    conventions.

define class <c-spy-function-descriptor> (<spy-function-descriptor>)

  slot runtime-entry-point :: false-or(<remote-value>),
    init-keyword: entry-point:,
    init-value: #f;

end class;


///// <DYLAN-SPY-FUNCTION-DESCRIPTOR>
//    Describes a spy function that has been written in dylan. Since we
//    cannot call the spy with dylan calling conventions, we have to
//    use a <C-SPY-FUNCTION-DESCRIPTOR> as the invoker.

define class <dylan-spy-function-descriptor> (<spy-function-descriptor>)

  slot runtime-function-object :: false-or(<remote-value>),
    init-keyword: function-object:,
    init-value: #f;

  constant slot runtime-namespace :: <dylan-name-context>,
    required-init-keyword: namespace:;

  constant slot dylan-function-invoker :: <C-spy-function-descriptor>,
    required-init-keyword: invoked-by:;

end class;



///// CALL-DEBUGGER-FUNCTION
//    Calls a function with arguments, ensuring that the call
//    happens in appropriate conditions for a spy call to the application.
//    Subclasses of <debug-target> should specialize this to implement
//    their own conditions. It's essential to perform a spy call on
//    the debugger manager's thread - so specialized methods may need to
//    synchronize as appropriate.

define open generic call-debugger-function
    (application :: <debug-target>, function :: <function>, #rest arguments)
  => (#rest vals :: <object>);


define method call-debugger-function
    (application :: <debug-target>, function :: <function>, #rest arguments)
  => (#rest vals :: <object>)
  ignore(application);
  apply(function, arguments);
end method;


///// USE-THREAD-FOR-SPY-FUNCTIONS
//    Indicates to the DM that a particular thread should be used to run
//    spy functions from now on. Note that #f can be passed as the thread,
//    meaning that the DM should subsequently use its own initiative to
//    find a spy thread.

//    A Debugger Manager client can create a special reserved thread for
//    running the Spy on behalf of regular application threads, and register
//    it with the debug-target by using the reserve? keyword

define method use-thread-for-spy-functions
    (application :: <debug-target>, thread :: false-or(<remote-thread>),
     #key reserve?)
       => ()
  case
    reserve? =>
      application.reserved-spy-thread? := #t;
      application.target-spy-thread := thread;
    application.reserved-spy-thread? => #f;
    otherwise =>
      unless (application.target-spy-thread)
	application.target-spy-thread := thread;
      end;
  end;
end method;


///// SELECT-THREAD-FOR-SPY
//    Attempts to find a <remote-thread> on which to run the spy. If the
//    hint has been supplied via USE-THREAD-FOR-SPY-FUNCTIONS, then this
//    thread will be used preferentially. Otherwise, we are willing to use
//    any thread whose topmost stack frame is running dylan code. If there
//    is no such thread, then we simply cannot call the spy.

define method select-thread-for-spy (application :: <debug-target>)
    => (thread-or-not :: false-or(<remote-thread>))
  application.target-spy-thread
end method;


define method locate-C-spy-function
    (application :: <debug-target>, descriptor :: <C-spy-function-descriptor>)
 => ()
  let spy-library =
     if (descriptor.runtime-component)
        find-library-called(application, descriptor.runtime-component)
     else
        application.application-dylan-runtime-library
     end if;

  // If this spy function has not been called yet, we need to locate its
  // entry point.

  if (~descriptor.attempted-to-locate? & spy-library)
    let remote-symbol =
      find-symbol(application.debug-target-access-path,
                  descriptor.runtime-name,
                  library: spy-library);
    if (remote-symbol)
      descriptor.runtime-symbol := remote-symbol;
      descriptor.runtime-entry-point := remote-symbol.remote-symbol-address;
    end if;
    descriptor.attempted-to-locate? := #t;
  end if;

end method;

///// RUN-SPY-ON-THREAD
//    Executes a spy function on the stack of a specified thread, and
//    returns a <remote-value> result if the call succeeds. #f is
//    returned if the spy function could not be located.

define method run-spy-on-thread
    (application :: <debug-target>, thread :: false-or(<remote-thread>),
     descriptor :: <C-spy-function-descriptor>, #rest arguments)
      => (hopefully-result :: false-or(<remote-value>))

  locate-C-spy-function(application, descriptor);

  let result = #f;

  // We now know the function's entry-point, if it exists. Check that
  // we found it.

  if (thread & descriptor.runtime-entry-point)
    let (spy-result, aborted?) =
      call-debugger-function(application,
			     apply,
			     remote-call-spy,
			     application.debug-target-access-path,
			     thread,
			     descriptor.runtime-entry-point,
			     arguments);
    result :=
      if (aborted?)
        #f
      else
        spy-result
      end if;
  end if;

  // Return whatever...
  result;
end method;

define method run-spy-on-thread
    (application :: <debug-target>, thread :: false-or(<remote-thread>),
     descriptor :: <dylan-spy-function-descriptor>, #rest arguments)
      => (hopefully-result :: false-or(<remote-value>))

  let result = #f;

  // If this spy function has not been called yet, we need to locate its
  // entry point.

  if (~descriptor.attempted-to-locate?)
    let remote-fun =
      resolve-dylan-name
         (application,
          descriptor.runtime-name,
          descriptor.runtime-namespace,
          indirect?: #t);
    if (remote-fun)
      descriptor.runtime-function-object := remote-fun;
    end if;
    descriptor.attempted-to-locate? := #t;
  end if;

  // We now know the function's entry-point, if it exists. Check that
  // we found it.

  if (thread & descriptor.runtime-function-object)
    result := apply(run-spy-on-thread,
                    application,
                    thread,
                    descriptor.dylan-function-invoker,
                    descriptor.runtime-function-object,
                    as-remote-value(size(arguments)),
                    arguments)
  end if;

  // Return whatever...
  result;
end method;


define abstract class <spy-catalogue> (<object>)
  constant slot spy-extensions :: <object-table> = make(<object-table>);
end class;

define class <C-spy-catalogue> (<spy-catalogue>)

  constant slot read-location-through-barrier :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_read_location_through_barrier");

  constant slot write-location-through-barrier :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_write_location_through_barrier");
/*
  constant slot call-dylan-function :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "call_dylan_function");

  constant slot call-dylan-function-returning-all-values 
                                    :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "call_dylan_function_returning_all_values");

  constant slot read-thread-variable-at-offset :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_read_thread_variable_at_offset");
*/
  constant slot mm-root-exact :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "MMRootExact");

  constant slot mm-root-static :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "MMRootStatic");

  constant slot mm-root-ambig :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "MMRootAmbig");

  constant slot fixup-imported-data :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_fixup_imported_dylan_data");

  constant slot fixup-unimported-data :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_fixup_unimported_dylan_data");
/*
  constant slot start-debugger-transaction :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_start_debugger_transaction");

  constant slot end-debugger-transaction :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_end_debugger_transaction");
*/
  constant slot load-extension-component :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "spy_load_extension_component");

  constant slot primitive-class-breakpoint-pending :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "primitive_class_breakpoint_pending");

  constant slot primitive-set-class-breakpoint :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "primitive_set_class_breakpoint");

  constant slot primitive-clear-class-breakpoint :: <C-spy-function-descriptor>
     = make(<C-spy-function-descriptor>,
            runtime-name: "primitive_clear_class_breakpoint");

end class;


// This is to enable Dylan spy functions to run unimpeded by class breakpoints and
// the like. All Dylan spy functions should use the C spy spy_call_dylan_function
// which will set and clear this variable.

define constant $running-dylan-spy-function? = "Prunning_dylan_spy_functionQ";


define constant $spy-namespace = $dylan-internal;

define class <dylan-spy-catalogue> (<spy-catalogue>)

  constant slot remote-object-strong-register 
                            :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-register-remote-object",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  constant slot remote-object-weak-register :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-register-weak-remote-object",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  constant slot remote-object-strong-lookup :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-remote-object-value",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  constant slot remote-object-weak-lookup :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-weak-remote-object-value",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  constant slot remote-object-strong-free :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-free-remote-object",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  constant slot remote-object-weak-free :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-weak-free-remote-object",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  constant slot resolve-string-to-symbol :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-resolve-keyword",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  // Note, spy-create-interactive-thread is handled specially because 
  // the created thread is still running as a spy even after the creating
  // thread has returned control. So the spy runtime variable is set and
  // restored in the debugger manager itself

  constant slot spy-create-interactive-thread 
                       :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-create-application-thread",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "call_dylan_function"));

  constant slot spy-format-string-keyword :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-format-string-keyword",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

  constant slot spy-format-arguments-keyword :: <dylan-spy-function-descriptor>
     = make(<dylan-spy-function-descriptor>,
            runtime-name: "spy-format-arguments-keyword",
            namespace: $spy-namespace,
            invoked-by: make(<C-spy-function-descriptor>,
                             runtime-name: "spy_call_dylan_function"));

end class;


///// ALLOCATE-TEMPORARY-DOWNLOAD-BLOCK-IN
//    As soon as it can, the debugger manager allocates a <STATIC-BLOCK>
//    of memory within the target application. This is necessary
//    because some of the in-dylan spy function accept non-immediate
//    objects as arguments, and the DM needs to download these into
//    the address space of the application.
//    This function is passed the <debug-target>, and also a <remote-thread>
//    upon which it is guaranteed safe to make a spy call. 

define method allocate-temporary-download-block-in
    (application :: <debug-target>, spy-thread :: <remote-thread>) => ()
  let path = application.debug-target-access-path;
  let lib = application.application-dylan-runtime-library;
  if (lib)
    let allocator = find-symbol(path, "dylan__malloc__misc", library: lib);
    if (allocator)
      application.temporary-download-block := 
        call-debugger-function
          (application,
           allocate-single-static-block,
           path,
           allocator.remote-symbol-address,
           byte-granularity: 256,
           thread-for-spy: spy-thread);
      application.interactive-thread-download-block := 
        call-debugger-function
          (application,
           allocate-single-static-block,
           path,
           allocator.remote-symbol-address,
           page-granularity: 2,
           thread-for-spy: spy-thread);
    end if
  end if
end method;


///// LOAD-RUNTIME-COMPONENT
//    Attempts to dynamically load a runtime component (DLL).

define method load-runtime-component
    (application :: <debug-target>, name :: <byte-string>)
       => (success? :: <boolean>)
  let spy-thread = select-thread-for-spy(application);
  let address-space = application.temporary-download-block;
  let path = application.debug-target-access-path;
  if (spy-thread & address-space)
    let downloaded-string =
      download-byte-string-into(path, address-space, name);
    if (downloaded-string)
      let spy-result =
        run-spy-on-thread(application, 
                          spy-thread, 
                          application.C-spy.load-extension-component,
                          downloaded-string);
      as-integer(spy-result) == 1  // Return #t if fully successful.
    end if;
  end if;
end method;


///// SPY-FUNCTION-RUNTIME-NAME
//    Exported accessor on a spy function descriptor.
//    Returns the name of the spy function.

define method spy-function-runtime-name (sf :: <spy-function-descriptor>)
    => (name :: <string>)
  sf.runtime-name
end method;


///// SPY-FUNCTION-RUNTIME-COMPONENT
//    Exported accessor on a spy function descriptor.
//    Returns the component name of the spy function.

define method spy-function-runtime-component
    (sf :: <spy-function-descriptor>) => (name :: <string>)
  sf.runtime-component | "{Not Specified}"
end method;


///// <SPY-CALL-ERROR>
//    The common superclass of all exceptions that can occur when
//    attempting to call a spy function.

define sealed abstract class <spy-call-error> (<error>)
  constant slot spy-call-function-descriptor :: <spy-function-descriptor>,
    required-init-keyword: function-descriptor:;
  constant slot spy-call-debug-target :: <debug-target>,
    required-init-keyword: debug-target:;
  constant slot spy-call-arguments :: <sequence>,
    required-init-keyword: arguments:;
end class;


///// <SPY-FUNCTION-NOT-LOCATED>
//    An attempt was made to call a spy function, but no definition
//    for it could be found.

define class <spy-function-not-located> (<spy-call-error>)
end class;


///// <SPY-CALL-ABORTED>
//    An attempt was made to call a spy function, but its execution
//    was aborted because it signalled an unhandled exception. The
//    result will not be available.

define class <spy-call-aborted> (<spy-call-error>)
end class;


///// <SPY-CALL-NO-AVAILABLE-THREAD>
//    An attempt was made to call a spy function without specifying
//    which thread should be used. Unforunately, no available thread
//    could be selected, and the function could not be called.

define class <spy-call-no-available-thread> (<spy-call-error>)
end class;


///// <SPY-CALL-CANNOT-USE-THREAD>
//    An attempt was made to call a spy function on a specific thread,
//    but the thread could not be used.

define class <spy-call-cannot-use-thread> (<spy-call-error>)
  constant slot spy-call-selected-thread :: <remote-thread>,
    required-init-keyword: selected-thread:;
end class;


///// CALL-SPY
//    The first of the exported interfaces to spy function invocation.

define method call-spy
    (spy-function :: <C-spy-function-descriptor>,
     application :: <debug-target>, #rest arguments)
  => (result :: <remote-value>)
  let thread = select-thread-for-spy(application);
  if (thread)
    let catalogue = application.C-spy;
    let per-target-descriptor =
       element(catalogue.spy-extensions, spy-function, default: #f) |
       begin
         let descr =
           make(<C-spy-function-descriptor>,
                runtime-name: spy-function.runtime-name,
                runtime-component: spy-function.runtime-component);
         element(catalogue.spy-extensions, spy-function) := descr;
         descr
       end;
    let result =
       apply(run-spy-on-thread,
             application,
             thread,
             per-target-descriptor,
             arguments);
    result
      | signal(make(<spy-call-aborted>,
                    function-descriptor: spy-function,
                    debug-target: application,
                    arguments: arguments))
  else
    signal(make(<spy-call-no-available-thread>,
                function-descriptor: spy-function,
                debug-target: application,
                arguments: arguments))
  end if
end method;


///// CALL-SPY-ON-THREAD
//    The second of the exported interfaces to spy function invocation.

define method call-spy-on-thread
    (spy-function :: <C-spy-function-descriptor>,
     application :: <debug-target>, thread :: <remote-thread>, 
     #rest arguments)
  => (result :: <remote-value>)
  let catalogue = application.C-spy;
  let per-target-descriptor =
     element(catalogue.spy-extensions, spy-function, default: #f) |
     begin
       let descr =
         make(<C-spy-function-descriptor>,
              runtime-name: spy-function.runtime-name,
              runtime-component: spy-function.runtime-component);
       element(catalogue.spy-extensions, spy-function) := descr;
       descr
     end;
  let result =
     apply(run-spy-on-thread,
           application,
           thread,
           per-target-descriptor,
           arguments);
  result
    | error(make(<spy-call-aborted>,
                  function-descriptor: spy-function,
                  debug-target: application,
                  arguments: arguments))
end method;
