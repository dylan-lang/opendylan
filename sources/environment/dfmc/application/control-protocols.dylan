Module:    dfmc-application
Synopsis:  environment protocols for running/stopping/continuing applications
Author:    Bill Chiles, Paul Howard.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// CONTINUE-APPLICATION-RUNTIME (Internal function)
//    Wraps up the CONTINUE-TARGET-APPLICATION protocol, and ensures that
//    any internal state modelling is updated.

define method continue-application-runtime
    (application :: <dfmc-application>, remote-thread) => ()
//application.instruction-step-flag := #f;
  application.application-signalling-breakpoints.size := 0;
  remove-all-keys!(application.application-function-breakpoint-values);
  invalidate-page-relative-object-table
    (application.application-proxy-factory.per-transaction-proxies);
  continue-target-application
    (application.application-target-app, remote-thread);
end method;


///// RUN-APPLICATION (Environment Protocol Method)
//    Runs the application. This includes instantiating the debug target,
//    and hence opening the tether. This also spawns a thread which
//    runs the debugger-manager's main loop.
//    This is basically Bill's original implementation, but altered
//    slightly.

define method run-application
    (application :: <dfmc-application>,
     #key startup-option :: <symbol> = #"start",
          client,
          filename :: false-or(<file-locator>),
          arguments :: false-or(<string>),
          process :: false-or(<process>) = #f,
          system-data = "",
          machine :: <machine> = environment-host-machine(),
          working-directory :: false-or(<directory-locator>) = #f,
          library-search-paths :: <sequence> = #[],
          share-console? :: <boolean> = #f,
          pause-before-termination? :: <boolean> = #f)
 => (application :: <dfmc-application>)
  ignore(client);

  // A couple of conveniences for filename processing stolen from
  // the console debugger.

  local

    ///// WRAPPED-SR-CALLBACK
    //    This callback is a valid stop reason callback for the DM, and is
    //    used as an adapter. It delegates to the registered stop-reason
    //    callback.

    method wrapped-sr-callback
        (target :: <target-application>, sr :: <stop-reason>)
     => (interested? :: <boolean>)
      let done = instance?(sr, <exit-process-stop-reason>);
      let client-visible? = #t;

      // We need to record this thread in our state-model table if it is
      // being created.

      if (instance?(sr, <create-thread-stop-reason>) |
            instance?(sr, <create-process-stop-reason>))

        register-thread-in-state-model(application, sr);
      end if;

      let thread :: <remote-thread> = sr.stop-reason-thread;
      if (application.registered-stop-reason-callback)
        let stopping? = application.registered-stop-reason-callback(application, sr);
        stopping?
      else
        #f
      end if
    end method,

    ///// WRAPPED-DT-PROLOG
    //    Another adapter.

    method wrapped-DT-prolog
        (target :: <target-application>, sr :: <stop-reason>) => ()
      reset-breakpoint-failure-recording(application);
      if (application.registered-debugger-transaction-prolog)
        application.registered-debugger-transaction-prolog(application, sr)
      end if;
    end method,

    ///// WRAPPED-DT-EPILOG
    //    Another adapter.

    method wrapped-DT-epilog
        (target :: <target-application>, sr :: <stop-reason>) => ()
      note-all-recorded-breakpoint-failures(application);
      if (application.registered-debugger-transaction-epilog)
        application.registered-debugger-transaction-epilog(application, sr)
      end if;
      if (instance?(sr, <internal-stop-reason>))
        process-next-interaction-request(application, sr.stop-reason-thread)
      end if;
    end method,

    ///// WRAPPED-INTERACTOR-HANDLER
    //    Aaaand another one...

    method wrapped-interactor-handler
        (target :: <target-application>, thread :: <remote-thread>,
         trans-id :: <object>, #rest rvals)
     => (stop? :: <boolean>)
      let project = application.server-project;
      let callback = application.registered-interactor-handler;
      let thread-obj
        = make-environment-object(<thread-object>,
                                  project: project,
                                  application-object-proxy: thread);
      let name-value-pairs = make(<vector>, size: rvals.size);
      let deferred-id
        = element(application.application-target-app.interactor-deferred-id-table,
                  trans-id,
                  default: #f);
      for (i :: <integer> from 0 below rvals.size)
        let (index, name)
          = record-object-in-thread-history(target, thread, rvals[i]);
        name-value-pairs[i] := pair(name, rvals[i]);
      end for;
      application.interactor-results-table[trans-id] := name-value-pairs;
      suspend-evaluator-thread(application, thread, trans-id);
      if (callback)
        callback(application, thread-obj, deferred-id | trans-id);
      else
        #f
      end if;
    end method,

    ///// WRAPPED-LIBRARY-INIT-HANDLER
    //    This is absolutely the _last_ one of 'em...

    method wrapped-library-init-handler
        (target :: <target-application>, thread :: <remote-thread>,
         lib :: <remote-library>, phase :: <library-initialization-phase>,
         top-level? :: <boolean>)
     => (interested? :: <boolean>)
      let project = application.server-project;
      let callback = application.registered-library-init-handler;
      let thread-obj = make-environment-object(<thread-object>,
                                               project: project,
                                               application-object-proxy: thread);
      let (dll-project?, dll-wrap?) = library-breakpoint-info(application, lib);
      let prepare-for-interaction?
        = if (top-level?)
            ~dll-project? & phase == #"start"
          else
            dll-project? & dll-wrap? & phase == #"end"
          end;
      debugger-message("Preparing %=: top? %=, dll? %=, dll-wrap? %=, phase %=",
                       prepare-for-interaction?, top-level?, dll-project?, dll-wrap?, phase);
      if (prepare-for-interaction?)
        initialize-interactive-threads(application, thread);
        maybe-initialize-allocation-profiling(application)
      end if;
      if (callback)
        callback(application, thread-obj, lib, phase, top-level?)
      else
        #f
      end if;
    end method,

    ///// WRAPPED-APPLICATION-STATE-SETTER
    //    ...apart from this one.

    method wrapped-application-state-setter
        (target :: <target-application>, new-state :: <symbol>) => ()
      if (new-state == #"closed")
        let project = application.server-project;
        disconnect-tether-from-all-projects(application);
        for (bp in project.project-breakpoints)
          invalidate-application-proxy(project, bp)
        end for;
        clear-profiling-results(application);
        application.application-target-app := #f;
        application.application-state := #"closed";
        project.project-application := #f
      else
        application.application-state := new-state
      end if;
    end method;

  if (application.application-target-app)
    application.application-target-app := #f;
  end;

  // Reset the counter that is used to generate thread titles.
  application.application-thread-counter := 1;
  application.evaluator-thread-counter := 1;

  // Get the filename as a string. It was either provided by this function,
  // or we can get it from an environment protocol. If _both_ are provided,
  // then we sanity check that they are the same.

  let fn :: <file-locator>
    = if (filename)
        let app-obj-fn = application.application-filename;
        if (filename ~= app-obj-fn)
          error("Filename supplied to 'make' of <application> different from "
                "filename supplied to 'run-application'.");
        else
          filename
        end
      else
        application.application-filename;
      end;

  // Get the arguments to send to the target program in the same way.

  let args :: <string>
    = if (arguments)
        let app-obj-args = application.application-arguments;
        if ((app-obj-args ~= "") & (arguments ~= app-obj-args))
          error("Arguments: keyword supplied to 'make' of <application> "
                "different from arguments: keyword supplied to "
                "'run-application'.");
        else
          arguments;
        end;
      else
        application.application-arguments;
      end;

  // Set the debugger flag if we want to debug this application.

  application.application-tether-status := startup-option;
  application.pause-before-termination-flag := pause-before-termination?;

  // Reset the record of projects that have been used by the
  // interactor.

  application.interactor-contexts-used := make(<stretchy-vector>);

  // Turn the arguments into a proper console command-line.
  args := concatenate(fn.locator-base, " ", args);

  // Spawn a thread to create the debug target, link it into the application
  // backend object, and run the DM.

  let project-object = application.server-project;
  let compiler-project = project-object.project-proxy;
  let context =
    if (compiler-project)
      compiler-project.project-browsing-context;
    else
      // There is no project associated with the application. It could
      // be a foreign application.
      #f
    end if;

  make(<thread>,
        function:
          method ()
            block (exit)

              let target-app
                = if (process)
                    make(<target-application>,
                         process: process.process-implementation-descriptor,
                         application-object: application,
                         compilation-context: context,
                         system-attachment-information: system-data,
                         debugger-connection:
                           machine.machine-debug-connection)
                  else
                    make(<target-application>,
                         application: as(<string>, fn),
                         application-object: application,
                         arguments: args,
                         working-directory:
                           (working-directory & as(<string>, working-directory)) | "",
                         library-search-paths: library-search-paths,
                         start-in-own-shell?:  ~share-console?,
                         compilation-context: context,
                         debugger-connection:
                           machine.machine-debug-connection)
                  end if;

              application.application-target-app := target-app;
              application.application-state := #"uninitialized";
              remove-all-keys!(application.application-thread-state-model);
              application.application-proxy-factory :=
                make(<application-proxy-factory>);
              application.application-proxy-factory.static-proxies
                := make(<page-relative-object-table>,
                        debug-target: target-app);
              application.application-proxy-factory.static-address-proxies
                := make(<page-relative-object-table>,
                        debug-target: target-app);
              application.application-proxy-factory.per-transaction-proxies
                := make(<page-relative-object-table>,
                        debug-target: target-app);
              run-target-application
                (target-app,
                 stop-reason-callback: wrapped-sr-callback,
                 dt-prolog: wrapped-dt-prolog,
                 dt-epilog: wrapped-dt-epilog,
                 interactor-callback: wrapped-interactor-handler,
                 library-init-callback: wrapped-library-init-handler,
                 application-state-callback: wrapped-application-state-setter);
            exception (type-union(<access-path-creation-error>, <abort>))
              application.application-target-app := #f;
              application.application-state := #"closed";
              note-run-application-failed(application);
            end block;
            note-application-threads-changed(application);
          end method,
          name: "DM Thread");

  application;
end method;


///// STOP-APPLICATION (Environment Protocol Method)
//    Stops the application at the next available opportunity.

define method stop-application
     (application :: <dfmc-application>, #key client-data = #f) => ()
  if (application.application-target-app)
    stop-target-application(application.application-target-app,
                            client-data: client-data)
  end if
end method;



///// CONTINUE-APPLICATION (Environment Protocol Method)
//    Notifies the application that it may continue when ready.

define method continue-application
    (application :: <dfmc-application>, #key thread) => ()
  let target = application.application-target-app;
  let thread = thread & thread.application-object-proxy;

  if ((target) & (target-application-state(target) ~== #"running"))
    continue-application-runtime(application, thread);
  end if;
end method;



// The new synchronization protocol requires continuations to
// happen inside debugger transactions, so clients must supply
// a continue callback

define method perform-continuing-debugger-transaction
    (application :: <dfmc-application>, remote-thread,
     transaction :: <function>)
 => (#rest results)
  perform-debugger-transaction
    (application.application-target-app, transaction,
     continue:
       method()
           continue-application-runtime(application, remote-thread)
       end);
end method;

///// CLOSE-APPLICATION (Environment Protocol Method)
//    Kills the target application, and closes down the tether.

define method close-application
    (application :: <dfmc-application>,
     #key wait-for-termination? :: <boolean>)
 => ()
  unless (application.application-closed?)
    let target = application.application-target-app;

    invalidate-interactive-compiler-proxies(application);

    perform-continuing-debugger-transaction
      (application, #f,
       method ()
         kill-target-application(target);
       end);

    // application.application-state := #"closed";
    application.runtime-class-user-class-mappings-initialized? := #f;
    for (id-proxy-pair in
           application.application-proxy-factory.proxy-factory-ordered-data)
      tail(id-proxy-pair) := #f;
    end;

    if (wait-for-termination?)
      // synchronize with the Debugger Manager thread
      with-lock (target.application-shut-down-lock, timeout: 20)
        failure
          error("Timeout expired in terminating application");
      end;
    end
  end
end method close-application;

define method invalidate-interactive-compiler-proxies
    (application :: <dfmc-application>) => ()
  //---*** Need to do this!
  debug-out(#"dfmc-environment-application",
            "Failing to invalidate interactive proxies!");
  /*
  let project = database.server-project;
  let object-table = compiler-object-table(database);
  for (object in object-table)
    invalidate-compiler-proxy(project, object)
  end
  */
end method invalidate-interactive-compiler-proxies;


///// APPLICATION-RUNNING? (Environment Protocol Method)
//    Is the application open and running?

define method application-running?
    (application :: <dfmc-application>) => (state :: <boolean>)
  target-application-state(application.application-target-app) == #"running"
end method application-running?;


///// APPLICATION-STOPPED? (Environment Protocol Method)
//    Is the application open and stopped?

define method application-stopped?
    (application :: <dfmc-application>) => (state :: <boolean>)
  target-application-state(application.application-target-app) == #"stopped"
end method application-stopped?;

///// APPLICATION-CLOSED? (Environment Protocol Method)
//    Is the application closed?

define method application-closed?
    (application :: <dfmc-application>) => (state :: <boolean>)
  let target-app = application.application-target-app;
  ~target-app | target-application-state(target-app) == #"closed"
end method application-closed?;


///// STEP-APPLICATION-OUT (Environment Protocol Method)
//    Instructs a particular thread to perform a step-out operation, and
//    allows the application to continue.

define method step-application-out
    (application :: <dfmc-application>, thread :: <thread-object>,
     #key stack-frame = #f)
 => ()

  let target = application.application-target-app;
  let remote-thread = thread.application-object-proxy;
  let call-frame =
    if (stack-frame &
            instance?(stack-frame.application-object-proxy, <call-frame>))
      stack-frame.application-object-proxy
    else
      #f
    end if;

  // Ensuring that a debugger transaction is in effect, tell the DM
  // to instruct the thread to perform the step-out operation.

  perform-continuing-debugger-transaction
     (application, remote-thread,
      method ()
        if (call-frame)
          instruct-thread-to-step-out(target, remote-thread,
                                      call-frame: call-frame);
        else
          instruct-thread-to-step-out(target, remote-thread);
        end if;
      end method);

end method;


///// STEP-APPLICATION-OVER (Environment Protocol Method)
//    Instructs a particular thread to perform a step-over operation, and
//    allows the application to continue.

define method step-application-over
    (application :: <dfmc-application>, thread :: <thread-object>,
     #key stack-frame = #f)
        => ()

  let target = application.application-target-app;
  let remote-thread = thread.application-object-proxy;
  let call-frame =
    if (stack-frame &
            instance?(stack-frame.application-object-proxy, <call-frame>))
      stack-frame.application-object-proxy
    else
      #f
    end if;

  // Ensuring that a debugger transaction is in effect, tell the DM
  // to instruct the thread to perform the step-over operation.

  perform-continuing-debugger-transaction
     (application, remote-thread,
      method ()
        if (call-frame)
          instruct-thread-to-step-over(target, remote-thread,
                                       call-frame: call-frame);
        else
          instruct-thread-to-step-over(target, remote-thread);
        end if;
      end method);

end method;


///// STEP-APPLICATION-INTO (Environment Protocol Method)
//    Instructs a particular thread to perform a step-into operation, and
//    allows the application to continue.

define method step-application-into
    (application :: <dfmc-application>, thread :: <thread-object>)
       => ()
  let target = application.application-target-app;
  let remote-thread = thread.application-object-proxy;
  let top-function = function-at-top-of-stack(application, thread);
  let callees =
    top-function & function-called-functions(application, top-function);
  perform-continuing-debugger-transaction
    (application, remote-thread,
     method ()
       let addresses =
         if (callees)
           let addresses-non-canonical
             = map(curry(function-object-breakpoint-address, application),
                   callees);
           // Some of the addresses we calculated may be duplicates, which
           // the debugger-manager does not allow. Further, some of
           // the address lookups may have failed, meaning that the mapped
           // sequence could be polluted with #f. Canonicalize the
           // sequence before passing it to the debugger manager.
           let addresses-canonical
             = remove
                 (remove-duplicates
                    (addresses-non-canonical, test: \=), #f);
           if (~empty?(addresses-canonical))
             addresses-canonical
           end if;
         end if;
       if (addresses)
         instruct-thread-to-step-into(target, remote-thread,
                                      precomputed-addresses: addresses);
       end if;
       instruct-thread-to-step-over(target, remote-thread);
     end method);
end method;


define method register-thread-in-state-model
    (application :: <dfmc-application>, stop-reason :: <stop-reason>)
 => ()
  let thread = stop-reason.stop-reason-thread;
  let thread-state
    = make(<application-thread-state>,
           thread-index: application.application-thread-counter);
  application.application-thread-counter
    := application.application-thread-counter + 1;
  thread-state-model(application, thread)
    := thread-state;
end method register-thread-in-state-model;


// Callback functions for the Debugger NUB to do explicit
// stop-reason handling for clients during spy calls

// This registers a thread created as part of a spy call in
// the environment

define method create-thread-event-handler
    (application :: <dfmc-application>, #rest keys, #key, #all-keys)
 => (stop-reason :: <stop-reason>)
  let stop-reason :: <stop-reason> =
    apply(create-thread-event-handler,
          application.application-target-app, keys);

  register-thread-in-state-model(application, stop-reason);

  stop-reason
end method;


// This processes the initial pre-arranged breakpoint event on a
// newly spawned interactive thread

define method interactive-thread-break-event-handler
    (application :: <dfmc-application>)
 => (stop-reason :: <stop-reason>)

  interactive-thread-break-event-handler
    (application.application-target-app);

end method;
