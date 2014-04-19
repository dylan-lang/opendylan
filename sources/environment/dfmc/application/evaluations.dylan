Module:    dfmc-application
Synopsis:  This file contains support for interactive evaluations.
Author:    Bill Chiles, Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// CONNECT-TETHER-TO-PROJECT (Internal Function)
//    Notifies the project-manager that an interactive evaluation is
//    taking place within a particular project.

define method connect-tether-to-project
    (application :: <dfmc-application>, project) => ()
  let target = application.application-target-app;
  project-current-debug-target(project) := target;
  add-new!(application.interactor-contexts-used, project);
end method;


///// DISCONNECT-TETHER-FROM-PROJECT (Internal Function)
//    Notified the project-manager that there is no longer an open
//    runtime to be associated with a particular project.

define method disconnect-tether-from-project
    (application :: <dfmc-application>, project) => ()
  let target = application.application-target-app;
  project-current-debug-target(project) := target;
  project-current-debug-target(project) := #f;
end method;


///// DISCONNECT-TETHER-FROM-ALL-PROJECTS (Internal Function)
//    A convenience function to notify the project-manager that the
//    runtime has been disconnected from all projects that were
//    used as interactor contexts.

define method disconnect-tether-from-all-projects
    (application :: <dfmc-application>) => ()
  for (project in application.interactor-contexts-used)
    disconnect-tether-from-project(application, project)
  end for
end method;


///// NOTIFY-WARNINGS-TO-ENVIRONMENT (Internal Function)
//    Finds the program notes associated with a particular transaction
//    ID, coerces them to "real" environment objects, and notifies them
//    upwards if there are any.

define method notify-warnings-to-environment
    (application :: <dfmc-application>, thread :: <thread-object>,
     transaction-id :: <object>, mod :: <module-object>) => ()
  let warnings = make(<stretchy-vector>);
  let project-object = 
    application.server-project;
  let project = module-project-proxy(project-object, mod);
  // Acquire the set of warnings from the Project Manager.
  let notes =
    project-interactive-execution-notes
       (project, record-id: transaction-id);

  // Use an iterator to build the sequence of compiler warning objects.
  do-program-notes(method (warning) add!(warnings, warning) end method, 
                   project-object,
                   environment-object-library(project-object, mod),
                   notes);

  if (~empty?(warnings))
    let message
      = make(<thread-interactive-warnings-message>, 
	     project: project-object,
	     thread: thread,
	     transaction-id: transaction-id,
	     warnings: warnings);
    broadcast($project-channel, message);
    let message
      = make(<project-warnings-updated-message>, project: project-object);
    broadcast($project-channel, message)
  end if
end method;


///// PROJECT-VALID-CODE? (Environment Protocol Method)
//    Sends a string of dylan code, and the appropriate contextual
//    information, to the interactive parser. The parser will inform us
//    whether the string constitutes valid dylan code, and will also
//    return a sequence of compiler warnings resulting from the analysis.

define method project-valid-code?
    (app :: <dfmc-application>, code :: <string>,
     thread :: <thread-object>,
     #key module = #f, runtime-context = #f, stack-frame = #f)
    => (valid? :: <boolean>, warnings :: <sequence>)

  // Get a copy of the <debug-target>.
  let target = app.application-target-app;

  // If the runtime context hasn't been supplied explicitly, use the
  // thread's "current" runtime context for the parse.

  unless (runtime-context)
    runtime-context :=
       project-runtime-context(app, thread, stack-frame: stack-frame)
  end unless;

  // Hopefully, 'module' will have been supplied, and we won't perform the
  // body of this clause...
  // (My real reason for hoping that, is that 'find-module' doesn't seem to
  // be implemented anywhere...)

  unless (module)
    module := find-module(app.server-project, "dylan-user");
  end unless;

  if (module)

    // ASSUMPTION: The primitive name for <module-object>, is the actual
    // name of the module it defines.

    let module-actual-name =
      environment-object-primitive-name(app.server-project, module);

    // ASSUMPTION: module's project is an instance of <interactive-project>.
    let project = module-project-proxy(app.server-project, module);
    connect-tether-to-project(app, project);

    // Call the project manager to do source code management, and to send
    // the code to be compiled.

    // Note the lack of warning/error handlers here. It is hoped that these
    // will be handled by the environment at a higher level.

    let (valid?, warnings) = 
      parse-expression(project,
                       runtime-context,
                       as(<symbol>, module-actual-name),
                       code);

    values(valid?, warnings);
  else
    error("Could not locate a <module-object> as evaluation context.");
    values(#f, #[]);
  end if;
end method;


///// PROJECT-RUNTIME-CONTEXT (Environment Protocol Method)
//    Returns the runtime context for a thread at its current code location.

define method project-runtime-context
    (application :: <dfmc-application>, thread :: <thread-object>,
     #key stack-frame = #f)
        => (runtime-context)
  let target = application.application-target-app;
  let context = #f;
  let remote-thread = thread.application-object-proxy;
  let frame = stack-frame & stack-frame.application-object-proxy;
  perform-debugger-transaction
     (target,
      method ()
        context := 
          current-runtime-context(target, remote-thread, stack-frame: frame);
      end method);
  context;
end method;


///// PROJECT-EXECUTE-CODE (Environment Protocol Method)
//    Sends a string of dylan code, and the appropriate contextual information,
//    to the interactive compiler, which will eventually invoke the
//    interactive downloader.

define method project-execute-code
    (app :: <dfmc-application>, code :: <byte-string>, 
     thread :: <thread-object>,
     #key module = #f, runtime-context = #f, stack-frame = #f)
    => (execution-id :: <object>, execution-deferred? :: <boolean>)

  // Assume that we will be able to call the interactive downloader
  // straight away.
  let execution-deferred? = #f;
  let state = app.application-state;

  // Get a copy of the <debug-target>.
  let target = app.application-target-app;
  let path = target.debug-target-access-path;
  let remote-thread = thread.application-object-proxy;

  // If the runtime context hasn't been supplied explicitly, use the
  // thread's "current" runtime context for the evaluation.

  unless (runtime-context)
    runtime-context := 
      project-runtime-context(app, thread, stack-frame: stack-frame)
  end unless;

  // Hopefully, 'module' will have been supplied, and we won't perform the
  // body of this clause...
  // (My real reason for hoping that, is that 'find-module' doesn't seem to
  // be implemented anywhere...)

  unless (module)
    module := find-module(app.server-project, "dylan-user");
  end unless;

  if (module)

    // ASSUMPTION: The primitive name for <module-object>, is the actual
    // name of the module it defines.

    let module-actual-name =
      environment-object-primitive-name(app.server-project, module);

    // ASSUMPTION: module's project is an instance of <interactive-project>.
    let project = module-project-proxy(app.server-project, module);
    connect-tether-to-project(app, project);

    // Call the project manager to do source code management, and to send
    // the code to be compiled.

    // Note the lack of warning/error handlers here. It is hoped that these
    // will be handled by the environment at a higher level.

    let id = #f;

    // Find out whether we can cope with this interaction now, or defer
    // it until later. If we need to defer, then issue the instruction that
    // will align the thread to an interactive source location, and
    // queue the interaction request.
    // Otherwise, call the project manager's API to actually download the
    // expression.

    perform-continuing-debugger-transaction
      (app, remote-thread,
       method ()

	 let evaluation-on-suspended-thread? =
	   thread-permanently-suspended?(path, remote-thread);

	 if (evaluation-on-suspended-thread?)
	   resume-evaluator-thread(app, thread.application-object-proxy);
	 end;

         if (evaluation-on-suspended-thread?
	     | thread-available-for-interaction?(target, remote-thread))
           execution-deferred? := #f;
	   let transaction :: <interactor-return-breakpoint> =
	     evaluate-expression(project,
				 runtime-context,
				 as(<symbol>, module-actual-name),
				 code);
	   id := transaction;

	     // Need to remember the application state on code entry
	     // for interactions on suspended threads, as these happen
	     // immediately behind the back of a running application

	   transaction.interaction-request-application-state := state;

	   if (evaluation-on-suspended-thread?)
	     thread-state-transaction(app, remote-thread) := transaction;
	   end;

           // Since we have actually called the compiler, notify
           // warnings.
           notify-warnings-to-environment
              (app, thread, id, module);

           // Register the transaction ID in the debugger frame at the start
	   // of the interaction -- this will be used to determine whether
	   // the frame needs refreshing when the interaction returns;
	   // NOTE: if we need to request an interaction and align to a 
	   // source code location (see below), the debugger stack pane 
	   // always needs to be refreshed, so it would be wrong to invoke
	   // this callback.
	   invoke-application-callback
	     (app, application-started-interaction-callback, thread, id);

         else
           execution-deferred? := #t;
           // Give the environment a deferred ID.
           id := request-interaction(app, remote-thread,
                                     runtime-context,
                                     module,
                                     code, state);
         end if;

       end method);

    // Update the application's compilation context.
    target.debug-target-compilation-context :=
       project.project-browsing-context;

    // Return the unique evaluation ID. Later, when the evaluation returns,
    // an environment callback will be invoked with this ID, along with the
    // return values, the project, and the thread.

    values(id, execution-deferred?);
  else
    error("Could not locate a <module-object> as evaluation context.");
    values(#f, #f);
  end if;
end method;

// A more general mechanism for interacting (e.g. calling C Runtime code) 
// without involving the project manager or compiler

define method execute-function
    (app :: <dfmc-application>, function :: <function>,
     thread :: <thread-object>,
     #key state = #f)
    => (execution-id :: <object>)

  let state = state | app.application-state;
  let target = app.application-target-app;
  let path = target.debug-target-access-path;
  let remote-thread = thread.application-object-proxy;
  let id = #f;

  perform-debugger-transaction
      (target,
       method ()

	 let evaluation-on-suspended-thread? =
	   thread-permanently-suspended?(path, remote-thread);

	 if (evaluation-on-suspended-thread?)
	   resume-evaluator-thread(app, thread.application-object-proxy);
	 end;

         if (evaluation-on-suspended-thread?
	     | thread-available-for-interaction?(target, remote-thread))
	   let transaction :: <interactor-return-breakpoint> = function();
	   id := transaction;

	   transaction.interaction-request-application-state := state;

	   if (evaluation-on-suspended-thread?)
	     thread-state-transaction(app, remote-thread) := transaction;
	   end;

	   invoke-application-callback
	     (app, application-started-interaction-callback, thread, id);

	 else
	   error("Cannot perform the requested interaction");
         end if;

       end method);

    id
end method;


///// FETCH-INTERACTOR-RETURN-VALUES (Environment Protocol Method)
//    Given an application, and the unique ID of an interactive transaction
//    that has returned, produce a sequence of environment objects for
//    the return results.

define method fetch-interactor-return-values
    (application :: <dfmc-application>, id :: <thread-interaction-request>)
  => (env-obj-seq :: <sequence>)
  let actual-id = id.interaction-request-actual-id;
  fetch-interactor-return-values(application, actual-id);
end method;

define method fetch-interactor-return-values
    (application :: <dfmc-application>, id :: <interactor-return-breakpoint>)
       => (env-obj-seq :: <sequence>)
  let target = application.application-target-app;
  let env-obj-seq = #[];
  let remote-value-seq =
    element(application.interactor-results-table, id, default: #f);
  if (remote-value-seq)
    env-obj-seq := make(<vector>, size: size(remote-value-seq));
    perform-debugger-transaction
       (target,
        method ()
          for (i from 0 below size(remote-value-seq))
            env-obj-seq[i] :=
              pair(head(remote-value-seq[i]),
                   make-environment-object-for-runtime-value
                      (application, tail(remote-value-seq[i])));
          end for;
        end method);
  end if;
  env-obj-seq;
end method;


///// TRANSACTION-ID-SOURCE-RECORD (Environment Protocol Method)
//    Given an application, and the unique ID of an interactive transaction
//    that has returned, return its interactive source record.

define method transaction-id-source-record
    (application :: <dfmc-application>, id :: <thread-interaction-request>)
 => (source-record :: false-or(<interactive-source-record>))
  let actual-id = id.interaction-request-actual-id;
  transaction-id-source-record(application, actual-id);
end method;

define method transaction-id-source-record
    (application :: <dfmc-application>, id :: <interactor-return-breakpoint>)
 => (source-record :: false-or(<interactive-source-record>))
  let project-object = application.server-project;
  let project = project-object.project-proxy;
  project-id-interactive-record(project, id)
end method;


///// DISPOSE-INTERACTOR-RETURN-VALUES (Environment Protocol Method)
//    Declare that there is no further need to map this unique ID to the
//    return values.

define method dispose-interactor-return-values
    (application :: <dfmc-application>, id :: <thread-interaction-request>)
  => ()
  let actual-id = id.interaction-request-actual-id;
  dispose-interactor-return-values(application, actual-id);
end method;

define method dispose-interactor-return-values
    (application :: <dfmc-application>, id :: <interactor-return-breakpoint>) => ()
  let remote-value-seq = 
    element(application.interactor-results-table, id, default: #f);
  if (remote-value-seq)
    remove-key!(application.interactor-results-table, id)
  end if
end method;

