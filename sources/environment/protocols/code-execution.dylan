Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong, Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Interactively Executing Code.
///

/// --- This class still needs to be defined.  The Debugger-manager and
/// Compiler-interface libraries need to agree on what this is, and we need
/// to bind to whatever type they agree on, or in some other way make it
/// available to users of the environment protocols.
///
define constant <execution-id> = <object>;

define sealed class <execution-info> (<object>)
  //
  // This slot holds the unique ID returned by the compiler that represents
  // an interactive evaluation request.
  constant slot execution-info-id :: <execution-id>,
    required-init-keyword: id:;
  //
  // This slot holds the thread on which the evaluation was performed.
  constant slot execution-info-thread :: <thread-object>,
    required-init-keyword: thread:;
end class <execution-info>;


/// Project-execute-code
///
/// This must be called while the target application is in a debugger
/// transaction.  Essentially, it creates a source-record for code, passes
/// the source record to the compiler to be sent to the target app, saves an
/// <execution-info> object that can later be retrived via
/// 'project-execution-info', and returns the execution info.  This function
/// returns #f when the compiler could not process the code.
///
/// Module must be a <module-object>.

define open generic project-execute-code
    (server :: <server>, code :: <string>, thread :: <thread-object>,
     #key module, runtime-context, stack-frame)
 => (execution-id :: <execution-id>, deferred-execution? :: <boolean>);

define open generic project-macroexpand-code
    (server :: <server>, module :: <module-object>, code :: <string>,
     #key expansion-stream :: <stream>, trace-stream :: <stream>)
 => ();

define open generic project-runtime-context
    (server :: <server>, thread :: <thread-object>,
     #key stack-frame)
 => (runtime-context);

define open generic fetch-interactor-return-values
    (server :: <server>, execution-id :: <execution-id>)
 => (environment-objects :: <sequence>);

define open generic dispose-interactor-return-values
    (server :: <server>, execution-id :: <execution-id>)
 => ();

define open generic project-valid-code?
    (server :: <server>, code :: <string>, thread :: <thread-object>,
     #key module, runtime-context, stack-frame)
 => (valid? :: <boolean>, warnings :: <sequence>);

define open generic application-state-at-code-entry
    (id :: <object>) => (state :: <application-state>);

define open generic transaction-id-source-record
    (server :: <server>, id :: <execution-id>)
 => (source-record :: false-or(<interactive-source-record>));


/// Record-return-values
///
/// Pass return values to the Interactor component via whatever interface it
/// provides, or whatever the environment protocols provide.
///
define open generic record-return-values (project :: <project-object>,
					  execution-id :: <execution-id>,
					  values :: <sequence>)
 => ();


/// Project-execution-info
///
/// Returns the execution info associated with id, or #f if there is none.
///
define open generic project-execution-info (project :: <project-object>,
					    id :: <execution-id>)
 => (info :: false-or(<execution-info>));

/// Project-remove-execution-info
///
/// Removes any references the project may have to the execution-info.
///
define open generic project-remove-execution-info
    (project :: <project-object>, info :: <execution-info>)
 => ();


///// INTERACTIVITY (Methods added by Paul Howard, June 1997)
//    Work is farmed out to the application server.

define method project-execute-code
    (project :: <project-object>, code :: <string>, thread :: <thread-object>,
     #key module = #f, runtime-context = #f, stack-frame = #f)
 => (execution-id :: <execution-id>, deferred-execution? :: <boolean>)
  let server = choose-server(project, thread, error?: #t);
  project-execute-code
    (server, code, thread,
     module: module,
     runtime-context: runtime-context,
     stack-frame: stack-frame)
end method;

define method project-macroexpand-code
    (project :: <project-object>, module :: <module-object>, code :: <string>,
     #key expansion-stream :: false-or(<stream>) = #f,
          trace-stream :: false-or(<stream>) = #f)
 => ()
  let database = ensure-database-server(project, module, error?: #t);
  project-macroexpand-code
    (database, module, code,
     expansion-stream: expansion-stream,
     trace-stream: trace-stream)
end method;

define method project-runtime-context
    (project :: <project-object>, thread :: <thread-object>,
     #key stack-frame = #f)
 => (runtime-context)
  let server = choose-server(project, thread, error?: #t);
  project-runtime-context(server, thread, stack-frame: stack-frame)
end method;

define method project-valid-code?
    (project :: <project-object>, code :: <string>, thread :: <thread-object>,
     #key module = #f, runtime-context = #f, stack-frame = #f)
 => (valid? :: <boolean>, warnings :: <sequence>)
  let server = choose-server(project, thread, error?: #t);
  project-valid-code?
    (server, code, thread,
     module: module, 
     runtime-context: runtime-context,
     stack-frame: stack-frame)
end method;

define method transaction-id-source-record
    (project :: <project-object>, id :: <execution-id>)
 => (record :: false-or(<interactive-source-record>))
  let server = project-application(project);
  server & transaction-id-source-record(server, id)
end method;

define method fetch-interactor-return-values
    (project :: <project-object>, execution-id :: <execution-id>)
 => (environment-objects :: <sequence>)
  if (project.project-application)
    fetch-interactor-return-values(project.project-application, execution-id)
  else
    #[]
  end if;
end method;

define method dispose-interactor-return-values
    (project :: <project-object>, execution-id :: <execution-id>)
 => ()
  if (project.project-application)
    dispose-interactor-return-values(project.project-application,
                                     execution-id);
  end if;
end method;
