Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Application objects

define constant <application-state>
  = one-of(#"uninitialized", #"running", #"stopped", #"closed");

define constant <application-startup-option>
  = one-of(#"start", #"debug", #"interact");

define open abstract primary class <application> 
    (<server>, <environment-object>)
  sealed constant slot server-project :: <project-object>,
    required-init-keyword: project:;
  sealed constant slot application-object-table :: <table> = make-object-cache();
  slot application-client :: <object>,
    required-init-keyword: client:;
  slot application-machine :: <machine>,
    required-init-keyword: machine:;
  slot application-filename :: <file-locator>,
    required-init-keyword: filename:;
  slot application-arguments :: <string> = "",
    init-keyword: arguments:;
  slot application-state :: <application-state> = #"uninitialized",
    setter: %state-setter;
  slot application-temporary-stop? :: <boolean> = #f;
end class <application>;

define method application-state-setter 
    (state :: <application-state>, application :: <application>)
 => (state :: <application-state>)
  unless (state == application.application-state)
    application.%state := state;
    unless (application.application-temporary-stop?)
      broadcast($project-channel,
		make(<application-state-changed-message>, 
		     project: application.server-project))
    end
  end;
  state
end method application-state-setter;

define macro with-application-transaction
  { with-application-transaction (?server:expression)
      ?body:body
    end }
 => { perform-application-transaction
        (?server,
	 method () ?body end) }
end macro with-application-transaction;

define open generic note-application-initialized
    (project :: <project-object>)
 => ();

define open generic perform-application-transaction
    (server :: <server>, function :: <function>)
 => (#rest values);

define open generic application-threads
    (application :: <application>, #key client)
 => (threads :: <sequence>);

define open generic application-running?
    (application :: <application>)
 => (running? :: <boolean>);

define open generic application-stopped?
    (application :: <application>)
 => (stopped? :: <boolean>);

define open generic application-closed?
    (application :: <application>)
 => (closed? :: <boolean>);

define open generic find-application-proxy
    (application :: <application>, id :: <id>)
 => (application-proxy);

define open generic application-proxy-id
    (application :: <application>, proxy)
 => (id :: false-or(<id>));

define open generic application-debug?
    (server :: <server>)
 => (debug? :: <boolean>);

define open generic application-pause-before-termination?
    (server :: <server>)
 => (well? :: <boolean>);

define open generic application-startup-option
    (server :: <server>) => (opt :: <application-startup-option>);

define open generic application-machine
    (server :: <server>) => (machine :: false-or(<machine>));

define open generic application-stop-reason-message
    (application :: <application>)
 => (message :: false-or(<string>));


/// Application handling

define open generic run-application
    (server :: <server>,
     #key startup-option,
          client,
          filename,
          arguments,
          pause-before-termination?,
          library-search-paths,
          working-directory,
          machine)
 => (application :: <application>);

define open generic initialize-application-client
    (client :: <object>, application :: <application>) => ();

// There's no NOTE-RUN-APPLICATION-REQUESTED, we just do the relevant
// work in RUN-APPLICATION.

define open generic note-run-application-failed
    (application :: <application>) => ();

define open generic update-application
    (server :: <server>, #key progress-callback) => ();

define open generic continue-application
    (server :: <server>, #key thread) => ();     

define open generic stop-application
    (server :: <server>, #key client-data) => ();     

define open generic close-application
    (server :: <server>, #key wait-for-termination? :: <boolean>) => ();

/// MAKE-PROJECT-APPLICATION (environment-protocols)

define open generic make-project-application
    (project :: <project-object>, #key, #all-keys)
 => (application :: <application>);



/// Application state implementation

define method application-running?
    (application :: <application>) => (running? :: <boolean>)
  application-state(application) = #"running"
end method application-running?;

define method application-stopped?
    (application :: <application>) => (stopped? :: <boolean>)
  application-state(application) = #"stopped"
end method application-stopped?;

define method application-closed?
    (application :: <application>) => (closed? :: <boolean>)
  application-state(application) = #"closed"
end method application-closed?;

define function application-tethered?
    (server :: <server>) => (tethered? :: <boolean>)
  let project = server.server-project;
  let application = project.project-application;
  application & ~application.application-closed?
end function application-tethered?;


/// Implementation

define method perform-application-transaction
    (project :: <project-object>, function :: <function>)
 => (#rest values)
  let application = project.project-application;
  if (application)
    perform-application-transaction(application, function)
  else
    function()
  end
end method perform-application-transaction;

define method initialize-application-client
    (client :: <object>, application :: <application>) => ()
  #f
end method initialize-application-client;

define method note-application-initialized
    (project :: <project-object>)
 => ()
  broadcast($project-channel,
	    make(<application-initialized-message>, project: project));
end method note-application-initialized;

define method choose-server
    (project :: <project-object>, application :: <application>, 
     #key error?, default-server)
 => (application :: <application>)
  ignore(error?, default-server);
  application
end method choose-server;

define method record-client-query
    (application :: <application>, client, object :: <application-object>, 
     type :: <query-type>)
 => ()
  record-client-query(server-project(application), client, object, type)
end method record-client-query;

define method environment-object-type-name
    (object :: <application>) => (label :: <string>)
  "Application"
end method environment-object-type-name;

define method lookup-environment-object-by-proxy
    (application :: <application>, proxy)
 => (object :: false-or(<application-object>))
  element(application-object-table(application), proxy, default: #f)
end method lookup-environment-object-by-proxy;

define method cache-environment-object
    (application :: <application>, proxy, object :: <application-object>)
 => (object :: <application-object>)
  element(application-object-table(application), proxy) := object
end method cache-environment-object;

define method environment-object-home-server?
    (application :: <application>, object :: <application-object>)
 => (home? :: <boolean>)
  let proxy = application-object-proxy(object);
  proxy & lookup-environment-object-by-proxy(application, proxy) & #t
end method environment-object-home-server?;


define method application-debug?
    (project :: <project-object>) => (debug? :: <boolean>)
  let application = project.project-application;
  application & application-debug?(application);
end method;

define method application-pause-before-termination?
    (project :: <project-object>) => (pause? :: <boolean>)
  let application = project.project-application;
  application & application-pause-before-termination?(application);
end method;

define method application-startup-option
    (project :: <project-object>) => (opt :: <application-startup-option>);
  let application = project.project-application;
  if (application)
    application-startup-option(application)
  else
    // TODO: Signal some kind of error!
    //       This is overly-defensive programming, and I deserve to die
    //       for this.
    #"start"
  end if
end method;


/// RUN-APPLICATION (environment-protocols) 

define method run-application 
    (project :: <project-object>,
     #key startup-option = #"start",
          client = project,
          filename,
          arguments,
          working-directory,
          pause-before-termination? 
            = project.project-default-pause-before-termination?,
          share-console? = #f,
          library-search-paths = vector(release-runtime-directory()),
          machine = unsupplied())
 => (application :: <application>)
  let machine :: <machine>
    = if (supplied?(machine)) machine else environment-host-machine() end;
  unless (filename & arguments & working-directory)
    let (f, a, wd) = project-debug-options(project);
    filename          := filename  | f;
    arguments         := arguments | a;
    working-directory := working-directory | wd;
  end;
  let application = project-application(project);
  if (application)
    assert(application.application-closed?,
	   "Attempting to restart an already running application: %s",
	   filename);
    application-filename(application) := filename;
    application-arguments(application) := arguments;
    application-machine(application) := machine;
    application-client(application) := client
  else
    application 
      := make-project-application(project,
				  client:    client,
				  machine:   machine,
				  filename:  filename, 
				  arguments: arguments);
    project-application(project) := application;
  end if;
  broadcast($project-channel,
	    make(<run-application-requested-message>, project: project));
  initialize-application-client(client, application);
  let host-machine? = machine == environment-host-machine();
  run-application(application,
		  startup-option: startup-option,
		  client:    client,
		  filename:  filename,
		  arguments: arguments,
                  machine:   machine,
		  library-search-paths: 
                     if (host-machine?)
                       library-search-paths
                     else
                       #[]
                     end,
		  working-directory: 
                     if (host-machine?)
                       working-directory | project.project-bin-directory
                     end,
		  share-console?:            share-console?,
		  pause-before-termination?: pause-before-termination?);
end method run-application;

define constant $dll-wrap-application = "bin/dll-wrap.exe";

define function project-debug-options
    (project :: <project-object>)
 => (filename :: <file-locator>, arguments :: <string>,
     working-directory :: false-or(<directory-locator>))
  let build-filename = project.project-build-filename;
  let bin-directory = project.project-bin-directory;
  let build-location = merge-locators(build-filename, bin-directory);
  let debug-filename = project.project-debug-filename;
  let debug-arguments = project.project-debug-arguments;
  let target-type = project.project-target-type;
  case
    debug-filename =>
      values(debug-filename, debug-arguments, #f);
    project.playground-project? =>
      let filename = project.playground-application-filename | build-location;
      values(filename, debug-arguments, filename.locator-directory);
    target-type == #"executable" =>
      values(build-location, debug-arguments, #f);
    target-type == #"dll" =>
      let directory = as(<directory-locator>, release-directory());
      values(merge-locators(as(<file-locator>, $dll-wrap-application),
			    directory),
	     format-to-string("\"%s\"",
			      as(<string>, build-location)),
	     #f);
    otherwise =>
      error("The project is neither a DLL nor an EXE?");
  end
end function project-debug-options;

define function project-default-pause-before-termination?
    (project :: <project-object>)
 => (pause-before-termination? :: <boolean>)
  project.project-interface-type == #"console"
  & project.project-target-type == #"executable"
end function project-default-pause-before-termination?;

//---*** hughg: Could add a method in dfmc-environment-application for
//---*** <dfmc-application> which does the cleanup work currently done
//---*** just before it calls this.
define method note-run-application-failed
    (application :: <application>) => ()
  let project = application.server-project;
  broadcast($project-channel,
	    make(<run-application-failed-message>, project: project))
end method note-run-application-failed;

define method continue-application 
    (project :: <project-object>, #key thread) => ()
  let application = project-application(project);
  application & continue-application(application, thread: thread)
end method continue-application;

define method stop-application 
    (project :: <project-object>, #key client-data = #f) => ()
  let application = project-application(project);
  application & stop-application(application, client-data: client-data)
end method stop-application;

define method close-application 
    (project :: <project-object>,
     #key wait-for-termination? :: <boolean>)
 => ()
  let application = project-application(project);
  application
    & close-application(application, 
			wait-for-termination?: wait-for-termination?)
end method close-application;

define method update-application 
    (project :: <project-object>, #key progress-callback) => ()
  let application = project-application(project);
  application
    & update-application(application, progress-callback: progress-callback)
end method update-application;


/// Proxy handling

//--- This default method means that the objects will never get linked, so
//--- we need a real solution in the application server implementation.
define method find-application-proxy
    (application :: <application>, id :: <id>)
 => (application-proxy)
  #f
end method find-application-proxy;

define function ensure-application-proxy
    (application :: <application>, object :: <application-object>)
 => (proxy)
  application-object-proxy(object)
    | begin
	let project = server-project(application);
	let id = environment-object-id(project, object);
	if (instance?(id, <id>))
	  let proxy = find-application-proxy(application, id);
	  if (proxy)
	    application-object-proxy(object) := proxy
	  end
	end
      end
end function ensure-application-proxy;
