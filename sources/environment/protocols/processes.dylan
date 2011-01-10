module:     environment-protocols
synopsis:   Descriptions of running processes.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DEBUG-ITERATOR

define method debug-iterator (x :: <object>) => ()
  debug-message("Debug iterator: %=", x);
end method;


///// <PROCESS>
//    The abstract class that describes a running process on a particular
//    machine.

define open abstract class <process> (<object>)

  constant slot process-host-machine :: <machine>,
    required-init-keyword: process-host-machine:;

  constant slot process-executable-file :: <file-locator>,
    required-init-keyword: process-executable-file:;

  constant slot process-id :: <string>,
    required-init-keyword: process-id:;

  constant slot process-debuggable? :: <boolean>,
    required-init-keyword: process-debuggable?:;

end class;


///// LOOKUP-PROCESS-BY-ID

define function lookup-process-by-id
    (id :: <string>, #key machine = environment-host-machine())
      => (proc :: false-or(<process>))
  block(return)
    do-active-processes
      (method (p :: <process>) => ()
         if (p.process-id = id)
           return(p)
         end if
       end method,
       machine: machine);
    return(#f)
  end block;
end function;


///// DO-ACTIVE-PROCESSES

define function do-active-processes
   (f :: <function>, #key machine = environment-host-machine()) => ()
  do-processes-on-machine(machine, f)
end function;


///// DO-PROCESSES-ON-MACHINE
//    This is an exported but undocumented function. This is identical
//    to DO-ACTIVE-PROCESSES except that it uses the <MACHINE> argument
//    in a GF-dispatching position so that the back-end can extend it
//    appropriately. (Without this, DO-ACTIVE-PROCESSES would need its
//    full implementation to be hoisted into this library, which would
//    be a disaster).

define open generic do-processes-on-machine
   (m :: <machine>, function :: <function>) => ();

define method do-processes-on-machine
   (m :: <machine>, function :: <function>) => ();
  // Empty default method.
end method;


///// ATTACH-LIVE-APPLICATION

define open generic attach-live-application
    (server :: <server>, process :: <process>, #key client, system-data)
 => (app :: <application>);

define method attach-live-application
    (project :: <project-object>, process :: <process>,
     #key client = project, system-data)
 => (application :: <application>)
  assert(~project-application(project),
	 "Attempting to attach a process to a project with an application!");
  let filename = process-executable-file(process);
  let machine = process-host-machine(process);
  let application 
    = make-project-application
        (project, 
	 client:   client,
	 machine:  machine,
	 filename: filename);
  project-application(project) := application;
  broadcast($project-channel,
	    make(<run-application-requested-message>, project: project));
  attach-live-application(application, process, system-data: system-data | "")
end method attach-live-application;
