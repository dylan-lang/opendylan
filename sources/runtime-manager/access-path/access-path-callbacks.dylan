Module:        access-path-implementation
Synopsis:      Callbacks to the Access Path
Author:        Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Clients are requested to set this up to enable spy calls 
// in the debugger NUB to callback to the access-path

define variable *current-access-paths* :: <stretchy-vector> =
  make(<stretchy-vector>);

define constant *access-path-lock* = make(<simple-lock>);

define method register-access-path
    (ap :: <access-path>)
  with-lock (*access-path-lock*)
    add!(*current-access-paths*, ap);
  end;
end method;

define method deregister-access-path
    (ap :: <access-path>)
  with-lock (*access-path-lock*)
    remove!(*current-access-paths*, ap);
  end;
end method;

define method lookup-access-path-application
    (proc :: <NUBPROCESS>) => (path, application)
  block (return)
    for (ap :: <access-path> in *current-access-paths*)
      let ap-process :: <NUBPROCESS> = ap.connection.connection-process;
      if (ap-process = proc)
	return(ap, ap.access-path-application-object)
      end
    end for;
  end block;
end;


// Callback functions for the Debugger NUB to do explicit
// stop-reason handling for clients during spy calls

// This registers a thread created as part of a spy call in
// the client

define c-callable-wrapper create-thread-stop-reason-handler-wrapper
    of create-thread-stop-reason-handler
  parameter process          :: <NUBPROCESS>;
  parameter thread           :: <NUBTHREAD>;
  parameter priority         :: <NUBINT>;
  c-name: "create_thread_stop_reason_handler";
end;

ignore(create-thread-stop-reason-handler-wrapper);

define function create-thread-stop-reason-handler
    (process :: <NUBPROCESS>, thread :: <NUBTHREAD>,
     priority :: <integer>) => ()
  let (path, application) = lookup-access-path-application(process);
  let process = make (<remote-process>,
                      nub-descriptor: process);
  let thread =
    find-or-make-thread 
    (path, thread, priority: priority);
  create-thread-event-handler
    (application, process: process, thread: thread);
  values();
end function;


define open generic create-thread-event-handler
    (application, #key) => (stop-reason :: <stop-reason>);


define method create-thread-event-handler
    (ap :: <access-path>,
     #key process, thread)
 => (stop-reason :: <stop-reason>)
  construct-stop-reason
  (ap, $create-thread, process: process, thread: thread);
end method;



// This processes the initial pre-arranged breakpoint event on a
// newly spawned interactive thread

define open generic interactive-thread-break-event-handler
    (application) => (stop-reason :: <stop-reason>);

define method interactive-thread-break-event-handler
    (ap :: <access-path>)
 => (stop-reason :: <stop-reason>)
  construct-stop-reason(ap, $hard-coded-breakpoint-exception);
end method;

