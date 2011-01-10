Module:    environment-protocols
Synopsis:  Protocol-level abstract descriptions of restarts and their
           invocation.
Author:    Paul Howard (Not the owner of this library!)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <RESTART-OBJECT>
//    An application-only object.
//    Provides an abstract handle on a restart that can be signalled on a
//    thread. Via this handle, a printable description of the restart can
//    be obtained, and the restart itself can be signalled on the thread
//    prepared to handle it.

define sealed class <restart-object> (<application-object>)
end class;


// Exported operations on <restart-object>

define open generic application-thread-restarts
    (server :: <server>, thread :: <thread-object>)
 => (sequence-of-restart-objects :: <sequence>);

define open generic application-restart-message
    (server :: <server>, rst :: <restart-object>)
 => (printable-string :: <string>);

define open generic application-restart-abort?
    (server :: <server>, rst :: <restart-object>)
 => (is-abort? :: <boolean>);

define open generic invoke-application-restart
    (server :: <server>, thread :: <thread-object>, rst :: <restart-object>)
 => ();


///
/// Methods at the project level.
/// (These just dispatch to the application server, which implements them)
///


define method application-thread-restarts
    (project :: <project-object>, thread :: <thread-object>)
 => (sequence-of-restart-objects :: <sequence>)
  let server = choose-server(project, thread, error?: #t);
  application-thread-restarts(server, thread)
end method;

define method application-restart-message
    (project :: <project-object>, rst :: <restart-object>)
 => (printable-string :: <string>)
  let server = choose-server(project, rst, error?: #t);
  application-restart-message(server, rst)
end method;

define method application-restart-abort?
    (project :: <project-object>, rst :: <restart-object>)
 => (is-abort? :: <boolean>)
  let server = choose-server(project, rst, error?: #t);
  application-restart-abort?(server, rst)
end method;

define method invoke-application-restart
    (project :: <project-object>, thread :: <thread-object>,
     rst :: <restart-object>)
 => ()
  let server = choose-server(project, rst, error?: #t);
  invoke-application-restart(server, thread, rst)
end method;

define method environment-object-type-name
    (object :: <restart-object>) => (name :: <string>)
  "Restart"
end method environment-object-type-name;


