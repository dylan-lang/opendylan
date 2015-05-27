module:     access-path-implementation
synopsis:   Implementation of debugger connections
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// define constant $dbg-transport-ok = 1;
// define constant $dbg-transport-illegal-binding = 2;
// define constant $dbg-transport-could-not-find-server = 3;
// define constant $dbg-transport-server-spawn-nub-failure = 4;
// define constant $dbg-transport-could-not-find-nub = 5;
// define constant $dbg-transport-nub-spawn-process-failure = 6;


///// <DEBUGGER-CONNECTION>
//    An instance of <debugger-connection> can be seen as an object that 
//    has the capability of creating debugger nubs.

define abstract class <debugger-connection> (<object>)
  slot connection-hostname :: <string>;
  constant slot connection-open-tethers :: <stretchy-vector> 
    = make(<stretchy-vector>);
  slot connection-process-list :: <stretchy-vector> 
    = make(<stretchy-vector>);
end class;


///// <LOCAL-DEBUGGER-CONNECTION>
//    A debugger connection to a process known to be running on the same
//    machine as the development environment.

define class <local-debugger-connection> (<debugger-connection>)
end class;


///// <REMOTE-DEBUGGER-CONNECTION>
//    A debugger connection to a process known to be running on a
//    different machine than the development environment.

define open abstract class <remote-debugger-connection> (<debugger-connection>)
  constant slot connection-network-address :: <string>,
    required-init-keyword: network-address:;
  constant slot connection-password :: <string>,
    required-init-keyword: password:;
  slot connection-open? :: <boolean> = #f;
end class;


///// Make sure that <debugger-connection> is instantiable.

define method make (class == <debugger-connection>, #rest keys, #key)
    => (connection)
  apply (make, <local-debugger-connection>, keys);
end method;


///// <DEBUGGER-CONNECTION-FAILURE>
//    Signalled when attempting to make a debugger connection and
//    it fails.

define abstract class <debugger-connection-failure> (<error>)
end class;

define class <open-debugger-connection-failure> 
                  (<debugger-connection-failure>)
  constant slot attempted-connection-network-address :: <string>,
    required-init-keyword: network-address:;
end class;

define class <open-debugger-connection-password-mismatch>
                  (<debugger-connection-failure>)
  constant slot attempted-connection-password :: <string>,
    required-init-keyword: wrong-password:;
end class;

  
///// *OPEN-DEBUGGER-CONNECTIONS*
//    Holds all open debugger connections.

define variable *open-debugger-connections* = make(<stretchy-vector>);


///// DO-OPEN-DEBUGGER-CONNECTIONS
//    Iterates over all open debugger connections.

define function do-open-debugger-connections (f :: <function>) => ()
  for (connection in *open-debugger-connections*)
    f(connection)
  end for
end function;


///// DO-OPEN-ACCESS-CONNECTIONS
//    Iterates over all open tethers within a particular debugger connection.

define function do-open-access-connections
  (f :: <function>, server :: <debugger-connection>) => ()
  for (connection in server.connection-open-tethers)
    f(connection)
  end for
end function;


///// INITIALIZE (Dylan)
//    Sets up debugger connections.

define constant $local-hostname :: <byte-string> =
  begin
    let hostname-length = get-local-hostname-length();
    let hostname = make(<byte-string>, size: hostname-length);
    get-local-hostname(hostname-length, hostname);
    hostname
  end;

define method initialize
    (connection :: <local-debugger-connection>, #key, #all-keys) => ()
  // A local debugger connection is really nothing more than a place
  // holder. All we need to do is fetch the local hostname from the
  // local debugger nub.
  next-method();
  connection.connection-hostname := $local-hostname;
  add!(*open-debugger-connections*, connection);
end method;


///// DESCRIBE-DEBUGGER-CONNECTION
//    Produces a printable representation of a debugger connection.
//    TODO: This should include a description of the transport
//          protocol in use by the server.

define method describe-debugger-connection
    (connection :: <local-debugger-connection>) => (desc :: <string>)
  format-to-string("%s (Local machine)", connection.connection-hostname)
end method;

define method describe-debugger-connection
    (connection :: <remote-debugger-connection>) => (desc :: <string>)
  format-to-string("%s (%s)", 
                   connection.connection-hostname,
                   connection.connection-network-address)
end method;


///// *DEFAULT-LOCAL-DEBUGGER-CONNECTION*
//    While our connection philosophy is currently very simple - there is
//    no real nub server or debugger connection, this object serves as
//    a place-holder for it.

define constant *default-local-debugger-connection* =
        make (<debugger-connection>);


///// HOST-MACHINE
//    A functional interface, returning the local debugger connection.

define function host-machine () => (connection :: <debugger-connection>)
  *default-local-debugger-connection*
end function;


///// <ACCESS-CONNECTION>
//    Instances of <access-connection> relate directly to instances of 
//    a debugger nub. They contain the information that is necessary to 
//    communicate with a specific debugger nub from the access path. An 
//    access connection can be local or remote, and (orthogonally) 32-bit 
//    or 64-bit.

define open abstract class <access-connection> (<object>)
  constant slot access-debugger-connection :: <debugger-connection>,
    required-init-keyword: debugger-connection:;
  constant slot access-connection-description :: <string>,
    init-value: "No description available",
    init-keyword: description:;
  slot connection-process :: <NUB>,
    init-keyword: process:;
end class;


///// START-APPLICATION-ON-CONNECTION
//    This function is called to initialize an instance of 
//    <application-access-path> and calls the server function to create 
//    the running process. If the access connection is local, then the 
//    server returns a packaged process descriptor (a <NUB>) which is 
//    saved in the access connection.

define open generic start-application-on-connection
  (conn :: <access-connection>,
   command :: <string>, 
   arguments :: <string>,
   symbol-file-directories :: <sequence>,
   working-directory :: false-or(<string>),
   library-search-paths :: <sequence>,
   #key) => ();


///// ATTACH-APPLICATION-ON-CONNECTION
//    This function is called to initialize an instance of 
//    <process-access-path> and calls the server function to attach to
//    the running process. If the access connection is local, then the 
//    server returns a packaged process descriptor (a <NUB>) which is 
//    saved in the access connection.

define open generic attach-application-on-connection
  (conn :: <access-connection>,
   process :: <remote-process>,
   symbol-file-directories :: <sequence>,
   system-info :: <string>) => ();


///// CLOSE-REMOTE-DEBUGGER-CONNECTION
//    Declares that a remote debugger connection is not going to be used
//    again within the lifetime of the calling program.

define method close-remote-debugger-connection
   (connection :: <remote-debugger-connection>) => ()
  remove!(*open-debugger-connections*, connection);
  connection.connection-open? := #f;
end method;
