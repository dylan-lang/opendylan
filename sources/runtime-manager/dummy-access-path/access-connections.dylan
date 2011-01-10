module:     access-path-implementation
synopsis:   Implementation of debugger connections
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <access-path-creation-error> (<error>)
end class;

///// Debugger connection

// An instance of <debugger-connection> can be seen as an object that has the
// capability of creating debugger nubs.


define abstract class <debugger-connection> (<object>)

       slot server :: <integer>,
            init-keyword: server:;

end class;

define class <local-debugger-connection> (<debugger-connection>)
end class;

define class <remote-debugger-connection> (<debugger-connection>)
end class;


define method make (class == <debugger-connection>, #rest keys)

       apply (make, <local-debugger-connection>, keys);

end method;


define constant *default-local-debugger-connection* =
                   make (<debugger-connection>, server: 0);


///// Access connection

// Instances of <access-connection> relate directly to instances of a debugger
// nub. They contain the information that is necessary to communicate with a
// specific debugger nub from the access path. An access connection can be
// local or remote, and (orthogonally) 32-bit or 64-bit.


define abstract class <access-connection> (<object>)
       
       slot machine :: <debugger-connection>,
            init-keyword: machine:;

//     slot access-lock :: <lock>,
//          init-function: method () make (<lock>) end;

end class;

define abstract class <remote-access-connection> (<access-connection>)

       slot socket :: <integer>,
            init-keyword: socket:;

end class;

define abstract class <local-access-connection> (<access-connection>)

       slot process,
            init-keyword: process:;

end class;

define class <remote-access-connection-32> (<remote-access-connection>)
end class;

define class <remote-access-connection-64> (<remote-access-connection>)
end class;

define class <local-access-connection-32>  (<local-access-connection>)
end class;

define class <local-access-connection-64>  (<local-access-connection>)
end class;


define method make (class == <access-connection>, #rest keys, #key, #all-keys)
       apply (make, <local-access-connection-32>, keys);
end method;


///// start-application-on-connection

// This function is called to initialize an instance of <application-access-path>
// and calls the server function to create the running process. If the
// access connection is local, then the server returns a packaged
// process descriptor (a <NUB>) which is saved in the access connection.


define method start-application-on-connection
    (conn :: <local-access-connection>,
     command :: <string>, arguments :: <string>) => ()
  conn.process := make(<simulation>);
  conn.process.debug-info := generate-runtime-vector();
  link-simulation(conn.process);
  reset-runtime(conn.process);
end method;


define method start-application-on-connection
                            (conn :: <remote-access-connection>,
                             command :: <string>, arguments :: <string>) => ()

end method;

