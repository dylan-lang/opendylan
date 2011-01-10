module:     access-path-implementation
synopsis:   Implementation of the <access-path> class
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*

    THREADING ISSUES:

    There are non-trivial thread issues associated with the access-path
    implementation. There is a restriction under Windows that debug events can
    only be listened for on the thread that started up the application. This
    means we may - in time - need some slightly convoluted locking mechanism to
    satisfy this implied serialization constraint.

    For now, I've ignored all this!

*/

define constant $empty-string = "";


///// <ACCESS-PATH>


define abstract class <access-path> (<object>)

       slot connection :: <access-connection>;

       slot access-path-abstract-handle :: <object>,
            init-value: #f;

       slot libraries :: <stretchy-vector>,
            init-function: method () 
                             make (<stretchy-vector>, size: 0) 
                           end method;

       slot threads :: <stretchy-vector>,
            init-function: method ()
                             make (<stretchy-vector>, size: 0)
                           end method;

       slot register-set :: <vector>,
            init-value: #[];

       slot cached-exception-set :: <sequence>,
            init-value: #[];

       slot cached-receivable-first-chance-exceptions? :: <boolean>,
            init-value: #f;

       slot first-chance-exception-set :: <stretchy-vector>,
            init-function: method ()
                             make (<stretchy-vector>, size: 0);
                           end method;

       slot state :: <symbol>,
            init-value: #"unstarted";

end class;


define class <application-access-path> (<access-path>)

       slot access-path-application :: <string>,
            required-init-keyword: application:;

       slot access-path-arguments :: <string>,
            init-keyword: arguments:,
            init-value: $empty-string;

end class;

// access-path-application should return #f for other "breeds" of access-path

define method access-path-application (ap :: <access-path>)
       #f;
end method;


define class <process-access-path> (<access-path>)

       slot access-path-process :: <remote-process>,
            required-init-keyword: process:;

       inherited slot state :: <symbol>,
            init-value: #"running";

end class;

// access-path-process should return #f for other "breeds" of access path

define method access-path-process (ap :: <access-path>)
       #f;
end method;

define class <core-file-access-path> (<access-path>)

       slot access-path-core-file :: <string>,
            required-init-keyword: core-file:;

       inherited slot state :: <symbol>,
            init-value: #"post-mortem";

end class;

// access-path-core-file should return #f for other "breeds" of access path

define method access-path-core-file (ap :: <access-path>)
       #f;
end method;


define method make (class == <access-path>,
                    #rest keys, #key application = #f,
                    arguments = #f, process = #f, core-file = #f,
                    #all-keys)

  // Create the appropriate instance.

  if (application)
    apply (make, <application-access-path>, keys);
  elseif (process)
    apply (make, <process-access-path>, keys);
  elseif (core-file)
    apply (make, <core-file-access-path>, keys);
  else
    // This is not a legal call to make on <access-path>
    error("Pardon me, but you've not supplied required keyword args.");
  end if;
end method;


define method initialize (ap :: <application-access-path>, #key) => ()
  
  next-method();

  // Create the connection to the debugger.
  // HACK: *default-local-debugger-connection* is used, though we really
  //       should be passing the server as an argument to make on
  //       <access-path>

  let debug-connection = make (<access-connection>,
                               machine: *default-local-debugger-connection*);

  // Fire up the application as a side-effect of making the access path.
  // The debugger nub will be spawned, and will start the application in
  // a "frozen" state. Clients must call restart() on the returned
  // instance to get things moving...

  start-application-on-connection (debug-connection,
                                   as (<string>, ap.access-path-application),
                                   ap.access-path-arguments);

  // Finally, save the generated access connection in the access path
  // instance for future communications.

  ap.connection := debug-connection;

end method;


define method initialize (ap :: <process-access-path>, #key) => ()
  next-method();
end method;


define method initialize (ap :: <core-file-access-path>, #key) => ()
  next-method();
end method;
