module:     access-path-implementation
synopsis:   Implementation of the <access-path> class
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant $empty-string = "";


///// <ACCESS-PATH-CREATION-ERROR>
//    Signalled if the world turns to housemite-shit while trying to fire up
//    a new access path.

define class <access-path-creation-error> (<error>)
end class;


///// $MAX-SPY-FUNCTION-ARGUMENTS
//    A constant limiting the number of arguments that can be passed to
//    a spy function.

define constant $max-spy-function-arguments = 16;
define constant $max-stepping-locations = 512;


///// <ACCESS-PATH>
//    Documented.

define abstract class <access-path> (<object>)

  slot connection :: <access-connection>;

  constant slot access-path-application-object,
    required-init-keyword: application-object:;

  slot access-path-abstract-handle :: <object>,
    init-value: #f;

  constant slot symbol-file-locations :: <sequence>,
    init-value: #[],
    init-keyword: symbol-file-locations:;

  slot libraries :: <stretchy-vector> = make(<stretchy-vector>);

  slot threads :: <stretchy-vector> = make(<stretchy-vector>);

  slot register-set :: <vector>,
    init-value: #[];

  slot cached-exception-set :: <sequence>,
    init-value: #[];

  slot cached-receivable-first-chance-exceptions? :: <boolean>,
    init-value: #f;

  slot first-chance-exception-set :: <stretchy-vector>
    = make(<stretchy-vector>);

  slot state :: <symbol>,
    init-value: #"unstarted";

  slot cached-remote-value-size :: <integer>;

  slot remote-value-size-known? :: <boolean>,
    init-value: #f;

  slot register-tables-built? :: <boolean>,
    init-value: #f;

  constant slot register-name-to-descriptor :: <string-table> 
    = make(<string-table>);

  constant slot register-code-to-descriptor :: <table>
    = make(<table>);

  constant slot spy-function-argument-vector :: <REMOTE-ARG-ARRAY>
    = make(<REMOTE-ARG-ARRAY>, element-count: $max-spy-function-arguments);

  slot spy-function-argument-remote-vector = #f;

  constant slot stepping-locations-vector :: <REMOTE-ARG-ARRAY>
    = make(<REMOTE-ARG-ARRAY>, element-count: $max-stepping-locations);

  slot stepping-locations-remote-vector = #f;

end class;


///// <APPLICATION-ACCESS-PATH>
//    An access path that was created with the "application:" keyword 
//    (and, optionally, the "arguments:" keyword).

define class <application-access-path> (<access-path>)

  constant slot access-path-application :: <string>,
    required-init-keyword: application:;

  constant slot access-path-arguments :: <string>,
    init-keyword: arguments:,
    init-value: $empty-string;

  constant slot access-path-own-shell? :: <boolean>,
    init-value: #t,
    init-keyword: start-in-own-shell?:;

  constant slot access-path-working-directory :: false-or(<string>),
    init-value: #f,
    init-keyword: working-directory:;

  constant slot access-path-library-search-paths :: <sequence>,
    init-value: #[],
    init-keyword: library-search-paths:;

end class;

// ACCESS-PATH-APPLICATION should return #f for <access-path> objects
// that are not <application-access-path> objects.

define method access-path-application (ap :: <access-path>)
       #f;
end method;


///// <PROCESS-ACCESS-PATH>
//    An access path that was attatched to an already-running process.
//    (This is currently not supported).

define class <process-access-path> (<access-path>)
  constant slot access-path-process :: <remote-process>,
    required-init-keyword: process:;
  constant slot access-path-system-attachment-information :: <string>,
    init-value: "",
    init-keyword: system-attachment-information:;
  inherited slot state,
    init-value: #"running";
end class;

// access-path-process should return #f for other "breeds" of access path

define method access-path-process (ap :: <access-path>)
       #f;
end method;


///// <CORE-FILE-ACCESS-PATH>
//    An access path that was attatched to a dumped core file.
//    (This is currently not supported).

define class <core-file-access-path> (<access-path>)
  constant slot access-path-core-file :: <string>,
    required-init-keyword: core-file:;
  inherited slot state,
    init-value: #"post-mortem";
end class;

// access-path-core-file should return #f for other "breeds" of access path

define method access-path-core-file (ap :: <access-path>)
       #f;
end method;


///// MAKE (<ACCESS-PATH> ...)
//    Creates and returns an <access-path>.

define method make 
  (class == <access-path>,
   #rest keys, #key application = #f,
                    arguments = #f, process = #f, core-file = #f,
                    debugger-connection = *default-local-debugger-connection*,
                    #all-keys) => (access-path)

  // Create the appropriate instance.

  if (application)
    apply (make, <application-access-path>, keys);
  elseif (process)
    apply (make, <process-access-path>, keys);
  elseif (core-file)
    apply (make, <core-file-access-path>, keys);
  else
    // This is not a legal call to make on <access-path>
    error("Bad keyword arguments supplied to MAKE on <access-path>");
  end if;
end method;


define open generic make-access-connection
    (ap :: <access-path>, conn :: <debugger-connection>,
     #key) => (conn :: <access-connection>);

define method make-access-connection
    (ap :: <access-path>, conn :: <local-debugger-connection>,
     #key description = ap.access-path-application)
 => (conn :: <local-access-connection>)
  make(<local-access-connection>,
       debugger-connection: conn,
       description: description)
end method;


///// INITIALIZE
//    Intializes an instance of <application-access-path>, which includes
//    firing up the remote process.

define method initialize 
    (ap :: <application-access-path>, 
     #key debugger-connection = *default-local-debugger-connection*, 
     #all-keys) => ()
  next-method();

  let access-connection =
    make-access-connection(ap, debugger-connection);

  // Fire up the application as a side-effect of making the access path.
  // The debugger nub will be spawned, and will start the application in
  // a "frozen" state. Clients must call restart() on the returned
  // instance to get things moving...

  start-application-on-connection 
    (access-connection,
     ap.access-path-application,
     ap.access-path-arguments,
     ap.symbol-file-locations,
     ap.access-path-working-directory,
     ap.access-path-library-search-paths,
     own-shell?: ap.access-path-own-shell?);

   // Finally, save the generated access connection in the access path
   // instance for future communications.

   ap.connection := access-connection;
end method;


define method initialize 
    (ap :: <process-access-path>, 
     #key debugger-connection = *default-local-debugger-connection*, 
     #all-keys) => ()
  next-method();

  let access-connection =
    make-access-connection
    (ap, debugger-connection,
     description:
       format-to-string 
       ("Attached to running process <PID = %s>",
	ap.access-path-process.remote-process-system-identifier));

  attach-application-on-connection 
    (access-connection,
     ap.access-path-process,
     ap.symbol-file-locations,
     ap.access-path-system-attachment-information);

  ap.connection := access-connection;
end method;


///// TODO: These will need their implementations later on...


define method initialize (ap :: <core-file-access-path>, #key) => ()
  next-method();
end method;



/// Debugging the Debugger is like groping around in the dark.
/// These functions will allow a full report to be generated for
/// everything that ever took place on every thread on both sides
/// of the tether during a debugger session.


define variable *debugging-debugger?* = #f;

define variable *debugger-stream* = #f;

define variable *debugger-stream-count* = 0;

define function make-debugger-stream(file-name :: <byte-string>)

  *debugging-debugger?* := #t;

  *debugger-stream* :=
    make (<file-stream>,
	  locator:   as(<file-locator>,
			format-to-string(concatenate(file-name, ".%d"), *debugger-stream-count*)),
	  direction: #"output");

  *debugger-stream-count* := *debugger-stream-count* + 1;

end function;

// make-debugger-stream("U:\\nosa\\dylan\\admin\\logs\\debugging");

define function close-debugger-stream()

  if (*debugger-stream*)
    close(*debugger-stream*);
  end;
end function;

define function debugger-message
    (string :: <string>, #rest args) => ()

  if (*debugging-debugger?*)
    let string :: <byte-string> = as(<byte-string>, string);
    if (*debugger-stream*)
      apply(format, *debugger-stream*, concatenate("\n### ", string, "\n"), args);
      // force-output(*debugger-stream*);
    else
      // apply(format-out, concatenate("\n### ", string, "\n"), args);
      apply(debug-message, string, args)
    end if;
  end if;

end function;

define c-callable-wrapper debugger-message-wrapper
    of debugger-message
  parameter message           :: <C-string>;
  parameter arg1              :: <TARGET-ADDRESS>;
  parameter arg2              :: <TARGET-ADDRESS>;
  c-name: "debugger_message"
end;

ignore(debugger-message-wrapper);

define function nub-debug-message
    (string :: <string>, #rest args) => ()

  let string :: <byte-string> = as(<byte-string>, string);
  apply(debug-message, string, args)
  // apply(format-out, concatenate("\n### ", string, "\n"), args);

end function;

define c-callable-wrapper nub-debug-message-wrapper
    of nub-debug-message
  parameter message           :: <C-string>;
  parameter arg1              :: <TARGET-ADDRESS>;
  parameter arg2              :: <TARGET-ADDRESS>;
  c-name: "nub_debug_message"
end;

ignore(nub-debug-message-wrapper);

define function debugger-error
    (string :: <string>, #rest args) => ()

  apply(error, as(<byte-string>, string), args)

end function;

define c-callable-wrapper debugger-error-wrapper
    of debugger-error
  parameter message           :: <C-string>;
  parameter arg1              :: <TARGET-ADDRESS>;
  parameter arg2              :: <TARGET-ADDRESS>;
  c-name: "debugger_error"
end;

ignore(debugger-error-wrapper);

