Module:       commands-internals
Synopsis:     Commands protocols and basic classes
Author:       Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Command protocol

define open abstract class <command> (<object>)
end class <command>;

define constant <command-oid> = type-union(<command>, <function>);

define open generic make-command
    (command-type :: <type>, #rest initargs,
     #key client, server, invoker, results-to, #all-keys)
 => (command :: <command>);

define open generic execute-command-type
    (command-type, #rest initargs,
     #key client, server, invoker, results-to, #all-keys)
 => (#rest values);			//--- should be (results :: false-or(<command-results>))
define open generic execute-command
    (command :: <command-oid>) => (#rest values);
define open generic do-execute-command
    (server, command :: <command-oid>) => (#rest values);

define open generic command-client
    (command :: <command-oid>) => (client);
define open generic command-server
    (command :: <command-oid>) => (server);
define open generic command-invoker
    (command :: <command-oid>) => (invoker);
define open generic command-results-to
    (command :: <command-oid>) => (results-to);

define open generic command-undoable?
    (command :: <command-oid>) => (undoable? :: <boolean>);
define open generic undo-command
    (command :: <command-oid>) => (#rest values);
define open generic redo-command
    (command :: <command-oid>) => (#rest values);


/// Command results

define open abstract class <command-results> (<object>)
end class <command-results>;

define open generic command-results
    (results :: <command-results>) => (#rest values);
define open generic command-results-available?
    (results :: <command-results>) => (available? :: <boolean>);
define open generic wait-for-command-results
    (results :: <command-results>, #key timeout) => (timed-out? :: <boolean>);


/// Default methods for <command>

define method make-command
    (command-type :: subclass(<command>), #rest initargs,
     #key client, server, invoker, results-to)
 => (command :: <command>)
  ignore(client, server, invoker, results-to);
  apply(make, command-type, initargs)
end method make-command;

define method execute-command-type
    (command :: <command>, #rest initargs,
     #key client, server, invoker, results-to)
 => (#rest values);			//--- should be (results :: false-or(<command-results>))
  ignore(initargs, client, server, invoker, results-to);
  execute-command(command)
end method execute-command-type;

define method execute-command-type
    (command-type :: subclass(<command>), #rest initargs,
     #key client, server, invoker, results-to)
 => (#rest values);			//--- should be (results :: false-or(<command-results>))
  ignore(client, server, invoker, results-to);
  let command = apply(make-command, command-type, initargs);
  execute-command(command)
end method execute-command-type;

define method execute-command-type
    (command-type :: <list>, #rest initargs,
     #key client, server, invoker, results-to)
 => (#rest values);			//--- should be (results :: false-or(<command-results>))
  ignore(client, server, invoker, results-to);
  let initargs     = concatenate-as(<vector>, initargs, tail(command-type));
  let command-type = head(command-type);
  let command = apply(make-command, command-type, initargs);
  execute-command(command)
end method execute-command-type;

define method execute-command
    (command :: <command>) => (#rest values)
  // Ask the server to execute the command
  do-execute-command(command-server(command), command)
end method execute-command;


// By default, we have a single-thread model where the client, server,
// invoker, and results-to are all the same.  Concrete subclasses of
// <command> must implement a method for 'command-server'.
define method command-client
    (command :: <command>) => (client)
  command-server(command)
end method command-client;

define method command-invoker
    (command :: <command>) => (invoker)
  command-server(command)
end method command-invoker;

define method command-results-to
    (command :: <command>) => (results-to)
  command-invoker(command)
end method command-results-to;


define method command-undoable?
    (command :: <command>) => (undoable? :: <boolean>)
  #f
end method command-undoable?;


/// Default methods for <function>

// No applicable methods if you try to 'make' a function command type
define sealed domain make-command (subclass(<function>));

define sealed method execute-command-type
    (function :: <function>, #rest initargs,
     #key client, server, invoker, results-to)
 => (#rest values);			//--- should be (results :: false-or(<command-results>))
  ignore(initargs, client, invoker, results-to);
  function(server)
end method execute-command-type;


define sealed method command-client
    (command :: <function>) => (client)
  #f
end method command-client;

define sealed method command-server
    (command :: <function>) => (server)
  #f
end method command-server;

define sealed method command-invoker
    (command :: <function>) => (invoker)
  #f
end method command-invoker;

define sealed method command-results-to
    (command :: <function>) => (results-to)
  #f
end method command-results-to;


define sealed method command-undoable?
    (command :: <function>) => (undoable? :: <boolean>)
  #f
end method command-undoable?;


/// Simple command classes

define open abstract primary class <basic-command> (<command>)
  sealed slot command-client = #f,
    init-keyword: client:;
  sealed constant slot command-server,
    required-init-keyword: server:;
  sealed slot command-invoker = #f,
    init-keyword: invoker:;
  sealed slot command-results-to = #f,
    init-keyword: results-to:;
end class <basic-command>;

define method initialize
    (command :: <basic-command>, #key client, server, invoker, results-to) => ()
  unless (client)     command-client(command)     := server end;
  unless (invoker)    command-invoker(command)    := server end;
  unless (results-to) command-results-to(command) := invoker | server end;
end method initialize;


define open abstract primary class <basic-undoable-command> (<basic-command>)
  sealed constant slot %undo-command :: false-or(<command>) = #f,
    init-keyword: undo-command:;
end class <basic-undoable-command>;

define method command-undoable?
    (command :: <basic-undoable-command>) => (true? :: <boolean>)
  #t
end method command-undoable?;

define method undo-command
    (command :: <basic-undoable-command>) => (#rest values)
  when (command.%undo-command)
    execute-command(command.%undo-command)
  end
end method undo-command;

define method redo-command
    (command :: <basic-undoable-command>) => (#rest values)
  execute-command(command)
end method redo-command;


// A functional command has a function and some arguments, and isn't undoable.
// The first argument to the function is always the server (e.g., DUIM frame).
define sealed class <functional-command> (<basic-command>)
  sealed constant slot command-function,
    required-init-keyword: function:;
  sealed constant slot command-arguments = #[],
    init-keyword: arguments:;
end class <functional-command>;

define sealed domain make (singleton(<functional-command>));
define sealed domain initialize (<functional-command>);

define sealed inline method make
    (class == <command>, #rest initargs, #key, #all-keys)
 => (command :: <functional-command>)
  apply(make, <functional-command>, initargs)
end method make;

define sealed method \=
    (command1 :: <functional-command>, command2 :: <functional-command>) => (true? :: <boolean>)
  command1 == command2			//---*** what other slots should we compare?
  | (  command-function(command1)  = command-function(command2)
     & command-arguments(command1) = command-arguments(command2))
end method \=;

define sealed method do-execute-command
    (server, command :: <functional-command>) => (#rest values)
  // Apply the command function to the server and the arguments
  apply(command-function(command), server, command-arguments(command))
end method do-execute-command;
