Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Decorators for commands, command tables, menus, etc.

define sealed class <command-decorator> (<command>)
  // The object being decorated and its intentional type
  sealed constant slot decorator-object,
    required-init-keyword: object:;
  sealed constant slot decorator-type,
    required-init-keyword: type:;
  // The label to be used in a menu or menu bar...
  sealed constant slot decorator-label :: false-or(<string>) = #f,
    init-keyword: label:;
  // The label to be used in a tool bar...
  sealed constant slot decorator-image :: false-or(type-union(<string>, <image>)) = #f,
    init-keyword: image:;
  // Other properties
  sealed constant slot decorator-accelerator = $unsupplied,
    init-keyword: accelerator:;
  sealed constant slot decorator-mnemonic = $unsupplied,
    init-keyword: mnemonic:;
  sealed constant slot decorator-documentation :: false-or(<string>) = #f,
    init-keyword: documentation:;
  sealed constant slot decorator-options = #(),
    init-keyword: options:;
  sealed constant slot decorator-resource-id = #f,
    init-keyword: resource-id:;
end class <command-decorator>;


/// Trampolines from decorators to commands

define method execute-command-type
    (decorator :: <command-decorator>, #rest initargs,
     #key client, server, invoker, results-to)
 => (results :: false-or(<command-results>))
  ignore(client, server, invoker, results-to);
  ensure-command-decorator(decorator);
  apply(execute-command-type, decorator-object(decorator), initargs)
end method execute-command-type;

define sealed method execute-command
    (decorator :: <command-decorator>) => (#rest values)
  ensure-command-decorator(decorator);
  execute-command(decorator-object(decorator))
end method execute-command;

define sealed method do-execute-command
    (server, decorator :: <command-decorator>) => (#rest values)
  ensure-command-decorator(decorator);
  do-execute-command(server, decorator-object(decorator))
end method do-execute-command;

define sealed method command-undoable?
    (decorator :: <command-decorator>) => (undoable? :: <boolean>)
  ensure-command-decorator(decorator);
  command-undoable?(decorator-object(decorator))
end method command-undoable?;

define sealed method undo-command
    (decorator :: <command-decorator>) => (#rest values)
  ensure-command-decorator(decorator);
  undo-command(decorator-object(decorator))
end method undo-command;

define sealed method redo-command
    (decorator :: <command-decorator>) => (#rest values)
  ensure-command-decorator(decorator);
  redo-command(decorator-object(decorator))
end method redo-command;

define inline function ensure-command-decorator
    (decorator :: <command-decorator>) => ()
  let type = decorator-type(decorator);
  assert(type == <command> | type == <function>,
	 "Trying to treat a non-command decorator as a command")
end function ensure-command-decorator;
