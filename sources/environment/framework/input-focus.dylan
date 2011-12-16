Module:    environment-framework
Synopsis:  Environment Framework
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Input focus

define open abstract class <frame-input-focus-mixin> (<frame>)
end class <frame-input-focus-mixin>;

define open generic note-frame-input-focus-changed
    (frame :: <frame-input-focus-mixin>) => ();

define method note-frame-input-focus-changed
    (frame :: <frame-input-focus-mixin>) => ()
  duim-debug-message("Updating availability of focus commands...");
  update-command-availability(frame, <frame-focus-command>)
end method note-frame-input-focus-changed;

define method handle-event
    (frame :: <frame-input-focus-mixin>,
     event :: <frame-input-focus-changed-event>)
 => ()
  next-method();
  let sheet = frame-input-focus(frame);
  note-frame-input-focus-changed(frame)
end method handle-event;


/// Focus commands

define open abstract class <frame-focus-command> (<basic-command>)
  sealed constant slot command-sheet :: <sheet>,
    required-init-keyword: sheet:;
end class <frame-focus-command>;

define method make-command
    (command-type :: subclass(<frame-focus-command>),
     #rest initargs,
     #key client, server, invoker, results-to)
 => (command :: <frame-focus-command>)
  ignore(client, invoker, results-to);
  let frame :: <frame> = server;
  let focus = frame-input-focus(frame);
  //---*** Maybe make the server be the sheet?
  apply(make, command-type, 
	sheet: focus,
	initargs)
end method make-command;

define method do-execute-command
    (frame :: <frame>, command :: <frame-focus-command>) => ()
  let sheet = command.command-sheet;
  execute-command-for-focus(sheet, command)
end method do-execute-command;

define open generic execute-command-for-focus
    (focus :: type-union(<abstract-sheet>, <frame>),
     command :: <frame-focus-command>)
 => ();

define open generic command-available-for-focus?
    (focus :: type-union(<abstract-sheet>, <frame>),
     command :: subclass(<frame-focus-command>))
 => (available? :: <boolean>);

define method command-available?
    (frame :: <frame>, command :: subclass(<frame-focus-command>))
 => (available? :: <boolean>)
  let sheet = frame-input-focus(frame);
  sheet & command-available-for-focus?(sheet, command)
end method command-available?;

define method execute-command-for-focus
    (focus :: <frame>, command :: <frame-focus-command>) => ()
  error("No implementation for execute-command-for-focus for %=", command)
end method execute-command-for-focus;

define method execute-command-for-focus
    (focus :: <sheet>, command :: <frame-focus-command>) => ()
  execute-command-for-focus(sheet-frame(focus), command)
end method execute-command-for-focus;

define method command-available-for-focus?
    (focus :: <frame>, command :: subclass(<frame-focus-command>))
 => (available? :: <boolean>)
  #f
end method command-available-for-focus?;

define method command-available-for-focus?
    (focus :: <sheet>, command :: subclass(<frame-focus-command>))
 => (available? :: <boolean>)
  let frame = sheet-frame(focus);
  frame & command-available-for-focus?(frame, command)
end method command-available-for-focus?;
