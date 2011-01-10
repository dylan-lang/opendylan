Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Enabling and disabling of commands

// A <command-oid> is something that acts like a command:
//  - a command class, or
//  - a command object, or
//  - a function that acts like a command
define constant <command-oid> = type-union(subclass(<command>), <command>, <function>);


// 'command' can be a <command-oid> or a <command-table>
define open generic command-enabled?
    (command, frame :: <abstract-frame>, #key command-table)
 => (enabled? :: <boolean>);
define open generic command-enabled?-setter
    (enabled? :: <boolean>, command, frame :: <abstract-frame>, #key do-inherited?)
 => (enabled? :: <boolean>);

define method command-enabled?
    (command :: <command-oid>, frame :: <basic-frame>,
     #key command-table = frame-command-table(frame))
 => (enabled? :: <boolean>)
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  ~gethash(disabled-commands, command)
  //--- CLIM used to do this, but do we need to?
  //  & command-accessible?(command-table, command)
end method command-enabled?;

define method command-enabled?-setter
    (enabled? == #t, command :: <command-oid>, frame :: <basic-frame>,
     #key do-inherited? = #f)
 => (enabled? :: <boolean>)
  ignore(do-inherited?);
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  remhash(disabled-commands, command);
  note-command-enabled(frame-manager(frame), frame, command);
  enabled?
end method command-enabled?-setter;

define method command-enabled?-setter
    (enabled? == #f, command :: <command-oid>, frame :: <basic-frame>,
     #key do-inherited? = #f)
 => (enabled? :: <boolean>)
  ignore(do-inherited?);
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  gethash(disabled-commands, command) := #t;
  note-command-disabled(frame-manager(frame), frame, command);
  enabled?
end method command-enabled?-setter;


define method command-enabled?
    (command :: <functional-command>, frame :: <basic-frame>,
     #key command-table = frame-command-table(frame))
 => (enabled? :: <boolean>)
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  let command :: <function> = command-function(command);
  ~gethash(disabled-commands, command)
  //--- CLIM used to do this, but do we need to?
  //  & command-accessible?(command-table, command)
end method command-enabled?;

define method command-enabled?-setter
    (enabled? == #t, command :: <functional-command>, frame :: <basic-frame>,
     #key do-inherited? = #f)
 => (enabled? :: <boolean>)
  ignore(do-inherited?);
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  let command :: <function> = command-function(command);
  remhash(disabled-commands, command);
  note-command-enabled(frame-manager(frame), frame, command);
  enabled?
end method command-enabled?-setter;

define method command-enabled?-setter
    (enabled? == #f, command :: <functional-command>, frame :: <basic-frame>,
     #key do-inherited? = #f)
 => (enabled? :: <boolean>)
  ignore(do-inherited?);
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  let command :: <function> = command-function(command);
  gethash(disabled-commands, command) := #t;
  note-command-disabled(frame-manager(frame), frame, command);
  enabled?
end method command-enabled?-setter;


// 'command-enabled?' and friends also work on command tables...
define method command-enabled?
    (comtab :: <command-table>, frame :: <basic-frame>,
     #key command-table)
 => (enabled? :: <boolean>)
  ignore(command-table);
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  ~gethash(disabled-commands, comtab)
end method command-enabled?;

define method command-enabled?-setter
    (enabled? == #t, comtab :: <command-table>, frame :: <basic-frame>,
     #key do-inherited? = #f)
 => (enabled? :: <boolean>)
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  remhash(disabled-commands, comtab);
  when (do-inherited?)
    do-command-table-commands(method (c, ct)
				ignore(ct);
				command-enabled?(c, frame) := #t
			      end method, comtab)
  end;
  note-command-enabled(frame-manager(frame), frame, comtab);
  enabled?
end method command-enabled?-setter;

define method command-enabled?-setter
    (enabled? == #f, comtab :: <command-table>, frame :: <basic-frame>,
     #key do-inherited? = #f)
 => (enabled? :: <boolean>)
  let disabled-commands :: <object-table> = frame-disabled-commands(frame);
  gethash(disabled-commands, comtab) := #t;
  when (do-inherited?)
    do-command-table-commands(method (c, ct)
				ignore(ct);
				command-enabled?(c, frame) := #f
			      end method, comtab)
  end;
  note-command-disabled(frame-manager(frame), frame, comtab);
  enabled?
end method command-enabled?-setter;


// This is where a command button might get ungrayed
// Note that COMMAND can be a <command>, <function>, or <command-table>
define open generic note-command-enabled
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>, command) => ();

define method note-command-enabled
    (framem :: <frame-manager>, frame :: <frame>, command) => ()
  // Enable all the gadgets in this frame corresponding to the command
  do-command-menu-gadgets
    (method(gadget) gadget-enabled?(gadget) := #t end,
     frame, command)
end method note-command-enabled;


// This is where a command button might get grayed out
// 'command' can be a <command-oid> or a <command-table>
define open generic note-command-disabled
    (framem :: <abstract-frame-manager>, frame :: <abstract-frame>, command) => ();

define method note-command-disabled
    (framem :: <frame-manager>, frame :: <frame>, command) => ()
  // Disable all the gadgets in this frame corresponding to the command
  do-command-menu-gadgets
    (method(gadget) gadget-enabled?(gadget) := #f end,
     frame, command)
end method note-command-disabled;


/// Callbacks on command menus generate these events

/// Command events

define sealed class <command-event> (<frame-event>)
  sealed constant slot event-command :: <command-oid>,
    required-init-keyword: command:;
end class <command-event>;

define sealed domain make (singleton(<command-event>));
define sealed domain initialize (<command-event>);

define method handle-event
    (handler :: <event-handler>, event :: <command-event>) => ()
  let command = event-command(event);
  execute-command-type(command, server: event-frame(event), client: handler)
end method handle-event;

define method distribute-command-event
    (sheet :: <sheet>, command :: <command-oid>) => ()
  let frame = sheet-frame(sheet);
  when (frame)
    distribute-command-event(frame, command)
  end
end method distribute-command-event;

define method distribute-command-event
    (frame :: <frame>, command :: <command-oid>) => ()
  let _port = port(frame);
  when (_port)
    distribute-event(_port, make(<command-event>,
				 frame:   frame,
				 command: command));
    let top-sheet = top-level-sheet(frame);
    when (top-sheet)
      generate-trigger-event(_port, top-sheet)
    end
  end
end method distribute-command-event;


/// Make commands be legal callbacks for gadgets and frames

//--- Should we be worried that the arguments are being ignored?
//--- It means that putting commands in for value callbacks will lose...
define sideways method execute-callback
    (gadget :: <gadget>, command :: <command>, #rest args) => ()
  ignore(args);
  //--- This could copy the command and plug in the new server and client...
  execute-command(command)
end method execute-callback;

define sideways method execute-callback
    (gadget :: <gadget>, command-type :: subclass(<command>), #rest args) => ()
  ignore(args);
  execute-command-type(command-type, server: sheet-frame(gadget), client: gadget)
end method execute-callback;

define sideways method execute-callback
    (gadget :: <gadget>, command-type :: <list>, #rest args) => ()
  ignore(args);
  execute-command-type(command-type, server: sheet-frame(gadget), client: gadget)
end method execute-callback;

define sideways method execute-callback
    (frame :: <frame>, command :: <command>, #rest args) => ()
  ignore(args);
  //--- This could copy the command and plug in the new server and client...
  execute-command(command)
end method execute-callback;

define sideways method execute-callback
    (frame :: <frame>, command-type :: subclass(<command>), #rest args) => ()
  ignore(args);
  execute-command-type(command-type, server: frame, client: frame)
end method execute-callback;

define sideways method execute-callback
    (frame :: <frame>, command-type :: <list>, #rest args) => ()
  ignore(args);
  execute-command-type(command-type, server: frame, client: frame)
end method execute-callback;
