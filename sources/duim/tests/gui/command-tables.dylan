Module:       duim-gui-test-suite
Author:       Andy Armstrong
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Clipboard command table

define method frame-undo (frame :: <frame>)
  frame-status-message(frame) := "Undo"
end method frame-undo;

define method frame-cut-selection (frame :: <frame>)
  frame-status-message(frame) := "Cut"
end method frame-cut-selection;

define method frame-copy-selection (frame :: <frame>)
  frame-status-message(frame) := "Copy"
end method frame-copy-selection;

define method frame-paste-from-clipboard (frame :: <frame>)
  frame-status-message(frame) := "Paste"
end method frame-paste-from-clipboard;

define command-table *edit-command-table* (*global-command-table*)
  menu-item "Undo"  = frame-undo,
    documentation: "Undo the last change";
  separator;
  menu-item "Cut"   = frame-cut-selection,        image: "X",
    documentation: "Cut to the clipboard";
  menu-item "Copy"  = frame-copy-selection,       image: "C",
    documentation: "Copy to the clipboard";
  menu-item "Paste" = frame-paste-from-clipboard, image: "P",
    documentation: "Paste from the clipboard";
end command-table *edit-command-table*;


/// File command table

define method frame-new (frame :: <frame>)
  frame-status-message(frame) := "New"
end method frame-new;

define command-table *exit-command-table* (*global-command-table*)
  menu-item "Close" = exit-frame,
    documentation: "Close the window";
end command-table *exit-command-table*;

define command-table *file-command-table* (*global-command-table*)
  menu-item "New"  = frame-new,
    documentation: "Make a new document";
  include *exit-command-table*;
end command-table *file-command-table*;


/// Nested command table

define command-table *nested-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
end command-table *nested-command-table*;


/// Surgery command table

define method frame-add-items (frame :: <frame>)
  unless (frame.%proceed-buttons)
    frame.%proceed-buttons
      := add-proceed-items(frame, "One", "Two", "Three");
    command-enabled?(frame-add-items, frame)    := #f;
    command-enabled?(frame-remove-items, frame) := #t
  end
end method frame-add-items;

define method frame-remove-items (frame :: <frame>)
  when (frame.%proceed-buttons)
    remove-proceed-items(frame, frame.%proceed-buttons);
    frame.%proceed-buttons := #f;
    command-enabled?(frame-add-items, frame)    := #t;
    command-enabled?(frame-remove-items, frame) := #f
  end
end method frame-remove-items;

define command-table *surgery-command-table* (*global-command-table*)
  menu-item "Add"    = frame-add-items,
    documentation: "Add some extra menu items to the Proceed menu";
  menu-item "Remove" = frame-remove-items,
    documentation: "Remove the extra menu items from the Proceed menu";
end command-table *surgery-command-table*;


/// Proceed command table

define method add-proceed-items
    (frame :: <frame>, #rest items) => (buttons :: false-or(<menu-box>))
  let menu
    = block (return)
	do-command-menu-gadgets(method (menu) return(menu) end,
				frame, *proceed-command-table*,
				tool-bar?: #f);
	#f
      end;
  when (menu)
    let buttons = make(<vector>, size: size(items));
    for (item in items,
         i :: <integer> from 0)
      let label    = item;			//---*** name of handler here
      let callback = method (sheet) #f end;	//---*** invoke the handler here
      let button   = make(<push-menu-button>,
			  label:   label,
			  activate-callback: callback);
      buttons[i] := button
    end;
    let menu-box = make(<push-menu-box>,
			children: as(<simple-vector>, buttons));
    add-child(menu, menu-box);
    menu-box
  end
end method add-proceed-items;

define method remove-proceed-items
    (frame :: <frame>, buttons :: <menu-box>) => ()
  let menu
    = block (return)
	do-command-menu-gadgets(method (menu) return(menu) end,
				frame, *proceed-command-table*,
				tool-bar?: #f);
	#f
      end;
  when (menu)
    remove-child(menu, buttons)
  end
end method remove-proceed-items;

define method frame-abort  (frame :: <frame>) end;
define method frame-resume (frame :: <frame>) end;

define command-table *proceed-command-table* (*global-command-table*)
  menu-item "Abort"  = frame-abort,
    documentation: "Do an abort!";
  menu-item "Resume" = frame-resume,
    documentation: "Resume!";
end command-table *proceed-command-table*;


/// Class-based commands

define class <show-subclasses> (<basic-command>)
  constant slot %all? = #f, init-keyword: all?:
end class <show-subclasses>;

define class <show-superclasses> (<basic-command>)
  constant slot %all? = #f, init-keyword: all?:
end class <show-superclasses>;

define command-table *class-command-table* (*global-command-table*)
  menu-item "Show Direct Subclasses" = <show-subclasses>,
    image: "v",
    documentation: "Show direct subclasses";
  menu-item "Show All Subclasses" = (<show-subclasses>, all?: #t),
    image: "v!",
    documentation: "Show all subclasses";
  menu-item "Show Direct Superclasses" = <show-superclasses>,
    image: "^",
    documentation: "Show direct superclasses";
  menu-item "Show All Superclasses" = (<show-superclasses>, all?: #t),
    image: "^!",
    documentation: "Show all superclasses";
end command-table *class-command-table*;

define method do-execute-command
    (frame :: <basic-frame>, command :: <show-subclasses>) => (#rest values)
  if (command.%all?)
    frame-status-message(frame) := "Here are all the subclasses"
  else
    frame-status-message(frame) := "Here are the direct subclasses"
  end
end method do-execute-command;

define method do-execute-command
    (frame :: <basic-frame>, command :: <show-superclasses>) => (#rest values)
  let frame = command-server(command);
  if (command.%all?)
    frame-status-message(frame) := "Here are all the superclasses"
  else
    frame-status-message(frame) := "Here are the direct superclasses"
  end
end method do-execute-command;


/// Test command table

define command-table *test-command-table* (*global-command-table*)
  menu-item "File"    = *file-command-table*;
  menu-item "Edit"    = *edit-command-table*;
  menu-item "Class"   = *class-command-table*;
  menu-item "Nested"  = *nested-command-table*;
  menu-item "Surgery" = *surgery-command-table*;
  menu-item "Proceed" = *proceed-command-table*;
end command-table *test-command-table*;


/// Now build a simple-frame

define frame <command-table-test-frame> (<simple-frame>)
  slot %proceed-buttons = #f;
  command-table (frame)
    *test-command-table*;
  tool-bar (frame)
    make-command-tool-bar(frame-manager(frame), frame);
  status-bar (frame)
    make(<status-bar>);
end frame <command-table-test-frame>;

install-test(<command-table-test-frame>, "Command tables");
