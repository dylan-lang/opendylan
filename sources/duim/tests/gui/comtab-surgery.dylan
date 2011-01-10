Module:    duim-gui-test-suite
Author:    Scott McKay
Synopsis:  DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Surgery test frame

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

define command-table *file-command-table* (*global-command-table*)
  menu-item "Add"    = frame-add-items;
  menu-item "Remove" = frame-remove-items;
  separator;
  menu-item "Exit"   = exit-frame;
end command-table *file-command-table*;


define method frame-abort  (frame :: <frame>) end;
define method frame-resume (frame :: <frame>) end;

define command-table *proceed-command-table* (*global-command-table*)
  menu-item "Abort"  = frame-abort;
  menu-item "Resume" = frame-resume;
end command-table *proceed-command-table*;


define command-table *surgery-command-table* (*global-command-table*)
  menu-item "File"    = *file-command-table*;
  menu-item "Proceed" = *proceed-command-table*;
end command-table *test-command-table*;


define frame <surgery-frame> (<simple-frame>)
  slot %proceed-buttons = #f;
  command-table (frame) *surgery-command-table*;
end frame <surgery-frame>;


/// Adding and removing proceed items

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


install-test(<surgery-frame>, "Dynamic Command Tables");
