Module:    debugger-server
Author:    Nosa Omo
Synopsis:  DUIM user interface for the Debugger Server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Icons

define variable *bitmaps-initialized?* :: <boolean> = #f;

define macro initialize-icon
  { initialize-icon(?icon:name, ?resource-id:expression) }
    => { let _id   = as(<byte-string>, ?resource-id);
	 let _icon = read-image-as(<win32-icon>, _id, #"small-icon");
	 when (_icon)
	   ?icon := _icon
	 end }
end macro initialize-icon;

define variable $AAA-icon   = #f;

define function initialize-images ()
  unless (*bitmaps-initialized?*)
    initialize-icon($AAA-icon,  "AAA");
    *bitmaps-initialized?* := #t
  end
end function initialize-images;

initialize-images();


// Frame

define frame <debugger-server-frame> (<simple-frame>)
  slot frame-debugger-server :: <NubServer-implementation>;

  pane debugger-nub-list (frame)
    make(<list-box>, items: #(),
         label-key: debugger-nub-description,
         value-changed-callback:
	   method (gadget) refresh-debugger-server-frame(frame) end);

  pane debugger-connection-list (frame)
    make(<list-box>, items: #(),
         label-key: debugger-connection-description);

  pane debugger-server-layout (frame)
    make(<column-splitter>,
	 width: 400,
	 height: 300,
	 ratios: #[2, 1],
	 children: vector(frame.debugger-nub-list,
			  frame.debugger-connection-list));

  pane debugger-server-status-bar (frame)
    make(<status-bar>, label: "");

  pane exit-button (frame)
    make(<push-button>, label: "Exit",
         command: debugger-server-exit,
         activate-callback: debugger-server-exit);

  pane change-password-button (frame)
    make(<push-button>, label: "Change Password",
         command: debugger-change-password,
         activate-callback: debugger-change-password);

  pane end-process-button (frame)
    make(<push-button>, label: "End Process",
	 command: debugger-end-process,
	 activate-callback: debugger-end-process);

  pane debugger-server-tool-bar (frame)
    make(<tool-bar>, child: horizontally ()
	                      frame.exit-button;
                              frame.change-password-button;
	                      frame.end-process-button;
                            end);

  layout (frame)        frame.debugger-server-layout;
  status-bar (frame)    frame.debugger-server-status-bar;
  tool-bar (frame)      frame.debugger-server-tool-bar;
  command-table (frame) *debugger-server-command-table*;
  keyword title: = "Functional Developer Debugger Server";
  keyword icon: = $AAA-icon;
end frame;

define method initialize (frame :: <debugger-server-frame>, #key)
  next-method();
  frame.frame-debugger-server :=
    start-debugger-server(refresh: curry(refresh-debugger-server-frame, frame));
  refresh-debugger-server-frame(frame);
end method;

define method prompt-for-password (#key title = "Type server password", owner)
 => (password :: false-or(<string>))
  let password = make(<text-field>,
		      label: "Server Password",
		      activate-callback: exit-dialog);
  let frame-password-dialog
    = make(<dialog-frame>,
           title: title,
           owner: owner,
           layout: password,
           input-focus: password);
  if (start-dialog(frame-password-dialog))
    gadget-value(password);
  end if;
end method;


define method refresh-debugger-nub-list (frame :: <debugger-server-frame>)
 => ()
  let list-box = frame.debugger-nub-list;
  let server = frame.frame-debugger-server;
  let nubs = nubs(server);
  if (gadget-items(list-box) == nubs)
    update-gadget(list-box);
  else
    gadget-items(list-box) := nubs;
  end if;
end method;

define method refresh-debugger-connection-list (frame :: <debugger-server-frame>)
 => ()
  let list-box = frame.debugger-connection-list;
  let server = frame.frame-debugger-server;
  let connections = connections(server);
  if (gadget-items(list-box) == connections)
    update-gadget(list-box);
  else
    gadget-items(list-box) := connections;
  end if;
end method;

define method refresh-debugger-status-bar (frame :: <debugger-server-frame>)
 => ()
  let server = frame-debugger-server(frame);
  let status-bar = debugger-server-status-bar(frame);
  gadget-label(status-bar) :=
    concatenate("Currently debugging ",
		integer-to-string(server.nubs.size),
		" Application(s) on ",
		integer-to-string(server.connections.size),
		" Machine(s)");
end method;

define method do-refresh-debugger-server-frame (frame :: <debugger-server-frame>)
 => ()
  block ()
    refresh-debugger-nub-list(frame);
    refresh-debugger-connection-list(frame);
    refresh-debugger-status-bar(frame);
    note-process-selection-change(frame);
  exception (condition :: <error>)
    let message = format-to-string("Error updating display. %s", condition);
    notify-user(message, title: "Debugger Server Error", style: #"error", owner: frame);
  end block;
end method;

define method refresh-debugger-server-frame (frame :: <debugger-server-frame>)
 => ()
  call-in-frame(frame, do-refresh-debugger-server-frame, frame);
end method;


define method note-process-selection-change (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  note-process-selection-change(frame);
end method;

define method note-process-selection-change (frame :: <debugger-server-frame>)
 => ()
  let list-box = frame.debugger-nub-list;
  let selection? =
    if (gadget-items(list-box).empty?) #f
    else
      let nub = gadget-value(list-box);
      nub & #t;
    end if;
  command-enabled?(debugger-end-process, frame) := selection?;
end method;


define macro debugger-command-definer
  { define debugger-command ?:name (?parameters:*)
     => (?results:*)
      ?:body
    end }
 =>
  { define method ?name (?parameters)
     => (?results)
      block ()
        ?body
      exception (condition :: <error>)
        let message = format-to-string("%s", condition);
        notify-user(message, title: "Debugger Server Error", style: #"error");
      end block;
    end method; }
end macro;


define method debugger-server-exit (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  debugger-command-exit(frame)
end method;

define method debugger-server-exit (frame :: <debugger-server-frame>)
 => ()
  debugger-command-exit(frame);
end method;

define debugger-command debugger-command-exit (frame :: <debugger-server-frame>)
 => ()
  exit-frame(frame);
end debugger-command;


define method debugger-change-password (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  debugger-command-change-password(frame);
  refresh-debugger-server-frame(frame);
end method;

define method debugger-change-password (frame :: <debugger-server-frame>)
 => ()
  debugger-command-change-password(frame);
end method;

define debugger-command debugger-command-change-password
  (frame :: <debugger-server-frame>)
 => ()
  let server = frame-debugger-server(frame);
  let password = prompt-for-password(owner: frame);
  if (password)
    server.server-password := password;
  end if;
end debugger-command;


define method debugger-end-process (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  debugger-command-end-process(frame);
  refresh-debugger-server-frame(frame);
end method;

define method debugger-end-process (frame :: <debugger-server-frame>)
 => ()
  debugger-command-end-process(frame);
end method;

define debugger-command debugger-command-end-process
  (frame :: <debugger-server-frame>)
 => ()
  let server = frame-debugger-server(frame);
  let option-box = debugger-nub-list(frame);
  let nub :: <remote-debugger-nub> = gadget-value(option-box);

  Rtmgr/NubServer/DestroyNub(server, nub.remote-nub-reference);
end debugger-command;


define method debugger-server-about (frame :: <debugger-server-frame>)
 => ()
  notify-user("Version 2.0 [Dylan]", title: "Debugger Server", owner: frame)
end method;

define method not-yet-implemented (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  not-yet-implemented(frame);
end method;

define method not-yet-implemented (frame :: <debugger-server-frame>)
 => ()
  notify-user("Not yet implemented!", title: "Debugger Server", owner: frame)
end method;

define function make-keyboard-gesture (keysym :: <symbol>, #rest modifiers)
 => (gesture :: <keyboard-gesture>)
  make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
end function make-keyboard-gesture;

define command-table *file-command-table* (*global-command-table*)
  menu-item "Exit" = debugger-server-exit,
    accelerator:   make-keyboard-gesture(#"f4", #"alt"),
    documentation: "Exit the application.";
end command-table *file-command-table*;

define command-table *edit-command-table* (*global-command-table*)
  menu-item "Cut" = not-yet-implemented,
    accelerator:   make-keyboard-gesture(#"x", #"control"),
    documentation: "Cut the selection to the clipboard.";
  menu-item "Copy" = not-yet-implemented,
    accelerator:   make-keyboard-gesture(#"c", #"control"),
    documentation: "Copy the selection to the clipboard.";
  menu-item "Paste" = not-yet-implemented,
    accelerator:   make-keyboard-gesture(#"v", #"control"),
    documentation: "Paste the selection in the clipboard at the current position.";
end command-table *edit-command-table*;

define command-table *server-command-table* (*global-command-table*)
  menu-item "End Process..." = debugger-end-process,
    documentation: "kill debugger process.";
end command-table *server-command-table*;

define command-table *help-command-table* (*global-command-table*)
  menu-item "About Debugger Server" = debugger-server-about,
    accelerator: make-keyboard-gesture(#"f1"),
    documentation: "Display information about the application.";
end command-table *help-command-table*;

define command-table *debugger-server-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "Server" = *server-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table *debugger-server-command-table*;

define method frame-start-debugger-server () => ()
  let frame = make(<debugger-server-frame>);
  start-frame(frame);
end method;

frame-start-debugger-server();


