Module:    chat-server-gui
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define frame <chat-server-frame> (<simple-frame>)
  slot frame-chat-server :: <chat-server>;

  pane chat-rooms (frame)
    make(<table-control>,
	 items: #(),
	 headings: #["Room", "Users"],
	 generators: vector(chat-room-name, chat-room-number-of-users));

  pane chat-log (frame)
    make(<list-box>, items: #(), lines: 15);

  pane chat-layout (frame)
    vertically ()
      frame.chat-rooms;
      frame.chat-log;
    end;

  pane chat-status-bar (frame)
    make(<status-bar>, label: "");

  pane chat-exit-button (frame)
    make(<push-button>, label: "Exit",
	 activate-callback: chat-exit);

  pane chat-create-room-button (frame)
    make(<push-button>, label: "Create Room",
	 activate-callback: chat-create-room);

  pane chat-delete-room-button (frame)
    make(<push-button>, label: "Delete Room",
         activate-callback: not-yet-implemented);

  pane chat-tool-bar (frame)
    make(<tool-bar>, child: horizontally ()
	                      frame.chat-exit-button;
                              frame.chat-create-room-button;
                              frame.chat-delete-room-button;
	                    end);

  layout (frame)        frame.chat-layout;
  status-bar (frame)    frame.chat-status-bar;
  tool-bar (frame)      frame.chat-tool-bar;
  command-table (frame) *chat-server-command-table*;
  keyword title: = "Chat Server";
end frame;

define method initialize (frame :: <chat-server-frame>, #key)
  next-method();
  frame.frame-chat-server := chat-server-start(refresh: curry(refresh-chat-server-frame, frame));
  refresh-chat-server-frame(frame);
end method;

define method do-refresh-chat-server-frame (frame :: <chat-server-frame>)
 => ()
  let server = frame.frame-chat-server;
  let log = chat-server-log(server);
  let list-box = frame.chat-log;
  if (gadget-items(list-box) == log)
    update-gadget(list-box);
  else
    gadget-items(list-box) := log;
  end if;

  let rooms = chat-server-rooms(server);
  let control-table = frame.chat-rooms;
  if (gadget-items(control-table) == rooms)
    update-gadget(control-table);
  else
    gadget-items(control-table) := rooms;
  end if;
end method;

define method refresh-chat-server-frame (frame :: <chat-server-frame>)
 => ()
  call-in-frame(frame, do-refresh-chat-server-frame, frame);
end method;

define method prompt-for-name (#key title = "Type new room name", owner)
 => (name :: false-or(<string>))
  let room-name = make(<text-field>,
		       label: "Chat Room Name:",
		       activate-callback: exit-dialog);
  let frame-room-name-dialog
     = make(<dialog-frame>,
	    title: title,
	    owner: owner,
	    layout: room-name,
	    input-focus: room-name);
  if (start-dialog(frame-room-name-dialog))
    gadget-value(room-name);
  end if;
end method;

define macro chat-command-definer
  { define chat-command ?:name (?parameters:*)
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
        notify-user(message, title: "Chat Server Error", style: #"error");
      end block;
    end method; }
end macro;

define method chat-exit (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-exit(frame);
end method;

define method chat-exit (frame :: <chat-server-frame>)
 => ()
  chat-command-exit(frame);
end method;

define chat-command chat-command-exit (frame :: <chat-server-frame>)
 => ()
  exit-frame(frame);
end chat-command;

define method chat-create-room (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-create-room(frame);
end method;

define method chat-create-room (frame :: <chat-server-frame>)
 => ()
  chat-command-create-room(frame);
end method;

define chat-command chat-command-create-room (frame :: <chat-server-frame>)
 => ()
  let server = frame.frame-chat-server;
  let name = prompt-for-name(owner: frame);
  if (name)
    chat-server-create-room(server, name);
  end if;
end chat-command;

define method chat-about (frame :: <chat-server-frame>)
 => ()
  notify-user("Version 1.0 [Dylan]", title: "Chat Server", owner: frame)
end method;

define method not-yet-implemented (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  not-yet-implemented(frame);
end method;

define method not-yet-implemented (frame :: <chat-server-frame>)
 => ()
  notify-user("Not yet implemented!", title: "Chat Server", owner: frame)
end method;

define function make-keyboard-gesture (keysym :: <symbol>, #rest modifiers)
 => (gesture :: <keyboard-gesture>)
  make(<keyboard-gesture>, keysym: keysym, modifiers: modifiers)
end function make-keyboard-gesture;

define command-table *file-command-table* (*global-command-table*)
  menu-item "Exit" = chat-exit,
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

define command-table *room-command-table* (*global-command-table*)
  menu-item "Create..." = chat-create-room,
    accelerator: make-keyboard-gesture(#"c", #"control", #"shift"),
    documentation: "Create a new chat room.";
  menu-item "Delete" = not-yet-implemented,
    accelerator: make-keyboard-gesture(#"d", #"control", #"shift"),
    documentation: "Delete a chat room.";
end command-table *room-command-table*;

define command-table *help-command-table* (*global-command-table*)
  menu-item "About Chat Server" = chat-about,
    accelerator: make-keyboard-gesture(#"f1"),
    documentation: "Display information about the application.";
end command-table *help-command-table*;

define command-table *chat-server-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "Room" = *room-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table *chat-server-command-table*;

define method start-chat-server-frame ()
 => ()
  block ()
    let frame = make(<chat-server-frame>);
    start-frame(frame);
  exception (condition :: <error>)
    let message = format-to-string("Cannot start Chat Server. %s", condition);
    notify-user(message, title: "Chat Server Error", style: #"error");
  end block;
end method;

start-chat-server-frame();

