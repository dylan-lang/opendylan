Module:    chat-client-gui
Author:    Keith Dennison
Synopsis:  DUIM user interface for the Chat Client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define frame <chat-client-frame> (<simple-frame>)
  slot frame-chat-client :: <chat-client>;

  pane chat-room-list (frame)
    make(<list-box>, items: #(), lines: 5,
         label-key: chat-room-name,
         value-changed-callback: method (gadget) refresh-chat-client-frame(frame) end,
	 activate-callback: chat-join-room);

  pane chat-user-list (frame)
    make(<list-box>, items: #(), lines: 5,
         label-key: chat-user-name);

  pane chat-message-list (frame)
    make(<list-box>, items: #(), lines: 15);

  pane chat-new-message (frame)
    make(<text-field>, label: "Message:",
         value-changed-callback: chat-send-message);

  pane chat-layout (frame)
    vertically ()
      horizontally ()
        frame.chat-room-list;
        frame.chat-user-list;
      end;
      frame.chat-message-list;
      frame.chat-new-message;
    end;

  pane chat-status-bar (frame)
    make(<status-bar>, label: "");

  pane exit-button (frame)
    make(<push-button>, label: "Exit",
         command: chat-exit,
         activate-callback: chat-exit);

  pane connect-button (frame)
    make(<push-button>, label: "Connect",
         command: chat-connect,
         activate-callback: chat-connect);

  pane disconnect-button (frame)
    make(<push-button>, label: "Disconnect",
         command: chat-disconnect,
         activate-callback: chat-disconnect);

  pane change-name-button (frame)
    make(<push-button>, label: "Change Name",
         command: chat-change-name,
         activate-callback: chat-change-name);

  pane join-room-button (frame)
    make(<push-button>, label: "Join Room",
         command: chat-join-room,
         activate-callback: chat-join-room);

  pane chat-tool-bar (frame)
    make(<tool-bar>, child: horizontally ()
	                      frame.exit-button;
                              frame.connect-button;
                              frame.disconnect-button;
                              frame.change-name-button;
                              frame.join-room-button;
                            end);

  layout (frame)        frame.chat-layout;
  status-bar (frame)    frame.chat-status-bar;
  tool-bar (frame)      frame.chat-tool-bar;
  command-table (frame) *chat-client-command-table*;
  keyword title: = "Chat Client";
end frame;

define method initialize (frame :: <chat-client-frame>, #key)
  next-method();
  let name = login-name() | owner-name()  | "unknown";
  frame.frame-chat-client := chat-client-start(name, refresh: curry(refresh-chat-client-frame, frame));
  refresh-chat-client-frame(frame);
end method;

define method refresh-client-message-list (frame :: <chat-client-frame>)
 => ()
 let list-box = frame.chat-message-list;
 let client = frame.frame-chat-client;
 let messages = client.chat-client-messages;
 if (gadget-items(list-box) == messages)
   update-gadget(list-box);
 else
   gadget-items(list-box) := messages;
 end if;
end method;

define method refresh-client-room-list (frame :: <chat-client-frame>)
 => ()
  let list-box = frame.chat-room-list;
  let client = frame.frame-chat-client;
  let rooms = chat-client-rooms(client);
  if (gadget-items(list-box) == rooms)
    update-gadget(list-box);
  else
    gadget-items(list-box) := rooms;
  end if;
end method;

define method refresh-client-user-list (frame :: <chat-client-frame>)
 => ()
  let list-box = frame.chat-user-list;
  let client = frame.frame-chat-client;
  let room = gadget-value(frame.chat-room-list);
  let users = if (room) chat-room-users(room) else #() end;
  if (gadget-items(list-box) == users)
    update-gadget(list-box);
  else
    gadget-items(list-box) := users;
  end if;
end method;

define method refresh-client-status-bar (frame :: <chat-client-frame>)
 => ()
  let client = frame-chat-client(frame);
  let user-name = chat-client-name(client);
  let label = concatenate("User ", user-name);
  let room = chat-client-current-room(client);
  if (room)
    let room-name = chat-room-name(room);
    label := concatenate(label, ", in room ", room-name);
  end if;
  label := concatenate(label, ".");
  let status-bar = chat-status-bar(frame);
  gadget-label(status-bar) := label;
end method;

define method do-refresh-chat-client-frame (frame :: <chat-client-frame>)
 => ()
  block ()
    refresh-client-message-list(frame);
    refresh-client-room-list(frame);
    refresh-client-user-list(frame);
    refresh-client-status-bar(frame);
    let client = frame-chat-client(frame);
    command-enabled?(chat-connect, frame) := ~chat-client-connected?(client);
    command-enabled?(chat-disconnect, frame) := chat-client-connected?(client);
    note-room-selection-change(frame);
  exception (condition :: <error>)
    let message = format-to-string("Error updating display. %s", condition);
    notify-user(message, title: "Chat Client Error", style: #"error", owner: frame);
  end block;
end method;

define method refresh-chat-client-frame (frame :: <chat-client-frame>)
 => ()
  call-in-frame(frame, do-refresh-chat-client-frame, frame);
end method;

define method note-room-selection-change (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  note-room-selection-change(frame);
end method;

define method note-room-selection-change (frame :: <chat-client-frame>)
 => ()
  let list-box = frame.chat-room-list;
  let room = gadget-value(list-box);
  let selection? = (room ~= #f);
  command-enabled?(chat-join-room, frame) := selection?;
end method;

define method prompt-for-name (#key title = "Type new user name", owner)
 => (name :: false-or(<string>))
  let user-name = make(<text-field>,
                       label: "User Name",
                       activate-callback: exit-dialog);
  let frame-name-dialog
    = make(<dialog-frame>,
           title: title,
           owner: owner,
           layout: user-name,
           input-focus: user-name);
  if (start-dialog(frame-name-dialog))
    gadget-value(user-name);
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
        notify-user(message, title: "Chat Client Error", style: #"error");
      end block;
    end method; }
end macro;

define method chat-exit (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-exit(frame);
end method;

define method chat-exit (frame :: <chat-client-frame>)
 => ()
  chat-command-exit(frame);
end method;

define chat-command chat-command-exit (frame :: <chat-client-frame>)
 => ()
  let client = frame-chat-client(frame);
  chat-client-shutdown(client);
  exit-frame(frame);
end chat-command;

define method chat-connect (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-connect(frame);
end method;

define method chat-connect (frame :: <chat-client-frame>)
 => ()
  chat-command-connect(frame);
end method;

define chat-command chat-command-connect (frame :: <chat-client-frame>)
 => ()
  let client = frame-chat-client(frame);
  let filename = choose-file(frame: frame,
			     default: "c:\\temp\\chat.ior",
			     direction: #"input");
  if (filename)
    chat-client-connect(client, filename);
    refresh-chat-client-frame(frame);
  end if;
end chat-command;

define method chat-disconnect (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-disconnect(frame);
end method;

define method chat-disconnect (frame :: <chat-client-frame>)
 => ()
  chat-command-disconnect(frame);
end method;

define chat-command chat-command-disconnect (frame :: <chat-client-frame>)
 => ()
  let client = frame-chat-client(frame);
  chat-client-disconnect(client);
  refresh-chat-client-frame(frame);
end chat-command;

define method chat-change-name (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-change-name(frame);
  refresh-chat-client-frame(frame);
end method;

define method chat-change-name (frame :: <chat-client-frame>)
 => ()
  chat-command-change-name(frame);
end method;

define chat-command chat-command-change-name (frame :: <chat-client-frame>)
 => ()
  let client = frame-chat-client(frame);
  let name = prompt-for-name(owner: frame);
  if (name)
    client.chat-client-name := name;
  end if;
end chat-command;

define method chat-create-room (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-create-room(frame);
end method;

define method chat-create-room (frame :: <chat-client-frame>)
 => ()
  chat-command-create-room(frame);
end method;

define chat-command chat-command-create-room (frame :: <chat-client-frame>)
 => ()
  let name = prompt-for-name(owner: frame, title: "Type new room name");
  if (name)
    let client = frame.frame-chat-client;
    chat-client-create-room(client, name);
    refresh-chat-client-frame(frame);
  end if;
end chat-command;

define method chat-join-room (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  chat-command-join-room(frame);
end method;

define method chat-join-room (frame :: <chat-client-frame>)
 => ()
  chat-command-join-room(frame);
end method;

define chat-command chat-command-join-room (frame :: <chat-client-frame>)
 => ()
  let option-box = chat-room-list(frame);
  let room = gadget-value(option-box);
  let client = frame-chat-client(frame);
  chat-client-join-room(client, room);
  refresh-chat-client-frame(frame);
end chat-command;

define method chat-send-message (gadget :: <gadget>)
 => ()
  chat-command-send-message(gadget);
end method;

define chat-command chat-command-send-message (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  let client = frame-chat-client(frame);
  let message = gadget-value(gadget);
  chat-client-send-message(client, message);
  gadget-value(gadget) := "";
end chat-command;

define method chat-about (frame :: <chat-client-frame>)
 => ()
  notify-user("Version 1.0 [Dylan]", title: "Chat Client", owner: frame)
end method;

define method not-yet-implemented (gadget :: <gadget>)
 => ()
  let frame = sheet-frame(gadget);
  not-yet-implemented(frame);
end method;

define method not-yet-implemented (frame :: <chat-client-frame>)
 => ()
  notify-user("Not yet implemented!", title: "Chat Client", owner: frame)
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

define command-table *server-command-table* (*global-command-table*)
  menu-item "Connect..." = chat-connect,
    documentation: "Connect to a Chat Server.";
  menu-item "Disconnect" = chat-disconnect,
    documentation: "Disconnect from the Chat Server.";
  separator;
  menu-item "Create Room..." = chat-create-room,
    documentation: "Create a new chat room.";
  menu-item "Join Room" = chat-join-room,
    documentation: "Join the selected room.";
end command-table *server-command-table*;

define command-table *help-command-table* (*global-command-table*)
  menu-item "About Chat Client" = chat-about,
    accelerator: make-keyboard-gesture(#"f1"),
    documentation: "Display information about the application.";
end command-table *help-command-table*;

define command-table *chat-client-command-table* (*global-command-table*)
  menu-item "File" = *file-command-table*;
  menu-item "Edit" = *edit-command-table*;
  menu-item "Server" = *server-command-table*;
  menu-item "Help" = *help-command-table*;
end command-table *chat-client-command-table*;

define method frame-start-chat-client () => ()
  let frame = make(<chat-client-frame>);
  start-frame(frame);
end method;

frame-start-chat-client();

