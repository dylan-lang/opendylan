Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module chat-server
  create
    <chat-user>,
    chat-user-send-message;

  create
    <chat-room>,
    <chat-room-sequence>,
    chat-room-name,
    chat-room-users,
    chat-room-number-of-users,
    chat-room-send-message,
    chat-room-add-user,
    chat-room-remove-user,
    chat-room-CORBA-reference;

  create
    <chat-server>,
    chat-server-start,
    chat-server-ui-refresh,
    chat-server-log,
    chat-server-rooms,
    chat-server-create-room;

end module chat-server;

define module corba-server
  create
    start-ChatServer,
    start-ChatRoom;
end module;

define module corba-server-implementation
  use functional-dylan;
  use dylan-orb;
  use streams;
  use chat-skeletons;
  use corba-server;
  use chat-server;
end module;

define module chat-server-implementation
  use functional-dylan;
  use dylan-orb;
  use format;
  use chat-stubs;
  use corba-server;
  use chat-server;
end module;

define module chat-server-gui
  use functional-dylan;
  use format;
  use duim;
  use threads;
  use chat-server;
end module;

