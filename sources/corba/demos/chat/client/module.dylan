Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module chat-client
  create
    <chat-room>,
    <chat-room-sequence>,
    chat-room-name,
    chat-room-users;

  create
    <chat-user>,
    <chat-user-sequence>,
    chat-user-name;

  create
    <chat-client>,
    chat-client-name,
    chat-client-name-setter,
    chat-client-messages,
    chat-client-ui-refresh,
    chat-client-start,
    chat-client-shutdown,
    chat-client-connect,
    chat-client-disconnect,
    chat-client-connected?,
    chat-client-rooms,
    chat-client-create-room,
    chat-client-current-room,
    chat-client-join-room,
    chat-client-leave-room,
    chat-client-send-message;

end module chat-client;

define module chat-client-callback
  use functional-dylan;
  use dylan-orb;
  use chat-skeletons;
  use chat-client;
  export
    <chat-client-callback>,
    start-chat-client-callback-server,
    chat-client-callback-reference;
end module;

define module chat-client-implementation
  use functional-dylan;
  use dylan-orb;
  use streams;
  use chat-stubs;
  use chat-client-callback;
  use chat-client;
end module;

define module chat-client-gui
  use functional-dylan;
  use duim;
  use threads;
  use operating-system;
  use format;
  use chat-client;
end module;

