Module:    chat-server-implementation
Author:    Keith Dennison
Synopsis:  Implementation of the Chat Server API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Chat Server API
// ===============
//
// <chat-user>
//
// chat-user-send-message chat-user string => ()
// chat-user-notify-add-user chat-user chat-user string => ()
// chat-user-notify-remove-user chat-user chat-user string => ()
// chat-user-notify-new-room chat-user chat-room => ()
//
// <chat-room>
//
// chat-room-name chat-room => string
// chat-room-number-of-users chat-room => integer
//
// <chat-server>
//
// chat-server-start #key refresh => chat-server
// chat-server-ui-refresh chat-server => ()
// chat-server-rooms chat-server => chat-room-sequence
// chat-server-create-room chat-server string => ()


define constant $user-inaccessible = "User's client may be down or inaccessible.";

define constant <chat-user> = <ChatClientCallback>;

define method chat-user-send-message (user :: <chat-user>, message :: <string>)
 => ()
  block ()
    ChatClientCallback/NewMessage(user, message);
  exception (CORBA/<exception>)
    error("Cannot send message %=. %s", message, $user-inaccessible);
  end block;
end method;

define method chat-user-notify-add-user (user :: <chat-user>, new-user :: <chat-user>, name :: <string>)
 => ()
  block ()
    ChatClientCallback/RegisterClient(user, new-user, name);
  exception (CORBA/<exception>)
    error("Cannot send notification that %s has joined room. %s", name, $user-inaccessible);
  end block;
end method;

define method chat-user-notify-remove-user (user :: <chat-user>, removed-user :: <chat-user>, name :: <string>)
 => ()
  block ()
    ChatClientCallback/RemoveClient(user, removed-user, name);
  exception (CORBA/<exception>)
    error("Cannot send notification that %s has left room. %s", name, $user-inaccessible);
  end block;
end method;

define method chat-user-notify-new-room (user :: <chat-user>, room :: <chat-room>)
 => ()
  block ()
    ChatClientCallback/NewRoom(user, room.chat-room-CORBA-reference, room.chat-room-name);
  exception (CORBA/<exception>)
    error("Cannot send notification that new room, %s, has been created. %s", room.chat-room-name, $user-inaccessible);
  end block;
end method;

define sealed class <chat-room> (<object>)
  constant slot chat-room-owner :: <chat-server>, required-init-keyword: owner:;
  constant slot chat-room-name :: <string>, required-init-keyword: name:;
  constant slot chat-room-users :: <stretchy-vector> = make(<stretchy-vector>);
  slot chat-room-CORBA-reference :: <ChatRoom> = make-nil(<ChatRoom>);
end class;

define sealed domain make (singleton(<chat-room>));
define sealed domain initialize (<chat-room>);

define method initialize (room :: <chat-room>, #key)
  next-method();
  room.chat-room-CORBA-reference := start-ChatRoom(room);
end method;

define constant <chat-room-sequence> = <sequence>;

define method chat-room-number-of-users (room :: <chat-room>)
 => (n :: <integer>)
  size(room.chat-room-users);
end method;

define method chat-room-send-message (room :: <chat-room>, message :: <string>)
 => ()
  do(rcurry(chat-user-send-message, message), room.chat-room-users);
  log-message(format-to-string("%s: SendMessage %=", room.chat-room-name, message), room);
end method;

define method chat-room-add-user (room :: <chat-room>, user :: <chat-user>, name :: <string>)
  add!(room.chat-room-users, user);
  for-all-users(rcurry(chat-user-notify-add-user, user, name), room);
  log-message(format-to-string("%s: Add user %=", room.chat-room-name, name), room);
end method;

define method chat-room-remove-user (room :: <chat-room>, user :: <chat-user>, name :: <string>)
  remove!(room.chat-room-users, user, test: \=);
  for-all-users(rcurry(chat-user-notify-remove-user, user, name), room);
  log-message(format-to-string("%s: Remove user %=", room.chat-room-name, name), room);
end method;

define sealed class <chat-server> (<object>)
  constant slot chat-server-log :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot chat-server-rooms :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot chat-server-refresh :: <function>, required-init-keyword: refresh:;
end class;

define sealed domain make (singleton(<chat-server>));
define sealed domain initialize (<chat-server>);

define method initialize (server :: <chat-server>, #key)
  next-method();
  start-ChatServer(server);
end method;

define method chat-server-start (#key refresh :: <function> = method () => () end)
 => (server :: <chat-server>)
  make(<chat-server>, refresh: refresh);
end method;

define method chat-server-ui-refresh (server :: <chat-server>)
 => ()
  server.chat-server-refresh();
end method;

define method chat-server-create-room (server :: <chat-server>, name :: <string>)
 => (room :: <chat-room>)
  let room = make(<chat-room>, owner: server, name: name);
  add!(server.chat-server-rooms, room);
  for-all-users(rcurry(chat-user-notify-new-room, room), server);
  log-message(format-to-string("Created new room %=", name), server);
  room;
end method;

define method for-all-users (function :: <function>, room :: <chat-room>)
 => ()
  for-all-users(function, room.chat-room-owner);
end method;

define method for-all-users (function :: <function>, server :: <chat-server>)
 => ()
  let all-users = apply(concatenate, map(chat-room-users, server.chat-server-rooms));
  do(function, all-users);
end method;

define method log-message (message :: <string>, room :: <chat-room>)
 => ()
  log-message(message, room.chat-room-owner);
end method;

define method log-message (message :: <string>, server :: <chat-server>)
 => ()
  add!(server.chat-server-log, message);
  chat-server-ui-refresh(server);
end method;
