Module:    corba-server-implementation
Author:    Keith Dennison
Synopsis:  Implementation of CORBA server for ChatRoom and ChatServer interfaces
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <ChatServer-i> (<ChatServer-servant>)
  constant slot ChatServer-i-ior-file :: <string> = "c:\\temp\\chat.ior";
  constant slot ChatServer-i-server :: <chat-server>, required-init-keyword: server:;
end class;

define sealed domain make (singleton(<ChatServer-i>));
define sealed domain initialize (<ChatServer-i>);

define variable *ChatServer* :: false-or(<ChatServer-i>) = #f;

define method make (class == <ChatServer-i>, #key)
 => (servant :: <ChatServer-i>)
  unless (*ChatServer*)
    *ChatServer* := next-method();
  end unless;
  *ChatServer*;
end method;

define method initialize (servant :: <ChatServer-i>, #key)
  next-method();
  let orb = CORBA/orb-init(make(CORBA/<arg-list>), "Functional Developer ORB");
  let root-poa = CORBA/orb/resolve-initial-references(orb, "RootPOA");
  let reference = PortableServer/POA/servant-to-reference(root-poa, servant);
  CORBA/orb/object-to-file(orb, servant.ChatServer-i-ior-file, reference);
  let poa-manager = PortableServer/POA/the-poamanager(root-poa);
  PortableServer/POAManager/activate(poa-manager);
end method;

define sealed method ChatServer/rooms (servant :: <ChatServer-i>)
 => (rooms :: limited(CORBA/<sequence>, of: <ChatRoom>))
  let server = servant.ChatServer-i-server;
  let rooms = chat-server-rooms(server);
  map-as(limited(CORBA/<sequence>, of: <ChatRoom>),
         chat-room-CORBA-reference,
         rooms);
end method;

define sealed method ChatServer/CreateChatRoom (servant :: <ChatServer-i>, name :: CORBA/<string>)
 => ()
  let server = servant.ChatServer-i-server;
  let room = chat-server-create-room(server, name);
  make(<ChatRoom-i>, room: room);
end method;

define method start-ChatServer (server :: <chat-server>)
 => (server :: <ChatServer-i>)
  make(<ChatServer-i>, server: server);
end method;

define sealed class <ChatRoom-i> (<ChatRoom-servant>)
  constant slot ChatRoom-i-room :: <chat-room>, required-init-keyword: room:;
  slot ChatRoom-i-reference :: <ChatRoom> = make-nil(<ChatRoom>);
end class;

define sealed domain make (singleton(<ChatRoom-i>));
define sealed domain initialize (<ChatRoom-i>);

define method initialize (servant :: <ChatRoom-i>, #key)
  next-method();
  let orb = CORBA/orb-init(make(CORBA/<arg-list>), "Functional Developer ORB");
  let root-poa = CORBA/orb/resolve-initial-references(orb, "RootPOA");
  let reference = PortableServer/POA/servant-to-reference(root-poa, servant);
  servant.ChatRoom-i-reference := as(<ChatRoom>, reference);
end method;

define method start-ChatRoom (room :: <chat-room>)
 => (room :: <ChatRoom>)
  let servant = make(<ChatRoom-i>, room: room);
  servant.ChatRoom-i-reference;
end method;

define method ChatRoom/name (servant :: <ChatRoom-i>)
 => (result :: CORBA/<string>)
  let room = servant.ChatRoom-i-room;
  chat-room-name(room);
end method;

define method ChatRoom/clients (servant :: <ChatRoom-i>)
 => (result :: limited(CORBA/<sequence>, of: <ChatClientCallback>))
  let room = servant.ChatRoom-i-room;
  map-as(limited(CORBA/<sequence>, of: <ChatClientCallback>),
         identity,
         chat-room-users(room));
end method;

define method ChatRoom/SendMessage (servant :: <ChatRoom-i>, message :: CORBA/<string>)
 => ()
  let room = servant.ChatRoom-i-room;
  chat-room-send-message(room, message);
end method;

define method ChatRoom/RegisterClient (servant :: <ChatRoom-i>, client :: <ChatClientCallback>, name :: CORBA/<string>)
 => ()
  let room = servant.ChatRoom-i-room;
  let user = as(<chat-user>, client);
  chat-room-add-user(room, user, name);
end method;

define method ChatRoom/RemoveClient (servant :: <ChatRoom-i>, client :: <ChatClientCallback>, name :: CORBA/<string>)
 => ()
  let room = servant.ChatRoom-i-room;
  let user = as(<chat-user>, client);
  chat-room-remove-user(room, user, name);
end method;

