Module:    chat-client-implementation
Author:    Keith Dennison
Synopsis:  Implementation of the Chat Client API
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Chat Client API
// ===============
//
// <chat-room>
//   a type, instances of which describe a Chat Room
//
// chat-room-name chat-room => string
// chat-room-users chat-room => chat-user-sequence
//
// <chat-room-sequence>
//   a subtype of <sequence> whose elements are instances of <chat-room>
//
// <chat-user>
//   a type, instances of which describe individual users
//
// chat-user-name chat-user => string
//
// <chat-user-sequence>
//   a subtype of <sequence> whose elements are instances of <chat-user>
//
// <chat-client>
//
// chat-client-ui-refresh chat-client => ()
// chat-client-start name #key refresh => chat-client
// chat-client-shutdown chat-client => ()
// chat-client-connect chat-client filename => ()
// chat-client-disconnect chat-client => ()
// chat-client-connected? chat-client => boolean
// chat-client-rooms chat-client => chat-room-sequence
// chat-client-create-room chat-client string => ()
// chat-client-current-room chat-client => false-or(<chat-room>)
// chat-client-join-room chat-client chat-room => ()
// chat-client-leave-room chat-client => ()
// chat-client-send-message chat-client string => ()


define constant $server-inaccessible = "Server may be down or inaccessible.";

define constant <chat-room> = <ChatRoom>;
define constant <chat-room-sequence> = <sequence>;

define constant <chat-user> = <ChatClientCallback>;
define constant <chat-user-sequence> = <sequence>;

define method chat-room-name (room :: <chat-room>)
 => (name :: <string>)
  block ()
    ChatRoom/name(room);
  exception (condition :: CORBA/<exception>)
    error("Cannot get chat room name. %s", $server-inaccessible);
  end block;
end method;

define method chat-room-users (room :: <chat-room>)
 => (users :: <chat-user-sequence>)
  block ()
    ChatRoom/clients(room);
  exception (condition :: CORBA/<exception>)
    error("Cannot get chat room users. %s", $server-inaccessible);
  end block;
end method;

define method chat-user-name (user :: <chat-user>)
 => (name :: <string>)
  block ()
    ChatClientCallback/name(user);
  exception (condition :: CORBA/<exception>)
    error("Cannot get name for user. User's client may be down or inaccessible.");
  end block;
end method;

define sealed class <chat-client> (<object>)
  slot chat-client-name :: <string>, required-init-keyword: name:;
  constant slot chat-client-messages :: <stretchy-vector> = make(<stretchy-vector>);
  slot chat-server :: false-or(<ChatServer>) = #f;
  slot chat-room :: false-or(<ChatRoom>) = #f;
  slot chat-callback :: <chat-client-callback>;
  constant slot chat-refresh :: <function>, required-init-keyword: refresh:;
end class;

define sealed domain make (singleton(<chat-client>));
define sealed domain initialize (<chat-client>);

define method initialize (client :: <chat-client>, #key)
  next-method();
  client.chat-callback := start-chat-client-callback-server(client);
end method;

define method chat-client-ui-refresh (client :: <chat-client>)
 => ()
  client.chat-refresh();
end method;

define method chat-client-start (name :: <string>, #key refresh :: <function> = method () => () end)
 => (client :: <chat-client>)
  make(<chat-client>, name: name, refresh: refresh);
end method;

define method chat-client-shutdown (client :: <chat-client>)
 => ()
  chat-client-disconnect(client);
end method;

define method chat-client-connect (client :: <chat-client>, filename :: <string>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let reference = CORBA/Orb/file-to-object(orb, filename);
  client.chat-server := as(<ChatServer>, reference);
end method;

define method chat-client-disconnect (client :: <chat-client>)
 => ()
  chat-client-leave-room(client);
  client.chat-server := #f;
end method;

define method chat-client-connected? (client :: <chat-client>)
 => (connected? :: <boolean>)
  client.chat-server ~= #f;
end method;

define method chat-client-rooms (client :: <chat-client>)
 => (rooms :: <chat-room-sequence>)
  block ()
    if (client.chat-server)
        ChatServer/rooms(client.chat-server);
    else
      #();
    end if;
  exception (condition :: CORBA/<exception>)
    error("Cannot get room list. %s", $server-inaccessible);
  end block;
end method;

define method chat-client-create-room (client :: <chat-client>, name :: <string>)
 => ()
  block ()
    if (client.chat-server)
      ChatServer/CreateChatRoom(client.chat-server, name);
    end if;
  exception (condition :: CORBA/<exception>)
    error("Cannot create chat room. %s", $server-inaccessible);
  end block;
end method;

define method chat-client-current-room (client :: <chat-client>)
 => (room :: false-or(<chat-room>))
  client.chat-room;
end method;

define method chat-client-join-room (client :: <chat-client>, room :: <chat-room>)
 => ()
  block ()
    chat-client-leave-room(client);
    client.chat-room := room;
    let callback-reference = chat-client-callback-reference(client.chat-callback);
    ChatRoom/RegisterClient(room, callback-reference, client.chat-client-name);
  exception (condition :: CORBA/<exception>)
    error("Cannot join chat room. %s", $server-inaccessible);
  end block;
end method;

define method chat-client-leave-room (client :: <chat-client>)
 => ()
  block ()
    if (client.chat-room)
      let callback-reference = chat-client-callback-reference(client.chat-callback);
      ChatRoom/RemoveClient(client.chat-room, callback-reference, client.chat-client-name);
    end if;
  exception (condition :: CORBA/<exception>)
    error("Cannot leave room. %s", $server-inaccessible);
  end block;
end method;

define method chat-client-send-message (client :: <chat-client>, message :: <string>)
 => ()
  block ()
    if (client.chat-room)
      let name = client.chat-client-name;
      ChatRoom/SendMessage(client.chat-room, concatenate("[", name, "] ", message));
    end if;
  exception (condition :: CORBA/<exception>)
    error("Cannot send message %=. %s", message, $server-inaccessible);
  end block;
end method;

