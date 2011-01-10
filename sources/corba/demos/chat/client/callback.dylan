Module:    chat-client-callback
Author:    Keith Dennison
Synopsis:  Implementation of the Chat Client callback server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <chat-client-callback> (<ChatClientCallback-servant>)
  constant slot chat-client :: <chat-client>, required-init-keyword: client:;
  slot chat-client-callback-reference :: <ChatClientCallback>;
end class;

define sealed domain make (singleton(<chat-client-callback>));
define sealed domain initialize (<chat-client-callback>);

define method start-chat-client-callback-server (client :: <chat-client>)
 => (callback :: <chat-client-callback>)
  let orb = CORBA/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = CORBA/Orb/resolve-initial-references(orb, "RootPOA");
  let poa = PortableServer/POA/create-poa(root-poa, "Chat Client CallBack POA", #f, lifespan-policy: #"transient");
  let callback = make(<chat-client-callback>, client: client);
  let reference = PortableServer/POA/servant-to-reference(poa, callback);
  callback.chat-client-callback-reference := as(<ChatClientCallback>, reference);
  let poa-manager = PortableServer/POA/the-poamanager(poa);
  PortableServer/poamanager/activate(poa-manager);
  callback;
end method;

define method ChatClientCallback/name (callback :: <chat-client-callback>)
 => (result :: CORBA/<string>)
  let client = callback.chat-client;
  client.chat-client-name;
end method;

define method ChatClientCallback/NewMessage (callback :: <chat-client-callback>, message :: CORBA/<string>)
 => ()
  let client = callback.chat-client;
  add!(client.chat-client-messages, message);
  chat-client-ui-refresh(client);
end method;

define method ChatClientCallback/RegisterClient (callback :: <chat-client-callback>, client :: <ChatClientCallback>, name :: CORBA/<string>)
 => ()
  let client = callback.chat-client;
  chat-client-ui-refresh(client);
end method;

define method ChatClientCallback/RemoveClient (callback :: <chat-client-callback>, client :: <ChatClientCallback>, name :: CORBA/<string>)
 => ()
  let client = callback.chat-client;
  chat-client-ui-refresh(client);
end method;

define method ChatClientCallback/NewRoom (callback :: <chat-client-callback>, room :: <ChatRoom>, name :: CORBA/<string>)
 => ()
  let client = callback.chat-client;
  chat-client-ui-refresh(client);
end method;

