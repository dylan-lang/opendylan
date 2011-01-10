Module:    chat-server
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <Chat-implementation> (<Chat-servant>)
  constant slot Chat-implementation-clients :: <stretchy-vector> = make(<stretchy-vector>);
end class;

define method Chat/SendMessage (chat :: <Chat-implementation>, message :: CORBA/<string>)
 => ()
  for (client :: <CallBack> in chat.Chat-implementation-clients)
    CallBack/NewMessage(client, message);
  end for;
end method;

define method Chat/RegisterClient (chat :: <Chat-implementation>, client :: <CallBack>, name :: corba/<string>)
 => ()
  add!(chat.Chat-implementation-clients, client);
end method;

define method Chat/RemoveClient (chat :: <Chat-implementation>, client :: <CallBack>, name :: corba/<string>)
 => ()
  remove!(chat.Chat-implementation-clients, client, test: CORBA/Object/is-equivalent);
end method;

define variable *chat* = #f;
define variable *chat-poa* = #f;
define constant $chat-ior-file = "c:\\temp\\chat.ior";

define method start-Chat-server ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  *chat-poa* := portableserver/poa/create-poa(root-poa, "Chat POA", #f, lifespan-policy: #"transient");
  *chat* := make(<chat-implementation>);
  let objectid = portableserver/poa/activate-object(*chat-poa*, *chat*);
  corba/orb/object-to-file(orb, $chat-ior-file, portableserver/poa/servant-to-reference(*chat-poa*, *chat*));
  let poa-manager = portableserver/poa/the-poamanager(*chat-poa*);
  portableserver/poamanager/activate(poa-manager);
//  corba/orb/run(orb);
end method;

register-server(start-chat-server);
