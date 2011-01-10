Module:    chat-client
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <CallBack-implementation> (<CallBack-servant>)
  constant slot CallBack-implementation-chat-client, required-init-keyword: chat-client:;
end class;

define method CallBack/NewMessage (object :: <CallBack-implementation>, message :: CORBA/<string>)
 => ()
  let client = object.CallBack-implementation-chat-client;
  add!(client.chat-client-messages, message);
end method;

define constant <string-sequence> = limited(corba/<sequence>, of: corba/<string>);

define class <chat-client> (<object>)
  constant slot chat-client-chat-ior-file = "c:\\temp\\chat.ior";

  constant slot chat-client-name = "Keith", init-keyword: name:;
  constant slot chat-client-messages :: <string-sequence> = make(<string-sequence>), init-keyword: messages:;
  slot chat-client-reference :: false-or(<Chat>) = #f;

  slot chat-client-callback-poa = #f;
  slot chat-client-callback-reference :: false-or(<Callback>) = #f;
end class;

define method initialize (client :: <chat-client>, #key)
  next-method();
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  client.chat-client-callback-poa := portableserver/poa/create-poa(root-poa, "CallBack POA", #f, lifespan-policy: #"transient");
  let callback = make(<CallBack-implementation>, chat-client: client);
  let reference = portableserver/poa/servant-to-reference(client.chat-client-callback-poa, callback);
  client.chat-client-callback-reference := as(<Callback>, reference);
  let poa-manager = portableserver/poa/the-poamanager(client.chat-client-callback-poa);
  portableserver/poamanager/activate(poa-manager);

  client.chat-client-reference := as(<Chat>, corba/orb/file-to-object(orb, client.chat-client-chat-ior-file));
end method;

define method chat-client-register (client :: <chat-client>)
 => ()
  Chat/RegisterClient(client.chat-client-reference,
                      client.chat-client-callback-reference,
                      client.chat-client-name);
end method;

define method chat-client-remove (client :: <chat-client>)
 => ()
  Chat/RemoveClient(client.chat-client-reference,
		    client.chat-client-callback-reference,
		    client.chat-client-name);
end method;

define method send-message (client :: <chat-client>, message :: <string>)
 => ()
  let reference = client.chat-client-reference;
  Chat/SendMessage(reference, message);
end method;

define constant $messages = #["Message one",
			      "Message two",
			      "Message three",
			      "Message four",
			      "Message five",
			      "Message six",
			      "Message seven",
			      "Message eight",
			      "Message nine",
			      "Message ten"];

define test chat-test ()
  let client = make(<chat-client>);
  chat-client-register(client);
  for (message in $messages)
    send-message(client, message);
  end for;
  chat-client-remove(client);

  let counters = make(<vector>, size: size($messages), fill: 0);
  for (message in client.chat-client-messages)
    let index = find-key($messages, curry(\=, message));
    counters[index] := counters[index] + 1;
  end for;

  for (i from 0 below size(counters))
    check-equal(format-to-string("Got message %d once in %=", i, counters), counters[i], 1);
  end for;
end test;

define suite chat-test-suite ()
  test chat-test;
end suite;
