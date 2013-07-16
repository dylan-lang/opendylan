Module:       sockets-tests-client
Author:       Jason Trenouth
Synopsis:     UDP Echo Client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method UDP-echo-client () => ();
  start-sockets();
  with-socket (client-socket, protocol: #"udp", host: "127.0.0.1", port: 7)
    test-echo(client-socket)
  end with-socket;
end method;

register-client("UDP Echo", udp-echo-client);

