Module:       sockets-tests-server
Author:       Jason Trenouth
Synopsis:     UDP echo server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $udp-echo-port = 7;

define method udp-echo-server () => ();
  start-sockets();
  with-server-socket (the-server, protocol: #"udp", port: $udp-echo-port)
    start-server(the-server, reply-socket)
      serve-echo(reply-socket)
    end start-server;
  end with-server-socket;
end method;

register-server("UDP Echo", udp-echo-server);

