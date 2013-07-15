Module:       sockets-tests-client
Author:       Jason Trenouth
Synopsis:     UDP Daytime Client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method udp-daytime-client () => ();
  start-sockets();
  with-socket (client-socket, protocol: #"udp", host: "127.0.0.1",  port: 13)
    write-line(client-socket, "Whats the time Mr Wolf?");
    test-daytime(client-socket);
  end with-socket;
end method;

register-client("UDP Daytime", udp-daytime-client);


