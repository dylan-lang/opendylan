Module:       sockets-tests-client
Author:       Jason Trenouth
Synopsis:     UDP Daytime Client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method udp-daytime-client () => ();
  start-sockets();
  with-socket (client-socket, protocol: #"udp", host: $local-host,  port: 13)
    write-line(client-socket, "Whats the time Mr Wolf?");
    test-daytime(client-socket);
  end with-socket;
end method;

register-client("UDP Daytime", udp-daytime-client);


