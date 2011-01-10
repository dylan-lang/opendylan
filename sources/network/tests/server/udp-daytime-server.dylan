Module:       sockets-tests-server
Author:       Jason Trenouth
Synopsis:     UDP-based daytime server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $udp-daytime-port = 13;

define method udp-daytime-server () => ();
  start-sockets();
  with-server-socket(my-server, protocol: #"udp", port: $udp-daytime-port)
    start-server(my-server, reply-socket)
      block ()
        let input = read-line(reply-socket);
        let output = human-readable-date-string(current-date());
        write-line(reply-socket, output);
        close(reply-socket);
      exception (non-fatal-condition :: <recoverable-socket-condition>)
        // Close the socket but don't try to force out any unwritten buffers
        // since the connection may not be working properly.
        close (reply-socket, abort?: #t);
      end block;
    end start-server;
  end with-server-socket;
end method;

register-server("UDP Daytime", udp-daytime-server);

