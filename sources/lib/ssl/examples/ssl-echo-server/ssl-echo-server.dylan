Module:       ssl-echo-server
Author:       Toby
Synopsis:     Simple echo server example sockets code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method echo-server () => ();
  start-sockets();
  let server-socket = make(<TCP-server-socket>,
                           port: 4007, ssl?: #t,
                           certificate: "certificate.pem",
                           key: "key.pem");
  block ()
    while (#t)
      let reply-socket = accept(server-socket);
      make(<thread>, function: method ()
                                 handle-request(reply-socket)
                               end)
    end;
  cleanup
    close(server-socket);
  end;
end method;

define method handle-request (socket)
  block ()
    format-out("Responding to client at %s port: %d\n",
               socket.remote-host.host-name,
               socket.remote-port);
    let input = read-line(socket, on-end-of-stream: #"eoi");
    until (input == #"eoi")
      format-out("Echoing: %s\n", input);
      write-line(socket, input);
      input := read-line(socket, on-end-of-stream: #"eoi");
    end;
    close(socket);
    format-out("Connection to %s port: %d closed\n",
               socket.remote-host.host-name,
               socket.remote-port);
  exception (condition :: <recoverable-socket-condition>)
    close(socket, abort?: #t);
    format-out("Connection to %s port: %d aborted\n",
               socket.remote-host.host-name,
               socket.remote-port);
  end block;
end method;

echo-server();


