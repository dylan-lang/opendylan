Module:       sockets-tests-server
Author:       Toby Weinberg, Jason Trenouth
Synopsis:     TCP echo server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $tcp-echo-port = 7;

define method tcp-echo-server () => ();
  start-sockets();
  with-server-socket (the-server, port: $tcp-echo-port)
    start-server(the-server, reply-socket)
      make(<thread>,
           function: method ()
                       serve-echo(reply-socket)
                     end method)
    end start-server;
  end with-server-socket;
end method;

register-server("TCP Echo", tcp-echo-server);

define method serve-echo (reply-socket)
  block ()
    let input = read-line(reply-socket, on-end-of-stream: #"eoi");
    format-out("Responding to client at %s port: %d\n",
               reply-socket.remote-host.host-name, 
               reply-socket.remote-port);      
    until (input == #"eoi")
      format-out("Echoing: %s\n", input);
      write-line(reply-socket, input);
      input := read-line(reply-socket, on-end-of-stream: #"eoi");
    end until;
    close(reply-socket);
    format-out("Connection to %s port: %d closed\n",
               reply-socket.remote-host.host-name, 
               reply-socket.remote-port);      
  exception (condition :: <recoverable-socket-condition>)
    close(reply-socket, abort?: #t);
    format-out("Connection to %s port: %d aborted\n",
               reply-socket.remote-host.host-name, 
               reply-socket.remote-port);
  end block;
end method;

