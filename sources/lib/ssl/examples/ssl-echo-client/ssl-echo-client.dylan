Module:       ssl-echo-client
Author:       Toby
Synopsis:     Simple echo client example sockets code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method echo-client () => ();
  start-sockets();
  let client-socket = make(<TCP-socket>, host: "127.0.0.1", port: 4007, ssl?: #t);
  block()
    format-out("Connected to echo server at %s port: %d\n",
	       client-socket.remote-host.host-name,
	       client-socket.remote-port);
    format-out("Type '.' on a line by itself to close the connection\n");
    force-out();
    let input = read-line(*standard-input*);
    until (input = ".")
      write-line(client-socket, input);
      let echoed = read-line(client-socket, on-end-of-stream: #"eoi");
      if (echoed == #"eoi")
	error("server died unexpectedly");
      end if;
      write-line(*standard-output*, echoed);
      force-out();
      input := read-line(*standard-input*, on-end-of-stream: #"eoi");
    end until;
    close(client-socket);
    format-out("Connection closed.  Bye\n");
  exception (condition :: <recoverable-socket-condition>)
    close(client-socket, abort?: #t);
    format-out("Connection aborted.  Bye\n");
  end block;
end method;

echo-client();


