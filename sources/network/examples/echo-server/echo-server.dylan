Module:       echo-server
Author:       Toby
Synopsis:     Simple echo server example sockets code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method echo-server () => ();
  start-sockets();
  let the-server = make(<TCP-server-socket>, port: 7);
  block ()
    while (#t)
      let reply-socket = accept(the-server);
      make(<thread>,
	   function:
	     method ()
	       block ()
		 format-out("Responding to client at %s port: %d\n",
			    reply-socket.remote-host.host-name, 
			    reply-socket.remote-port);      

		 let stuff-to-echo = 
		   read-line(reply-socket, on-end-of-stream: #"eoi");
		 until (stuff-to-echo == #"eoi")
                   format-out("Echoing: %s\n", stuff-to-echo);
		   write-line(reply-socket, stuff-to-echo);
		   stuff-to-echo :=
		     read-line(reply-socket, on-end-of-stream: #"eoi");
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
	     end method);
    end while;
  cleanup
    close(the-server);
  end block;
end method;

echo-server();


