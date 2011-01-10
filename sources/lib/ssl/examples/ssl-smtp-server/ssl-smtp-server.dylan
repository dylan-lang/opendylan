Module:       ssl-smtp-server
Author:       Toby
Synopsis:     Simple echo server example sockets code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method smtp-server () => ();
  start-sockets();
  let the-server = make(<TCP-server-socket>, port: 1025, ssl?: #t, certificate: "certificate.pem", key: "key.pem", starttls?: #t);
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
		 write-line(reply-socket, "220 foohost ESMTP");
		 let greeting = read-line(reply-socket, on-end-of-stream: #"eoi");
		 format-out("read greeting %s\n", greeting);
		 if (greeting = "EHLO") //startswith
		   write-line(reply-socket, "250-foohost");
		   write-line(reply-socket, "250 STARTTLS");
		 else
		   write-line(reply-socket, "250 nice to meet you!");
		 end;
		 let next = read-line(reply-socket, on-end-of-stream: #"eoi");
		 format-out("next is %s\n", next);
		 if (next = "STARTTLS")
		   write-line(reply-socket, "220 Ready to start TLS");
		   force-output(reply-socket);
		   format-out("calling start-tls\n");
		   reply-socket := start-tls(the-server, reply-socket);
		   format-out("Starting TLS\n");
		 end;

		 let stuff-to-echo = read-line(reply-socket, on-end-of-stream: #"eoi");
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

smtp-server();


