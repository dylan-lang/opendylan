module: sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *port* = #f;
define variable *connected?* = #f;

define method connect-to-application (*executable* :: <string>)
local method receive-string-from-application()
 let *listen-to-application* = #t;
 let the-string = "";
 while (*listen-to-application*)
  let char = as(<character>, read-element(*remote-application*));
   select(char by \=)
    ' ' => *listen-to-application* := #f;
    otherwise =>
     the-string := add(the-string, char);
   end select;
 end while;
 the-string.reverse;
end method receive-string-from-application;

  close-application();

  *remote-application* := make(<tcp-stream>,
			       host: *host*, port: 998, element-type: <byte>);
  send-string("LISPWORKS ");

  *port* := as(<integer>, receive-string-from-application());

  close(*remote-application*);

  *connected?* := #t;

end method connect-to-application;

define method closed-socket-handler()

  if (*connected?*)
    format-out("\nConnecting to Remote Application...\n");
    force-output(*standard-output*);

    *remote-application* := make(<tcp-stream>,
			         host: *host*, port: *port*, element-type: <byte>);
    *remote-application-control* := make(<tcp-stream>,
                                         host: *host*, port: *port*);

    // deferred buffer-handshake setting
    *remote-application*.output-buffer-handshake := tcp-buffer-handshake;
    *remote-application*.initial-handshake := startup-handshake;

    write(*remote-application-control*, "V ");
    force-output(*remote-application-control*);

    write(*remote-application-control*, "V ");
    force-output(*remote-application-control*);

    format-out("\nConnection Successful!\n");
 
    *remote-application*
  else
    format-out("\nClosed Tcp Connection -- rerun application");
  end if;

end method closed-socket-handler;

define method close-application()
 if (*remote-application*)
  close(*remote-application*);
  *remote-application* := #f;
 end if;
 if (*remote-application-control*)
  close(*remote-application-control*);
  *remote-application-control* := #f;
 end if;

 *connected?* := #f;

end method close-application;
