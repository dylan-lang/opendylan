Module: orb-connections
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <socket-connection-manager> (<connection-manager>)
end class;

define method connection-manager-stream-class (manager :: <socket-connection-manager>)
  <TCP-socket>
end method;

define method connection-manager-error-class (manager :: <socket-connection-manager>)
  type-union(<socket-error>, <recoverable-socket-condition>)
end method;

define method receive-connections
    (manager :: <socket-connection-manager>,
     port :: false-or(<integer>),
     initialize-callback :: <function>,
     connection-callback :: <function>)
  let server-socket = make(<TCP-server-socket>,
			   element-type: <byte>,
			   port: port);
  initialize-callback(manager, server-socket);
  block()
    while (#t)
      let reply-socket =
	accept(server-socket, 
	       force-output-before-read?: #f,
	       element-type: <byte>);
      connection-callback(reply-socket);
    end while;
  cleanup
    close(server-socket, abort?: #t);
  exception (condition :: connection-manager-error-class(manager))
    debug-out(#"poa", "Error in socket listener: %=", condition)
  end block;
end method;

define method hostname ()
  host-name($local-host)
end method;
