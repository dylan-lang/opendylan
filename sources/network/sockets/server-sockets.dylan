Module:       sockets-internals
Author:       Toby
Synopsis:     Server sockets -- protocol independent classes
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignorable(default-element-type-setter);

define open abstract primary class
    <server-socket> (<abstract-socket>)
  slot socket-descriptor :: false-or(<accessor-socket-descriptor>);
  slot local-host :: <internet-address>;
  slot local-port :: <integer>;
end class;

define method make 
    (class == <server-socket>, #rest initargs,
     #key protocol :: type-union(<string>, <symbol>) = #"TCP")
 => (server :: <server-socket>)
  apply(make, server-class-for-protocol(as(<symbol>, protocol)), initargs)
end method make;

define open generic server-class-for-protocol
  (protocol :: <symbol>) => (server-class :: subclass(<server-socket>));

define method server-class-for-protocol
  (protocol :: <symbol>) => (server-class :: subclass(<server-socket>))
  error("unrecognized server socket protocol: %s", protocol);
end method;

define open generic accept (server-socket :: <server-socket>, #rest args, #key, #all-keys)
   => (result :: <socket>);

// Worry about time out for accept, retrying etc.

define method accept 
    (server-socket :: <server-socket>, #rest args, #key element-type = #f, #all-keys)
 => (connected-socket :: <socket>);
  let manager = current-socket-manager();
  let descriptor = accessor-accept(server-socket);
  with-lock (socket-manager-lock(manager))
    apply(make,
	  client-class-for-server(server-socket), 
	  descriptor: descriptor,
	  element-type: element-type | server-socket.default-element-type,
	  args)
  end with-lock;
end method;

define open generic client-class-for-server
    (server-socket :: <server-socket>)
=> (class :: subclass(<socket>));

define method close (the-server :: <server-socket>, #rest keys, #key) => ()
  let manager = current-socket-manager();
  with-lock (socket-manager-lock(manager))
    if (socket-open?(the-server))
      // Call next-method first so that socket-open? will still be true
      // for the next methods.
      apply(next-method, the-server, already-unregistered?: #f, keys);
      accessor-close-socket(the-server.socket-descriptor);
      the-server.socket-descriptor := #f;
    end if;
  end with-lock;
end method;


define macro with-server-socket
  { with-server-socket (?server:name,
			#rest ?keys:expression)
      ?body:body
    end }
  => { invoke-with-server-socket(<server-socket>,
                                 method(?server) ?body end method,
                                 ?keys) }

  { with-server-socket (?server:name \:: ?class:expression,
			#rest ?keys:expression)
      ?body:body
    end }
  => { invoke-with-server-socket(?class,
                                 method(?server) ?body end method,
                                 ?keys) }
end macro;

define method invoke-with-server-socket
    (class :: subclass(<server-socket>), body :: <function>, #rest keys, #key, #all-keys)
 => ()
  let server = #f;
  block ()
    server := apply(make, class, keys);
    let server :: class = server;
    body(server);
  cleanup
    if (server & socket-open?(server))
      close(server)
    end if;
  end block;
end method;

// macro start-server

define macro start-server
  { start-server (?server-var:name = ?socket-server-instance:expression,
		  ?socket-var:name, #rest ?keys:expression)
      ?body:body
    end }
  => { invoke-start-server(?socket-server-instance,
                           method (?socket-server-instance, ?socket-var) ?body end method,
                           ?keys) }
  { start-server (?socket-server-instance:expression,
		  ?socket-var:name, #rest ?keys:expression)
      ?body:body
    end }
  => { invoke-start-server(?socket-server-instance,
                           method (?socket-var) ?body end method,
                           ?keys) }
end macro start-server;

define method invoke-start-server
    (server-socket :: <server-socket>, body :: <function>, #rest keys, #key, #all-keys)
 => ()
  select (function-arguments(body))
    1 => while(#t)
           let reply-socket = apply(accept, server-socket, keys);
           body(reply-socket);
         end while;
    2 => while(#t)
           let reply-socket = apply(accept, server-socket, keys);
           body(server-socket, reply-socket);
         end while;
    otherwise => error("unknown body function for start-server");
  end select;
end method;


