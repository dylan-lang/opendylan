Module: orb-connections
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <connection-manager> (<object>)
//  constant slot connection-manager-orb :: corba/<orb>, required-init-keyword: orb:;
  constant slot connection-manager-lock :: <lock> = make(<recursive-lock>);
  constant slot connection-manager-connections :: <table> = make(<table>);
  constant slot connection-manager-reclaimer-sleep :: <integer> = 300; // NB seconds
  slot connection-manager-reclaimer-thread :: false-or(<thread>) = #f;
  slot connection-manager-sender-thread :: false-or(<thread>) = #f;
  constant slot connection-manager-sender-mailbox :: <mailbox> = make(<mailbox>);
end class;

define sealed domain make (subclass(<connection-manager>));
define sealed domain initialize (<connection-manager>);

define class <connection> (<object>)
  constant slot connection-manager :: <connection-manager>, required-init-keyword: manager:;
  constant slot connection-stream :: <stream>, required-init-keyword: stream:;
  constant slot connection-requests :: <table> = make(<table>), init-keyword: requests:;
  constant slot connection-lock :: <lock> = make(<lock>);
  slot connection-message-id :: <integer> = 0, init-keyword: message-id:;
  slot connection-busy? :: <boolean> = #t, init-keyword: busy?:;
  constant slot connection-host :: <string>, init-keyword: host:;
  constant slot connection-port :: <integer>, init-keyword: port:;
end class;

define sealed domain make (subclass(<connection>));
define sealed domain initialize (<connection>);

define method connection-thread-setter (thread :: <thread>, connection :: <connection>)
 => (thread :: <thread>)
  thread // ---*** may want to store connection-thread in a slot for later but just discard for now
end method;

define macro with-connection-manager
  { with-connection-manager (?manager:expression) ?body:body end }
    => 
    { with-lock (connection-manager-lock(?manager)) ?body end }
end macro;

define macro with-each-connection
  { with-each-connection (?connection:name = ?manager:expression) ?body:body end }
    => 
    { do-connections(?manager, method (?connection) ?body end method) }
end macro;

define orb-arg-processor
  syntax: "-ORBcomms",
  value?: #t,
  callback: method (orb :: corba/<orb>, value :: <string>)
	      orb-connection-manager(orb) :=
		make(select (value by \=)
		       "sockets" => <socket-connection-manager>;
		       "in-memory" => <in-memory-connection-manager>;
		     end select,
		     orb: orb)
	    end method,
  initializer: method (orb :: corba/<orb>)
		 orb-connection-manager(orb) :=
		   make(<socket-connection-manager>, orb: orb);
	       end method
end orb-arg-processor;

define method note-result-invalid (request :: <request>)
  request-result(request) :=
    make(corba/<inv-objref>, completed: #"completed-no", minor: 1);
  note-request-status-changed(request, #"system-exception");
end method;

define method note-comm-failure (request :: <request>)
  request-result(request) :=
    make(corba/<comm-failure>, completed: #"completed-maybe", minor: 1);
  note-request-status-changed(request, #"system-exception");
end method;

define method queue-request (manager :: <connection-manager>, request :: <request>)
  block ()
    let ior = corba/object/ior(request-object(request));
    let profile-body = get-iiop-profile(ior);
    let port = iiop/ProfileBody-1-0/port(profile-body);
    let host = iiop/ProfileBody-1-0/host(profile-body);
    let connection = lookup-connection(manager, host, port);
    ensure-reclaimer-started(manager);
    request-connection(request) := connection;
    with-marshalling-stream (stream, inner-stream: connection-stream(connection))
      marshall-request(request, stream); // ---*** handle and resignal marshalling exceptions?
    end with-marshalling-stream;
    debug-out(#"connection", "Passing request to sender");
    push(connection-manager-sender-mailbox(manager), request);
  exception (condition :: connection-manager-error-class(manager)) // NB in case LOOKUP-CONNECTION fails
    debug-out(#"connection", "Error establishing connection: %=", condition);
    note-result-invalid(request);
  end block;
end method;

define method make-connection-id (manager :: <connection-manager>, host :: <string>, port :: <integer>)
 => (id)
  as(<symbol>, format-to-string("%s:%d", host, port));
end method;

define method lookup-connection (manager :: <connection-manager>, host :: <string>, port :: <integer>)
  with-connection-manager (manager)
    let connections :: <table> = connection-manager-connections(manager);
    let id = make-connection-id(manager, host, port);
    let connection = element(connections, id, default: #f);
    connection
      | begin
	  let new-connection = open-connection(manager, host, port);
	  element(connections, id) := new-connection;
	  new-connection
	end;
  end with-connection-manager;
end method;

define method open-connection (manager :: <connection-manager>, host :: <string>, port :: <integer>)
  let connection = make(<connection>,
			manager: manager,
			stream: make(connection-manager-stream-class(manager),
				     host: host,
				     port: port, 
				     force-output-before-read?: #f,
				     element-type: <byte>),
			host: host,
			port: port);
  debug-out(#"connection", "Opened a connection to %= (%=)", host, port);
  ensure-sender-started(manager);
  ensure-receiver-started(manager, connection);
  connection;
end method;

define method do-connections (manager :: <connection-manager>, function :: <function>)
  with-connection-manager (manager)
    for (connection in connection-manager-connections(manager))
      function(connection);
    end for;
  end;
end method;

// NB assumes callers lock manager
define method remove-connection (manager :: <connection-manager>, connection :: <connection>)
  remove-key!(connection-manager-connections(manager),
	      make-connection-id(manager,
				 connection-host(connection),
				 connection-port(connection)));
end method;

define method note-connection-closed
    (manager :: <connection-manager>, connection :: <connection>,
     #key close? :: <boolean> = #f, orderly? :: <boolean> = #f)
  with-connection-manager (manager)
    remove-connection(manager, connection);
    if (orderly?)
      // Reissue requests
      for (request in connection-requests(connection))
	queue-request(manager, request);
      end for;
    else
      // Note failure
      for (request in connection-requests(connection))
	note-comm-failure(request);
      end for;
    end if;
  end with-connection-manager;
end method;

/// CLOSE-CONNECTION will cause an error to be signalled
/// in RECEIVE-REPLY (unless we're already doing that)
/// and hence NOTE-CONNECTION-CLOSED will get called from there.

define method close-connection (manager :: <connection-manager>, connection :: <connection>)
  let stream = connection-stream(connection);
  with-lock (stream-lock(stream))
    close(stream, abort?: #t);	
  end with-lock;
  debug-out(#"connection", "Closed connection to %= (%=)", connection-host(connection), connection-port(connection));
end method;

