Module: orb-connections
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ENSURE-RECEIVER-STARTED

define method ensure-receiver-started (manager :: <connection-manager>, connection :: <connection>)
  connection-thread(connection) :=
    make(<thread>,
	 name: format-to-string("Connection request reply receiver for message sent to %= (%=)",
				connection-host(connection),
				connection-port(connection)),
	 function: method ()
		     with-socket-thread ()
		       receive-reply(manager, connection)
		     end with-socket-thread;
		   end method);
end method;

/// *REQUEST*

define thread variable *request* :: false-or(<request>) = #f;

define method current-request ()
 => (request :: false-or(<request>))
  *request*
end method;

define method reset-current-request ()
 => ()
  *request* := #f;
end method;

define method current-request-setter (request :: <request>)
 => (request :: false-or(<request>))
  *request* := request;
end method;

/// <CONNECTION-CLOSED>

define class <server-closed-connection> (<serious-condition>)
end class;

/// RECEIVE-REPLY

define method receive-reply (manager :: <connection-manager>, connection :: <connection>)
  block (exit)
    while (#t)
      with-marshalling-stream(stream, inner-stream: connection-stream(connection))
	marshalling-stream-output-index(stream) := 0;
	reset-current-request();
	block ()
	  debug-out(#"connection", "Waiting for reply from server");
	  force-input(stream);
	exception (condition :: <end-of-stream-error>)
	  debug-out(#"connection", "End of file on client request stream");
	  close-connection(manager, connection);
	  note-connection-closed(manager, connection, orderly?: #f);
	  exit();
	exception (condition :: connection-manager-error-class(manager))
	  debug-out(#"connection", "Error on client request stream: %=", condition);
	  close-connection(manager, connection);
	  note-connection-closed(manager, connection, orderly?: #f);
	  exit();
	end block;
	block ()
	  connection-busy?(connection) := #t;
	  let giop-header :: giop/<messageheader-1-0>
	    = unmarshall(class-typecode(giop/<messageheader-1-0>), stream);
	  marshalling-stream-little-endian?(stream) := giop/messageheader-1-0/byte-order(giop-header);
	  process-reply(giop/messageheader-1-0/message-type(giop-header),
			manager,
			stream,
			connection,
			giop-header);
	exception (condition :: <server-closed-connection>)
	  debug-out(#"connection", "Close connection received on client.");
	  note-connection-closed(manager, connection, close?: #f, orderly?: #t);
	  exit();
	exception (condition :: <serious-condition>)
	  debug-out(#"connection", "Error during client receive:\n%=", condition);
	  let request = current-request();
	  when (request)
	    request-result(request) := make(corba/<marshal>,
					    minor: 38383, // ---*** magic
					    completed: #"completed-maybe");
	    note-request-status-changed(request, #"system-exception");
	  end when;
	end block;
      end with-marshalling-stream;
    end while;
  end block;
end method;

/// PROCESS-REPLY

define method process-reply (type == 1, // #"reply"
			     manager :: <connection-manager>,
			     marshalling-stream :: <marshalling-stream>,
			     connection :: <connection>,
			     giop-header :: giop/<messageheader-1-0>)
  let reply-header :: giop/<replyheader>
    = unmarshall(class-typecode(giop/<replyheader>), marshalling-stream);
  let message-id = giop/replyheader/request-id(reply-header);
  let request = note-reply-received(connection, giop-header, message-id);
  let status = giop/replyheader/reply-status(reply-header);
  request-stream(request) := marshalling-stream;
  debug-out(#"connection", "Processing request reply");
  process-request-reply(status, request)
end method;

define method process-reply (type == 4, // #"locatereply"
			     manager :: <connection-manager>,
			     marshalling-stream :: <marshalling-stream>,
			     connection :: <connection>,
			     giop-header :: giop/<messageheader-1-0>)
  let locate-reply-header :: giop/<locatereplyheader>
    = unmarshall(class-typecode(giop/<locatereplyheader>), marshalling-stream);
  let message-id = giop/locatereplyheader/request-id(locate-reply-header);
  let request = note-reply-received(connection, giop-header, message-id);
  let status = giop/locatereplyheader/locate-status(locate-reply-header);
  request-stream(request) := marshalling-stream;
  debug-out(#"connection", "Processing locate reply");
  process-locate-reply(status, request);
end method;

define method process-reply (type == 5, // #"closeconnection"
			     manager :: <connection-manager>,
			     marshalling-stream :: <marshalling-stream>,
			     connection :: <connection>,
			     giop-header :: giop/<messageheader-1-0>)
  error(make(<server-closed-connection>));
end method;

define method process-reply (type == 6, // #"messageerror"
			     manager :: <connection-manager>,
			     marshalling-stream :: <marshalling-stream>,
			     connection :: <connection>,
			     giop-header :: giop/<messageheader-1-0>)
  
  // ---*** what to do here? Lisp ORB tries changing protocol from 1.1 for 1.0
  debug-out(#"connection", "Received message error.");
end method;

/// NOTE-REPLY-RECEIVED

define method note-reply-received
    (connection :: <connection>, giop-header :: giop/<messageheader-1-0>, message-id :: <integer>)
  let requests :: <table> = connection-requests(connection);
  with-lock(connection-lock(connection))
    let request = element(requests, message-id, default: #f);
    if (request)
      remove-key!(requests, message-id);
      current-request() := request
    else
      error(make(<orphan-response-error>,
		 message-header: giop-header,
		 message-id: message-id,
		 requests: requests));
    end if;
  end with-lock;
end method;

/// PROCESS-LOCATE-REPLY

define method process-locate-reply (status == #"unknown-object", request :: <request>)
  request-result(request) :=
    make(corba/<inv-objref>, minor: 666, completed: #"completed-no");
  note-request-status-changed(request, #"system-exception");
end method;

define method process-locate-reply (status == #"object-forward", request :: <request>)
  corba/object/ior(request-object(request)) :=
    unmarshall(class-typecode(iop/<ior>), request-stream(request));
  note-request-status-changed(request, #"no-exception");
end method;

define method process-locate-reply (status == #"object-here", request :: <request>)
  note-request-status-changed(request, #"no-exception");
end method;


/// PROCESS-REQUEST-REPLY

define method process-request-reply (status :: corba/<exception-type>, request :: <request>)
  note-request-status-changed(request, status);
end method;

define method process-request-reply (status == #"location-forward", request :: <request>)
  let manager = connection-manager(request-connection(request));
  let stream = request-stream(request);
  let new-ior = unmarshall(class-typecode(iop/<ior>), stream);
  debug-out(#"connection", "Request %= forwarded to %=", request, new-ior);
  corba/object/ior(request-object(request)) := new-ior;
  queue-request(manager, request);
end method;


/// UNMARSHALL-RESPONSE

define method unmarshall-response (request :: <request>)
  let stream = request-stream(request);
  select (request-status(request))
    #"no-exception" => unmarshall-reply(request, stream);
    #"user-exception", #"system-exception" => process-exception(request-result(request), request);
  end select;
end method;


/// UNMARSHALL-REPLY

define method unmarshall-reply (request :: <request>, stream :: <marshalling-stream>)
  block ()
  let result = request-result(request);
    when (result)
      corba/any/value(result) := unmarshall(corba/any/type(result), stream);
    end;
    for (arg in request-out-args(request))
      let nv-argument = corba/namedvalue/argument(arg);
      corba/any/value(nv-argument) := unmarshall(corba/any/type(nv-argument), stream);
    end for;
  exception (condition :: <serious-condition>)
    error(make(corba/<marshal>, minor: 12345, completed: #"completed-maybe"));
  end block;
end method;


/// PROCESS-EXCEPTION

define method process-exception
    (result :: <condition>, request :: <request>)
  error(result)
end method;

define method process-exception
    (result :: corba/<any>, request :: <request>)
  unmarshall-exception(request, request-stream(request));
end method;


/// UNMARSHALL-EXCEPTION

define method unmarshall-exception (request :: <request>, stream :: <marshalling-stream>)
  let condition = #f;
  block ()
    let status = request-status(request);
    let exception-typecode = unmarshall-reply-exception-typecode(stream, status, request);
    if (exception-typecode)
      debug-out(#"connection", "Exception name in reply matches class %=", typecode-native-type(exception-typecode));
      condition := unmarshall(exception-typecode, stream);
    end if;
  exception (condition :: <serious-condition>)
    error(make(corba/<marshal>, minor: 12345, completed: #"completed-maybe"));
  end block;
  if (condition)
    error(condition);
  else
    error(make(corba/<unknown>, minor: 2929, completed: #"completed-maybe"));
  end if;
end method;


/// UNMARSHALL-REPLY-EXCEPTION-TYPECODE

define method unmarshall-reply-exception-typecode
    (stream :: <marshalling-stream>, status :: corba/<exception-type>, request :: <request>)
  let exception-name = unmarshall(corba/$string-typecode, stream);
  debug-out(#"connection", "Exception returned: %=", exception-name);
  let typecodes = compute-possible-exception-typecodes(request, status);
  debug-out(#"connection", "Possible exception matches are %=", typecodes);
  find-element(typecodes, method (tc) typecode-repository-id(tc) = exception-name end method);
end method;

/// COMPUTE-POSSIBLE-EXCEPTIONS

define method compute-possible-exception-typecodes (request :: <request>, status == #"user-exception")
  request-user-exception-typecodes(request)
end method;

define method compute-possible-exception-typecodes (request :: <request>, status == #"system-exception")
  request-system-exception-typecodes(request)
end method;

