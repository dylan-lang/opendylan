Module: orb-connections
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ENSURE-SENDER-STARTED

define method ensure-sender-started (manager :: <connection-manager>)
  unless (connection-manager-sender-thread(manager))
    connection-manager-sender-thread(manager) :=
      make(<thread>,
	   function: method () send-request(manager) end method,
	   name: "Connection request sender");
  end unless;
end method;


/// SEND-REQUEST

define method send-request (manager :: <connection-manager>)
  let mailbox = connection-manager-sender-mailbox(manager);
  while (#t)
    let request = pop(mailbox); // NB blocks if empty
    block ()
      note-request-sent(request);
      debug-out(#"connection", "Sending request to server");
      force-output(request-stream(request));
    exception (condition :: <serious-condition>)
      debug-out(#"connection", "Error during client send %=", condition);
      request-result(request) := condition;
      note-request-status-changed(request, #"system-exception");
    end block;
  end while;
end method;


/// NOTE-REQUEST-SENT

define method note-request-sent (request :: <request>)
  let connection = request-connection(request);
  with-lock(connection-lock(connection))
    connection-busy?(connection) := #t;
    unless (request-oneway?(request))
      element(connection-requests(connection), connection-message-id(connection)) := request;
    end unless;
    connection-message-id(connection) := connection-message-id(connection) + 1;
  end with-lock;
end method;


/// MARSHALL-REQUEST

define method marshall-request (request :: <request>, stream :: <marshalling-stream>)
  request-stream(request) := stream;
  marshalling-stream-little-endian?(stream) := architecture-little-endian?();
  marshalling-stream-output-index(stream) := 0;
  let start-of-giop :: dylan/<integer> = marshalling-stream-output-index(stream);
  marshall-message-header(stream);
  marshall-request-header(request, request-connection(request), stream);
  marshall-request-arguments(request, stream);
  marshall-request-context(request, stream);
  set-buffer-size(stream,
		  start-of-giop + 8, ///---*** magic number
		  marshalling-stream-output-index(stream) - start-of-giop - 12); // ---*** magic number
end method;  


/// MARSHALL-MESSAGE-HEADER

define constant $giop-request-messageheader-1-0 = make(giop/<messageheader-1-0>,
						       message-type: 0, // #"request"
						       magic: $giop-magic,
						       giop-version: $giop-version,
						       message-size: 0,
						       byte-order: architecture-little-endian?());

define method marshall-message-header (stream :: <marshalling-stream>)
  marshalling-stream-little-endian?(stream) := giop/messageheader-1-0/byte-order($giop-request-messageheader-1-0);
  marshall(class-typecode(giop/<messageheader-1-0>), $giop-request-messageheader-1-0, stream);
end method;

define constant $empty-service-context = make(IOP/<ServiceContextList>);

/// MARSHALL-REQUEST-HEADER

define method marshall-request-header (request :: <request>, connection :: <connection>, stream :: <marshalling-stream>)
  let giop-request-header =
    make(giop/<requestheader-1-0>,
	 service-context: $empty-service-context,
	 request-id: connection-message-id(connection),
	 response-expected: ~request-oneway?(request),
 	 object-key: iiop/ProfileBody-1-0/object-key(get-iiop-profile(corba/object/ior(request-object(request)))),
	 operation: request-operation-name(request),
	 requesting-principal: operating-system-user());
  marshall(class-typecode(giop/<requestheader-1-0>), giop-request-header, stream);
end method;

/// MARSHALL-REQUEST-ARGUMENTS

define method marshall-request-arguments (request :: <request>, stream :: <marshalling-stream>)
  for (named-arg in request-in-args(request))
    let arg = corba/namedvalue/argument(named-arg);
    marshall(corba/any/type(arg), corba/any/value(arg), stream);
  end for;
end method;

/// MARSHALL-REQUEST-CONTEXT

define method marshall-request-context (request :: <request>, stream :: <marshalling-stream>)
  let context-values = request-context-values(request);
  when (context-values)
    marshall(make(<sequence-typecode>, element-typecode: corba/$string-typecode, max-length: 0),
	     context-values,
	     stream);
  end when;
end method;
