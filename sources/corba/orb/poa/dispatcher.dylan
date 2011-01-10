Module: orb-poa
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method ensure-poa-listener-started (poa :: <root-poa>)
  let orb = poa-orb(poa);
  let port = poa-port(poa);
  make(<thread>,
       name: format-to-string("POA port listener for %s (%=)", hostname(), port | "dynamic"),
       function: method ()
		   with-socket-thread (server?: #t)
		     receive-connections(orb-connection-manager(orb),
					 port,
					 method (manager, socket)
					   orb-service-port(orb) := local-port(socket);
					 end method,					   
					 method (stream)
					   ensure-poa-dispatcher-started(poa, stream)
					 end method)
		   end with-socket-thread;
		 end method);
  wait-for-orb-port(orb);
end method;

define method ensure-poa-dispatcher-started (poa :: <root-poa>, stream :: <stream>)
  let orb = poa-orb(poa);
  let port = poa-port(poa) | "dynamic";
  make(<thread>,
       name: format-to-string("POA dispatcher for %s (%=)", hostname(), port),
       function: method ()
		   with-socket-thread ()
		     debug-out(#"poa", "Connection made to server %s (%=)", hostname(), port);
		     dispatch-requests(orb, make(<poa-connection>, stream: stream));
		   end with-socket-thread;
		 end method)
end method;

define class <poa-connection> (<object>)
  constant slot poa-connection-stream :: <stream>, init-keyword: stream:;
  constant slot poa-connection-request-ids :: <table> = make(<table>);
end class;

define sealed domain make (subclass(<poa-connection>));
define sealed domain initialize (<poa-connection>);

define class <request-already-processed> (<simple-error>)
  keyword format-string: = "Discarding already processed request %d";
end class;

define sealed domain make (subclass(<request-already-processed>));
define sealed domain initialize (<request-already-processed>);

define method make (class == <request-already-processed>, #key request-id)
 => (condition :: <request-already-processed>)
  next-method(class, format-arguments: vector(request-id));
end method;

define variable *check-request-already-processed?* :: <boolean> = #f;

define method check-request-already-processed (connection :: <poa-connection>, request-id :: <integer>)
  if (*check-request-already-processed?*)
    let processed-request-ids = poa-connection-request-ids(connection);
    let processed? = element(processed-request-ids, request-id, default: #f);
    if (processed?)
      error(make(<request-already-processed>, request-id: request-id));
    else
      element(processed-request-ids, request-id) := #t;
    end if;
  end if;
end method;

define method dispatch-requests (orb :: corba/<orb>, connection :: <poa-connection>)
  let cstream = poa-connection-stream(connection);
  let manager = orb-connection-manager(orb);
  block ()
    while (#t)
      with-marshalling-stream (marshalling-stream, inner-stream: cstream)
	block (exit-header-processing)
	  debug-out(#"poa", "Waiting to fill marshalling buffer");
	  force-input(marshalling-stream);
	  debug-out(#"poa", "Filled marshalling buffer. Now processing request", 
			    reverse(marshalling-buffer-as-string(marshalling-stream-buffer(marshalling-stream),
								 reversed?: #t))); // ---*** *print-base* 16
	  let giop-header :: giop/<messageheader-1-0> =
	    unmarshall(class-typecode(giop/<messageheader-1-0>), marshalling-stream);
	  marshalling-stream-little-endian?(marshalling-stream) :=
	    giop/messageheader-1-0/byte-order(giop-header);
          process-request(giop/messageheader-1-0/message-type(giop-header),
			  make(<server-request>,
			       marshalling-stream: marshalling-stream,
			       giop-header: giop-header,
			       connection: connection));
	exception (condition :: <request-already-processed>)
	  debug-out(#"poa", format-to-string("%s", condition));
	exception (condition :: <giop-message-error>)
	  send-dispatcher-message-error(marshalling-stream);
	end block;
      end with-marshalling-stream;
    end while;
  exception (condition :: <end-of-stream-error>)
    debug-out(#"poa", "End of file on server request stream");
    with-lock (stream-lock(cstream))
      close(cstream, abort?: #t);
    end with-lock;
  exception (condition :: connection-manager-error-class(manager))
    debug-out(#"poa", "Error on server request stream: %=", condition);
    with-lock (stream-lock(cstream))
      close(cstream, abort?: #t);
    end with-lock;
  end block;
end method;

define method process-request (type == 2, request :: <server-request>) // #"cancelrequest"
  // ---*** We are allowed to do nothing for cancelrequest messages so we do it.
  // ---*** We could try and track down the message-id in some POA's mailbox
  // ---*** and attempt to delete it before it is processed.
  let marshalling-stream = server-request-marshalling-stream(request);
  let header = unmarshall(class-typecode(GIOP/<CancelRequestHeader>), marshalling-stream);
  let request-id = giop/cancelrequestheader/request-id(header);
  debug-out(#"poa", "Received cancel request for request id %d", request-id);
end method;

define constant $giop-locatereply-messageheader-1-0 =
  make(giop/<messageheader-1-0>,
       message-type: 4, // #"locatereply"
       magic: $giop-magic,
       giop-version: $giop-version,
       message-size: 0,
       byte-order: architecture-little-endian?());

define method process-request (type == 3, request :: <server-request>) // #"locaterequest"
  // ---*** We could invoke a servantmanager or see if a forwardlocation
  // ---*** would be returned for locaterequest messages. Instead we just
  // ---*** say it is here.
  let marshalling-stream = server-request-marshalling-stream(request);
  let locate-request-header :: giop/<locaterequestheader> =
    unmarshall(class-typecode(giop/<locaterequestheader>), marshalling-stream);
  let start-of-giop = send-dispatcher-reply-header(marshalling-stream, $giop-locatereply-messageheader-1-0);
  let locate-reply = make(giop/<locatereplyheader>,
			  request-id: giop/locaterequestheader/request-id(locate-request-header),
			  locate-status: #"object-here");
  marshall(class-typecode(giop/<locatereplyheader>), locate-reply, marshalling-stream);
  send-dispatcher-reply-footer(marshalling-stream, start-of-giop);
end method;

define method process-request (type == 0, request :: <server-request>) // #"request"
  let marshalling-stream = server-request-marshalling-stream(request);
  let connection = server-request-connection(request);
  let giop-header = server-request-giop-header(request);
  let giop-request-header :: false-or(giop/<requestheader-1-0>) = #f;
  block ()
    giop-request-header := unmarshall(class-typecode(giop/<requestheader-1-0>), marshalling-stream);
    let request-id = giop/requestheader-1-0/request-id(giop-request-header);
    check-request-already-processed(connection, request-id);
    let operation = giop/requestheader-1-0/operation(giop-request-header);
    let key = giop/requestheader-1-0/object-key(giop-request-header);
    let (objectid, poa, poa-id) = compute-applicable-poa(key);
    debug-out(#"poa", "Dispatching request on operation %= to %s", operation, poa-name(poa));
    server-request-giop-request-header(request) := giop-request-header;
    server-request-objectid(request) := objectid;
    server-request-poa(request) := poa;
    check-poa-state(poa, poa-id);
    push(poa-mailbox(poa), request);
  exception (condition :: corba/<exception>)
    send-dispatcher-exception-reply(request, condition);
  end block;
end method;

define method check-poa-state (poa :: false-or(<poa>), poa-id :: <string>)
  if (~poa | obsolete-transient-reference?(poa, poa-id))
    error(make(corba/<object-not-exist>, minor: 1, completed: #"completed-no"));
  end if;
  select (poa-manager-state(poa-manager(poa)))
    #"discarding" => error(make(corba/<transient>, minor: 1, completed: #"completed-no"));
    #"inactive" => error(make(corba/<obj-adapter>, minor: 1, completed: #"completed-no"));
    otherwise => #f;
  end select;
end method;

define method obsolete-transient-reference? (poa :: <poa>, pid :: <string>)
 => (obsolete? :: <boolean>)
  (poa-lifespan-policy(poa-policies(poa)) = #"transient")
  & (poa-id(poa) ~= pid)
end method;

define method process-request (type == 6, request :: <server-request>) // #"messageerror"
  // ---*** Just print debug message and do nothing for messageerrors
  debug-out(#"poa", "Received message error.");
end method;

define constant $giop-messageerror-messageheader-1-0 =
  make(giop/<messageheader-1-0>,
       message-type: 6, // #"messageerror"
       magic: $giop-magic,
       giop-version: $giop-version,
       message-size: 0,
       byte-order: architecture-little-endian?());

define method send-dispatcher-message-error
    (marshalling-stream :: <marshalling-stream>)
  let start-of-giop = send-dispatcher-reply-header(marshalling-stream, $giop-messageerror-messageheader-1-0);
  send-dispatcher-reply-footer(marshalling-stream, start-of-giop);
end method;

define method send-dispatcher-reply-header
    (marshalling-stream :: <marshalling-stream>,
     giop-header :: giop/<messageheader-1-0>)
  giop/messageheader-1-0/message-type(giop-header) := 1; // #"reply"
  marshalling-stream-output-index(marshalling-stream) := 0;
  let start-of-giop = marshalling-stream-output-index(marshalling-stream);
  marshalling-stream-little-endian?(marshalling-stream) :=
    giop/messageheader-1-0/byte-order(giop-header);
  marshall(class-typecode(giop/<messageheader-1-0>), giop-header, marshalling-stream);
  start-of-giop
end method;

define method send-dispatcher-reply-footer (marshalling-stream :: <marshalling-stream>,
					    start-of-giop :: <integer>)
  set-buffer-size(marshalling-stream,
		  (start-of-giop + 8),
		  (marshalling-stream-output-index(marshalling-stream) - start-of-giop - 12));
  debug-out(#"poa", "Reply message being sent %=",
		    marshalling-buffer-as-string(marshalling-stream-buffer(marshalling-stream)));
  force-output(marshalling-stream);
end method;

define method send-dispatcher-exception-reply (request :: <server-request>, condition :: <condition>)
  let marshalling-stream = server-request-marshalling-stream(request);
  let giop-header = server-request-giop-header(request);
  let giop-request-header = server-request-giop-request-header(request);
  let start-of-giop = send-dispatcher-reply-header(marshalling-stream, giop-header);
  let reply-header = make(giop/<replyheader>,
                          service-context: giop/requestheader-1-0/service-context(giop-request-header),
                          request-id: giop/requestheader-1-0/request-id(giop-request-header),
                          reply-status: #"system-exception");
  marshall(class-typecode(giop/<replyheader>), reply-header, marshalling-stream);
  marshall-exception(condition, marshalling-stream);
  send-dispatcher-reply-footer(marshalling-stream, start-of-giop);
end method;

define method compute-applicable-poa (encoded-key)
 => (objectid :: <string>, poa :: false-or(<poa>), poa-id :: <string>)
  let object-key = decode-object-key(encoded-key);
  let objectid = object-key-objectid(object-key);
  let poa-path = object-key-poa-path(object-key);
  let poa-id = object-key-poa-id(object-key);
  let current-poa = #f;
  block (return)
    for (poa-name in poa-path)
      current-poa := resolve-poa-name(current-poa, poa-name);
      unless (current-poa)
	return()
      end unless;
    end for;
  end block;
  values(objectid, current-poa, poa-id);
end method;

define method resolve-poa-name (poa == #f, poa-name :: <string>)
  if (poa-name = "RootPOA")
    let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
    corba/orb/resolve-initial-references(orb, "RootPOA");
  end if;
end method;

define method resolve-poa-name (current-poa :: portableserver/<poa>, poa-name :: <string>)
  portableserver/poa/find-poa(current-poa, poa-name, #t)
end method;

define sideways method portableserver/poa/find-poa
    (current-poa :: portableserver/<poa>, poa-name :: corba/<string>, activate-it? :: corba/<boolean>)
 => (poa :: portableserver/<poa>)
  let poa = lookup-poa(current-poa, poa-name);
  if (~poa & activate-it?)
    let activated? = activate-poa(current-poa, poa-name);
    if (activated?)
      poa := lookup-poa(current-poa, poa-name);
    end if;
  end if;
  unless (poa)
    error(make(portableserver/poa/<adapternonexistent>));
  end unless;
  poa
end method;

define method lookup-poa (current-poa :: portableserver/<poa>, pname :: corba/<string>)
  find-element(poa-children(current-poa),
	       method (child-poa)
		 poa-name(child-poa) = pname
	       end method);
end method;

define method activate-poa (current-poa :: portableserver/<poa>, poa-name :: <string>)
  let activator = poa-activator(current-poa);
  let state = poa-manager-state(poa-manager(current-poa));
  if (state = #"discarding" | state = #"holding")
    error(make(corba/<transient>));
  else
    portableserver/adapteractivator/unknown-adapter(activator, current-poa, poa-name);
  end if;
end method;

define method find-poa-port (poa :: <poa>)
  poa-port(poa)
    | begin
	let parent = poa-parent(poa);
	poa-port(poa) := parent & find-poa-port(parent)
      end 
end method;

define method find-poa-port (poa :: <root-poa>)
  poa-port(poa)
    | (poa-port(poa) := orb-service-port(poa-orb(poa)))
end method;
