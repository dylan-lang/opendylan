Module: orb-poa
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *current-poa* :: false-or(<poa>) = #f;

define thread variable *current-object-id* :: false-or(<string>) = #f;

define thread variable *cookie* = #f;

define class <current> (portableserver/<current>)
end class;

define sealed domain make (subclass(<current>));
define sealed domain initialize (<current>);

define constant $current = make(<current>);

define initial-service POACurrent (orb)
  $current
end initial-service;

define method portableserver/current/get-poa (current :: <current>)
 => (poa :: <poa>)
  *current-poa* | error(make(portableserver/current/<nocontext>))
end method;

define method portableserver/current/get-object-id (current :: <current>)
 => (objectid :: <string>)
  *current-object-id* | error(make(portableserver/current/<nocontext>))
end method;

define variable *break-on-server-error* = #f;

define orb-arg-processor
  syntax: "-ORBdebug",
  callback: method (orb :: corba/<orb>)
	      *break-on-server-error* := #t
	    end method
end orb-arg-processor;

define method receive-request (poa :: <poa>)
  let thread = current-thread();
  note-poa-thread-created(poa, thread);
  block (exit)
    while (#t)
      let mailbox = poa-mailbox(poa);
      let manager = poa-manager(poa);
      let state = poa-manager-state(manager);
      when (state =  #"holding")
	wait-for-poa-manager-state-change(manager);
      end when;

      when ((state = #"inactive") & (empty?(mailbox)))
	error("need to shutdown the POA") /// ---*** implement
      end when;

      let request = pop(mailbox);

      when (destroy-thread?(request))
	when (destroy-poa?(request))
	  shutdown-poa(server-request-poa(request), request);
	end when;
	exit();
      end when;

      let poa = server-request-poa(request);

      block ()
        let handler <serious-condition> = handle-application-error;
	invoke-request(poa, request);
	send-request-reply(poa, request);
      exception (condition :: portableserver/<forwardrequest>)  
	send-forwarding-reply(poa, request, Portableserver/ForwardRequest/forward-reference(condition));
      exception (condition :: corba/<exception>)
	send-exception-reply(poa, request, condition);
      end block;
    end while;
  end block;
  debug-out(#"poa", "Terminating: %s", thread-name(thread));
  note-poa-thread-destroyed(poa, thread);
end method;

define method handle-application-error (condition :: corba/<exception>, next-handler :: <function>)
    => ()
  next-handler();
end method;

define method handle-application-error (condition :: portableserver/<forwardrequest>, next-handler :: <function>)
    => ()
  next-handler();
end method;

define method handle-application-error (condition :: <serious-condition>, next-handler :: <function>)
    => ()
  debug-out(#"poa", "Error raised when processing operation on server: %=", condition);
  when (*break-on-server-error*)
    break("Server error -- breaking with condition %=", condition);
  end when;
  error(make(corba/<bad-operation>, minor: 26, completed: #"completed-maybe"));
end method;

define method shutdown-poa (poa :: portableserver/<poa>, request :: <destroy-poa-request>)
  when (request)
    let policies = poa-policies(poa);
    when ((POA-request-processing-policy(policies) = #"use-servant-manager")
	    & poa-servant-manager(poa))
      if (server-request-etherealize-objects?(request))
	for (servant keyed-by objectid in poa-active-object-table(poa))
	  remove-key!(poa-active-object-table(poa), objectid);
	  portableserver/servantactivator/etherealize(poa-servant-manager(poa),
						      objectid, poa, servant, #t,
						      remaining-activations?(poa, servant));
	end for;
      else
	for (servant keyed-by objectid in poa-active-object-table(poa))
	  remove-key!(poa-active-object-table(poa), objectid);
	end for;
      end if;
    end when;
  end when;
  note-poa-shutdown(poa);
end method;

define method remaining-activations? (poa :: portableserver/<poa>, servant :: portableserver/<servant>)
  member?(servant, poa-active-object-table(poa));
end method;

define method invoke-request (poa :: portableserver/<poa>, request :: <server-request>)
  let objectid = server-request-objectid(request);
  dynamic-bind (*cookie* = *cookie*)
    debug-out(#"poa", "POA processing request message %s in %s", objectid, poa-name(poa));
    let servant = compute-servant(poa, request);
    dynamic-bind (*current-poa* = poa)
      dynamic-bind (*current-object-id* = objectid)
        corba/serverrequest/invoke(request, servant);
      end dynamic-bind;
    end dynamic-bind;
    note-request-invoked(poa, servant, request);
  end;
end method;

define method note-request-invoked (poa :: <poa>, servant :: portableserver/<servant>, request :: corba/<serverrequest>)
  if (*cookie*)
    portableserver/servantlocator/postinvoke(poa-servant-manager(poa),
					     server-request-objectid(request),
					     poa,
					     corba/serverrequest/operation(request),
					     *cookie*,
					     servant);
  end if;
end method;

define method compute-servant (poa :: portableserver/<poa>, request :: corba/<serverrequest>)
  if (poa-manager-state(poa-manager(poa)) = #"inactive")
    error(make(corba/<obj-adapter>, completed: #"completed-no", minor: 11));
  else
    retained-servant(poa, request)
      | default-servant(poa, request)
      | manager-servant(poa, request)
      | error(make(corba/<object-not-exist>, completed: #"completed-no", minor: 11))
  end if;
end method;

define method retained-servant (poa :: portableserver/<poa>, request :: corba/<serverrequest>)
  let policies = poa-policies(poa);
  let objectid = server-request-objectid(request);
  if (POA-servant-retention-policy(policies) = #"retain")
    element(poa-active-object-table(poa), objectid, default: #f)
  end if;
end method;

/// ---*** perhaps convert errors signalled here into <obj-adaptor> exceptions
define method default-servant (poa :: portableserver/<poa>, request :: corba/<serverrequest>)
  let policies = poa-policies(poa);
  if (POA-request-processing-policy(policies) = #"use-default-servant")
    poa-default-servant(poa);
  end if;
end method;

/// ---*** perhaps convert errors signalled here into <obj-adaptor> exceptions
define method manager-servant (poa :: portableserver/<poa>, request :: corba/<serverrequest>)
  let policies = poa-policies(poa);
  let objectid = server-request-objectid(request);
  if (POA-request-processing-policy(policies) = #"use-servant-manager")
    let servant-manager = poa-servant-manager(poa);
    if (POA-servant-retention-policy(policies) = #"retain")
      let servant = portableserver/servantactivator/incarnate(servant-manager, objectid, poa);
      portableserver/poa/activate-object-with-id(poa, objectid, servant);
      servant;
    else
      let (servant, cookie) = portableserver/servantlocator/preinvoke(servant-manager,
								      objectid,
								      poa,
								      corba/serverrequest/operation(request));
      *cookie* := cookie;
      servant;
    end if;
  end if;
end method;

define method send-reply-header (poa :: portableserver/<poa>,
				 server-request :: <server-request>,
				 status :: giop/<replystatustype>)
  let giop-header = server-request-giop-header(server-request);
  let giop-request-header = server-request-giop-request-header(server-request);
  let marshalling-stream = server-request-marshalling-stream(server-request);
  let reply-header = make(giop/<replyheader>,
			  service-context: giop/requestheader-1-0/service-context(giop-request-header),
			  request-id: giop/requestheader-1-0/request-id(giop-request-header),
			  reply-status: status);
  giop/messageheader-1-0/message-type(giop-header) := 1; // #"reply"
  marshalling-stream-output-index(marshalling-stream) := 0;
  let start-of-giop = marshalling-stream-output-index(marshalling-stream);
  marshalling-stream-little-endian?(marshalling-stream) := giop/messageheader-1-0/byte-order(giop-header);
  marshall(class-typecode(giop/<messageheader-1-0>), giop-header, marshalling-stream);
  marshall(class-typecode(giop/<replyheader>), reply-header, marshalling-stream);
  start-of-giop
end method;

define method send-reply-footer (poa :: portableserver/<poa>,
				 server-request :: <server-request>,
				 start-of-giop)
  let marshalling-stream = server-request-marshalling-stream(server-request);
  set-buffer-size(marshalling-stream,
		  (start-of-giop + 8),
		  (marshalling-stream-output-index(marshalling-stream) - start-of-giop - 12));
  debug-out(#"poa", "Reply message being sent %=",
		    marshalling-buffer-as-string(marshalling-stream-buffer(marshalling-stream)));
  force-output(marshalling-stream);
end method;

define method send-request-reply (poa :: portableserver/<poa>, server-request :: <server-request>)
  let giop-request-header = server-request-giop-request-header(server-request);
  let marshalling-stream = server-request-marshalling-stream(server-request);
  when (giop/requestheader-1-0/response-expected(giop-request-header))
    let start-of-giop = send-reply-header(poa, server-request, #"no-exception");
    block ()
      let result = server-request-result(server-request);
      when (result)
	marshall(corba/any/type(result), corba/any/value(result), marshalling-stream);
      end when;
      for (arg in server-request-arguments(server-request))
	let modes = corba/namedvalue/arg-modes(arg);
	when ((logand(modes, corba/$arg-out) ~= 0)
		| logand(modes, corba/$arg-inout) ~= 0)
	  marshall(corba/any/type(corba/namedvalue/argument(arg)),
		   corba/any/value(corba/namedvalue/argument(arg)),
		 marshalling-stream);
	end when;
      end for;
    exception (condition :: <serious-condition>)
      debug-out(#"poa", "Errors during result marshalling in server, send exception instead", condition);
      send-exception-reply(poa, server-request, make(corba/<marshal>, minor: 13, completed: #"completed-maybe"));
    end block;
    send-reply-footer(poa, server-request, start-of-giop);
  end when;
end method;

define method send-forwarding-reply
    (poa :: portableserver/<poa>, server-request :: <server-request>, forward :: corba/<object>)
  let giop-request-header = server-request-giop-request-header(server-request);
  if (giop/requestheader-1-0/response-expected(giop-request-header))
    let marshalling-stream = server-request-marshalling-stream(server-request);
    let start-of-giop = send-reply-header(poa, server-request, #"location-forward");
    marshall(class-typecode(iop/<ior>),
	     corba/object/ior(forward),
	     marshalling-stream);
    send-reply-footer(poa, server-request, start-of-giop);
  else
    corba/serverrequest/forward(server-request, forward, context: corba/orb/get-default-context(poa-orb(poa)))
  end if;
end method;

define method send-exception-reply
    (poa :: portableserver/<poa>, server-request :: <server-request>, condition :: corba/<exception>)
  let giop-request-header = server-request-giop-request-header(server-request);
  when (giop/requestheader-1-0/response-expected(giop-request-header))
    debug-out(#"poa", "Returning exception %=", condition);
    let start-of-giop = send-reply-header(poa, server-request, compute-exception-status(condition));
    marshall-exception(condition, server-request-marshalling-stream(server-request));
    send-reply-footer(poa, server-request, start-of-giop);
  end when;
end method;

define method marshall-exception
    (condition :: <condition>, stream :: <marshalling-stream>)
  => ()
  let typecode = object-typecode(condition);
  marshall(corba/$string-typecode, typecode-repository-id(typecode), stream);
  for (member in typecode-members(typecode))
    marshall(typecode-member-typecode(member),
             typecode-member-getter(member)(condition),
             stream);
  end for;
end method;

define method compute-exception-status (condition :: corba/<exception>)
 => (status :: giop/<replystatustype>)
  if (member?(condition,
	      system-exceptions(),
	      test: method (condition, class)
		      instance?(condition, class)
		    end method))
    #"system-exception"
  else
    #"user-exception"
  end if;
end method;


