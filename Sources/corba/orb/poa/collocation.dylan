Module: orb-poa
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define orb-arg-processor
  syntax: "-ORBno-co-location",
  callback: method (orb :: corba/<orb>)
	      *optimize-collocation?* := #f;
	    end method
end orb-arg-processor;

define class <collocated-cache> (<object>)
  constant slot collocated-cache-servant :: portableserver/<servant>, required-init-keyword: servant:;
  constant slot collocated-cache-id :: <string>, required-init-keyword: id:;
  constant slot collocated-cache-poa :: portableserver/<poa>, required-init-keyword: poa:;
end class;

define sealed domain make (subclass(<collocated-cache>));
define sealed domain initialize (<collocated-cache>);

define class <collocated-server-request> (corba/<serverrequest>)
  constant slot collocated-client-request :: corba/<request>, required-init-keyword: request:;
  slot server-request-objectid, init-keyword: objectid:;
  slot server-request-arguments, init-keyword: arguments:;
end class;

define sealed domain make (subclass(<collocated-server-request>));
define sealed domain initialize (<collocated-server-request>);

define variable *collocation-ticket* :: <boolean-ticket> = initial-ticket(<boolean-ticket>);

define method invalidate-collocations ()
  *collocation-ticket* := reissue-ticket(*collocation-ticket*)
end method;

define method collocation-ticket ()
  *collocation-ticket*
end method;

define method collocated-servant? (server-request :: <collocated-server-request>)
 => (collocated? :: <boolean>)
  when (*optimize-collocation?*)
    let request = collocated-client-request(server-request);
    let object = request-object(request);
    if (valid-ticket?(object-reference-collocated-ticket(object), collocation-ticket()))
      let cache = object-reference-collocated-cache(object);
      cache & #t
    else
      let (servant, poa, id) = compute-collocated-servant(object, server-request);
      let cache = if (servant)
		    make(<collocated-cache>, poa: poa, id: id, servant: servant);
		  else
		    #f
		  end if;
      object-reference-collocated-cache(object) := cache;
      object-reference-collocated-ticket(object) := collocation-ticket();
      cache & #t;
    end if;
  end when;
end method;    
    
define method compute-collocated-servant (object :: <object-reference>, server-request :: <collocated-server-request>)
 => (servant :: false-or(portableserver/<servant>) , poa :: false-or(portableserver/<poa>), id :: false-or(<string>))
  if (collocated-server?(object))
    let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
    let ior = corba/object/ior(object);
    let profile = get-iiop-profile(ior);
    let key = iiop/profilebody-1-0/object-key(profile);
    let (objectid, poa, poa-id) = compute-applicable-poa(key);
    check-poa-state(poa, poa-id);
    server-request-objectid(server-request) := objectid;
    values(compute-servant(poa, server-request), poa, objectid);
  else
    values(#f, #f, #f);
  end if;
end method;

define sideways method maybe-collocated-invoke (request :: corba/<request>, flags :: corba/<flags>)
 => (invoked? :: <boolean>, forwarded? :: <boolean>)
  dynamic-bind (*cookie* = *cookie*)
    let server-request = make(<collocated-server-request>, request: request);
    if (collocated-servant?(server-request))
      collocated-invoke(server-request, flags)
    else
      values(#f, #f)
    end if;
  end;
end method;
    
define method collocated-invoke (server-request :: <collocated-server-request>, flags :: corba/<flags>)
 => (invoked? :: <boolean>, forwarded? :: <boolean>)
  let request = collocated-client-request(server-request);
  let object = request-object(request);
  let cache = object-reference-collocated-cache(object);
  let servant = collocated-cache-servant(cache);
  let poa = collocated-cache-poa(cache);
  let id = collocated-cache-id(cache);
  block ()
    block ()
      let handler <serious-condition> = handle-application-error;
      dynamic-bind (*current-poa* = poa)
	dynamic-bind (*current-object-id* = id)
	  corba/serverrequest/invoke(server-request, servant);
	end;
      end;
    end block;
    note-request-invoked(poa, servant, server-request);
    for (sarg in server-request-arguments(server-request),
	 carg in request-arguments(request))
      corba/any/value(corba/namedvalue/argument(carg)) := corba/any/value(corba/namedvalue/argument(sarg));
    end for;
    values(#t, #f);
  exception (condition :: portableserver/<forwardrequest>)
    let new-ior = corba/object/ior(portableserver/forwardrequest/forward-reference(condition));
    debug-out(#"poa", "Request %= forwarded to %=", request, new-ior);
    corba/object/ior(request-object(request)) := new-ior;
    values(#f, #t);
  end block;
end method;

/// ---*** really should just invalidate cached servant and not cached poa
define method note-request-invoked (poa :: <poa>, servant :: portableserver/<servant>, request :: <collocated-server-request>)
  next-method();
  if (*cookie*)
    object-reference-collocated-ticket(request-object(collocated-client-request(request)))
      := invalid-ticket(<boolean-ticket>); // NB decache if not retained
  end if;
end method;

define method collocated-server? (object :: <object-reference>)
 => (collocated? :: <boolean>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let adaptor = orb-adaptor(orb);
  if (adaptor)
    let profile = get-iiop-profile(corba/object/ior(object));
    let host = iiop/profilebody-1-0/host(profile);
    let port = iiop/profilebody-1-0/port(profile);
    if (host = hostname())
      poa-matching-port?(adaptor, port);
    end if;
  end if;
end method;

define method poa-matching-port? (poa :: portableserver/<poa>, port :: <integer>)
  (poa-port(poa) = port)
    | any?(rcurry(poa-matching-port?, port), poa-children(poa))
end method;

define method corba/serverrequest/set-result (server-request :: <collocated-server-request>, any :: corba/<any>)
 => ()
  let request = collocated-client-request(server-request);
  corba/any/value(request-result(request)) := corba/any/value(any);
end method;

define method corba/serverrequest/set-exception (server-request :: <collocated-server-request>, excep :: corba/<any>)
 => ()
  error(corba/any/value(excep));
end method;

define method corba/serverrequest/operation (server-request :: <collocated-server-request>)
 => (identifier :: corba/<identifier>);
  request-operation-name(collocated-client-request(server-request))
end method;

define method corba/serverrequest/arguments (server-request :: <collocated-server-request>, nvl :: corba/<nvlist>)
 => (nvl :: corba/<nvlist>)
  let arguments = request-arguments(collocated-client-request(server-request));
  let (state, limit, next, finished?, key, elt) = forward-iteration-protocol(arguments);
  local method next-argument-value (nv)
	  let value = corba/any/value(corba/namedvalue/argument(elt(arguments, state)));
	  unless (finished?(arguments, state, limit))
	    state := next(arguments, state);
	  end unless;
	  value
	end method;
  build-arguments(server-request, nvl, next-argument-value);
end method;

define method corba/serverrequest/ctx (server-request :: <collocated-server-request>)
 => (context :: corba/<context>)
  build-values-context(corba/serverrequest/operation(server-request),
		       compute-context-values(collocated-client-request(server-request)));
end method;

