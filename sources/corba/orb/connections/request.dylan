Module: orb-connections
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <string-sequence> = limited(corba/<sequence>, of: corba/<string>);
define constant $empty-strings = make(<string-sequence>);

define class <request> (corba/<request>)
  constant slot request-object :: corba/<object>, required-init-keyword: object:;
  constant slot request-operation-name :: corba/<string>, required-init-keyword: operation-name:;
  slot request-arguments :: corba/<nvlist> = make(corba/<nvlist>), required-init-keyword: arguments:;
  slot request-in-args :: corba/<nvlist>, init-keyword: ins:;
  slot request-out-args :: corba/<nvlist>, init-keyword: outs:;
  slot request-result :: false-or(corba/<any>, <condition>) = #f, init-keyword: result:;
  slot request-oneway? :: <boolean> = #f, init-keyword: oneway:;
  slot request-user-exception-typecodes :: <sequence> = #[], init-keyword: exceptions:;
  constant class slot request-system-exception-typecodes :: <sequence> = map(class-typecode, system-exceptions());
  constant slot request-context :: false-or(corba/<context>) = #f, init-keyword: context:;
  slot request-context-expression :: <string-sequence> = make(<string-sequence>), init-keyword: context-expression:;
  slot request-context-values :: <string-sequence> = $empty-strings;
  slot request-status :: false-or(corba/<exception-type>) = #f;
  constant slot request-status-notification :: <notification> =
    make(<notification>, name: "Waiting for Reply", lock: make(<lock>));
  slot request-connection :: false-or(<connection>) = #f;
  slot request-stream :: false-or(<marshalling-stream>) = #f;
end class;

define sealed domain make (subclass(<request>));
define sealed domain initialize (<request>);

define sideways method make (request == corba/<request>, #rest initargs, #key)
 => (object :: <request>)
  apply(make, <request>, initargs);
end method;

define method corba/request/delete (request :: <request>)
 => ()
  // NB do nothing
end method;

define method note-request-status-changed (request :: <request>, status :: corba/<exception-type>)
  let note = request-status-notification(request);
  with-lock (associated-lock(note))
    request-status(request) := status;
    release-all(note);
  end;
end method;

define sideways method corba/object/create-request (object :: corba/<object>,
						    context :: corba/<context>,
						    operation :: corba/<identifier>,
						    arg-list :: corba/<NVList>,
						    result :: corba/<NamedValue>,
						    req-flags :: corba/<Flags>)
 => (result :: corba/<NamedValue>, request :: corba/<request>)
  ignore(req-flags);
  let request = make(<request>,
		      object: object,
		      operation-name: operation,
		      arguments: arg-list,
		      result: when (result) corba/namedvalue/argument(result) end,
		      finished: #f,
		      context: context);
  values(result, request);
end method;

define method corba/request/add-context (request :: <request>, propname :: corba/<identifier>)
 => ()
  request-context-expression(request)
    := add!(request-context-expression(request), propname);
end method;

define method corba/request/add-exception (request :: <request>, exception-typecode :: <typecode>)
 => ()
  request-user-exception-typecodes(request)
    := add!(request-user-exception-typecodes(request), exception-typecode);
end method;

define method corba/request/invoke (request :: <request>, flags :: corba/<Flags>)
 => ()
  if (corba/object/is-nil(request-object(request)))
    error(make(corba/<object-not-exist>, minor: 0, completed: #"completed-no"));
  end if;
  let (invoked?, forwarded?) = maybe-collocated-invoke(request, flags);
  if (~invoked?)
    if (forwarded?)
      corba/request/invoke(request, flags)
    else
      remote-invoke(request, flags)
    end if;
  end if;
end method;

define method remote-invoke (request :: <request>, flags :: corba/<flags>)
 => ()
  corba/request/send(request, flags);
  unless (request-oneway?(request))
    corba/request/get-response(request, flags);
  end unless;
end method;

define method corba/request/add-arg
    (request :: <request>,
     name :: corba/<Identifier>,
     arg-type :: <typecode>,
     value :: <object>,
     len :: corba/<long>,
     flags :: corba/<Flags>)
 => ()
  request-arguments(request) :=
    add!(request-arguments(request),
	 make(corba/<namedvalue>,
	      name: name,
	      argument: make(corba/<any>, type: arg-type, value: value),
	      len: len,
	      arg-modes: flags));
end method;

define method corba/request/send (request :: <request>, flags :: corba/<Flags>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let manager = orb-connection-manager(orb);
  if (corba/object/is-nil(request-object(request)))
    error(make(corba/<object-not-exist>, minor: 0, completed: #"completed-no"));
  end if;
  process-request-arguments(request);
  when (logand(flags, corba/$inv-no-response) ~= 0)
    request-oneway?(request) := #t;
  end;
  request-context-values(request) := compute-context-values(request);
  queue-request(manager, request);
end method;


define method split-operation-arguments (nvlist :: corba/<nvlist>)
  local method direction= (modes, bit)
	  logand(modes, bit) ~= 0
	end method;
  let (ins, outs) = values(make(corba/<nvlist>), make(corba/<nvlist>));
  for (nv in nvlist)
    let modes = corba/namedvalue/arg-modes(nv);
    select (modes by direction=)
      corba/$arg-in =>
	ins := add!(ins, nv);
      corba/$arg-out =>
	outs := add!(outs, nv);
      corba/$arg-inout =>
	ins := add!(ins, nv);
	outs := add!(outs, nv);
    end select;
  end for;
  values(ins, outs)
end method;

define method process-request-arguments (request :: <request>)
 => ()
  unless (slot-initialized?(request, request-in-args)
	    & slot-initialized?(request, request-out-args))
    let (ins, outs) = split-operation-arguments(request-arguments(request));
    request-in-args(request) := ins;
    request-out-args(request) := outs;
  end unless;
end method;

define method corba/request/get-response (request :: <request>, flags :: corba/<Flags>)
  => ()
  unless (logand(flags, corba/$inv-no-response) ~= 0)
    let note = request-status-notification(request);
    with-lock (associated-lock(note))
      unless (request-status(request))
        wait-for(note);
      end unless;
    end;
  end unless;
  if (request-status(request))
    unmarshall-response(request);
  end if;
end method;

define method compute-context-values (request :: <request>)
  => (values :: <string-sequence>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let context = request-context(request) | corba/orb/get-default-context(orb);
  let context-expression = request-context-expression(request);
  if (~empty?(context-expression))
    let context-values = make(<string-sequence>);
    for (key in context-expression)
      context-values := add!(context-values, key);
      context-values := add!(context-values, as(<string>, corba/namedvalue/argument(corba/context/get-values(context, "", 0, key)[0])));
    end for;
    context-values;
  else
    $empty-strings
  end if;
end method;

  
