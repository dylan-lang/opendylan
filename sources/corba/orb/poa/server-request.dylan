Module: orb-poa
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <server-request> (corba/<serverrequest>)
  constant slot server-request-connection, init-keyword: connection:;
  constant slot server-request-marshalling-stream, init-keyword: marshalling-stream:;
  constant slot server-request-giop-header, init-keyword: giop-header:;
  slot server-request-giop-request-header, init-keyword: giop-request-header:;
  slot server-request-objectid, init-keyword: objectid:;
  slot server-request-poa, init-keyword: poa:;
  slot server-request-arguments, init-keyword: arguments:;
  slot server-request-result = #f, init-keyword: result:;
end class;

define sealed domain make (subclass(<server-request>));
define sealed domain initialize (<server-request>);

define class <destroy-thread-request> (corba/<serverrequest>)
end class;

define class <destroy-POA-request> (<destroy-thread-request>)
  constant slot server-request-poa, required-init-keyword: poa:;
  constant slot server-request-etherealize-objects?, required-init-keyword: etherealize-objects?:;
end class;

define sealed domain make (subclass(<destroy-POA-request>));
define sealed domain initialize (<destroy-POA-request>);

define method destroy-thread? (request :: corba/<serverrequest>)
  #f
end method;

define method destroy-thread? (request :: <destroy-thread-request>)
  #t
end method;

define method destroy-poa? (request :: corba/<serverrequest>)
 => (destroy? :: <boolean>)
  #f
end method;

define method destroy-poa? (request :: <destroy-POA-request>)
 => (destroy? :: <boolean>)
  #t
end method;

define method corba/serverrequest/set-result (server-request :: <server-request>, any :: corba/<any>)
 => ()
  server-request-result(server-request) := any;
end method;

define method corba/serverrequest/set-exception (server-request :: <server-request>, exception :: corba/<any>)
 => ()
  error(corba/any/value(exception));
end method;

define method corba/serverrequest/operation (server-request :: <server-request>)
 => (identifier :: corba/<identifier>);
  giop/requestheader-1-0/operation(server-request-giop-request-header(server-request));
end method;

define method corba/serverrequest/arguments (server-request :: <server-request>, nvl :: corba/<nvlist>)
 => (nvl :: corba/<nvlist>)
  local method next-argument-value (nv)
	  unmarshall(corba/any/type(corba/namedvalue/argument(nv)),
		     server-request-marshalling-stream(server-request));
	end method;
  build-arguments(server-request, nvl, next-argument-value);
end method;

define method build-arguments
    (server-request :: corba/<serverrequest>, nvl :: corba/<nvlist>, next-argument-value :: <function>)
  => (nvl :: corba/<nvlist>)
  server-request-arguments(server-request) := nvl;
  block ()
    for (nv in nvl)
      when ((logand(corba/namedvalue/arg-modes(nv), corba/$arg-in) ~= 0)
	      | (logand(corba/namedvalue/arg-modes(nv), corba/$arg-inout) ~= 0))
	corba/any/value(corba/namedvalue/argument(nv)) := next-argument-value(nv);
      end when;
    end for;
    nvl;
  exception (condition :: <serious-condition>)
    error(make(corba/<marshal>, minor: 33, completed: #"completed-no"));
  end block;
end method;

define method corba/serverrequest/ctx (server-request :: <server-request>)
 => (context :: corba/<context>)
  let values =
    unmarshall(make(<sequence-typecode>, element-typecode: corba/$string-typecode, max-length: 0),
	       server-request-marshalling-stream(server-request));
  build-values-context(corba/serverrequest/operation(server-request), values); // ---*** what to call context?
end method;

define method build-values-context (name :: corba/<string>, values :: <sequence>)
 => (context :: corba/<context>)
  let default-context = corba/orb/get-default-context(corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB"));
  let context = corba/context/create-child(default-context, generate-name(name));
  for (i from 0 below size(values) by 2)
    corba/context/set-one-value(context, values[i], as(corba/<any>, values[i + 1]));
  end for;
  context;
end method;

define sideways method corba/serverrequest/invoke
    (server-request :: corba/<serverrequest>, object :: PortableServer/<dynamic-servant>)
 => ()
  // NB users _must_ specialize this
  error(make(corba/<no-implement>))
end method;

define sideways method corba/serverrequest/invoke
    (server-request :: corba/<serverrequest>, object :: PortableServer/<servant>)
 => ()
  invoke-server-request(server-request, object)
end method;

// NB separate invoke-server-request out in case collocation needs to specialize it
define method invoke-server-request
    (server-request :: corba/<serverrequest>, object :: PortableServer/<servant>)
 => ()
  let operation = corba/serverrequest/operation(server-request);
  invoke-operation(object, server-request, as(<symbol>, operation));
end method;

define sideways method invoke-operation (object :: portableserver/<servant>,
				server-request :: corba/<serverrequest>,
				operation :: <symbol>)
 => ()
  error(make(corba/<bad-operation>,
	     minor: 48,
	     completed: #"completed-no"));
end method;
  
define sideways method invoke-operation (object :: portableserver/<servant>,
				server-request :: corba/<serverrequest>,
				operation == #"_is_a")
 => ()
  let arg = make(corba/<namedvalue>,
		 name: "logical_type_id",
		 argument: make(corba/<any>, type: corba/$string-typecode),
		 len: 0,
		 arg-modes: corba/$arg-in);
  let arguments = make(corba/<nvlist>);
  arguments := add!(arguments, arg);
  corba/serverrequest/arguments(server-request, as(corba/<nvlist>, arguments));
  let result = corba/object/-is-a(object, as(corba/<string>, corba/namedvalue/argument(arg)));
  corba/serverrequest/set-result(server-request, as(corba/<any>, result));
end method;

define sideways method invoke-operation (object :: portableserver/<servant>,
				server-request :: corba/<serverrequest>,
				operation == #"_non_existent")
 => ()
  corba/serverrequest/arguments(server-request, make(corba/<nvlist>));
  corba/serverrequest/set-result(server-request, as(corba/<any>, #f));
end method;

define sideways method corba/object/-is-a (object :: PortableServer/<servant>, desired-repository-id :: corba/<string>)
 => (result :: corba/<boolean>)
  let desired-typecode = as(<typecode>, desired-repository-id);
  let desired-type = desired-typecode & typecode-native-type(desired-typecode);
  desired-type & instance?(object, desired-type);
end method;

