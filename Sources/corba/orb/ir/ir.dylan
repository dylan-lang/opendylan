Module: orb-ir
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GET-INTERFACE

define open generic corba/object/get-interface (object :: <object>)
 => (result :: corba/<InterfaceDef>);

// ---*** what about caching?
define sideways method corba/object/get-interface (object :: <object-reference>)
 => (result :: corba/<interfacedef>)
  corba/object/-interface(object);
end method;

define method corba/object/-interface (object :: <object-reference>)
 => (result :: corba/<InterfaceDef>)
  let context = corba/orb/get-default-context(corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB"));
  let (result, request) = corba/object/create-request(object,
						      context,
						      "_interface",
						      make(corba/<nvlist>),
						      make(corba/<namedvalue>,
							   name: "result",
							   argument: make(corba/<any>,
									  type: class-typecode(corba/<InterfaceDef>)),
							   len: 0,
							   arg-modes: 0),
						      0);
  dynamic-bind (*optimize-collocation?* = #t) // force this here even if off elsewhere
    corba/request/invoke(request, 0);
  end;
  as(corba/<InterfaceDef>, corba/namedvalue/argument(result));
end method;

define sideways method invoke-operation (object :: portableserver/<servant>,
				server-request :: corba/<serverrequest>,
				operation == #"_interface")
 => ()
  corba/serverrequest/arguments(server-request, make(corba/<nvlist>));
  let interface = corba/object/get-interface(object);
  corba/serverrequest/set-result(server-request, as(corba/<any>, interface));
end method;

define method corba/object/get-interface (object :: portableserver/<servant>)
 => (result :: corba/<InterfaceDef>);
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let ir = as(corba/<Repository>, corba/orb/resolve-initial-references(orb, "InterfaceRepository"));
  if (ir)
    let current = corba/orb/resolve-initial-references(orb, "POACurrent");
    let poa = portableserver/current/get-POA(current);
    let objectid = portableserver/current/get-object-id(current);
    let repository-id = portableserver/servant/primary-interface(object, objectid, poa);
    corba/repository/lookup-id(ir, repository-id);
  else
    make-nil(corba/<interfacedef>);
  end if;
end method;

define sideways method corba/serverrequest/forward
    (server-request :: corba/<serverrequest>, forward :: corba/<object>, #key context)
 => ()
  let request = compute-request(forward,
				corba/serverrequest/operation(server-request),
				context: context);
  corba/serverrequest/arguments(server-request, request-arguments(request));
  corba/request/invoke(request, corba/$inv-no-response);
end method;

/// TYPECODE CREATION PROTOCOL

define open generic CORBA/ORB/create-struct-tc (object :: CORBA/<ORB>, id :: CORBA/<RepositoryId>, name :: CORBA/<Identifier>, members :: CORBA/<StructMemberSeq>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-union-tc (object :: CORBA/<ORB>, id :: CORBA/<RepositoryId>, name :: CORBA/<Identifier>, discriminator-type :: CORBA/<TypeCode>, members :: CORBA/<UnionMemberSeq>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-enum-tc (object :: CORBA/<ORB>, id :: CORBA/<RepositoryId>, name :: CORBA/<Identifier>, members :: CORBA/<EnumMemberSeq>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-alias-tc (object :: CORBA/<ORB>, id :: CORBA/<RepositoryId>, name :: CORBA/<Identifier>, original-type :: CORBA/<TypeCode>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-exception-tc (object :: CORBA/<ORB>, id :: CORBA/<RepositoryId>, name :: CORBA/<Identifier>, members :: CORBA/<StructMemberSeq>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-interface-tc (object :: CORBA/<ORB>, id :: CORBA/<RepositoryId>, name :: CORBA/<Identifier>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-string-tc (object :: CORBA/<ORB>, bound :: CORBA/<unsigned-long>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-wstring-tc (object :: CORBA/<ORB>, bound :: CORBA/<unsigned-long>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-fixed-tc (object :: CORBA/<ORB>, digits :: CORBA/<unsigned-short>, scale :: CORBA/<short>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-sequence-tc (object :: CORBA/<ORB>, bound :: CORBA/<unsigned-long>, element-type :: CORBA/<TypeCode>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-recursive-sequence-tc (object :: CORBA/<ORB>, bound :: CORBA/<unsigned-long>, offset :: CORBA/<unsigned-long>)
 => (result :: CORBA/<TypeCode>);

define open generic CORBA/ORB/create-array-tc (object :: CORBA/<ORB>, length :: CORBA/<unsigned-long>, element-type :: CORBA/<TypeCode>)
 => (result :: CORBA/<TypeCode>);


/// TYPECODE CREATION IMPLEMENTATION

define sideways method CORBA/ORB/create-struct-tc
    (object :: CORBA/<ORB>,
     id :: CORBA/<RepositoryId>,
     name :: CORBA/<Identifier>,
     members :: CORBA/<StructMemberSeq>)
 => (result :: <typecode>)
  make-structure-typecode(<struct-typecode>, id, name, members)
end method;

define sideways method CORBA/ORB/create-union-tc
    (object :: CORBA/<ORB>,
     id :: CORBA/<RepositoryId>,
     name :: CORBA/<Identifier>,
     discriminator-type :: <typecode>,
     members :: CORBA/<UnionMemberSeq>)
 => (result :: <typecode>)
  make(<union-typecode>,
       name: name,
       repository-id: id,
       discriminant: discriminator-type,
       members: map(method (member)
		      make(<typecode-branch>,
			   name: CORBA/UnionMember/name(member),
			   typecode: CORBA/UnionMember/type(member),
			   label-value: CORBA/UnionMember/label(member))
		    end method,
		    members))
end method;

define sideways method CORBA/ORB/create-enum-tc
    (object :: CORBA/<ORB>,
     id :: CORBA/<RepositoryId>,
     name :: CORBA/<Identifier>,
     members :: CORBA/<EnumMemberSeq>)
 => (result :: <typecode>)
  make(<enum-typecode>,
       name: name,
       repository-id: id,
       members: map(curry(as, <symbol>), members))
end method;

define sideways method CORBA/ORB/create-alias-tc
    (object :: CORBA/<ORB>,
     id :: CORBA/<RepositoryId>,
     name :: CORBA/<Identifier>,
     original-type :: <typecode>)
 => (result :: <typecode>)
  make(<alias-typecode>, name: name, repository-id: id, aliased: original-type)
end method;

define sideways method CORBA/ORB/create-exception-tc
    (object :: CORBA/<ORB>,
     id :: CORBA/<RepositoryId>,
     name :: CORBA/<Identifier>,
     members :: CORBA/<StructMemberSeq>)
 => (result :: <typecode>)
  make-structure-typecode(<exception-typecode>, id, name, members)
end method;

define sideways method make-structure-typecode
    (class :: subclass(<struct-typecode>),
     id :: CORBA/<RepositoryId>,
     name :: CORBA/<Identifier>,
     members :: CORBA/<StructMemberSeq>)
 => (typecode :: <struct-typecode>)
  make(class,
       name: name,
       repository-id: id,
       members: map(method (member)
		      make(<typecode-member>,
			   name:  CORBA/StructMember/name(member),
			   typecode: CORBA/StructMember/type(member))
		    end method,
		    members))		     
end method;

define sideways method CORBA/ORB/create-interface-tc
    (object :: CORBA/<ORB>, id :: CORBA/<RepositoryId>, name :: CORBA/<Identifier>)
 => (result :: <typecode>)
  make(<object-reference-typecode>, name: name, repository-id: id)
end method;

define sideways method CORBA/ORB/create-string-tc
    (object :: CORBA/<ORB>, bound :: CORBA/<unsigned-long>)
 => (result :: <typecode>)
  make(<string-typecode>, max-length: bound);
end method;

define sideways method CORBA/ORB/create-sequence-tc
    (object :: CORBA/<ORB>, bound :: CORBA/<unsigned-long>, element-type :: <typecode>)
 => (result :: <typecode>)
  make(<sequence-typecode>, max-length: bound, element-typecode: element-type)
end method;

define sideways method CORBA/ORB/create-recursive-sequence-tc
    (object :: CORBA/<ORB>, bound :: CORBA/<unsigned-long>, offset :: CORBA/<unsigned-long>)
 => (result :: <typecode>)
  make(<sequence-typecode>,
       max-length: bound,
       element-typecode: make(<indirection-typecode>, offset: offset))
end method;

define sideways method CORBA/ORB/create-array-tc
    (object :: CORBA/<ORB>, length :: CORBA/<unsigned-long>, element-type :: <typecode>)
 => (result :: <typecode>)
  make(<array-typecode>, max-length: length, element-typecode: element-type)
end method;

/// CREATE-OPERATION-LIST

define open generic corba/orb/create-operation-list
    (orb :: corba/<orb>, operationdef :: corba/<operationdef>)
 => (new-list :: corba/<NVList>);

define method corba/orb/create-operation-list
    (orb :: corba/<orb>, operationdef :: corba/<operationdef>)
 => (new-list :: corba/<NVList>)
  let nvl = make(corba/<nvlist>);
  for (p in corba/operationdef/params(operationdef))
    nvl := add!(nvl, make(corba/<namedvalue>,
			  name: corba/parameterdescription/name(p),
			  argument: make(corba/<any>,
				      type: corba/parameterdescription/type(p)),
			  arg-modes: select (corba/parameterdescription/mode(p))
                                       #"param-in" => corba/$arg-in;
                                       #"param-out" => corba/$arg-out;
                                       #"param-inout" => corba/$arg-inout;
                                     end select,
			  len: 0));
  end for;
  nvl
end method;

/// WITH-DII
///
/// ---*** Is operation name local name, scoped name, or dylan name?
/// ---*** How to allow for context passing with generated functions?
/// ---*** How to cope with exceptions?
/// ---*** Allow method specialization?
/// ---*** Allow different function name from operation name?
/// ---*** Mangle function names like IDL compiler?
/// ---*** Invalidate of operation-request caches on get-interface?

define method dii
    (operation :: <string>, object :: corba/<object>, #rest arguments)
  call-dii(operation, object, arguments)
end method;

define macro with-dii
  { with-dii () ?body:body end }
  =>
  { begin ?body end }

  { with-dii (?fn:name) ?body:body end }
  =>
  { with-dii-aux (?fn) ?body end }

  { with-dii (?fn:name, ?fns:*) ?body:body end }
  =>
  { with-dii-aux (?fn)
      with-dii (?fns)
        ?body
      end;
    end;
  }
end macro;

define macro with-dii-aux
  { with-dii-aux (?fn:name) ?body:body end }
  => 
  { begin
      local method ?fn (object :: corba/<object>, #rest args)
	      call-dii(?"fn", object, args)
	    end method;
      local method ?fn ## "-setter" (new-value, object :: corba/<object>)
		call-dii(?"fn", object, list(new-value), setter?: #t);
	    end method;
      ?body
    end
     }
end macro;

define macro dii-methods-definer
  { define dii-methods end }
  => {}

  { define dii-methods ?fn:name, ?fns:* end }
    =>
    { define dii-methods-aux ?fn end;
      define dii-methods ?fns end }
end macro;

define macro dii-methods-aux-definer
  { define dii-methods-aux ?fn:name end }
  =>
  { define method ?fn (object :: corba/<object>, #rest args)
      call-dii(?"fn", object,args)
    end method;
    define method ?fn ## "-setter" (new-value, object :: corba/<object>)
	call-dii(?"fn", object, list(new-value), setter?: #t);
    end method;
  }
end macro;

define class <no-interface-definition> (<simple-error>)
  keyword format-string: = "Cannot find interface definition for %= in Interface Repository";
end class;

define sealed domain make (subclass(<no-interface-definition>));
define sealed domain initialize (<no-interface-definition>);

define method make (class == <no-interface-definition>, #key object, #all-keys)
 => (condition :: <no-interface-definition>)
  next-method(class, format-arguments: vector(object));
end method;

define class <no-operation-definition> (<simple-error>)
  keyword format-string: = "Cannot find operation definition for %s in interface definition %=";
end class;

define sealed domain make (subclass(<no-operation-definition>));
define sealed domain initialize (<no-operation-definition>);

define method make (class == <no-operation-definition>, #key interface, operation, #all-keys)
 => (condition :: <no-operation-definition>)
  next-method(class, format-arguments: vector(operation, interface));
end method;

// $operation-requests is a table of object-references to tables
// of operation names to requests.
define constant $operation-requests :: <table> = make(<table>, weak: #"key");

define method call-dii
    (operation :: <string>, object :: corba/<object>, arguments :: <sequence>,
     #key context = unsupplied(), setter? = #f)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let context = if (supplied?(context))
		  context
		else
		  corba/orb/get-default-context(orb)
		end if;
  // ---*** need to strip out #key context arg from arguments (if any) and pass to get-request
  let request = get-request(object, operation, context: context, setter?: setter?);
  insert-operation-arguments(operation, request-in-args(request), arguments);
  corba/request/invoke(request, 0);
  extract-operation-results(if (setter?)
			      corba/namedvalue/argument(first(request-in-args(request)))
			    else
			      request-result(request)
			    end if,
			    request-out-args(request))
end method;

define method invalidate-requests (object :: corba/<object>)
 => ()
  remove-key!($operation-requests, object);
end method;

define method get-request (object :: corba/<object>, operation :: <string>, #key context, setter?)
 => (request :: corba/<request>)
  let opkey = as(<symbol>, operation);
  let optab = element($operation-requests, object, default: #f);
  if (optab)
    let request = element(optab, opkey, default: #f);
    if (request)
      copy-request(request);
    else
      element(optab, opkey) := compute-request(object, operation, context: context, setter?: setter?)
    end if;
  else
    let optab = make(<table>);
    element($operation-requests, object) := optab;
    element(optab, opkey) := compute-request(object, operation, context: context, setter?: setter?)
  end if;
end method;

define method copy-request (request :: <request>)
 => (request :: corba/<request>)
  make(<request>,
       object: request-object(request),
       operation-name: request-operation-name(request),
       arguments: copy-request-arguments(request-arguments(request)),
       ins: copy-request-arguments(request-in-args(request)),
       outs: copy-request-arguments(request-out-args(request)),
       result: copy-any(request-result(request)),
       oneway?: request-oneway?(request),
       exceptions: request-user-exception-typecodes(request),
       context: request-context(request),
       context-expression: request-context-expression(request));
end method;

define method copy-namedvalue (result :: corba/<namedvalue>)
 => (nv :: corba/<namedvalue>)
  make(corba/<namedvalue>,
       name: corba/namedvalue/name(result),
       argument: copy-any(corba/namedvalue/argument(result)),
       arg-modes: corba/namedvalue/arg-modes(result),
       len: 0);
end method;

define method copy-any (any :: corba/<any>)
 => (any :: corba/<any>)
  make(corba/<any>, type: corba/any/type(any));
end method;

define method copy-request-arguments (nvl :: corba/<nvlist>)
 => (nvl :: corba/<nvlist>)
  let length :: <integer> = size(nvl);
  let new-nvl :: corba/<nvlist> = make(corba/<nvlist>, size: length);
  for (i from 0 below length)
    new-nvl[i] := copy-namedvalue(nvl[i])
  end for;
  new-nvl
end method;

define method compute-request (object :: corba/<object>, operation :: <string>, #key context, setter?)
 => (request :: corba/<request>)
  let interfacedef = corba/object/get-interface(object);
  if (corba/object/is-nil(interfacedef))
    error(make(<no-interface-definition>, object: object));
  end if;
  let operationdef = corba/container/lookup(interfacedef, operation);
  if (corba/object/is-nil(operationdef))
    error(make(<no-operation-definition>, interface: interfacedef, operation: operation))
  end if;
  let (result, request) = corba/object/create-request(object,
						      context,
						      operation-name(operationdef, operation, setter?: setter?),
						      operation-arguments(operationdef, setter?: setter?),
						      make(corba/<namedvalue>,
							   name: "result",
							   argument: make(corba/<any>, type: operation-result(operationdef, setter?: setter?)),
							   len: 0,
							   arg-modes: 0),
						      0);
  process-request-arguments(request);
  request
end method;

define class <argument-list-mismatch> (<simple-error>)
  keyword format-string: = "Mismatched number of arguments passed to DII call: %s. Expected %d, given %d.";
end class;

define sealed domain make (subclass(<argument-list-mismatch>));
define sealed domain initialize (<argument-list-mismatch>);

define method make (class == <argument-list-mismatch>, #key operation, expected, given, #all-keys)
 => (condition :: <argument-list-mismatch>)
  next-method(class, format-arguments: vector(operation, expected, given))
end method;

define method insert-operation-arguments (operation :: <string>, nvlist :: corba/<nvlist>, arguments :: <sequence>)
 => ()
  // ---*** handle any/value-setter errors and report in this context?
  let expected = size(nvlist);
  let given = size(arguments);
  unless (expected = given)
    error(make(<argument-list-mismatch>, operation: operation, expected: expected, given: given))
  end unless;
  for (arg in arguments,
       nv in nvlist)
    corba/any/value(corba/namedvalue/argument(nv)) := arg;
  end for;
end method;

define method extract-operation-results (result :: corba/<any>, nvlist :: corba/<nvlist>)
  apply(values,
        corba/any/value(result),
	map(method (nv)
	      corba/any/value(corba/namedvalue/argument(nv))
	    end method,
	    nvlist))
end method;  

define method operation-result (operationdef :: corba/<operationdef>, #key setter?)
 => (typecode :: corba/<typecode>)
  corba/operationdef/result(operationdef)
end method;

define method operation-result (attributedef :: corba/<attributedef>, #key setter?)
 => (typecode :: corba/<typecode>)
  if (setter?)
    corba/$void-typecode
  else
    corba/attributedef/type(attributedef)
  end if;
end method;

define method operation-name (operationdef :: corba/<operationdef>, operation :: <string>, #key setter?)
 => (name :: <string>)
  operation
end method;

define method operation-name (attributedef :: corba/<attributedef>, operation :: <string>, #key setter?)
 => (name :: <string>)
  concatenate(if (setter?) "_set_" else "_get_" end if,
	      operation)
end method;

define method operation-arguments (operationdef :: corba/<operationdef>, #key setter?)
 => (arguments :: corba/<nvlist>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  corba/orb/create-operation-list(orb, operationdef)
end method;

define method operation-arguments (attributedef :: corba/<attributedef>, #key setter?)
 => (arguments :: corba/<nvlist>)
  let nvl = make(corba/<nvlist>);
  if (setter?)
    nvl := add!(nvl,
		make(corba/<namedvalue>,
		     name: "value",
		     argument: make(corba/<any>,
				    type: corba/attributedef/type(attributedef)),
		     arg-modes: corba/$arg-in,
		     len: 0));
  end if;
  nvl
end method;




/*
Example:
Without compiling IDl into client:

  with-dii (width, height, set, get)
    width(grid);
    height(grid);
    width(grid) := 10;
    height(grid) := 10;
    set(grid, 5, 5, 99);
    get(grid, 5, 5);
  end with-dii;
*/
