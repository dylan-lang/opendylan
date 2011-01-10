Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-interface> (<dim-named-type>)
  slot dim-interface-abstract-class-name :: <string>;
  slot dim-interface-client-class-name :: <string>;
  slot dim-interface-servant-class-name :: <string>;
  slot dim-interface-parents :: <sequence>;
  slot dim-interface-operations :: <stretchy-vector>;
end class;

define method initialize (interface :: <dim-interface>, #key node :: <ast-interface>)
  next-method();
  let class-name = map-to-dylan-interface-class-name(declarator-scoped-name(node));
  interface.dim-type-native-type := class-name;
  interface.dim-interface-abstract-class-name := class-name;
  interface.dim-interface-client-class-name
    := map-to-dylan-reference-class-name(declarator-scoped-name(node));
  interface.dim-interface-servant-class-name
    := map-to-dylan-servant-class-name(declarator-scoped-name(node));
  let parent-models = map(curry(make, <dim>, node:), interface-inherits(node));
  interface.dim-interface-parents := parent-models;
  dim-interface-sort-parents(interface);
  interface.dim-interface-operations := make(<stretchy-vector>);
end method;

define method dim-interface-sort-parents (interface :: <dim-interface>)
 => ()
  local method compare-interfaces (interface1, interface2)
	  interface1.dim-interface-abstract-class-name
	    < interface2.dim-interface-abstract-class-name
	end method;
  interface.dim-interface-parents
    := sort!(interface.dim-interface-parents, test: compare-interfaces)
end method;

define method dim-interface-parent-names (interface :: <dim-interface>)
 => (names :: <sequence>)
  let parent-models = dim-interface-parents(interface);
  if (empty?(parent-models))
    #["<object>"];
  else
    map(dim-interface-abstract-class-name, parent-models);
  end if;
end method;

define method dim-interface-parent-client-names (interface :: <dim-interface>)
 => (names :: <sequence>)
  let parent-models = dim-interface-parents(interface);
  if (empty?(parent-models))
    #["<object-reference>"];
  else
    map(dim-interface-client-class-name, parent-models);
  end if;
end method;

define method dim-interface-parent-servant-names (interface :: <dim-interface>)
 => (names :: <sequence>)
  let parent-models = dim-interface-parents(interface);
  if (empty?(parent-models))
    #["PortableServer/<servant>"];
  else
    map(dim-interface-servant-class-name, parent-models);
  end if;
end method;

define method emit-typecode-class-name (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream, "<object-reference-typecode>");
end method;

define method emit-from-any-code (stream :: <stream>, type :: <dim-interface>, expression :: <string>)
 => ()
  format(stream, "as(%s, CORBA/any/value(%s))", dim-type-native-type(type), expression);
end method;


// VARIABLES
define thread variable *current-operations* :: <stretchy-vector> = make(<stretchy-vector>);

define method make-dim (node :: <ast-interface>)
 => (model :: <dim-interface>)
  make(<dim-interface>, node: node);
end method;

define method before-code-emission (interface :: <dim-interface>)
 => ()
  *current-operations* := dim-interface-operations(interface);
end method;

define method protocol-exports (interface :: <dim-interface>)
 => (exports :: <sequence>)
  vector(dim-interface-abstract-class-name(interface));
end method;

define method stubs-exports (interface :: <dim-interface>)
 => (exports :: <sequence>)
  vector(dim-interface-client-class-name(interface));
end method;

define method skeletons-exports (interface :: <dim-interface>)
 => (exports :: <sequence>)
  vector(dim-interface-servant-class-name(interface));
end method;

define method emit-protocol-code (interface :: <dim-interface>, stream :: <stream>)
 => ()
  emit-interface-abstract-class-definition(stream, interface);
  emit-typecode-methods(stream, interface);
end method;

define method emit-shared-code (interface :: <dim-interface>, stream :: <stream>)
 => ()
  let interface-class-name = dim-interface-abstract-class-name(interface);
  if (member?(interface-class-name, *forward*, test: \=))
    remove!(*forward*, interface-class-name, test: \=);
  else
    emit-typecode-constant-definition(stream, interface);
  end if;
end method;

define method emit-stubs-code (interface :: <dim-interface>, stream :: <stream>)
 => ()
  emit-interface-reference-class-definition(stream, interface);
  emit-interface-make-method(stream, interface);
  emit-interface-reference-class-coercion-method(stream, interface);
  emit-from-any-method(stream, interface);
end method;

define method emit-skeletons-code (interface :: <dim-interface>, stream :: <stream>)
 => ()
  emit-interface-servant-class-definition(stream, interface);
end method;

define method emit-interface-abstract-class-definition (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream, "define open abstract class %s (", dim-interface-abstract-class-name(interface));
  print-separated-collection(dim-interface-parent-names(interface), ", ", stream,
                             printer: method (object, stream) format(stream, "%s", object) end);
  format(stream, ")\n");
  format(stream, "end class;\n\n");
end method;

define method emit-interface-reference-class-definition (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream, "define class %s (%s, ", dim-interface-client-class-name(interface), dim-interface-abstract-class-name(interface));
  print-separated-collection(dim-interface-parent-client-names(interface), ", ", stream,
                             printer: method (object, stream) format(stream, "%s", object) end);
  format(stream, ")\nend class;\n\n");
end method;

define method emit-interface-servant-class-definition (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream, "define open abstract class %s (%s, ",
	 dim-interface-servant-class-name(interface), dim-interface-abstract-class-name(interface));
  print-separated-collection(dim-interface-parent-servant-names(interface), ", ", stream,
                             printer: method (object, stream) format(stream, "%s", object) end);
  format(stream, ")\nend class;\n\n");
end method;

/* --- Not currently used
define method emit-interface-invoke-operation-methods (stream :: <stream>, interface :: <dim-interface>)
 => ()
  for (operation :: <dim-operation> in dim-interface-operations(interface))
    emit-interface-invoke-operation-method(stream, operation, interface);
  end for;
end method;
*/

define method emit-interface-invoke-operation-method (stream :: <stream>, operation :: <dim-operation>, interface :: <dim-interface>)
 => ()
  format(stream, "define method invoke-operation (object :: %s, request :: CORBA/<ServerRequest>, operation == #\"%s\")\n",
         dim-interface-servant-class-name(interface), dim-operation-idl-name(operation));
  format(stream, " => ()\n");

  format(stream, "  let arguments = make(CORBA/<NVList>);\n");
  for (parameter :: <dim-parameter> in dim-operation-parameters(operation))
    let name = dim-parameter-dylan-name(parameter);
    let arg-modes = dim-parameter-arg-flags(parameter);
    let type = dim-parameter-type(parameter);
    format(stream, "  let %s = make(CORBA/<NamedValue>, name: %=, argument: make(CORBA/<any>, type: %s), len: 0, arg-modes: %s);\n",
           name, name, dim-type-typecode-as-string(type), arg-modes);
    format(stream, "  arguments := add!(arguments, %s);\n", name);
  end for;
  format(stream, "  arguments := CORBA/ServerRequest/arguments(request, arguments);\n");

  for (exception in dim-operation-handlers(operation))
    format(stream, "  let handler %s = method (cc, next-handler)\n", map-to-dylan-class-name(declarator-scoped-name(exception)));
    format(stream, "                     CORBA/ServerRequest/set-exception(server-request, exception-to-any(cc));\n");
    format(stream, "                   end method;\n");
  end for;

  if (dim-operation-context?(operation))
    format(stream, "  let context = CORBA/ServerRequest/ctx(request);\n");
    emit-interface-invoke-method-servant-call(stream, operation, interface, context: "context");
  else
    emit-interface-invoke-method-servant-call(stream, operation, interface);
  end if;
  unless (dim-operation-return-type-void?(operation))
    let return-type-node = dim-operation-return-type(operation);
    let type = make(<dim>, node: return-type-node);
    format(stream, "  CORBA/ServerRequest/set-result(request, make(CORBA/<any>, type: %s, value: results[0]));\n",
           dim-type-typecode-as-string(type));
  end unless;

  let parameters = dim-operation-parameters(operation);
  let result-index = if (dim-operation-return-type-void?(operation)) 0 else 1 end;
  for (index :: <integer> from 0 below size(parameters))
    result-index := emit-interface-invoke-method-set-out-arg(stream, parameters[index], index, result-index);
  end for;
  format(stream, "end method;\n\n");
end method;

define method emit-interface-invoke-method-servant-call (stream :: <stream>, operation :: <dim-operation>, interface :: <dim-interface>, #key context :: false-or(<string>) = #f)
 => ()
  format(stream, "  let (#rest results) = %s(object", dim-operation-generic-function-name(operation));
  let parameters = dim-operation-parameters(operation);
  for (index :: <integer> from 0 below size(parameters))
    emit-interface-invoke-method-servant-call-in-arg(stream, parameters[index], index);
  end for;
  if (context)
    format(stream, ", context: %s", context);
  end if;
  format(stream, ");\n");
end method;

define method emit-interface-invoke-method-servant-call-in-arg (stream :: <stream>, parameter :: <dim-parameter>, index :: <integer>)
 => ()
  let type = dim-parameter-type(parameter);
  format(stream, ", ");
  emit-from-any-code(stream, type, format-to-string("CORBA/NamedValue/argument(arguments[%d])", index));
end method;

define method emit-interface-invoke-method-servant-call-in-arg (stream :: <stream>, parameter :: <dim-out-parameter>, index :: <integer>)
 => ()
end method;

define method emit-interface-invoke-method-set-out-arg (stream :: <stream>, parameter :: <dim-parameter>, index :: <integer>, result-index :: <integer>)
 => (next-result-index :: <integer>)
  format(stream,
	   "  arguments[%d] := make(CORBA/<NamedValue>, name: CORBA/NamedValue/name(arguments[%d]), "
	   "argument: make(CORBA/<any>, type: %s, value: results[%d]), "
	   "len: CORBA/NamedValue/len(arguments[%d]), arg-modes: CORBA/NamedValue/arg-modes(arguments[%d]));\n",
	 index, index, dim-type-typecode-as-string(dim-parameter-type(parameter)), result-index, index, index);
  result-index + 1;
end method;

define method emit-interface-invoke-method-set-out-arg (stream :: <stream>, parameter :: <dim-in-parameter>, index :: <integer>, result-index :: <integer>)
 => (next-result-index :: <integer>)
  result-index;
end method;

define method emit-class-typecode-method (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream,
	   "define method class-typecode (class :: subclass(%s))\n"
	   " => (typecode :: <object-reference-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-interface-abstract-class-name(interface),
	 dim-type-typecode-constant-name(interface));
end method;

define method emit-object-typecode-method (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream,
	   "define method object-typecode (object :: %s)\n"
	   " => (typecode :: <object-reference-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-interface-abstract-class-name(interface),
	 dim-type-typecode-constant-name(interface));
end method;

define method emit-interface-make-method (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream,
	   "define sideways method make (class == %s, #rest initargs, #key)\n"
	   " => (object :: %s)\n"
	   "  apply(make, %s, initargs);\n"
	   "end method;\n\n",
	 dim-interface-abstract-class-name(interface),
	 dim-interface-client-class-name(interface),
	 dim-interface-client-class-name(interface));
end method;

define method emit-interface-reference-class-coercion-method (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream,
	   "define sideways method as (class == %s, object :: <object-reference>)\n"
	   " => (object :: %s)\n"
	   "  as(%s, object);\n"
	   "end method;\n\n",
	 dim-interface-abstract-class-name(interface),
	 dim-interface-client-class-name(interface),
	 dim-interface-client-class-name(interface));

  format(stream,
	   "define method as (class == %s, object :: %s)\n"
	   " => (object :: %s)\n"
	   "  object;\n"
	   "end method;\n\n",
	 dim-interface-abstract-class-name(interface),
	 dim-interface-client-class-name(interface),
	 dim-interface-client-class-name(interface));
end method;

define method emit-from-any-method (stream :: <stream>, interface :: <dim-interface>)
 => ()
  format(stream,
	   "define sideways method as (class == %s, object :: CORBA/<any>)\n"
	   " => (object :: %s)\n"
	   "  as(%s, object);\n"
	   "end method;\n\n",
	 dim-interface-abstract-class-name(interface),
	 dim-interface-client-class-name(interface),
	 dim-interface-client-class-name(interface));
end method;
