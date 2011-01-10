Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <dim-parameter> (<dim>)
  slot dim-parameter-idl-name :: <string>;
  slot dim-parameter-dylan-name :: <string>;
  slot dim-parameter-type :: <dim-type>;
  constant slot dim-parameter-arg-flags :: <string> = "0";
end class;

define method make (class == <dim-parameter>, #rest keys, #key node :: <ast-argument>)
 => (parameter :: <dim-parameter>)
  let instantiable-class
    = select (argument-direction(node))
        $in-argument-direction => <dim-in-parameter>;
        $out-argument-direction => <dim-out-parameter>;
        $inout-argument-direction => <dim-inout-parameter>;
      end select;
  apply(make, instantiable-class, keys);
end method;

define method initialize (parameter :: <dim-parameter>, #key node :: <ast-field>)
  next-method();
  parameter.dim-parameter-idl-name := identifier-label(declarator-local-name(node));
  parameter.dim-parameter-dylan-name := map-to-dylan-name(parameter.dim-parameter-idl-name);
  parameter.dim-parameter-type := make(<dim>, node: field-type(node));
end method;

define method dim-parameter-arg-value (parameter :: <dim-parameter>)
 => (value :: <string>)
  dim-parameter-dylan-name(parameter);
end method;

define class <dim-in-parameter> (<dim-parameter>)
  inherited slot dim-parameter-arg-flags = "CORBA/$ARG-IN";
end class;

define class <dim-out-parameter> (<dim-parameter>)
  inherited slot dim-parameter-arg-flags = "CORBA/$ARG-OUT";
  constant slot dim-parameter-arg-value :: <string> = "#f";
end class;

define class <dim-inout-parameter> (<dim-parameter>)
  inherited slot dim-parameter-arg-flags = "CORBA/$ARG-INOUT";
end class;

define abstract class <dim-operation> (<dim>)
  slot dim-operation-idl-name :: <string>;
  slot dim-operation-parameters :: <sequence> /*---***limited(<sequence>, of: <dim-parameter>)***---*/ = #[];
  slot dim-operation-return-type :: <ast-type> = make(<ast-predefined-type>, type: $void-idl-type);
  slot dim-operation-generic-function-name :: <string>;
  constant slot dim-operation-handlers :: <sequence> = #[];
end class;

define method dim-operation-flag (operation :: <dim-operation>)
  operation-flag(dim-node(operation));
end method;

define method dim-operation-interface (operation :: <dim-operation>)
 => (interface :: <dim-interface>)
  let interface :: <ast-interface> = declarator-scope(dim-node(operation));
  make(<dim>, node: interface);
end method;

define method dim-operation-interface-class-name (operation :: <dim-operation>)
 => (class-name :: <string>)
  dim-interface-abstract-class-name(dim-operation-interface(operation));
end method;

define method dim-operation-return-type-void? (operation :: <dim-operation>)
 => (well? :: <boolean>)
  let return-type = dim-operation-return-type(operation);
  if (instance?(return-type, <ast-predefined-type>))
    instance?(predefined-type(return-type), <void-idl-type>);
  else
    #f;
  end if;
end method;

define method dim-operation-context? (operation :: <dim-operation>)
 => (well? :: <boolean>)
  #f;
end method;

define method dim-operation-context? (operation :: <dim-regular-operation>)
 => (well? :: <boolean>)
  let context = operation-context(dim-node(operation));
  context & ~empty?(context);
end method;

define class <dim-regular-operation> (<dim-operation>)
end class;

define method initialize (operation :: <dim-regular-operation>, #key node :: <ast-operation>)
  next-method();
  operation.dim-operation-idl-name := identifier-label(declarator-local-name(node));
  let parameters :: <stretchy-vector> = make(<stretchy-vector> /*---***limited(<stretchy-vector>, of: <dim-parameter>)***---*/ );
  for (parameter :: <ast-argument> in node.scope-declarators)
    add!(parameters, make(<dim-parameter>, node: parameter));
  end for;
  operation.dim-operation-parameters := parameters;
  if (operation-return-type(node))
    operation.dim-operation-return-type := operation-return-type(node);
  end if;
  let idl-scoped-name = declarator-scoped-name(node);
  operation.dim-operation-generic-function-name := map-to-dylan-name(idl-scoped-name);
end method;

define method make-dim (node :: <ast-operation>)
 => (model :: <dim-operation>)
  make(<dim-regular-operation>, node: node);
end method;

define method before-code-emission (operation :: <dim-operation>)
 => ()
  add!(*current-operations*, operation);
end method;

define method protocol-exports (operation :: <dim-operation>)
 => (exports :: <sequence>)
  vector(dim-operation-generic-function-name(operation));
end method;

define method emit-protocol-code (operation :: <dim-operation>, stream :: <stream>)
 => ()
  emit-operation-generic-function(stream, operation);
end method;

define method emit-stubs-code (operation :: <dim-operation>, stream :: <stream>)
 => ()
  emit-operation-stub-method(stream, operation);
end method;

define method emit-skeletons-code (operation :: <dim-operation>, stream :: <stream>)
 => ()
  emit-interface-invoke-operation-method(stream, operation, dim-operation-interface(operation));
end method;

define method emit-operation-generic-function (stream :: <stream>, operation :: <dim-operation>)
 => ()
  format(stream, "define open generic %s (object :: %s",
	 dim-operation-generic-function-name(operation),
         dim-operation-interface-class-name(operation));
  emit-operation-parameters(stream, operation);
  if (dim-operation-context?(operation))
    format(stream, ", #key context");
  end if;
  format(stream, ")\n");
  emit-operation-values(stream, operation);
  format(stream, ";\n\n");
end method;

define method emit-operation-stub-method-signature (stream :: <stream>, operation :: <dim-operation>)
 => ()
  format(stream, "(object :: %s", dim-interface-client-class-name(dim-operation-interface(operation)));
  emit-operation-parameters(stream, operation);
  if (dim-operation-context?(operation))
    format(stream, ", #key context = corba/orb/get-default-context(corba/orb-init(make(corba/<arg-list>), \"Functional Developer ORB\"))");
  end if;
  format(stream, ")\n");
  emit-operation-values(stream, operation);
end method;

define method emit-operation-stub-method (stream :: <stream>, operation :: <dim-operation>)
 => ()
  format(stream, "define method %s ", dim-operation-generic-function-name(operation));
  emit-operation-stub-method-signature(stream, operation);
  format(stream, "\n");
  emit-operation-add-args(stream, operation);
  emit-operation-create-request(stream, operation);
  emit-operation-add-exceptions(stream, operation);
  emit-operation-add-contexts(stream, operation);
  if (dim-operation-flag(operation) = $oneway-operation-flag)
    emit-operation-send(stream, operation);
  else
    emit-operation-invoke(stream, operation);
  end if;
  format(stream, "end method;\n\n");
end method;

define method emit-operation-parameters (stream :: <stream>, operation :: <dim-operation>)
 => ()
  for (parameter :: <dim-parameter> in dim-operation-parameters(operation))
    emit-operation-parameter(stream, parameter);
  end for;
end method;

define method emit-operation-parameter (stream :: <stream>, parameter :: <dim-parameter>)
 => ()
  let type = dim-parameter-type(parameter);
  format(stream, ", %s :: %s", dim-parameter-dylan-name(parameter), dim-type-native-type(type));
end method;

define method emit-operation-parameter (stream :: <stream>, parameter :: <dim-out-parameter>)
 => ()
end method;

define method emit-operation-values (stream :: <stream>, operation :: <dim-operation>)
 => ()
  if (dim-operation-flag(operation) = $oneway-operation-flag)
    format(stream, " => ()");
  else
    format(stream, " => (");
    let omit-comma? = #t;
    unless (dim-operation-return-type-void?(operation))
      let type = make(<dim>, node: dim-operation-return-type(operation));
      format(stream, "result :: %s", dim-type-native-type(type));
      omit-comma? := #f;
    end unless;
    for (parameter :: <dim-parameter> in dim-operation-parameters(operation))
      omit-comma? := emit-operation-value(stream, parameter, omit-comma?);
    end for;
    format(stream, ")");
  end if;
end method;

define method emit-operation-value (stream :: <stream>, parameter :: <dim-parameter>, omit-comma? :: <boolean>)
 => (omit-comma? :: <boolean>)
  let type = dim-parameter-type(parameter);
  unless (omit-comma?)
    format(stream, ", ");
  end unless;
  format(stream, "%s :: %s", dim-parameter-dylan-name(parameter), dim-type-native-type(type));
  #f;
end method;

define method emit-operation-value (stream :: <stream>, parameter :: <dim-in-parameter>, omit-comma? :: <boolean>)
 => (omit-comma? :: <boolean>)
  omit-comma?;
end method;

define method emit-operation-create-request (stream :: <stream>, operation :: <dim-operation>)
 => ()
  let type = make(<dim>, node: dim-operation-return-type(operation));
  let arg-list = "make(CORBA/<NVList>)";
  let result
    = format-to-string("make(CORBA/<NamedValue>, name: \"result\", argument: make(CORBA/<any>, type: %s), len: 0, arg-modes: 0)",
		       dim-type-typecode-as-string(type));
  let req-flags = "0";
  unless (dim-operation-context?(operation))
    format(stream, "  let context = CORBA/orb/get-default-context(CORBA/orb-init(make(CORBA/<arg-list>), \"Functional Developer ORB\"));\n");
  end unless;
  format(stream, "  let (result, request) = CORBA/Object/create-request(object, context, %=, arguments, %s, %s);\n",
	 dim-operation-idl-name(operation), result, req-flags);
end method;

define method emit-operation-add-args (stream :: <stream>, operation :: <dim-operation>)
 => ()
  format(stream, "  let arguments = make(CORBA/<NVList>);\n");
  for (parameter :: <dim-parameter> in dim-operation-parameters(operation))
    format(stream,
	     "  let %s = make(CORBA/<NamedValue>, name: %=, argument: make(CORBA/<any>, type: %s, value: %s), len: 0, arg-modes: %s);\n"
	     "  arguments := add!(arguments, %s);\n",
	   dim-parameter-dylan-name(parameter),
	   dim-parameter-idl-name(parameter),
	   dim-type-typecode-as-string(dim-parameter-type(parameter)),
	   dim-parameter-arg-value(parameter),
	   dim-parameter-arg-flags(parameter),
	   dim-parameter-dylan-name(parameter));
  end for;
end method;

define method emit-operation-add-exceptions (stream :: <stream>, operation :: <dim-operation>)
 => ()
  let node = dim-node(operation);
  let exceptions = operation-exceptions(node);
  if (exceptions & ~empty?(exceptions))
    for (exception in exceptions)
      let exception-idl-name = declarator-scoped-name(exception);
      let exception-class-name = map-to-dylan-class-name(exception-idl-name);
      format(stream, "  CORBA/Request/add-exception(request, class-typecode(%s));\n", exception-class-name);
    end for;
  end if;
end method;

define method emit-operation-add-contexts (stream :: <stream>, operation :: <dim-operation>)
 => ()
end method;

define method emit-operation-add-contexts (stream :: <stream>, operation :: <dim-regular-operation>)
 => ()
  let context-sequence = operation-context(dim-node(operation));
  if (context-sequence & ~empty?(context-sequence))
    for (context-string in context-sequence)
      format(stream, "  CORBA/Request/add-context(request, %s);\n", convert-literal(context-string));
    end for;
  end if;
end method;

define method emit-operation-send (stream :: <stream>, operation :: <dim-operation>)
 => ()
  format(stream, "  CORBA/Request/send(request, CORBA/$INV-NO-RESPONSE);\n");
  format(stream, "  values();\n");
end method;

define method emit-operation-invoke (stream :: <stream>, operation :: <dim-operation>)
 => ()
  format(stream, "  CORBA/Request/invoke(request, 0);\n");
  format(stream, "  values(");

  let omit-comma? :: <boolean> = #t;
  unless (dim-operation-return-type-void?(operation))
    let type = make(<dim>, node: dim-operation-return-type(operation));
    emit-from-any-code(stream, type, "CORBA/NamedValue/argument(result)");
    omit-comma? := #f;
  end unless;

  for (parameter :: <dim-parameter> in dim-operation-parameters(operation), count :: <integer> from 0)
    omit-comma? := emit-operation-out-argument(stream, parameter, count, omit-comma?);
  end for;

  format(stream, ");\n");
end method;

define method emit-operation-out-argument (stream :: <stream>, parameter :: <dim-parameter>, count :: <integer>, omit-comma? :: <boolean>)
 => (omit-comma? :: <boolean>)
  let type = dim-parameter-type(parameter);
  unless(omit-comma?)
    format(stream, ", ");
  end unless;
  emit-from-any-code(stream, type, format-to-string("CORBA/NamedValue/argument(arguments[%d])", count));
  #f;
end method;

define method emit-operation-out-argument (stream :: <stream>, parameter :: <dim-in-parameter>, count :: <integer>, omit-comma? :: <boolean>)
 => (omit-comma? :: <boolean>)
  omit-comma?;
end method;
