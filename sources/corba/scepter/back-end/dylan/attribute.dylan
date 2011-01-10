Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <dim-attribute> (<dim>)
  slot dim-attribute-type :: <dim-type>;
end class;

define method initialize (attribute :: <dim-attribute>, #key node :: <ast-attribute>)
  next-method();
  let type = field-type(node);
  attribute.dim-attribute-type := make(<dim>, node: type);
end method;

define method dim-attribute-interface (attribute :: <dim-attribute>)
 => (interface :: <dim-interface>)
  let interface :: <ast-interface> = declarator-scope(dim-node(attribute));
  make(<dim>, node: interface);
end method;

define method dim-attribute-interface-class-name (attribute :: <dim-attribute>)
 => (class-name :: <string>)
  dim-interface-abstract-class-name(dim-attribute-interface(attribute));
end method;

define class <dim-read-only-attribute> (<dim-attribute>)
  slot dim-attribute-getter-name :: <string>;
  slot dim-attribute-get-operation-name :: <string>;
  slot dim-attribute-get-operation :: <dim-operation>;
end class;

define method initialize (attribute :: <dim-read-only-attribute>, #key node :: <ast-attribute>)
  next-method();
  let idl-scoped-name = declarator-scoped-name(node);
  let dylan-scoped-name = map-to-dylan-name(idl-scoped-name);
  attribute.dim-attribute-getter-name := dylan-scoped-name;
  let local-name = identifier-label(declarator-local-name(node));
  let get-operation-name = concatenate("_get_", local-name);
  attribute.dim-attribute-get-operation-name := get-operation-name;
  attribute.dim-attribute-get-operation := make(<dim-attribute-get-operation>, node: node, attribute: attribute);
end method;

define class <dim-read-write-attribute> (<dim-read-only-attribute>)
  slot dim-attribute-setter-name :: <string>;
  slot dim-attribute-set-operation-name :: <string>;
  slot dim-attribute-set-operation :: <dim-operation>;
end class;

define sealed method initialize (attribute :: <dim-read-write-attribute>, #key node :: <ast-attribute>)
  next-method();
  let getter-name = attribute.dim-attribute-getter-name;
  let setter-name = concatenate(getter-name, "-setter");
  attribute.dim-attribute-setter-name := setter-name;
  let local-name = identifier-label(declarator-local-name(node));
  let set-operation-name = concatenate("_set_", local-name);
  attribute.dim-attribute-set-operation-name := set-operation-name;
  attribute.dim-attribute-set-operation := make(<dim-attribute-set-operation>, node: node, attribute: attribute);
end method;

define abstract class <dim-attribute-accessor-operation> (<dim-operation>)
  constant slot dim-attribute-accessor-attribute :: <dim-attribute>, required-init-keyword: attribute:;
end class;

define class <dim-attribute-get-operation> (<dim-attribute-accessor-operation>)
end class;

define method initialize (operation :: <dim-attribute-get-operation>, #key node :: <ast-attribute>, attribute :: <dim-attribute>)
  next-method();
//  let attribute = dim-attribute-accessor-attribute(operation);
  operation.dim-operation-idl-name := dim-attribute-get-operation-name(attribute);
  operation.dim-operation-return-type := field-type(node);
  operation.dim-operation-generic-function-name := dim-attribute-getter-name(attribute);
end method;

define class <dim-attribute-set-operation> (<dim-attribute-accessor-operation>)
end class;

define method initialize (operation :: <dim-attribute-set-operation>, #key node :: <ast-attribute>, attribute :: <dim-attribute>)
  next-method();
//  let attribute = dim-attribute-accessor-attribute(operation);
  operation.dim-operation-idl-name := dim-attribute-set-operation-name(attribute);
  operation.dim-operation-parameters := vector(make(<dim-in-parameter>, node: node));
  operation.dim-operation-generic-function-name := dim-attribute-setter-name(attribute);
end method;


define method make-dim (node :: <ast-attribute>)
 => (model :: <dim-attribute>)
  if (attribute-read-only?(node))
    make(<dim-read-only-attribute>, node: node);
  else
    make(<dim-read-write-attribute>, node: node);
  end if;
end method;

define method after-code-emission (attribute :: <dim-read-only-attribute>)
 => ()
  add!(*current-operations*, dim-attribute-get-operation(attribute));
end method;

define method after-code-emission (attribute :: <dim-read-write-attribute>)
 => ()
  next-method();
  add!(*current-operations*, dim-attribute-set-operation(attribute));
end method;

define method emit-protocol-code (attribute :: <dim-read-only-attribute>, stream :: <stream>)
 => ()
  emit-protocol-code(dim-attribute-get-operation(attribute), stream);
end method;

define method emit-shared-code (attribute :: <dim-read-only-attribute>, stream :: <stream>)
 => ()
  emit-shared-code(dim-attribute-get-operation(attribute), stream);
end method;

define method emit-stubs-code (attribute :: <dim-read-only-attribute>, stream :: <stream>)
 => ()
  emit-stubs-code(dim-attribute-get-operation(attribute), stream);
end method;

define method emit-skeletons-code (attribute :: <dim-read-only-attribute>, stream :: <stream>)
 => ()
  emit-skeletons-code(dim-attribute-get-operation(attribute), stream);
end method;

define method emit-protocol-code (attribute :: <dim-read-write-attribute>, stream :: <stream>)
 => ()
  next-method();
  emit-protocol-code(dim-attribute-set-operation(attribute), stream);
end method;

define method emit-shared-code (attribute :: <dim-read-write-attribute>, stream :: <stream>)
 => ()
  next-method();
  emit-shared-code(dim-attribute-set-operation(attribute), stream);
end method;

define method emit-stubs-code (attribute :: <dim-read-write-attribute>, stream :: <stream>)
 => ()
  next-method();
  emit-stubs-code(dim-attribute-set-operation(attribute), stream);
end method;

define method emit-skeletons-code (attribute :: <dim-read-write-attribute>, stream :: <stream>)
 => ()
  next-method();
  emit-skeletons-code(dim-attribute-set-operation(attribute), stream);
end method;

define method protocol-exports (attribute :: <dim-read-only-attribute>)
 => (exports :: <sequence>)
  protocol-exports(dim-attribute-get-operation(attribute));
end method;

define method shared-exports (attribute :: <dim-read-only-attribute>)
 => (exports :: <sequence>)
  shared-exports(dim-attribute-get-operation(attribute));
end method;

define method stubs-exports (attribute :: <dim-read-only-attribute>)
 => (exports :: <sequence>)
  stubs-exports(dim-attribute-get-operation(attribute));
end method;

define method skeletons-exports (attribute :: <dim-read-only-attribute>)
 => (exports :: <sequence>)
  skeletons-exports(dim-attribute-get-operation(attribute));
end method;

define method protocol-exports (attribute :: <dim-read-write-attribute>)
 => (exports :: <sequence>)
  concatenate(next-method(), protocol-exports(dim-attribute-set-operation(attribute)));
end method;

define method shared-exports (attribute :: <dim-read-write-attribute>)
 => (exports :: <sequence>)
  concatenate(next-method(), shared-exports(dim-attribute-set-operation(attribute)));
end method;

define method stubs-exports (attribute :: <dim-read-write-attribute>)
 => (exports :: <sequence>)
  concatenate(next-method(), stubs-exports(dim-attribute-set-operation(attribute)));
end method;

define method skeletons-exports (attribute :: <dim-read-write-attribute>)
 => (exports :: <sequence>)
  concatenate(next-method(), skeletons-exports(dim-attribute-set-operation(attribute)));
end method;

define method emit-attribute-getter-signature (attribute :: <dim-read-only-attribute>, stream :: <stream>)
 => ()
  format(stream, "(object :: %s)\n", dim-attribute-interface-class-name(attribute));
  let type = dim-attribute-type(attribute);
  format(stream, " => (result :: %s)", dim-type-native-type(type));
end method;

define method emit-operation-generic-function (stream :: <stream>, operation :: <dim-attribute-get-operation>)
 => ()
  let attribute = dim-attribute-accessor-attribute(operation);
  format(stream, "define open generic %s ", dim-attribute-getter-name(attribute));
  emit-attribute-getter-signature(attribute, stream);
  format(stream, ";\n\n");
end method;

define method emit-operation-stub-method (stream :: <stream>, operation :: <dim-attribute-get-operation>)
 => ()
  let attribute = dim-attribute-accessor-attribute(operation);
  format(stream, "define method %s ", dim-attribute-getter-name(attribute));
  format(stream, "(object :: %s)\n", dim-interface-client-class-name(dim-attribute-interface(attribute)));
  format(stream, " => (result :: %s)\n", dim-type-native-type(dim-attribute-type(attribute)));
  format(stream, "  let context = CORBA/orb/get-default-context(CORBA/orb-init(make(CORBA/<arg-list>), \"Functional Developer ORB\"));\n");
  let type = dim-attribute-type(attribute);
  let result = format-to-string("make(CORBA/<NamedValue>, name: \"result\", argument: make(CORBA/<any>, type: %s), len: 0, arg-modes: 0)",
                                dim-type-typecode-as-string(type));
  format(stream, "  let (result, request) = CORBA/Object/create-request(object, context, %=, make(CORBA/<NVList>), %s, 0);\n",
	 dim-attribute-get-operation-name(attribute),
         result);
  format(stream, "  CORBA/Request/invoke(request, 0);\n");
  emit-from-any-code(stream, type, "CORBA/NamedValue/argument(result)");
  format(stream, ";\n");
  format(stream, "end method;\n\n");
end method;

define method emit-attribute-setter-signature (attribute :: <dim-read-write-attribute>, stream :: <stream>)
 => ()
  let type = dim-attribute-type(attribute);
  format(stream, "(new-value :: %s, object :: %s)\n",
         dim-type-native-type(type),
         dim-attribute-interface-class-name(attribute));
  format(stream, " => (result :: %s)", dim-type-native-type(type));
end method;

define method emit-operation-generic-function (stream :: <stream>, operation :: <dim-attribute-set-operation>)
 => ()
  let attribute = dim-attribute-accessor-attribute(operation);
  let type = dim-attribute-type(attribute);
  format(stream, "define open generic %s ", dim-attribute-setter-name(attribute));
  emit-attribute-setter-signature(attribute, stream);
  format(stream, ";\n\n");
end method;

define method emit-operation-stub-method (stream :: <stream>, operation :: <dim-attribute-set-operation>)
 => ()
  let attribute = dim-attribute-accessor-attribute(operation);
  format(stream, "define method %s ", dim-attribute-setter-name(attribute));
  format(stream, "(new-value :: %s, object :: %s)\n",
	 dim-type-native-type(dim-attribute-type(attribute)),
	 dim-interface-client-class-name(dim-attribute-interface(attribute)));
  format(stream, " => (result :: %s)\n", dim-type-native-type(dim-attribute-type(attribute)));
  format(stream, "  let context = CORBA/orb/get-default-context(CORBA/orb-init(make(CORBA/<arg-list>), \"Functional Developer ORB\"));\n");
  let type = dim-attribute-type(attribute);
  let result = "make(CORBA/<NamedValue>, name: \"result\", argument: make(CORBA/<any>, type: make(<void-typecode>)), len: 0, arg-modes: 0)";
  format(stream, "  let (result, request) = CORBA/Object/create-request(object, context, %=, make(CORBA/<NVList>), %s, 0);\n",
	 dim-attribute-set-operation-name(attribute),
         result);
  format(stream, "  CORBA/Request/add-arg(request, \"value\", %s, new-value, 0, CORBA/$ARG-IN);\n", dim-type-typecode-as-string(type));
  format(stream, "  CORBA/Request/invoke(request, 0);\n");
  format(stream, "  new-value;\n");
  format(stream, "end method;\n\n");
end method;

define method emit-interface-invoke-method-servant-call (stream :: <stream>, operation :: <dim-attribute-set-operation>, interface :: <dim-interface>, #key context :: false-or(<string>) = #f)
 => ()
  format(stream, "  %s(", dim-operation-generic-function-name(operation));
  let parameters = dim-operation-parameters(operation);
  let type = dim-parameter-type(parameters[0]);
  emit-from-any-code(stream, type, "CORBA/NamedValue/argument(arguments[0])");
  format(stream, ", object);\n");
end method;
