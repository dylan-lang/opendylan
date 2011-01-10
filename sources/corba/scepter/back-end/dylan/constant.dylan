Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-constant> (<dim>)
  slot dim-constant-name :: <string> = "";
end class;

define method initialize (constant :: <dim-constant>, #key node :: <ast-constant>)
  next-method();
  constant.dim-constant-name := map-to-dylan-constant-name(declarator-scoped-name(node));
end method;

define method dim-constant-dylan-type-string (constant :: <dim-constant>)
 => (type :: <string>)
  let type = constant-expression-type(dim-node(constant));
  let type-model = make(<dim>, node: type);
  dim-type-native-type(type-model);
end method;

define method dim-constant-dylan-value-string (constant :: <dim-constant>)
 => (value :: <string>)
  let expression = constant-value(dim-node(constant));
  convert-const-exp(expression);
//  let value = expression-value(expression);
//  format-to-string("%=", value);
end method;

define method make-dim (node :: <ast-constant>)
 => (model :: <dim-constant>)
  make(<dim-constant>, node: node);
end method;

define method emit-protocol-code (constant :: <dim-constant>, stream :: <stream>)
 => ()
  emit-constant-definition(constant, stream);
end method;

define method protocol-exports (constant :: <dim-constant>)
 => (exports :: <sequence>)
  vector(dim-constant-name(constant));
end method;

define method emit-constant-definition (constant :: <dim-constant>, stream :: <stream>)
  => ()
  format(stream, "define constant %s :: %s = %s;\n\n",
	 dim-constant-name(constant),
	 dim-constant-dylan-type-string(constant),
	 dim-constant-dylan-value-string(constant));
end method;

