Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-typedef> (<dim-type>)
  slot dim-typedef-type :: <dim-type>;
end class;

define method initialize (typedef :: <dim-typedef>, #key node :: <ast-typedef>)
  next-method();
  typedef.dim-type-native-type := map-to-dylan-class-name(declarator-scoped-name(node));
  typedef.dim-typedef-type := dynamic-bind (*type-level* = *type-level* + 1)
                                make(<dim>, node: typedef-base-type(node));
                              end;
end method;

define method emit-typecode (stream :: <stream>, type :: <dim-typedef>)
 => ()
  emit-typecode(stream, dim-typedef-type(type));
end method;

define method emit-from-any-code (stream :: <stream>, type :: <dim-typedef>, expression :: <string>)
 => ()
  emit-from-any-code(stream, dim-typedef-type(type), expression);
end method;

define method make-dim (node :: <ast-typedef>)
 => (model :: <dim-typedef>)
  make(<dim-typedef>, node: node);
end method;

define method protocol-exports (typedef :: <dim-typedef>)
 => (exports :: <sequence>)
  vector(dim-type-native-type(typedef));
end method;

define method emit-protocol-code (typedef :: <dim-typedef>, stream :: <stream>)
 => ()
  emit-typedef-constant-definition(typedef, stream);
end method;

define method emit-typedef-constant-definition (typedef :: <dim-typedef>, stream :: <stream>)
 => ()
  format(stream, "define constant %s = %s;\n\n",
	 dim-type-native-type(typedef),
	 dim-type-native-type(dim-typedef-type(typedef)));
end method;
