Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-exception> (<dim-structure>)
end class;

define method emit-typecode-class-name (stream :: <stream>, exception :: <dim-exception>)
 => ()
  format(stream, "<exception-typecode>");
end method;

define method make-dim (node :: <ast-exception>)
 => (model :: <dim-exception>)
  make(<dim-exception>, node: node, level: *type-level*);
end method;

define method before-code-emission (exception :: <dim-exception>)
 => ()
  initialize-members(exception);
end method;  

define method after-code-emission (excepion :: <dim-exception>)
 => ()
end method;

define method protocol-exports (exception :: <dim-exception>)
 => (exports :: <sequence>)
  let exports = make(<stretchy-vector>);
  add!(exports, dim-type-native-type(exception));
  for (member :: <dim-member> in dim-type-members(exception))
    add!(exports, dim-member-getter-name(member));
  end for;
  exports;
end method;

define method emit-protocol-code (exception :: <dim-exception>, stream :: <stream>)
 => ()
  emit-class-definition(stream, exception);
end method;

define method emit-shared-code (exception :: <dim-exception>, stream :: <stream>)
 => ()
  emit-typecode-constant-definition(stream, exception);
  emit-typecode-methods(stream, exception);
end method;

define method emit-class-definition (stream :: <stream>, exception :: <dim-exception>)
 => ()
  format(stream, "define sealed class %s (CORBA/<user-exception>)\n", dim-type-native-type(exception));
  for (member :: <dim-member> in dim-type-members(exception))
    format(stream, "  slot %s :: %s, required-init-keyword: %s;\n",
	   dim-member-slot-name(member),
	   dim-type-native-type(dim-member-type(member)),
	   dim-member-init-keyword(member));
  end for;
  format(stream, "end class;\n");
  format(stream, "define sealed domain make (singleton(%s));\n", dim-type-native-type(exception));
  format(stream, "define sealed domain initialize (%s);\n\n", dim-type-native-type(exception));
end method;
