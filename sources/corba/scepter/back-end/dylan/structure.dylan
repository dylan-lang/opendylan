Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-recursive-type-mixin> (<object>)
  constant slot dim-recursive-type-level :: <integer>, required-init-keyword: level:;
end class;

define class <dim-structure> (<dim-membered-type>, <dim-recursive-type-mixin>)
end class;

define method initialize (structure :: <dim-structure>, #key node :: <ast-structure>)
  next-method();
  structure.dim-type-native-type := map-to-dylan-class-name(declarator-scoped-name(node));
end method;

define method initialize-members (structure :: <dim-structure>)
  let members = scope-declarators(dim-node(structure));
  structure.dim-type-members := map(method (member) make(<dim-member>, node: member) end, members);
end method;

define class <dim-member> (<dim>)
  slot dim-member-slot-name :: <string>;
  slot dim-member-init-keyword :: <string>;
  slot dim-member-type :: <dim-type>;
  slot dim-member-getter-name :: <string>;
  slot dim-member-setter-name :: <string>;
end class;

define method initialize (member :: <dim-member>, #key node :: <ast-field>)
  next-method();
  let idl-scoped-name = declarator-scoped-name(node);
  let slot-name = map-to-dylan-name(idl-scoped-name);
  member.dim-member-slot-name := slot-name;
  member.dim-member-init-keyword := concatenate(dim-dylan-local-name(member), ":");
  member.dim-member-type
    := dynamic-bind (*type-level* = *type-level* + 1)
         make(<dim>, node: field-type(node));
       end;
  member.dim-member-getter-name := slot-name;
  member.dim-member-setter-name := concatenate(slot-name, "-setter");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <dim-structure>)
 => ()
  format(stream, "<struct-typecode>");
end method;

define method emit-typecode-member (stream :: <stream>, member :: <dim-member>)
 => ()
  format(stream, "make(<typecode-member>, name: %=, type: %s, getter: %s, setter: %s, init-keyword: %s, typecode: ",
	 dim-idl-local-name(member),
	 dim-type-native-type(dim-member-type(member)),
	 dim-member-getter-name(member),
	 dim-member-setter-name(member),
	 dim-member-init-keyword(member));
  emit-typecode(stream, dim-member-type(member));
  format(stream, ")");
end method;

define thread variable *recursive-types* :: <list> = #();

define method make-dim (node :: <ast-structure>)
 => (model :: <dim-structure>)
  make(<dim-structure>, node: node, level: *type-level*);
end method;

define method before-code-emission (structure :: <dim-structure>)
 => ()
  *recursive-types* := pair(structure, *recursive-types*);
  initialize-members(structure);
end method;

define method after-code-emission (structure :: <dim-structure>)
 => ()
  *recursive-types* := tail(*recursive-types*);
end method;

define method protocol-exports (structure :: <dim-structure>)
 => (exports :: <sequence>)
  let exports = make(<stretchy-vector>);
  add!(exports, dim-type-native-type(structure));
  for (member :: <dim-member> in dim-type-members(structure))
    add!(exports, dim-member-getter-name(member));
    add!(exports, dim-member-setter-name(member));
  end for;
  exports;
end method;

define method emit-protocol-code (structure :: <dim-structure>, stream :: <stream>)
 => ()
  emit-class-definition(stream, structure);
end method;

define method emit-shared-code (structure :: <dim-structure>, stream :: <stream>)
 => ()
  emit-typecode-constant-definition(stream, structure);
  emit-typecode-methods(stream, structure);
end method;

define method emit-class-definition (stream :: <stream>, structure :: <dim-structure>)
 => ()
  let class-name = dim-type-native-type(structure);
  format(stream, "define sealed class %s (CORBA/<struct>)\n", class-name);
  for (member :: <dim-member> in dim-type-members(structure))
    format(stream, "  slot %s :: %s, required-init-keyword: %s;\n",
	   dim-member-slot-name(member),
	   dim-type-native-type(dim-member-type(member)),
	   dim-member-init-keyword(member));
  end for;
  format(stream, "end class;\n");
  format(stream, "define sealed domain make (singleton(%s));\n", class-name);
  format(stream, "define sealed domain initialize (%s);\n\n", class-name);
end method;

define method emit-class-typecode-method (stream :: <stream>, structure :: <dim-structure>)
 => ()
  format(stream,
	   "define method class-typecode (class == %s)\n"
	   " => (typecode :: <struct-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(structure),
	 dim-type-typecode-constant-name(structure));
end method;

define method emit-object-typecode-method (stream :: <stream>, structure :: <dim-structure>)
 => ()
  format(stream,
	   "define method object-typecode (object :: %s)\n"
	   " => (typecode :: <struct-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(structure),
	 dim-type-typecode-constant-name(structure));
end method;
