Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-branch> (<dim-member>)
end class;

define method dim-branch-default? (branch :: <dim-branch>)
 => (well? :: <boolean>)
  let labels = union-branch-labels(dim-node(branch));
  any?(rcurry(instance?, <ast-default-union-branch-label>), labels);
end method;

define method dim-branch-local-variable-name (branch :: <dim-branch>)
 => (name :: <string>)
  dim-dylan-local-name(branch);
end method;

define method dim-branch-switch-value (branch :: <dim-branch>)
 => (value :: <string>)
  block (return)
    for (label in union-branch-labels(branch.dim-node))
      if (~instance?(label, <ast-default-union-branch-label>))
	return(union-branch-label-discriminator(label));
      end if;
    end for;
    union-branch-default-discriminator(branch);
  end;
end method;

define method union-branch-label-discriminator (label :: <ast-union-branch-label>)
 => (value :: <string>)
  let label-value = label.union-branch-label-value;
  if (instance?(label-value.expression-combinator, <symbol-combinator>))
    format-to-string("%s", map-to-dylan-symbol(label-value.expression-scoped-name.last));
  else
    format-to-string("%=", label-value.expression-value);
  end if;    
end method;

define method union-branch-label-discriminator (label :: <ast-default-union-branch-label>)
 => (value :: <string>)
  "otherwise";
end method;

define method union-branch-default-discriminator (branch :: <dim-branch>)
 => (value :: <string>)
  let union = declarator-scope(branch.dim-node);
  let type = union-discriminator-type(union);
  find-unused-value(type, union.scope-declarators);
end method;

define method find-unused-value (type :: <ast-predefined-type>, branches :: <collection>)
 => (unused-value :: <string>)
  find-unused-value(type.predefined-type, branches);
end method;

define method find-unused-value (type :: <real-idl-type>, branches :: <collection>)
 => (unused-value :: <string>)
  let labels = apply(concatenate, map(union-branch-labels, branches));
  let values = label-values(labels);
  block (return)
    for (n from type.range-max to type.range-min by -1)
      unless (member?(n, values))
	return(format-to-string("%=", n));
      end unless;
    end for;
    error("No free numbers for default union discriminator");
  end;
end method;

define method find-unused-value (type :: <char-idl-type>, branches :: <collection>)
 => (unused-value :: <string>)
  let labels = apply(concatenate, map(union-branch-labels, branches));
  let values = label-values(labels);
  block (return)
    for (i from 255 to 0 by -1)
      let c = as(<character>, i);
      unless (member?(c, values))
	return(format-to-string("as(<character>, %d)", i));
      end unless;
    end for;
    error("No free characters for default union discriminator");
  end;
end method;

define method find-unused-value (type :: <boolean-idl-type>, branches :: <collection>)
 => (unused-value :: <string>)
  let labels = apply(concatenate, map(union-branch-labels, branches));
  let values = label-values(labels);
  if (member?(#f, values))
    if (member?(#t, values))
      error("No free booleans for default union discriminator");
    else
      "#t";
    end if;
  else
    "#f";
  end if;
end method;

define method find-unused-value (type :: <ast-enum>, values :: <collection>)
 => (unused-value :: <string>)
  block (return)
    let enum = make(<dim>, node: type);
    for (symbol in enum.dim-type-members)
      unless (member?(symbol, values))
	return(format-to-string("%s", symbol));
      end unless;
    end for;
  end;
end method;

define method label-values (labels :: <sequence> /*---***limited(<sequence>, of: <ast-generic-union-branch-label>)***---*/ )
 => (values :: <sequence>)
  map(method (label :: <ast-generic-union-branch-label>)
       => (value :: <object>)
	if (instance?(label, <ast-default-union-branch-label>))
	  list(#"default");
	else
	  expression-value(label.union-branch-label-value);
	end if;
      end method,
      labels);
end method;

define class <dim-union> (<dim-membered-type>, <dim-recursive-type-mixin>)
  slot dim-union-default-position :: <integer> = -1;
  slot dim-union-discriminator-type :: <dim-type>;
end class;

define method initialize (union :: <dim-union>, #key node :: <ast-union>)
  next-method();
  union.dim-type-native-type := map-to-dylan-class-name(declarator-scoped-name(node));
  union.dim-union-discriminator-type := make(<dim>, node: union-discriminator-type(node));
end method;

define method initialize-members (union :: <dim-union>)
  let members = scope-declarators(dim-node(union));
  union.dim-type-members := map(method (member) make(<dim-branch>, node: member) end, members);
  for (branch :: <dim-branch> in union.dim-type-members, current-position :: <integer> from 0)
    if (dim-branch-default?(branch))
      union.dim-union-default-position := current-position;
    end if;
  end for;
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <dim-union>)
 => ()
  format(stream, "<union-typecode>");
end method;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-union>)
 => ()
  next-method();
  format(stream, ", default-used: %d, discriminator: ", dim-union-default-position(type));
  emit-typecode(stream, dim-union-discriminator-type(type));
end method;

define method emit-typecode-member (stream :: <stream>, member :: <dim-branch>)
 => ()
  let labels = union-branch-labels(member.dim-node);
  format(stream, "make(<typecode-branch>, name: %=, type: %s, getter: %s, setter: %s, init-keyword: %s, label-value: %s, typecode: ",
	 dim-idl-local-name(member),
	 dim-type-native-type(dim-member-type(member)),
	 dim-member-getter-name(member),
	 dim-member-setter-name(member),
	 dim-member-init-keyword(member),
	 dim-branch-switch-value(member));
  emit-typecode(stream, dim-member-type(member));
  format(stream, ")");
end method;

define method make-dim (node :: <ast-union>)
 => (model :: <dim-union>)
  make(<dim-union>, node: node, level: *type-level*);
end method;

define method before-code-emission (union :: <dim-union>)
 => ()
  *recursive-types* := pair(union, *recursive-types*);
  initialize-members(union);
end method;

define method after-code-emission (union :: <dim-union>)
 => ()
  *recursive-types* := tail(*recursive-types*);
end method;

define method protocol-exports (union :: <dim-union>)
 => (exports :: <sequence>)
  let exports = make(<stretchy-vector>);
  add!(exports, dim-type-native-type(union));
  for (branch :: <dim-branch> in dim-type-members(union))
    add!(exports, dim-member-getter-name(branch));
    add!(exports, dim-member-setter-name(branch));
  end for;
  exports;
end method;

define method emit-protocol-code (union :: <dim-union>, stream :: <stream>)
 => ()
  emit-class-definition(stream, union);
  emit-make-method(stream, union);
  for (branch :: <dim-branch> in dim-type-members(union))
    emit-union-getter-generic-function(stream, union, branch);
    emit-union-setter-generic-function(stream, union, branch);
  end for;
end method;

define method emit-shared-code (union :: <dim-union>, stream :: <stream>)
 => ()
  emit-typecode-constant-definition(stream, union);
  emit-typecode-methods(stream, union);

  for (branch :: <dim-branch> in dim-type-members(union))
    emit-union-branch-is-method(stream, union, branch);
    emit-union-getter-method(stream, union, branch);
    emit-union-setter-method(stream, union, branch);
  end for;

  let types = make(<table>);
  let clauses = make(<stretchy-vector>);
  for (branch :: <dim-branch> in dim-type-members(union))
    let type = dim-member-type(branch);
    unless (element(types, type, default: #f))
      types[type] := #t;
      emit-to-union-as-method(stream, union, branch);
      clauses := add!(clauses, with-output-to-string (stream)
				 emit-from-union-as-method-clause(stream, union, branch)
			       end);
    end unless;
  end for;
  emit-combined-from-union-as-method(stream, union, clauses);
end method;

define method emit-class-definition (stream :: <stream>, union :: <dim-union>)
 => ()
  format(stream,
	   "define sealed class %s (CORBA/<union>)\n"
	   "end class;\n\n"
	   "define sealed domain make (singleton(%s));\n"
	   "define sealed domain initialize (%s);\n\n",
	 dim-type-native-type(union),
	 dim-type-native-type(union),
	 dim-type-native-type(union));
end method;

define method emit-make-method (stream :: <stream>, union :: <dim-union>)
 => ()
  format(stream, "define method make (class == %s, #key discriminator = unsupplied(), value = unsupplied()",
         dim-type-native-type(union));
  for (branch :: <dim-branch> in dim-type-members(union))
    format(stream, ", %s = unsupplied()", dim-branch-local-variable-name(branch));
  end for;
  format(stream,
           ")\n"
           " => (object :: %s)\n"
           "  case\n"
           "    (supplied?(discriminator) & supplied?(value)) => next-method();\n",
         dim-type-native-type(union));
  for (branch :: <dim-branch> in dim-type-members(union))
    format(stream, "    supplied?(%s) => next-method(class, discriminator: %s, value: %s);\n",
           dim-branch-local-variable-name(branch),
           dim-branch-switch-value(branch),
           dim-branch-local-variable-name(branch));
  end for;
  format(stream,
	 "    otherwise => error(\"no suitable initialization keywords supplied for union creation\");\n"
           "  end case;\n"
           "end method;\n\n");
end method;

define method emit-class-typecode-method (stream :: <stream>, union :: <dim-union>)
 => ()
  format(stream,
	   "define method class-typecode (class == %s)\n"
	   " => (typecode :: <union-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(union),
	 dim-type-typecode-constant-name(union));
end method;

define method emit-object-typecode-method (stream :: <stream>, union :: <dim-union>)
 => ()
  format(stream,
	   "define method object-typecode (object :: %s)\n"
	   " => (typecode :: <union-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(union),
	 dim-type-typecode-constant-name(union));
end method;

define method emit-union-branch-is-method (stream :: <stream>, union :: <dim-union>, branch :: <dim-branch>)
 => ()
  format(stream,
	   "define method %sis-%s? (union :: %s)\n"
	   " => (well? :: <boolean>)\n"
	   "  select (union.CORBA/union/discriminator)\n",
	 dim-type-prefix(union),
	 dim-dylan-local-name(branch),
	 dim-type-native-type(union));
  if (dim-branch-default?(branch))
    for (other-branch in union.dim-type-members)
      if (other-branch ~= branch)
	let labels = union-branch-labels(other-branch.dim-node);
	format(stream, "    %s", union-branch-label-discriminator(labels[0]));
	for (i from 1 below size(labels))
	  format(stream, ", %s", union-branch-label-discriminator(labels[i]));
	end for;
	format(stream, " => #f;\n");
      end if;
    end for;
    format(stream, "    otherwise => #t;\n");
  else
    let labels = union-branch-labels(branch.dim-node);
    format(stream, "    %s", union-branch-label-discriminator(labels[0]));
    for (i from 1 below size(labels))
      format(stream, ", %s", union-branch-label-discriminator(labels[i]));
    end for;
    format(stream,
	     " => #t;\n"
	     "    otherwise => #f;\n");
  end if;
  format(stream,
	   "  end select;\n"
	   "end method;\n\n");
end method;

define method emit-union-getter-generic-function (stream :: <stream>, union :: <dim-union>, branch :: <dim-branch>)
 => ()
  format(stream,
	   "define sealed generic %s (union :: %s)\n"
	   " => (%s :: %s);\n\n",
	 dim-member-getter-name(branch),
	 dim-type-native-type(union),
	 dim-branch-local-variable-name(branch),
	 dim-type-native-type(dim-member-type(branch)));
end method;

define method emit-union-getter-method (stream :: <stream>, union :: <dim-union>, branch :: <dim-branch>)
 => ()
  format(stream,
	   "define method %s (union :: %s)\n"
	   " => (%s :: %s)\n"
	   "  if (union.%sis-%s?)\n"
	   "    union.CORBA/union/value;\n"
	   "  else\n"
	   "    error(\"Union discriminator incompatible with getter\");\n"
	   "  end if;\n"
	   "end method;\n\n",
	 dim-member-getter-name(branch),
	 dim-type-native-type(union),
	 dim-branch-local-variable-name(branch),
	 dim-type-native-type(dim-member-type(branch)),
	 dim-type-prefix(union),
	 dim-dylan-local-name(branch));
end method;

define method emit-union-setter-generic-function (stream :: <stream>, union :: <dim-union>, branch :: <dim-branch>)
 => ()
  format(stream,
	   "define sealed generic %s (%s :: %s, union :: %s)\n"
	   " => (%s :: %s);\n\n",
	 dim-member-setter-name(branch),
	 dim-branch-local-variable-name(branch),
	 dim-type-native-type(dim-member-type(branch)),
	 dim-type-native-type(union),
	 dim-branch-local-variable-name(branch),
	 dim-type-native-type(dim-member-type(branch)));
end method;

define method emit-union-setter-method (stream :: <stream>, union :: <dim-union>, branch :: <dim-branch>)
 => ()
  format(stream,
	   "define method %s (%s :: %s, union :: %s)\n"
	   " => (%s :: %s)\n"
	   "  union.CORBA/union/value := %s;\n"
	   "  union.CORBA/union/discriminator := %s;\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-member-setter-name(branch),
	 dim-branch-local-variable-name(branch),
	 dim-type-native-type(dim-member-type(branch)),
	 dim-type-native-type(union),
	 dim-branch-local-variable-name(branch),
	 dim-type-native-type(dim-member-type(branch)),
	 dim-branch-local-variable-name(branch),
	 dim-branch-switch-value(branch),
	 dim-branch-local-variable-name(branch));
end method;

define method emit-to-union-as-method
    (stream :: <stream>, union :: <dim-union>, branch :: <dim-branch>)
 => ()
  format(stream,
           "define method as (class == %s, object :: %s)\n"
           " => (union :: %s)\n"
           "  make(%s, %s: object);\n"
           "end method;\n\n",
         dim-type-native-type(union),
         dim-type-native-type(dim-member-type(branch)),
         dim-type-native-type(union),
         dim-type-native-type(union),
         dim-branch-local-variable-name(branch));
end method;

define method emit-from-union-as-method-clause
    (stream :: <stream>, union :: <dim-union>, branch :: <dim-branch>)
 => ()
  format(stream,
	 "    %s => %s(object);\n",
	 dim-type-native-type(dim-member-type(branch)),
         dim-member-getter-name(branch));
end method;  

define method emit-combined-from-union-as-method
    (stream :: <stream>, union :: <dim-union>, clauses :: <sequence>)
 => ()
  format(stream,
	 "define method as (class :: <type>, object :: %s)\n"
	   " => (object :: <object>)\n"
	   "  select (class by subtype?)\n",
	 dim-type-native-type(union));
  for (clause in clauses)
    write(stream, clause)
  end for;
  format(stream,
	 "    otherwise => next-method();\n"
	 "  end select;\n"
	   "end method;\n\n")
end method;

