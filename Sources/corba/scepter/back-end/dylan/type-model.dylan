Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define thread variable *type-level* :: <integer> = 0;


// <DIM-TYPE>

define abstract class <dim-type> (<dim>)
  slot dim-type-native-type :: false-or(<string>) = #f, init-keyword: type:;
end class;

define method dim-type-prefix (type :: <dim-type>)
 => (prefix :: <string>)
  let node :: <ast-declarator> = dim-node(type);
  map-to-dylan-prefix(declarator-scoped-name(node));
end method;

define sealed generic emit-from-any-code (stream :: <stream>, type :: <dim-type>, expression :: <string>) => ();

define method emit-from-any-code (stream :: <stream>, type :: <dim-type>, expression :: <string>)
 => ()
  format(stream, "CORBA/any/value(%s)", expression);
end method;


// <DIM-NAMED-TYPE>

define abstract class <dim-named-type> (<dim-type>)
end class;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-named-type>)
 => ()
  next-method();
  format(stream, ", name: %=, repository-id: %=", dim-idl-local-name(type), dim-repository-id(type));
end method;


define abstract class <dim-membered-type> (<dim-named-type>)
  slot dim-type-members :: <sequence> = #[];
end class;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-membered-type>)
 => ()
  next-method();
  format(stream, ", members: vector(");
  let members = dim-type-members(type);
  if (size(members) > 0)
    emit-typecode-member(stream, members[0]);
    for (i from 1 below size(members))
      format(stream, ", ");
      emit-typecode-member(stream, members[i]);
    end for;
  end if;
  format(stream, ")");  
end method;


// <DIM-TEMPLATE-TYPE>

define abstract class <dim-template-type> (<dim-type>)
end class;

define method emit-typecode (stream :: <stream>, type :: <dim-template-type>)
 => ()
  emit-typecode-constructor(stream, type);
end method;


// <DIM-STRING>

define class <dim-string> (<dim-template-type>)
  slot dim-string-max-size :: <string> = "0";
end class;

define method initialize (string :: <dim-string>, #key node :: <ast-string>)
  next-method();
  string.dim-type-native-type := "CORBA/<string>";
  let size = string-max-size(node);
  if (size)
    string.dim-string-max-size := convert-const-exp(size);
  end if;
end method;

define method make-dim (node :: <ast-string>)
 => (model :: <dim-type>)
  make(<dim-string>, node: node);
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <dim-string>)
 => ()
  format(stream, "<string-typecode>");
end method;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-string>)
 => ()
  next-method();
  format(stream, ", max-length: %s", dim-string-max-size(type));
end method;


// <DIM-SEQUENCE>

define class <dim-sequence> (<dim-template-type>)
  slot dim-sequence-recursive? :: <boolean> = #f;
  slot dim-sequence-member-type :: <dim-type>;
  slot dim-sequence-max-size :: <string> = "0";
end class;

define method initialize (sequence :: <dim-sequence>, #key node :: <ast-sequence>)
  next-method();
  let key = find-key(*recursive-types*, method (model) dim-node(model) == sequence-base-type(node) end, failure: #f);
  let element-type
    = if (key)
	sequence.dim-sequence-recursive? := #t;
	*recursive-types*[key];
      else
	dynamic-bind (*type-level* = *type-level* + 2)
	  make(<dim>, node: sequence-base-type(node));
        end;
      end if;
  sequence.dim-type-native-type := format-to-string("limited(CORBA/<sequence>, of: %s)", dim-type-native-type(element-type));
  sequence.dim-sequence-member-type := element-type;
  let size = sequence-max-size(node);
  if (size)
    sequence.dim-sequence-max-size := convert-const-exp(size)
  end if;
end method;

define method make-dim (node :: <ast-sequence>)
 => (model :: <dim-type>)
  make(<dim-sequence>, node: node);
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <dim-sequence>)
 =>()
  format(stream, "<sequence-typecode>");
end method;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-sequence>)
 => ()
  next-method();
  if (dim-type-native-type(type))
    format(stream, ", type: %s", dim-type-native-type(type));
  end if;
  format(stream, ", max-length: %s, element-typecode: ", dim-sequence-max-size(type));
  if (dim-sequence-recursive?(type))
    let member-type = dim-sequence-member-type(type);
    let nesting = dim-recursive-type-level(member-type) + 2;
    emit-indirection-typecode(stream, nesting);
  else
    emit-typecode(stream, dim-sequence-member-type(type));
  end if;
end method;


// I don't think we need this method.
/*
define method make-type-model (type :: <ast-scoped-name>)
 => (model :: <dim-type>)
  let (scoped-name, found, subobject-data)
    = lookup-scoped-name(type);
  unless (found)
    error("Unkown type-spec %=", type);
  end unless;
  values( // (if (consp scoped-name) (first scoped-name) scoped-name)
	 environment-entry-typecode(subobject-data),
	 environment-entry-to-any(subobject-data),
	 environment-entry-from-any(subobject-data),
	 environment-entry-to-any-external(subobject-data),
	 environment-entry-from-any-external(subobject-data));
end method;
*/


// <DIM-ARRAY>

define class <dim-array> (<dim-template-type>)
  slot dim-array-dimensions :: <sequence> /*---***limited(<sequence>, of: <string>)***---*/ = #[];
end class;

define method initialize (array :: <dim-array>, #key node :: <ast-array>)
  next-method();

  let dimensions = map(convert-const-exp, array-dimensions(node));
  array.dim-array-dimensions := dimensions;

  let element-type = make(<dim>, node: array-base-type(node));
  let type = format-to-string("limited(CORBA/<array>, of: %s, dimensions: #[%s", dim-type-native-type(element-type), dimensions[0]);
  for (i from 1 below size(dimensions))
    type := concatenate(type, ", ", dimensions[i]);
  end for;
  type := concatenate(type, "])");
  array.dim-type-native-type := type;
end method;

define method make-dim (node :: <ast-array>)
 => (model :: <dim-type>)
  make(<dim-array>, node: node);
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <dim-array>)
 =>()
  format(stream, "<array-typecode>");
end method;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-array>)
 => ()
  next-method();
  if (dim-type-native-type(type))
    format(stream, ", type: %s", dim-type-native-type(type));
  end if;
  let dimensions = dim-array-dimensions(type);

  if (size(dimensions) = 1)
    format(stream, ", length: %s, element-typecode: ", dimensions[0]);
    emit-typecode(stream, make(<dim>, node: array-base-type(dim-node(type))));
  else
    format(stream, ", length: %s, element-typecode: ", dimensions[0]);
    for (i from 1 below size(dimensions))
      format(stream, "make(<array-typecode>, length: %s, element-typecode: ", dimensions[i])
    end for;
    let element-type = make(<dim>, node: array-base-type(dim-node(type)));
    emit-typecode(stream, element-type);
    format(stream, ", type: %s", dim-type-native-type(element-type));
    for (i from 1 below size(dimensions))
      format(stream, ")");
    end for;
  end if;
end method;


// <DIM-PREDEFINED-TYPE>

define abstract class <dim-predefined-type> (<dim-type>)
end class;

define method make-dim (node :: <ast-predefined-type>)
 => (model :: <dim-type>)
  make-dim-predefined-type(node.predefined-type, node);
end method;


// <DIM-BASE-TYPE>

define class <dim-base-type> (<dim-predefined-type>)
end class;

define method emit-typecode (stream :: <stream>, type :: <dim-base-type>)
 => ()
  emit-typecode-constructor(stream, type);
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <dim-base-type>)
 => ()
  emit-typecode-class-name(stream, predefined-type(dim-node(type)));
end method;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-base-type>)
 => ()
end method;

define generic make-dim-predefined-type (predefined-type :: <object>, type :: <ast-type>)
 => (model :: <dim-predefined-type>);


// VOID

define method make-dim-predefined-type (predefined-type :: <void-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<void>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <void-idl-type>)
 => ()
  format(stream, "<void-typecode>");
end method;


// LONG

define method make-dim-predefined-type
    (predefined-type :: <long-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<long>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <long-idl-type>)
 => ()
  format(stream, "<long-typecode>");
end method;


// SHORT

define method make-dim-predefined-type
    (predefined-type :: <short-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<short>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <short-idl-type>)
 => ()
  format(stream, "<short-typecode>");
end method;


// UNSIGNED LONG

define method make-dim-predefined-type
    (predefined-type :: <ulong-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<unsigned-long>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <ulong-idl-type>)
 => ()
  format(stream, "<unsigned-long-typecode>");
end method;


// UNSIGNED SHORT

define method make-dim-predefined-type
    (predefined-type :: <ushort-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<unsigned-short>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <ushort-idl-type>)
 => ()
  format(stream, "<unsigned-short-typecode>");
end method;


// FLOAT

define method make-dim-predefined-type
    (predefined-type :: <float-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<float>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <float-idl-type>)
 => ()
  format(stream, " <float-typecode>");
end method;


// DOUBLE

define method make-dim-predefined-type
    (predefined-type :: <double-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<double>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <double-idl-type>)
 => ()
  format(stream, "<double-typecode>");
end method;


// CHAR

define method make-dim-predefined-type
    (predefined-type :: <char-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<char>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <char-idl-type>)
 => ()
  format(stream, "<char-typecode>");
end method;


// BOOLEAN

define method make-dim-predefined-type
    (predefined-type :: <boolean-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<boolean>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <boolean-idl-type>)
 => ()
  format(stream, "<boolean-typecode>");
end method;


// OCTET

define method make-dim-predefined-type
    (predefined-type :: <octet-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<octet>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <octet-idl-type>)
 => ()
  format(stream, "<octet-typecode>");
end method;


// ANY

define method make-dim-predefined-type
    (predefined-type :: <any-idl-type>, type :: <ast-type>)
 => (model :: <dim-base-type>)
  make(<dim-base-type>,
       node: type,
       type: "CORBA/<any>");
end method;

define method emit-typecode-class-name (stream :: <stream>, type :: <any-idl-type>)
 => ()
  format(stream, "<any-typecode>");
end method;


// <DIM-PSEUDO-TYPE>

define class <dim-pseudo-type> (<dim-predefined-type>)
end class;

define method emit-typecode (stream :: <stream>, type :: <dim-pseudo-type>)
 => ()
  emit-pseudo-object-typecode(stream, type.dim-node.predefined-type);
end method;


// OBJECT

define method make-dim-predefined-type
    (predefined-type :: <Object-idl-type>, type :: <ast-type>)
 => (model :: <dim-pseudo-type>)
  make(<dim-pseudo-type>,
       node: type,
       type: "CORBA/<Object>");
end method;

define method emit-pseudo-object-typecode (stream :: <stream>, type :: <Object-idl-type>)
 => ()
  format(stream, "class-typecode(<object-reference>)");
end method;


// TYPECODE

define method make-dim-predefined-type
    (predefined-type :: <TypeCode-idl-type>, type :: <ast-type>)
 => (model :: <dim-pseudo-type>)
  make(<dim-pseudo-type>,
       node: type,
       type: "CORBA/<TypeCode>");
end method;

define method emit-pseudo-object-typecode (stream :: <stream>, type :: <TypeCode-idl-type>)
 => ()
  format(stream, "class-typecode(CORBA/<TypeCode>)");
end method;


// NAMEDVALUE

define method make-dim-predefined-type
    (predefined-type :: <NamedValue-idl-type>, type :: <ast-type>)
 => (model :: <dim-pseudo-type>)
  make(<dim-pseudo-type>,
       node: type,
       type: "CORBA/<NamedValue>");
end method;

define method emit-pseudo-object-typecode (stream :: <stream>, type :: <NamedValue-idl-type>)
 => ()
  format(stream, "class-typecode(CORBA/<NamedValue>)");
end method;


// PRINCIPAL

define method make-dim-predefined-type
    (predefined-type :: <Principal-idl-type>, type :: <ast-type>)
 => (model :: <dim-pseudo-type>)
  make(<dim-pseudo-type>,
       node: type,
       type: "CORBA/<Principal>");
end method;

define method emit-pseudo-object-typecode (stream :: <stream>, type :: <Principal-idl-type>)
 => ()
  format(stream, "class-typecode(CORBA/<Principal>)");
end method;
