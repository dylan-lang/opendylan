Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <dim-enum> (<dim-membered-type>)
end class;

define method initialize (enum :: <dim-enum>, #key node :: <ast-enum>)
  next-method();
  enum.dim-type-native-type := map-to-dylan-class-name(declarator-scoped-name(node));
  enum.dim-type-members := map(compose(map-to-dylan-symbol, declarator-local-name), scope-declarators(node));
end method;

define method emit-typecode-class-name (stream :: <stream>, enum :: <dim-enum>)
 =>()
  format(stream, "<enum-typecode>");
end method;

define method emit-typecode-member (stream :: <stream>, symbol :: <string>)
 => ()
  format(stream, symbol);
end method;

define method make-dim (node :: <ast-enum>)
 => (model :: <dim-enum>)
  make(<dim-enum>, node: node);
end method;

define method protocol-exports (enum :: <dim-enum>)
 => (exports :: <sequence>)
  vector(dim-type-native-type(enum),
	 concatenate(dim-type-prefix(enum), "successor"),
	 concatenate(dim-type-prefix(enum), "predecessor"),
	 concatenate(dim-type-prefix(enum), ">"),
	 concatenate(dim-type-prefix(enum), "<"));
end method;

define method emit-protocol-code (enum :: <dim-enum>, stream :: <stream>)
 => ()
  emit-enum-type-definition(stream, enum);
  emit-enum-successor-generic-function(stream, enum);
  emit-enum-predecessor-generic-function(stream, enum);
  emit-enum-<-generic-function(stream, enum);
  emit-enum->-generic-function(stream, enum);
end method;

define method emit-shared-code (enum :: <dim-enum>, stream :: <stream>)
 => ()
  emit-typecode-constant-definition(stream, enum);
  emit-typecode-methods(stream, enum);
  emit-enum-integer-symbol-mapping(stream, enum);
  emit-enum-successor-method(stream, enum);
  emit-enum-predecessor-method(stream, enum);
  emit-enum-<-method(stream, enum);
  emit-enum->-method(stream, enum);
end method;

define method emit-class-typecode-method (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define method class-typecode (class == %s)\n"
	   " => (typecode :: <enum-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(enum),
	 dim-type-typecode-constant-name(enum));
end method;

define method emit-object-typecode-method (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define sideways method object-typecode (object :: %s)\n"
	   " => (typecode :: <enum-typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(enum),
	 dim-type-typecode-constant-name(enum));
end method;

define method emit-enum-type-definition (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream, "define constant %s = one-of(", dim-type-native-type(enum));
  print-separated-collection(dim-type-members(enum), ", ", stream, printer: method (object, stream) format(stream, object) end);
  format(stream, ");\n\n");
end method;

define method emit-enum-integer-symbol-mapping (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define inline-only function %sas-symbol (integer :: <integer>)\n"
	   " => (symbol :: %s)\n"
	   "  let typecode = class-typecode(%s);\n"
	   "  typecode-members(typecode)[integer];\n"
	   "end function;\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum));

  format(stream,
	   "define inline-only function %sas-integer (symbol :: %s)\n"
	   " => (integer :: <integer>)\n"
	   "  let typecode = class-typecode(%s);\n"
	   "  typecode-symbol-index(typecode)[symbol];\n"
	   "end function;\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum));
end method;

define method emit-enum-successor-generic-function (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define generic %ssuccessor (symbol :: %s)\n"
	   " => (succ :: %s);\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum));
end method;

define method emit-enum-successor-method (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define method %ssuccessor (symbol :: %s)\n"
	   " => (succ :: %s)\n"
	   "  let i = %sas-integer(symbol);\n"
	   "  %sas-symbol(modulo(i + 1, %d));\n"
	   "end method;\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum),
	 dim-type-prefix(enum),
	 dim-type-prefix(enum),
	 size(dim-type-members(enum)));
end method;

define method emit-enum-predecessor-generic-function (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define generic %spredecessor (symbol :: %s)\n"
	   " => (pred :: %s);\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum));
end method;

define method emit-enum-predecessor-method (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define method %spredecessor (symbol :: %s)\n"
	   " => (pred :: %s)\n"
	   "  let i = %sas-integer(symbol);\n"
	   "  %sas-symbol(modulo(%d + i - 1, %d));\n"
	   "end method;\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum),
	 dim-type-prefix(enum),
	 dim-type-prefix(enum),
	 size(dim-type-members(enum)),
	 size(dim-type-members(enum)));
end method;

define method emit-enum->-generic-function (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define generic %s> (greater :: %s, lesser :: %s)\n"
	   " => (greater? :: <boolean>);\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum));
end method;

define method emit-enum->-method (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define method %s> (greater :: %s, lesser :: %s)\n"
	   " => (greater? :: <boolean>)\n"
	   "  %sas-integer(greater) > %sas-integer(lesser);\n"
	   "end method;\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum),
	 dim-type-prefix(enum),
	 dim-type-prefix(enum));
end method;

define method emit-enum-<-generic-function (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define generic %s< (lesser :: %s, greater :: %s)\n"
	   " => (lesser? :: <boolean>);\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum));
end method;

define method emit-enum-<-method (stream :: <stream>, enum :: <dim-enum>)
 => ()
  format(stream,
	   "define method %s< (lesser :: %s, greater :: %s)\n"
	   " => (lesser? :: <boolean>)\n"
	   "  %sas-integer(lesser) < %sas-integer(greater);\n"
	   "end method;\n\n",
	 dim-type-prefix(enum),
	 dim-type-native-type(enum),
	 dim-type-native-type(enum),
	 dim-type-prefix(enum),
	 dim-type-prefix(enum));
end method;
