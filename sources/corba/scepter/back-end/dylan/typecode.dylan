Module:    scepter-dylan-back-end
Author:    Keith Dennison
Synopsis:  Protocols to support emission of typecodes
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Protocol for emitting typecodes
//
define generic emit-typecode (stream :: <stream>, type :: <dim-type>) => ();
define generic emit-indirection-typecode (stream :: <stream>, nesting :: <integer>) => ();
define generic dim-type-typecode-as-string (type :: <dim-type>) => (typecode :: <string>);

define method emit-typecode (stream :: <stream>, type :: <dim-type>)
 => ()
  format(stream, "class-typecode(%s)", dim-type-native-type(type));
end method;

define method emit-indirection-typecode (stream :: <stream>, nesting :: <integer>)
 => ()
  format(stream, "make(<indirection-typecode>, nesting: %d)", nesting);
end method;

define method dim-type-typecode-as-string (type :: <dim-type>)
 => (typecode :: <string>)
  with-output-to-string(stream)
    emit-typecode(stream, type);
  end with-output-to-string;
end method;


// Protocol for emitting a call to make to construct a typecode.
//
define generic emit-typecode-constructor (stream :: <stream>, type :: <dim-type>) => ();
define generic emit-typecode-class-name (stream :: <stream>, type :: type-union(<dim-type>, <idl-type>)) => ();
define generic emit-typecode-keyword-args (stream :: <stream>, type :: <dim-type>) => ();
define generic emit-typecode-member (stream :: <stream>, member :: <object>) => ();

define method emit-typecode-constructor (stream :: <stream>, type :: <dim-type>)
 => ()
  format(stream, "make(");
  emit-typecode-class-name(stream, type);
  emit-typecode-keyword-args(stream, type);
  format(stream, ")");
end method;

define method emit-typecode-keyword-args (stream :: <stream>, type :: <dim-type>)
 => ()
  if (dim-type-native-type(type))
    format(stream, ", type: %s", dim-type-native-type(type));
  end if;
end method;


// Protocol to support constant typecode objects for constructed types.
//
define generic dim-type-typecode-constant-name (type :: <dim-type>) => (name :: <string>);
define generic emit-typecode-constant-definition (stream :: <stream>, type :: <dim-named-type>) => ();

define method dim-type-typecode-constant-name (type :: <dim-type>)
 => (name :: <string>)
  map-to-dylan-name(declarator-scoped-name(dim-node(type)),
                    prefix: $dylan-constant-name-prefix,
                    suffix: "-typecode");
end method;

define method emit-typecode-constant-definition (stream :: <stream>, type :: <dim-named-type>)
 => ()
  format(stream, "define constant %s = ", dim-type-typecode-constant-name(type));
  emit-typecode-constructor(stream, type);
  format(stream, ";\n\n");
end method;


// Protocol for emitting class-typecode and object-typecode methods. This is restricted to
// <dim-named-type>s, but could be extended in the future to <dim-template-type>s.
//
define generic emit-typecode-methods (stream :: <stream>, type :: <dim-named-type>) => ();
define generic emit-class-typecode-method (stream :: <stream>, type :: <dim-named-type>) => ();
define generic emit-object-typecode-method (stream :: <stream>, type :: <dim-named-type>) => ();

define method emit-typecode-methods (stream :: <stream>, type :: <dim-named-type>)
 => ()
  emit-class-typecode-method(stream, type);
  emit-object-typecode-method(stream, type);
end method;

define method emit-class-typecode-method (stream :: <stream>, type :: <dim-named-type>)
 => ()
  format(stream,
	   "define method class-typecode (class :: subclass(%s))\n"
	   " => (typecode :: <typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(type),
	 dim-type-typecode-constant-name(type));
end method;

define method emit-object-typecode-method (stream :: <stream>, type :: <dim-named-type>)
 => ()
  format(stream,
	   "define method object-typecode (object :: %s)\n"
	   " => (typecode :: <typecode>)\n"
	   "  %s;\n"
	   "end method;\n\n",
	 dim-type-native-type(type),
	 dim-type-typecode-constant-name(type));
end method;
