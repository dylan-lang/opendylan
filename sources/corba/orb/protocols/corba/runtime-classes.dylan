Module: corba-protocol
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BASIC RUNTIME TYPES

define constant corba/$Identifier-typecode = make(<alias-typecode>, aliased: corba/$string-typecode);

/// FLAGS

define constant corba/<Flags> = corba/<unsigned-long>;

define constant corba/$Flags-typecode = make(<alias-typecode>, aliased: corba/$unsigned-long-typecode);

//---*** clashes with long/ulong at moment
//define method class-typecode (class == corba/<Flags>)
// => (typecode :: <typecode>)
//  corba/$Flags-typecode;
//end method;

/// NAMEDVALUE

define sealed class corba/<NamedValue> (corba/<struct>)
  sealed slot corba/NamedValue/name :: corba/<Identifier>, required-init-keyword: name:;
  sealed slot corba/NamedValue/argument :: corba/<any>, required-init-keyword: argument:;
  sealed slot corba/NamedValue/len :: corba/<long>, required-init-keyword: len:;
  sealed slot corba/NamedValue/arg-modes :: corba/<Flags>, required-init-keyword: arg-modes:;
end class;

define constant corba/$NamedValue-typecode =
  make(<struct-typecode>,
       name: "NamedValue",
       type: corba/<namedvalue>,
       repository-id: "IDL:omg.org/CORBA/NamedValue:1.0",
       members: vector(make(<typecode-member>,
			    name: "name",
			    typecode: corba/$identifier-typecode,
			    native-type: corba/<Identifier>,
			    getter: corba/NamedValue/name,
			    setter: corba/NamedValue/name-setter,
			    init-keyword: name:),
		       make(<typecode-member>,
			    name: "argument",
			    typecode: corba/$any-typecode,
			    native-type: corba/<any>,
			    getter: corba/NamedValue/argument,
			    setter: corba/NamedValue/argument-setter,
			    init-keyword: argument:),
		       make(<typecode-member>,
			    name: "len",
			    typecode: corba/$long-typecode,
			    native-type: corba/<long>,
			    getter: corba/NamedValue/len,
			    setter: corba/NamedValue/len-setter,
			    init-keyword: len:),
		       make(<typecode-member>,
			    name: "arg_modes",
			    typecode: corba/$Flags-typecode,
			    native-type: corba/<Flags>,
			    getter: corba/NamedValue/arg-modes,
			    setter: corba/NamedValue/arg-modes-setter,
			    init-keyword: arg-modes:)));

define method class-typecode (class == corba/<NamedValue>)
 => (typecode :: <typecode>)
  corba/$NamedValue-typecode
end method;

define method object-typecode (object :: corba/<NamedValue>)
 => (typecode :: <typecode>)
  corba/$NamedValue-typecode
end method;

/// NVLIST

define constant corba/<NVList> = limited(corba/<sequence>, of: corba/<NamedValue>);

define constant corba/$NVList-typecode =
  make(<sequence-typecode>, element-typecode: corba/$NamedValue-typecode);

define method class-typecode (class == corba/<NVList>)
 => (typecode :: <typecode>)
  corba/$NVList-typecode
end method;

