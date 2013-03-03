Module:    scepter-dylan-back-end
Author:    Keith Dennison, Clive Tong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This method isn't used at the moment. It takes a Dylan name and
// returns a possibly new name which is guaranteed not to clash with
// any Dylan name which could be derived from an IDL file according
// to the Dylan IDL binding.
/*
define method ensure-non-corba-name (name :: <string>)
 => (name :: <string>)
  concatenate(name, "-%");
end method;
*/

define constant $dylan-scoped-name-separator = "/";
define constant $dylan-constant-name-prefix = "$";
define constant $dylan-class-name-prefix = "<";
define constant $dylan-class-name-suffix = ">";
define constant $dylan-reference-class-name-suffix = "-reference>";
define constant $dylan-servant-class-name-suffix = "-servant>";

define method map-to-dylan-prefix (idl-name :: type-union(<ast-scoped-name>, <ast-identifier>))
 => (prefix :: <string>)
  concatenate(map-to-dylan-name(idl-name), $dylan-scoped-name-separator);
end method;

define method map-to-dylan-symbol (idl-name :: type-union(<ast-scoped-name>, <ast-identifier>))
 => (dylan-symbol :: <string>)
  concatenate("#\"", map-to-dylan-name(idl-name), "\"");
end method;

define method map-to-dylan-constant-name (idl-name :: type-union(<ast-scoped-name>, <ast-identifier>))
 => (dylan-id :: <string>)
  map-to-dylan-name(idl-name, prefix: $dylan-constant-name-prefix);
end method;

define method map-to-dylan-class-name (idl-name :: type-union(<ast-scoped-name>, <ast-identifier>),
				       #key prefix :: <string> = $dylan-class-name-prefix,
				            suffix :: <string> = $dylan-class-name-suffix)
 => (dylan-id :: <string>)
  map-to-dylan-name(idl-name, prefix: prefix, suffix: suffix);
end method;

define method map-to-dylan-interface-class-name (idl-name :: type-union(<ast-scoped-name>, <ast-identifier>))
 => (dylan-id :: <string>)
  map-to-dylan-class-name(idl-name);
end method;

define method map-to-dylan-reference-class-name (idl-name :: type-union(<ast-scoped-name>, <ast-identifier>))
 => (dylan-id :: <string>)
  map-to-dylan-class-name(idl-name, suffix: $dylan-reference-class-name-suffix);
end method;

define method map-to-dylan-servant-class-name (idl-name :: type-union(<ast-scoped-name>, <ast-identifier>))
 => (dylan-id :: <string>)
  map-to-dylan-class-name(idl-name, suffix: $dylan-servant-class-name-suffix);
end method;

define method map-to-dylan-name (idl-name :: <ast-scoped-name>, #rest keys, #key prefix :: <string> = "", suffix :: <string> = "")
 => (dylan-id :: <string>)

  debug-assert(size(idl-name) > 1);
//  debug-assert(idl-name[0] = "::");

  let dylan-id = "";
  let limit = size(idl-name) - 1;
  for (i from 1 below limit)
    dylan-id := concatenate(dylan-id, map-to-dylan-name(idl-name[i]), $dylan-scoped-name-separator);
  end for;
  concatenate(dylan-id, apply(map-to-dylan-name, idl-name[limit], keys));
end method;

define method map-to-dylan-name (idl-name :: <ast-identifier>, #key prefix :: <string> = "", suffix :: <string> = "")
 => (dylan-id :: <string>);
  map-to-dylan-name(identifier-label(idl-name), prefix: prefix, suffix: suffix);
//  concatenate(prefix, map-to-dylan-name(idl-name.identifier-label), suffix);
end;

define method map-to-dylan-name (idl-name :: <string>, #key prefix :: <string> = "", suffix :: <string> = "")
 => (dylan-id :: <string>);
  let total-size = size(idl-name) + size(prefix) + size(suffix);
  let reserved? = dylan-reserved?(idl-name);
  let result = make(<string>, size: if (reserved?) total-size + 2; else total-size end);

  let limit1 = size(prefix);
  for (i :: <integer> from 0 below limit1)
    result[i] := prefix[i];
  end for;

  let limit2 = limit1 + size(idl-name);
  for (i :: <integer> from limit1 below limit2)
    let char = idl-name[i - limit1];
    if (char == '_') char := '-' end;
    result[i] := char;
  end;

  let limit3 = limit2 + size(suffix);
  for (i :: <integer> from limit2 below limit3)
    result[i] := suffix[i - limit2];
  end for;

  if (reserved?)
    result[total-size] := '-';
    result[total-size + 1] := '%';
  end;
  result
end;

define constant $dylan-reserved-words
  = #("let", "define", "method");
	// and lots more and anything exported from
	// the dylan library that could be an idl id

define method dylan-reserved? (s :: <string>) => (yes? :: <boolean>)
  // this is going to be  hideously slow.  We really need to hash the reserved words,
  // and compare hash codes.
  let sz = size(s);
  let setter-size = size("-setter");
  ((sz > setter-size) & copy-sequence(s, start: sz - setter-size, end: sz) = "-setter")
    | any?(method (word) word = s end, $dylan-reserved-words)
end;

