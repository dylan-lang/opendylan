Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-enum> (<ast-concrete-type>, <scope>)
  slot enum-counter :: <integer> = 0;
end class;

define method can-be-redefined? (enum :: <ast-enum>)
  #t;
end method;

define class <ast-enum-value> (<ast-constant>)
  keyword type: = resolve-primitive-type(last(scepter-scopes(get-scepter())), $ulong-idl-type);
end class;

define method initialize (enum-value :: <ast-enum-value>, #key)
  next-method();
  unless (enum-value.constant-value)
    enum-value.constant-value :=
      make(<ast-expression>,
        combinator: $no-combinator,
        value: next-enum-value(first(scepter-scopes(get-scepter()))));
  end unless;
end method;

define method can-be-redefined? (enum-value :: <ast-enum-value>)
  #t;
end method;

// initializer may be needed to pass string list to declarator
// but I think that declarator can take care of that

define method add-type (enum :: <ast-enum>)
  add-declarator-to-scope(enum.declarator-scope, enum);
  call-add(enum);
end method;

define method next-enum-value (enum :: <ast-enum>)
  let current-value = enum.enum-counter;
  enum.enum-counter := current-value + 1;
  current-value;
end method;

define method munge-name-for-enum-value (scoped-name :: <ast-scoped-name>)
  => (result :: <ast-scoped-name>)
  let length = scoped-name.size;
  scoped-name[length - 2] := scoped-name[length - 1];
  scoped-name.size := length - 1;
  scoped-name;
end method;

define method add-declarator (enum :: <ast-enum>, enum-value :: <ast-enum-value>)
  let mirror-enum-value = make(<ast-enum-value>,
                               value: enum-value.constant-value, // some other accesses and conversions here
                               scoped-name: enum-value.declarator-scoped-name,
                               pragmas: enum-value.declarator-pragmas);
  munge-name-for-enum-value(enum-value.declarator-scoped-name);
  munge-name-for-enum-value(mirror-enum-value.declarator-scoped-name);
  next-method();
  add-declarator(second(scepter-scopes(get-scepter())), mirror-enum-value);
end method;

