Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-array> (<ast-concrete-type>)
  constant slot array-dimensions :: <sequence> = #[], init-keyword: dimensions:;
  slot array-base-type :: false-or(<ast-type>) = #f, init-keyword: type:;
end class;

define method can-be-redefined? (array :: <ast-array>)
  #t;
end method;

define method add-type (array :: <ast-array>)
  add-declarator-to-scope(scepter-root(get-scepter()), array);
end method;

define method add-declarator-to-scope (scope :: <scope>, array :: <ast-array>)
  next-method();
  unless (array.array-base-type.declarator-added?)
    add-type(array.array-base-type)
  end unless;
end method;




