Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-sequence> (<ast-concrete-type>)
  constant slot sequence-max-size :: false-or(<ast-expression>) = #f, init-keyword: size:;
  constant slot sequence-base-type :: <ast-type>, init-keyword: type:;
end class;

define method initialize (sequence :: <ast-sequence>, #key)
  next-method();
  sequence.declarator-scoped-name := add!(sequence.declarator-scoped-name, "sequence");
end method;

define method can-be-redefined? (sequence :: <ast-sequence>)
  #t;
end method;

define method add-type (sequence :: <ast-sequence>)
  add-declarator-to-scope(scepter-root(get-scepter()), sequence);
end method;

define method add-declarator-to-scope (scope :: <scope>, sequence :: <ast-sequence>)
  next-method();
  unless (sequence.sequence-base-type.declarator-added?)
    add-type(sequence.sequence-base-type)
  end unless;
end method;


