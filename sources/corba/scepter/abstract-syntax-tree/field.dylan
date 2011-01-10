Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-field> (<ast-declarator>)
  constant slot field-type :: false-or(<ast-type>) = #f, init-keyword: type:;
end class;

define method can-be-redefined? (field :: <ast-field>)
  #f;
end method;

define method add-declarator-to-scope (scope :: <scope>, field :: <ast-field>)
  next-method();
  unless (field.field-type.declarator-added?)
    add-type(field.field-type)
  end unless;
end method;


