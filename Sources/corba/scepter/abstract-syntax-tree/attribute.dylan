Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-attribute> (<ast-field>)
  constant slot attribute-read-only? :: <boolean> = #t, init-keyword: read-only?:;
end class;

define method can-be-redefined? (attribute :: <ast-attribute>)
  #f;
end method;

define method add-declarator-to-scope (scope :: <scope>, attribute :: <ast-attribute>)
  next-method();
  unless (attribute.field-type.declarator-added?)
    add-type(attribute.field-type);
  end unless;
end method;




