Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-typedef> (<ast-type>)
  constant slot typedef-base-type :: <ast-type>, init-keyword: type:;
end class;

define method declarator-base-type (declarator :: <ast-declarator>)
  declarator;
end method;

define method declarator-base-type (typedef :: <ast-typedef>)
  typedef.typedef-base-type.declarator-base-type;
end method;

define method can-be-redefined? (typedef :: <ast-typedef>)
  #t;
end method;

define method add-declarator-to-scope (scope :: <scope>, typedef :: <ast-typedef>)
  next-method();
  unless (typedef.typedef-base-type.declarator-added?)
    add-type(typedef.typedef-base-type)
  end unless;
end method;


