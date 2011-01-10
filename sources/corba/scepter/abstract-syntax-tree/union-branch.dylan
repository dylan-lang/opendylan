Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-union-branch> (<ast-field>)
  constant slot union-branch-labels :: <sequence> = #[], init-keyword: labels:;
end class;

define method can-be-redefined? (union-branch :: <ast-union-branch>)
  #f;
end method;

define method add-declarator-to-scope (scope :: <scope>, union-branch :: <ast-union-branch>)
  next-method();
  unless (union-branch.field-type.declarator-added?)
    add-type(union-branch.field-type)
  end unless;
end method;

