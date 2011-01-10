Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-structure> (<ast-concrete-type>, <scope>)
end class;

define method can-be-redefined? (struct :: <ast-structure>)
  #t;
end method;

define method add-type (struct :: <ast-structure>)
  add-declarator-to-scope(struct.declarator-scope, struct);
  call-add(struct);
end method;

