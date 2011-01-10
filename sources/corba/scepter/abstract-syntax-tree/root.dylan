Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-root> (<scepter-ast-root>, <ast-module>)
end class;

define method add-declarator (root :: <ast-root>, declarator :: <ast-sequence>)
  declarator.declarator-scoped-name := add!(declarator.declarator-scoped-name,
                                            make(<ast-identifier>, label: "local type"));
  add-to-scope(root, declarator);
end method;

define method add-declarator (root :: <ast-root>, declarator :: <ast-string>)
  declarator.declarator-scoped-name := add!(declarator.declarator-scoped-name, 
                                            make(<ast-identifier>, label: "local type"));
  add-to-scope(root, declarator);
end method;

define method add-declarator (root :: <ast-root>, declarator :: <ast-array>)
  declarator.declarator-scoped-name := add!(declarator.declarator-scoped-name, 
                                            make(<ast-identifier>, label: "local type"));
  add-to-scope(root, declarator);
end method;

