Module: scepter
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method scope-declarators-as-list (scope :: <object>)
  #();
end method;

define method scope-declarators-as-list (scope :: <scope>)
  choose(user-defined?, as(<list>, concatenate(scope.scope-local-types, scope.scope-declarators)));
end method;

define method user-defined? (dcl :: <ast-declarator>)
  #t;
end method;

define method user-defined? (type :: <ast-predefined-type>)
  #f;
end method;

define method scope-root (#rest args)
  *root*;
end method;

import-cl-functions(cl-user(graph-ast)(as: graph-ast));

define method print-object (object :: <ast-declarator>, stream :: <stream>)
  next-method();
  format(stream, ":{");
  print-separated-collection(object.declarator-scoped-name, "::", stream,
                            printer: method (x, stream) format(stream, "%s", x.identifier-label) end method);
  format(stream, "}");
end method;

define method print-object (object :: <ast-identifier>, stream :: <stream>)
  next-method();
  format(stream, ":{%s}", object.identifier-label);
end method;

define method print-object (object :: <interface-header>, stream :: <stream>)
  next-method();
  format(stream, ":{%s}", object.interface-header-local-name.identifier-label);
end method;

define method print-object (object :: <idl-parser-state>, stream :: <stream>)
  next-method();
  format(stream, ":{%s}", object.syntax-error-message);
end method;

define method print-object (object :: <idl-type>, stream :: <stream>)
  next-method();
  format(stream, ":{%s}", object.idl-type-name);
end method;

define method print-object (object :: <ast-expression>, stream :: <stream>)
  next-method();
  format(stream, ":{%s}", object.expression-combinator.combinator-name);
end method;

define method where-is (query :: <string>)
  let results = make(<deque>);
  where-is-rec(*root*, make(<ast-identifier>, label: query), results);
  results;
end method;

define method where-is-rec (scope :: <object>, identifier :: <ast-identifier>, results :: <deque>)
end method;

define method where-is-rec (scope :: <scope>, identifier :: <ast-identifier>, results :: <deque>)
  let decl = resolve-identifier(scope, identifier);
  if (decl)
    push(results, list(scope, decl, decl.declarator-scoped-name, decl.declarator-local-name));
  end if;
  for (type in scope.scope-local-types)
    where-is-rec(type, identifier, results);
  end for;
  for (declarator in scope.scope-declarators)
    where-is-rec(declarator, identifier, results);
  end for;
end method;

