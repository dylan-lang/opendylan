Module:    scepter-ast
Author:    Keith Dennison, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open class <idl-declarators-error> (<idl-error>)
  constant slot idl-condition-declarators :: <vector>, required-init-keyword: declarators:;
end class;

define method idl-condition-source (condition :: <idl-declarators-error>)
 => (source :: <scepter-source>)
  let decls = condition.idl-condition-declarators;
  debug-assert(size(decls) > 0);
  decls[0].declarator-source;
end method;

define method idl-condition-body (stream :: <stream>, condition :: <idl-declarators-error>)
 => ()
  print-separated-collection(condition.idl-condition-declarators,
                             ", ",
                             stream,
                             key: declarator-name-as-string);
end method;

define class <idl-illegal-redefinition> (<idl-declarators-error>)
  inherited slot idl-condition-string = "illegal redefinition ";
end class;

define class <idl-redefinition-after-use> (<idl-declarators-error>)
  inherited slot idl-condition-string = "redefinition after use, ";
end class;

define class <idl-redefinition-in-scope> (<idl-error>)
  constant slot idl-condition-declarator, init-keyword: declarator:;
  constant slot idl-condition-scope, init-keyword: scope:;
  inherited slot idl-condition-string = "redefinition inside defining scope: ";
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-redefinition-in-scope>)
 => ()
  format(stream, "%s, %s",
         declarator-name-as-string(condition.idl-condition-declarator),
         declarator-name-as-string(condition.idl-condition-scope));
end method;

define class <idl-lookup-error> (<idl-error>)
  inherited slot idl-condition-string = "error in lookup of symbol: ";
  constant slot idl-condition-declarator-name, init-keyword: name:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-lookup-error>)
 => ()
  format(stream, "%s", scoped-name-as-string(condition.idl-condition-declarator-name));
end method;

define class <idl-forward-declarator-lookup-error> (<idl-error>)
  constant slot idl-condition-declarator-name, init-keyword: name:;
  constant slot idl-condition-interface, init-keyword: interface:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-forward-declarator-lookup-error>)
 => ()
  format(stream, "trying to look up %s in undefined forward declared interface %s",
         scoped-name-as-string(condition.idl-condition-declarator-name),
         identifier-label(declarator-local-name(condition.idl-condition-interface)));
end method;

define class <idl-declarator-not-defined> (<idl-error>)
  inherited slot idl-condition-string = "forward declared but never defined: ";
  constant slot idl-condition-declarator, init-keyword: declarator:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-declarator-not-defined>)
 => ()
  format(stream, "interface %s",
         identifier-label(declarator-local-name(condition.idl-condition-declarator)));
end method;

define class <idl-scope-conflict> (<idl-declarators-error>)
  inherited slot idl-condition-string = "definition scope is different than forward declared scope, ";
end class;

define class <idl-coercion-failure> (<idl-error>)
  inherited slot idl-condition-string = "coercion failure ";
  constant slot coercion-expression-value, init-keyword: value:;
  constant slot coercion-expression-type, init-keyword: type:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-coercion-failure>)
 => ()
  format(stream, "%s to %s",
         condition.coercion-expression-value,
         condition.coercion-expression-type.idl-type-name);
end method;

define class <idl-evaluation-error> (<idl-error>)
  inherited slot idl-condition-string = "expression evaluation error: ";
  constant slot idl-condition-declarator, init-keyword: declarator:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-evaluation-error>)
 => ()
  format(stream, "%s", declarator-name-as-string(condition.idl-condition-declarator));
end method;

define class <idl-constant-expected> (<idl-error>)
  inherited slot idl-condition-string = "constant expected: ";
  constant slot idl-condition-declarator, init-keyword: declarator:;
  constant slot idl-condition-constant, init-keyword: constant:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-constant-expected>)
 => ()
  format(stream, "%s bound to %s",
         declarator-name-as-string(condition.idl-condition-constant),
         declarator-name-as-string(condition.idl-condition-declarator));
end method;

define class <idl-nonvoid-oneway-operation> (<idl-declarators-error>)
  inherited slot idl-condition-string = "non-void return type in oneway operation: ";
end class;

define class <idl-oneway-operation-conflict> (<idl-declarators-error>)
  inherited slot idl-condition-string = "oneway operation with OUT or INOUT parameters, ";
end class;

define class <idl-not-a-type> (<idl-declarators-error>)
  inherited slot idl-condition-string = "specified symbol is not a type: ";
end class;

define class <idl-duplicate-union-branch-label> (<idl-declarators-error>)
  inherited slot idl-condition-string = "union with duplicate branch label ";
end class;

define class <idl-enum-value-expected> (<idl-error>)
  inherited slot idl-condition-string = "enumerator expected: ";
  constant slot idl-condition-union-branch-label, init-keyword: label:;
  constant slot idl-condition-union, init-keyword: union:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-enum-value-expected>)
 => ()
  format(stream, " union %s",
         declarator-name-as-string(condition.idl-condition-union),
         condition.idl-condition-union-branch-label);
end method;

define class <idl-enum-value-lookup-failure> (<idl-error>)
  inherited slot idl-condition-string = "enumerator by this name is not defined: ";
  constant slot idl-condition-union, init-keyword: union:;
  constant slot idl-condition-declarator, init-keyword: declarator:;
  constant slot idl-condition-enum, init-keyword: enum:;
end class;

define method idl-condition-body (stream :: <stream>, condition :: <idl-enum-value-lookup-failure>)
 => ()
  format(stream, "union %s, enum %s enumerator %s",
         identifier-label(condition.idl-condition-union.declarator-local-name),
         identifier-label(condition.idl-condition-enum.declarator-local-name),
         declarator-name-as-string(condition.idl-condition-declarator));
end method;
