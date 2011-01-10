Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TRACE LIST:
// 	resolve-scoped-name
// 	do-resolve-scoped-name
// 	resolve-scoped-name-head-via-scopes
// 	resolve-scoped-name-head-via-inherited
// 	resolve-identifier
// 	resolve-primitive-type
//

// RESOLVE-SCOPED-NAME

define method resolve-scoped-name
  (scope :: <scope>, name :: <ast-scoped-name>,
  #key error? :: <boolean> = #f, reference? :: <boolean> = #f, start :: <integer> = 0)
  let declarator = do-resolve-scoped-name(scope, name, reference?: reference?, start: start);
  declarator |
    (error? & signal-lookup-error(name));
end method;

define method signal-lookup-error (name :: <ast-scoped-name>)
  block ()
    error(make(<idl-lookup-error>, name: name));
  exception (condition :: <idl-condition-restart>)
    #f;
  end block;
end method;

// DO-RESOLVE-SCOPED-NAME
//
// Could put caching tables here at this top level
// (and even remove ordinary lookup tables from scopes)

define method do-resolve-scoped-name
  (scope :: <scope>, name :: <ast-scoped-name>, #key start :: <integer> = 0, reference? :: <boolean> = #f)
  let declarator =
    if (name[start].global-name?)
      start := start + 1;
      resolve-scoped-name-head-via-scopes(scepter-root(get-scepter()), name[start]);
    else
      resolve-scoped-name-head-via-scopes(scope, name[start]);
    end if;
  declarator & resolve-scoped-name-tail(scope, declarator, name, start: start + 1, reference?: reference?);
end method;

// RESOLVE-SCOPED-NAME-HEAD-VIA-SCOPES

define method resolve-scoped-name-head-via-scopes (scope == #f, identifier :: <ast-identifier>)
  #f;
end method;

define method resolve-scoped-name-head-via-scopes (scope :: <scope>, identifier :: <ast-identifier>)
  resolve-identifier(scope, identifier)
  | resolve-scoped-name-head-via-scopes(scope.declarator-scope, identifier)
  | resolve-scoped-name-head-via-inherited(scope, identifier)
end method;

// RESOLVE-SCOPED-NAME-HEAD-VIA-INHERITED

define method resolve-scoped-name-head-via-inherited (scope :: <scope>, name :: <ast-identifier>)
  #f;
end method;

define method resolve-scoped-name-head-via-inherited (interface :: <ast-interface>, identifier :: <ast-identifier>)
  if (interface.interface-defined?)
    any?(rcurry(resolve-identifier, identifier), interface.interface-inherits)
    | any?(rcurry(resolve-scoped-name-head-via-inherited, identifier), interface.interface-inherits);
  else
    error(make(<idl-forward-declarator-lookup-error>, interface: interface, name: identifier));
  end if;
end method;

// RESOLVE-SCOPED-NAME-TAIL

define method resolve-scoped-name-tail
  (scope :: <scope>, declarator :: <ast-declarator>, scoped-name :: <ast-scoped-name>,
  #key start :: <integer> = 0, reference? :: <boolean> = #f)
  let declarator :: false-or(<ast-declarator>) = declarator;
  block (return)
    for (i from start below scoped-name.size)
      if (reference?)
        add-to-referenced(scope, declarator);
      end if;
      declarator := resolve-identifier(declarator, scoped-name[i]);
      unless (declarator)
        return(#f);
      end unless;
    end for;
    declarator; // used to do & declarator.declarator-base-type;
  end block;
end method;

// RESOLVE-IDENTIFIER

define method resolve-identifier (scope :: <scope>, identifier :: <ast-identifier>)
  let declarator = element(scope.scope-declarators-table, identifier, default: #f);
  declarator & declarator.full-definition;
end method;

// RESOLVE-PRIMITIVE-TYPE

define method predefined-type (x :: <object>)
  #f;
end method;

define method resolve-primitive-type (scope :: <scope>, expression-type :: <idl-type>)
  find(expression-type, scope.scope-declarators, key: predefined-type);
end method;


