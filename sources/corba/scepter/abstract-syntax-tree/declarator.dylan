Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <ast-declarator> (<scepter-declarator>)
  slot declarator-imported? :: <boolean>,
    init-function: method () scepter-imported?(get-scepter()); end method;
//  slot declarator-in-main-file? :: <boolean>,
//    init-function: method () *in-main-file?*; end method;
  constant slot declarator-scope :: false-or(<scope>),
    init-keyword: scope:,
    init-function: method () if (~empty?(scepter-scopes(get-scepter()))) first(scepter-scopes(get-scepter())) end if; end method;
  slot declarator-source :: false-or(<scepter-source>),
    init-keyword: source:,
    init-function: compose(scepter-source, get-scepter);
  slot declarator-scoped-name :: <ast-scoped-name> = make(<ast-scoped-name>),
    init-keyword: scoped-name:;
  slot declarator-pragmas :: <stretchy-vector> =  make(<stretchy-vector>);
  slot declarator-added? :: <boolean> = #f;
  slot declarator-repository-id-internal :: false-or(<string>) = #f;
  slot declarator-repository-id-prefix :: false-or(<string>) = #f;
  slot declarator-repository-id-version :: <string> = "1.0";
end class;

define sealed domain make (subclass(<ast-declarator>));
define sealed domain initialize (<ast-declarator>);

define open generic declarator-scoped-name (declarator :: <object>)
 => (scoped-name :: <ast-scoped-name>);

define method can-be-redefined? (declarator :: <ast-declarator>)
  #f;
end method;

// INITIALIZE
//
// Builds scoped-name depending on whether scoepd name
// or local-name is given and whether there is a containing scope.
//
define method initialize (declarator :: <ast-declarator>, #rest args, #key local-name, scoped-name)
  next-method();
  let scope = declarator.declarator-scope;
  let local-name = local-name | (scoped-name & scoped-name.last);
  if (local-name)
    let scoped-name = if (scope)
                        copy-sequence(scope.declarator-scoped-name)
                      else
                        make(<stretchy-vector>)
                      end if;
    declarator.declarator-scoped-name := add!(scoped-name, local-name);
  end if;
  if (scope)
    declarator.declarator-repository-id-prefix := scope.scope-repository-id-prefix;
  end if;
  add!(scepter-nodes(get-scepter()), declarator);
end method;

define method declarator-local-name (declarator :: <ast-declarator>)
//  if (~ declarator.declarator-scoped-name.empty?)
    declarator.declarator-scoped-name.last;
//  end if;
end method;

define method declarator-name-as-string (declarator :: <ast-declarator>)
 => (name :: <string>)
  with-output-to-string (stream)
    print-separated-collection(declarator.declarator-scoped-name, "::", stream,
                               key: identifier-label,
                               printer: method (x, stream) write(stream, x) end)
  end with-output-to-string;
end method;

define function declarator-repository-id (declarator :: <ast-declarator>)
 => (id :: <string>)
  if (declarator-repository-id-internal(declarator))
    declarator-repository-id-internal(declarator);
  else
    let id = "IDL:";
    let prefix = declarator-repository-id-prefix(declarator);
    if (prefix & (prefix ~= ""))
      id := concatenate(id, prefix, "/");
    end if;
    let scoped-name = declarator-scoped-name(declarator);
    assert(size(scoped-name) > 1);
//    assert(identifier-label(scoped-name[0]) = "::");
    concatenate(id, identifier-label(scoped-name.last), ":", declarator-repository-id-version(declarator));
  end if;
end function;

define method has-ancestor?(declarator :: <ast-declarator>, scope :: <ast-declarator>)
  => (has-ancestor? :: <boolean>)
  case
    (declarator == scope) => #t;
    (~(declarator.declarator-scope)) => #f;
    otherwise => has-ancestor?(declarator.declarator-scope, scope);
  end case;
end method;

define method add-type (declarator :: <ast-declarator>)
end method;



