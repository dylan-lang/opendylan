Module:    scepter-ast
Author:    Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// bit wastfeful in space at the moment:
// use vectors to preserve order for dumping
// and tables for fast lookup

define class <scope> (<object>)
  constant slot scope-referenced :: <table> = make(<table>);
  constant slot scope-declarators-table :: <identifier-table> = make(<identifier-table>);
  slot scope-declarators :: <stretchy-vector> = make(<stretchy-vector>);
  slot sequence-allows-recursive-types? :: <boolean> = #f;
  slot scope-pragma-prefix :: false-or(<string>) = #f;
end class;

define method scope-repository-id-prefix (scope :: <scope>)
 => (prefix :: <string>)
  if (scope.scope-pragma-prefix)
    scope.scope-pragma-prefix;
  else
    let parent-prefix = scope.declarator-scope & scope.declarator-scope.scope-repository-id-prefix;
    if (parent-prefix)
      concatenate(parent-prefix, "/", scope.declarator-local-name.identifier-label);
    else
      scope.declarator-local-name.identifier-label;
    end if;
  end if;
end method;

define method scope-repository-id-prefix (scope :: <ast-root>)
 => (prefix :: false-or(<string>))
  scope.scope-pragma-prefix;
end method;

define method add-declarator (scope :: <scope>, declarator :: <ast-declarator>)
  block ()
    check-before-add(scope, declarator);
    add-to-scope(scope, declarator);
    add-to-referenced(scope, declarator);
  exception (<idl-condition-restart>)
  end block;
end method;

define method add-to-scope (scope :: <scope>, declarator :: <ast-declarator>)
  scope.scope-declarators := add!(scope.scope-declarators, declarator);
  scope.scope-declarators-table[declarator.declarator-local-name] := declarator;
end method;

define method add-to-scope (scope :: <scope>, declarator :: <ast-sequence>)
end method;

define method add-to-scope (scope :: <scope>, declarator :: <ast-array>)
end method;

define method add-to-scope (scope :: <scope>, declarator :: <ast-string>)
end method;

define method referenced? (scope :: <scope>, declarator :: <ast-declarator>)
  element(scope.scope-referenced, declarator, default: #f);
end method;

define method check-before-add (scope :: <scope>, declarator :: <ast-declarator>)
  => (result :: <boolean>)
  let prior-declarator = resolve-identifier(scope, declarator.declarator-local-name);
  if (prior-declarator)
    case
      ((declarator-scope(prior-declarator) = scope) | ~can-be-redefined?(prior-declarator)) =>
        error(make(<idl-illegal-redefinition>,
                  declarators: vector(declarator, scope, prior-declarator)));
      (referenced?(scope, prior-declarator)) =>
        error(make(<idl-redefinition-after-use>,
                  declarators: vector(declarator, scope, prior-declarator)));
      (has-ancestor?(declarator, prior-declarator)) =>
        error(make(<idl-redefinition-in-scope>, declarator: declarator, scope: prior-declarator));
    end case;
  end if;
end method;

// ADD-TO-REFERENCED

define method add-to-referenced (scope :: <scope>, declarator :: <ast-declarator>, #key recursive? = #f)
  unless (can-be-redefined-after-use?(scope, declarator))
    unless (referenced?(scope, declarator))
      scope.scope-referenced[declarator] := #t;
      if (recursive? &
          ~ has-ancestor?(declarator, scope))
        let defining-scope = declarator.declarator-scope;
        if (defining-scope)
          add-to-referenced(defining-scope, declarator, recursive?: recursive?)
        end if;
      end if;
    end unless;
  end unless;
end method;

define method can-be-redefined-after-use? (scope :: <scope>, declarator :: <ast-declarator>)
  #f;
end method;

define method add-declarator-to-scope (scope :: <scope>, declarator :: <ast-declarator>)
  declarator.declarator-added? := #t;
end method;

define method add-declarator-to-scope (scope :: <scope>, object :: <object>);
end method;

define method call-add (x)
end method;

define method call-add (scope :: <scope>)
  for (declarator in scope.scope-declarators)
    add-declarator-to-scope(scope, declarator);
    call-add(declarator);
  end for;
end method;

