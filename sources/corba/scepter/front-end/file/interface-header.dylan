Module: scepter-file-front-end
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <interface-header> (<object>)
  constant slot interface-header-local-name :: <ast-identifier>, init-keyword: local-name:;
  slot interface-header-inherits :: <stretchy-vector> = make(<stretchy-vector>), init-keyword: inherits:;
end class;

define method initialize (interface-header :: <interface-header>, #rest args, #key name-list)
  next-method();
  compile-inheritance(interface-header, name-list);
end method;

define method compile-inheritance (interface-header :: <interface-header>,
                                   name-list :: false-or(<deque>))
  if (name-list)
    for (name in name-list)
      let scepter = get-scepter();
      let declarator = resolve-scoped-name(scepter.scepter-scopes.first, name, reference?: #t, error?: #t);
      declarator := declarator.declarator-base-type;
      unless (instance?(declarator, <ast-interface>))
        error(make(<idl-illegal-inheritance>, name: interface-header.interface-header-local-name, declarator: declarator));
      end unless;
      unless (declarator.interface-defined?)
        error(make(<idl-illegal-inheritance>, name: interface-header.interface-header-local-name, declarator: declarator));
      end unless;
      interface-header.interface-header-inherits :=
        add-new!(interface-header.interface-header-inherits, declarator);
    end for;
  end if;
end method;

define method build-interface (interface-header :: <interface-header>)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  let scoped-name = copy-sequence(scope.declarator-scoped-name);
  add!(scoped-name, interface-header.interface-header-local-name);
  let declarator = resolve-scoped-name(scope, scoped-name);

  let interface =
    if (declarator &
	  instance?(declarator, <ast-interface>) &
	  ~ interface-defined?(declarator))
      if (~ declarator.declarator-scope == scope)
	error(make(<idl-scope-conflict>, declarators: vector(declarator, scope)));
      else
	declarator.interface-inherits := interface-header.interface-header-inherits;
	declarator.interface-defined? := #t;
	declarator.declarator-imported? := scepter.scepter-imported?;
//	declarator.declarator-in-main-file? := scepter.scepter-in-main-file?;
	declarator.declarator-source := scepter.scepter-source;
	declarator.declarator-pragmas := scepter.scepter-pragmas;
	declarator;
      end if;
    else
      make(<ast-interface>,
	   local-name: interface-header.interface-header-local-name,
	   inherits: interface-header.interface-header-inherits);
    end if;
  add-declarator(scope, interface);
  push(scepter.scepter-scopes, interface);
end method;
