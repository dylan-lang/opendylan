Module: scepter-file-front-end
Author: Jason Trenouth, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function parser-action-module-identifier (arg$1)
  let scepter = get-scepter();
  let identifier = make(<ast-identifier>, label: arg$1);
  let module = reopen-module(scepter.scepter-scopes.first, identifier);
  unless (module)
    module := define-module(scepter.scepter-scopes.first, identifier);
  end unless;
  push(scepter.scepter-scopes, module);
end function;

define function parser-action-module-qs-seen ()
  pop(scepter-scopes(get-scepter()));
end function;

define function parser-action-interface-header-seen (arg$1)
  build-interface(arg$1);
end function;

define function parser-action-interface-header (arg$1, arg$2)
  make(<interface-header>, local-name: arg$1, name-list: arg$2);
end function;

define function parser-action-scoped-names (names, next-name)
  update-parser-state($idl-parser-scoped-name-seen);
  push-last(names, next-name);
  names;
end function;

define function parser-action-scoped-name$1 (id)
  update-parser-state($idl-parser-scoped-name-id-seen);
  let scoped-name = make(<ast-scoped-name>);
  add!(scoped-name, id);
end function;

define function parser-action-scoped-name$2 (scope-delimiter, id)
  update-parser-state($idl-parser-scoped-name-id-seen);
  let scoped-name = make(<ast-scoped-name>);
  scoped-name := add!(scoped-name, make(<ast-identifier>, label: "::"));
  add!(scoped-name, id);
end function;

define function parser-action-scoped-name$3 (scoped-name, scope-delimiter, id)
  update-parser-state($idl-parser-scoped-name-id-seen);
  add!(scoped-name, id);
end function;

define function parser-action-id (identifier)
  make(<ast-identifier>, label: identifier);
end function;

define function parser-action-forward (interface-decl)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  let forward-interface = make(<ast-forward-interface>,
			       local-name: interface-decl,
			       pragmas: scepter.scepter-pragmas);
  update-parser-state($idl-parser-forward-declaration-seen);
  add-declarator(scope, forward-interface);
end function;

define function parser-action-const-dcl (type, name, expression)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-constant-expression-seen);
  if (expression)
    coerce(expression, type.declarator-base-type.predefined-type);
    let constant = make(<ast-constant>,
                        type: type,
                        value: expression,
                        local-name: name,
                        pragmas: scepter.scepter-pragmas);
    add-declarator(scope, constant);
  end if;
end function;

define function parser-action-positive-int-expr (const-expr)
  evaluate(const-expr, $constant-evaluation-kind);
  make(<ast-expression>, expression: const-expr, type: $ulong-idl-type);
end function;

define function parser-action-type-declarator (type-spec, declarators)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-declarators-seen);
  if (type-spec & declarators)
    for (declarator in declarators)
      let typedef = make(<ast-typedef>,
			 type: compose-declarator-type(declarator, type-spec),
			 scoped-name: declarator.declarator-scoped-name,
			 pragmas: scepter.scepter-pragmas);
      add-declarator(scope, typedef);
    end for;
  end if;
end function;

define function parser-action-struct-type-id-seen (id)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-struct-id-seen);
  let struct = make(<ast-structure>, local-name: id, pragmas: scepter.scepter-pragmas);
  add-declarator(scope, struct);
  push(scepter.scepter-scopes, struct);
end function;

define function parser-action-member (type-spec, declarators)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-member-declarators-completed);
  if (type-spec & illegal-recursive-type?(type-spec))
    error(make(<idl-illegal-recursive-type>, declarators: vector(type-spec)));
  end if;
  if (type-spec & declarators)
    for (declarator in declarators)
      let type = compose-declarator-type(declarator, type-spec);
      let field = make(<ast-field>,
		       type: type,
		       scoped-name: declarator.declarator-scoped-name,
		       pragmas: scepter.scepter-pragmas);
      add-declarator(scope, field);
    end for;
  end if;
end function;

define function parser-action-union-type-start (name, type)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-switch-close-paren-seen);
  let union = make(<ast-union>,
		   type: type,
		   local-name: name,
		   pragmas: scepter.scepter-pragmas);
  add-declarator(scope, union);
  push(scepter.scepter-scopes, union);
end function;

define function parser-action-case-branch (case-labels, field)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-union-element-completed);
  if (case-labels & field)
    let branch = make(<ast-union-branch>,
		      labels: case-labels,
		      type: field.field-type,
		      scoped-name: field.declarator-scoped-name,
		      pragmas: field.declarator-pragmas);
    add-declarator(scope, branch);
  end if;
end function;

define function parser-action-element-spec (type-spec, declarator)
  let scepter = get-scepter();
  update-parser-state($idl-parser-union-element-declarator-seen);
  if (type-spec & illegal-recursive-type?(type-spec))
    error(make(<idl-illegal-recursive-type>, declarators: vector(type-spec)));
  end if;
  if (type-spec & declarator)
    let type = compose-declarator-type(declarator, type-spec);
    make(<ast-field>,
	 type: type,
	 scoped-name: declarator.declarator-scoped-name,
	 pragmas: scepter.scepter-pragmas);
  end if;
end function;

define function parser-action-enum-type-id-seen (id)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-enum-id-seen);
  let enum = make(<ast-enum>, local-name: id, pragmas: scepter.scepter-pragmas);
  add-declarator(scope, enum);
  push(scepter.scepter-scopes, enum);
end function;

define function parser-action-enumerator (identifier)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  let enum-value = make(<ast-enum-value>,
			local-name: make(<ast-identifier>, label: identifier),
			pragmas: scepter.scepter-pragmas);
  add-declarator(scope, enum-value);
end function;

define function parser-action-sequence-type-spec$1 (type, expression)
  update-parser-state($idl-parser-sequence-qs-seen);
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  scope.sequence-allows-recursive-types? := #f;
  coerce(expression, $ulong-idl-type);
  let sequence = make(<ast-sequence>, type: type, size: expression);
  add-declarator(scepter.scepter-root, sequence);
  sequence;
end function;

define function parser-action-sequence-type-spec$2 (type)
  update-parser-state($idl-parser-sequence-qs-seen);
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  scope.sequence-allows-recursive-types? := #f;
  let sequence = make(<ast-sequence>, type: type);
  add-declarator(scepter.scepter-root, sequence);
  sequence;
end function;

define function parser-action-seq-head-sequence-seen ()
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-sequence-seen);
  scope.sequence-allows-recursive-types? := #t;
end function;

define function parser-action-string-type-spec$1 (expression)
  update-parser-state($idl-parser-string-qs-seen);
  let scepter = get-scepter();
  coerce(expression, $ulong-idl-type);
  let string = make(<ast-string>, size: expression);
  add-declarator(scepter.scepter-root, string);
  string;
end function;

define function parser-action-string-type-spec$2 ()
  update-parser-state($idl-parser-string-completed);
  let scepter = get-scepter();
  let string = make(<ast-string>);
  add-declarator(scepter.scepter-root, string);
  string;
end function;

define function parser-action-wstring-type-spec$1 (expression)
  update-parser-state($idl-parser-string-qs-seen);
  let scepter = get-scepter();
  coerce(expression, $ulong-idl-type);
  let wstring = make(<ast-wstring>, size: expression);
  add-declarator(scepter.scepter-root, wstring);
  wstring;
end function;

define function parser-action-wstring-type-spec$2 ()
  update-parser-state($idl-parser-string-completed);
  let scepter = get-scepter();
  let wstring = make(<ast-wstring>);
  add-declarator(scepter.scepter-root, wstring);
  wstring;
end function;

define function parser-action-attribute-dcl (flag, type-spec, declarators)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-attribute-completed);
  if (type-spec & declarators)
    for (declarator in declarators)
      let type = compose-declarator-type(declarator, type-spec);
      let attribute = make(<ast-attribute>,
			   read-only?: flag,
			   type: type,
			   scoped-name: declarator.declarator-scoped-name,
			   pragmas: scepter.scepter-pragmas);
      add-declarator(scope, attribute);
    end for;
  end if;
end function;

define function parser-action-exception-id-seen (id)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-exception-id-seen);
  let excep = make(<ast-exception>, local-name: id, pragmas: scepter.scepter-pragmas);
  add-declarator(scope, excep);
  push(scepter.scepter-scopes, excep);
end function;

define function parser-action-operation (raises, context)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-operation-completed);
  add-declarator(scope, raises);
  add-declarator(scope, context);
  pop(scepter.scepter-scopes);
end function;

define function parser-action-operation-id-seen (flag, type, identifier)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  let identifier = make(<ast-identifier>, label: identifier);
  update-parser-state($idl-parser-operation-id-seen);
  if (type)
    if (instance?(type, <ast-exception>))
      error(make(<idl-not-a-type>, declarators: vector(type)));
    end if;
    let operation = make(<ast-operation>,
			 flag: flag,
			 return-type: type,
			 local-name: identifier,
			 pragmas: scepter.scepter-pragmas);
    add-declarator(scope, operation);
    push(scepter.scepter-scopes, operation);
  end if;
end function;

define function parser-action-operation-raises-completed (raises)
  update-parser-state($idl-parser-operation-raises-completed);
  let exceptions = make(<exception-collection>);
  exceptions.collection := raises;
  exceptions;
end function;

define function parser-action-parameter (direction, type, declarator)
  let scepter = get-scepter();
  let scope = scepter.scepter-scopes.first;
  update-parser-state($idl-parser-operation-parameter-declarator-seen);
  if (type & declarator)
    let type = compose-declarator-type(declarator, type);
    let argument = make(<ast-argument>,
			direction: direction,
			type: type,
			scoped-name: declarator.declarator-scoped-name,
			pragmas: scepter.scepter-pragmas);
    add-declarator(scope, argument);
  end if;
end function;

define function parser-action-opt-context (string-literals)
  update-parser-state($idl-parser-operation-context-qs-seen);
  let context = make(<context-collection>);
  context.collection := string-literals;
  context;
end function;
