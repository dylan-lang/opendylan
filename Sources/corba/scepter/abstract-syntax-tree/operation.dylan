Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <operation-flag> (<object>)
  constant slot operation-flag-name :: <string>, required-init-keyword: name:;
end class;

define constant $no-operation-flag = make(<operation-flag>, name: "");
define constant $oneway-operation-flag = make(<operation-flag>, name: "oneway");
define constant $idempotent-operation-flag = make(<operation-flag>, name: "idempotent");

define class <exception-collection> (<object>)
  slot collection :: false-or(<sequence>) = #f, init-keyword: collection:;
end class;

define class <context-collection> (<object>)
  slot collection :: false-or(<sequence>) = #f, init-keyword: collection:;
end class;

define class <ast-operation> (<ast-declarator>, <scope>)
  constant slot operation-return-type :: false-or(<ast-type>) = #f, init-keyword: return-type:;
  constant slot operation-flag :: <operation-flag> = $no-operation-flag, init-keyword: flag:;
  slot operation-context :: false-or(<sequence>) = #f, init-keyword: context:;
  slot operation-exceptions :: false-or(<sequence>) = #f, init-keyword: exceptions:;
end class;

define method initialize (operation :: <ast-operation>, #key)
  next-method();
  if (operation.operation-flag == $oneway-operation-flag &
      operation.operation-return-type &
      ~(operation.operation-return-type.predefined-type == $void-idl-type))
        error(make(<idl-nonvoid-oneway-operation>, declarators: vector(operation)));
  end if;
end method;

define method can-be-redefined? (operation :: <ast-operation>)
  #f;
end method;

define method add-declarator-to-scope (scope :: <scope>, operation :: <ast-operation>)
  next-method();
  unless (operation.operation-return-type.declarator-added?)
    add-type(operation.operation-return-type)
  end unless;
end method;

define method add-declarator (operation :: <ast-operation>, context :: <context-collection>)
  operation.operation-context := context.collection;
end method;

define method add-declarator (operation :: <ast-operation>, exceptions :: <exception-collection>)
  if (exceptions.collection)
    unless (operation.operation-exceptions)
      operation.operation-exceptions := make(<stretchy-vector>);
    end unless;
    for (name in exceptions.collection)
      let excep = resolve-scoped-name(operation, name, reference?: #t, error?: #t);
      unless (instance?(excep, <ast-exception>))
        error(make(<idl-lookup-error>, name: name));
      end unless;
      operation.operation-exceptions := add!(operation.operation-exceptions, excep);
    end for;
  end if;
end method;

define method check-before-add (operation :: <ast-operation>, argument :: <ast-argument>)
  next-method();
  if ((argument.argument-direction == $out-argument-direction |
       argument.argument-direction == $inout-argument-direction) &
      operation.operation-flag == $oneway-operation-flag)
    error(make(<idl-oneway-operation-conflict>, declarators: vector(operation, argument)));
  end if;
end method;
      
