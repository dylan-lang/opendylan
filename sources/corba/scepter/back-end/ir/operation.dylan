Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-operation>, container :: corba/<InterfaceDef>)
 => (irobject :: corba/<OperationDef>)
  let id = node.declarator-repository-id;
  let name = identifier-label(node.declarator-local-name);
  let version = node.declarator-repository-id-version;
  let result = load-definition(back-end, node.operation-return-type);
  let mode = if (node.operation-flag = $oneway-operation-flag)
               #"OP-ONEWAY"
             else
               #"OP-NORMAL";
             end if;
  let params = make(corba/<ParDescriptionSeq>);
  let exceptions = make(corba/<ExceptionDefSeq>);
  let contexts = make(corba/<ContextIdSeq>);
  corba/InterfaceDef/create-operation(container, id, name, version, result, mode, params, exceptions, contexts);
end method;

define method load-deferred-contents (back-end :: <ir-back-end>, node :: <ast-operation>, def :: CORBA/<OperationDef>)
 => ()
  let parameters = make(CORBA/<ParDescriptionSeq>);
  for (param in node.scope-declarators)
    when(emit-declarator?(back-end, param))
      add!(parameters, make(CORBA/<ParameterDescription>,
                            name: identifier-label(param.declarator-local-name),
                            type: CORBA/$void-typecode,
                            type-def: load-definition(back-end, param.field-type),
                            mode: select (param.argument-direction)
                                    $in-argument-direction => #"PARAM-IN";
                                    $out-argument-direction => #"PARAM-OUT";
                                    $inout-argument-direction => #"PARAM-INOUT";
                                  end select));
    end when;
  end for;
  CORBA/OperationDef/params(def) := parameters;

  when (node.operation-exceptions)
    let exceptions = make(CORBA/<ExceptionDefSeq>);
    for (exception in node.operation-exceptions)
      when(emit-declarator?(back-end, exception))
        add!(exceptions, load-definition(back-end, exception));
      end when;
    end for;
    CORBA/OperationDef/exceptions(def) := exceptions;
  end when;

  when (node.operation-context)
    let contexts = make(CORBA/<ContextIdSeq>);
    for (id in node.operation-context)
      add!(contexts, id);
    end for;
    CORBA/OperationDef/contexts(def) := contexts;
  end when;

end method;

