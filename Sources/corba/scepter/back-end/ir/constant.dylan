Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-constant>, container :: CORBA/<Container>)
 => (irobject :: CORBA/<ConstantDef>)
  let id = declarator-repository-id(node);
  let name = identifier-label(declarator-local-name(node));
  let version = declarator-repository-id-version(node);
  let type = load-definition(back-end, node.constant-expression-type);
  let value = make(CORBA/<any>, type: type.CORBA/IDLType/type, value: expression-value(node.constant-value));
  CORBA/Container/create-constant(container, id, name, version, type, value);
end method;

