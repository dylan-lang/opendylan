Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-typedef>, container :: corba/<container>)
 => (irobject :: corba/<irobject>)
  let id = declarator-repository-id(node);
  let name = identifier-label(declarator-local-name(node));
  let version = declarator-repository-id-version(node);
  let original-type = load-definition(back-end, node.typedef-base-type);
  corba/container/create-alias(container, id, name, version, original-type);
end method;
