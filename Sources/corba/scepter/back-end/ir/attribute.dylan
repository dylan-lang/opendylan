Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-attribute>, container :: corba/<InterfaceDef>)
 => (irobject :: corba/<AttributeDef>)
  let id = node.declarator-repository-id;
  let name = identifier-label(node.declarator-local-name);
  let version = node.declarator-repository-id-version;
  let type = load-definition(back-end, node.field-type);
  let mode = if (node.attribute-read-only?)
               #"ATTR-READONLY";
             else
               #"ATTR-NORMAL"
             end if;
  corba/InterfaceDef/create-attribute(container, id, name, version, type, mode);
end method;

