Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-interface>, container :: corba/<container>)
 => (irobject :: corba/<irobject>)
  let id = declarator-repository-id(node);
  let name = identifier-label(declarator-local-name(node));
  let version = declarator-repository-id-version(node);
  let base-interfaces = make(corba/<InterfaceDefSeq>);
  corba/container/create-interface(container, id, name, version, base-interfaces);
end method;

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-forward-interface>, container :: corba/<Container>)
 => (irobject :: corba/<irobject>)
  load-definition(back-end, node.full-definition);
end method;

define method load-deferred-contents (back-end :: <ir-back-end>, node :: <ast-interface>, def :: corba/<InterfaceDef>)
 => ()
  let base-interface-nodes = node.interface-inherits;
  let base-interfaces = make(corba/<InterfaceDefSeq>);
  for (base-node in base-interface-nodes)
    add!(base-interfaces, load-definition(back-end, base-node));
  end for;
  corba/InterfaceDef/base-interfaces(def) := base-interfaces;
  for (declarator in node.scope-declarators)
    when (emit-declarator?(back-end, declarator))
      load-definition(back-end, declarator, container: def);
    end when;
  end for;
end method;

