Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-module>, container :: corba/<container>)
 => (irobject :: corba/<ModuleDef>)
  let id = declarator-repository-id(node);
  let name = identifier-label(declarator-local-name(node));
  let version = declarator-repository-id-version(node);
  corba/container/create-module(container, id, name, version);
end method;

define method load-deferred-contents (back-end :: <ir-back-end>, node :: <ast-module>, def :: corba/<ModuleDef>)
 => ()
  for (declarator in node.scope-declarators)
    when (emit-declarator?(back-end, declarator))
      load-definition(back-end, declarator, container: def);
    end when;
  end for;
end method;

