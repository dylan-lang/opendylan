Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-enum>, container :: corba/<container>)
 => (irobject :: corba/<irobject>)
  let id = declarator-repository-id(node);
  let name = identifier-label(declarator-local-name(node));
  let version = declarator-repository-id-version(node);
  let members = make(corba/<EnumMemberSeq>);
  for (field in node.scope-declarators)
    when (emit-declarator?(back-end, field))
      add!(members, identifier-label(declarator-local-name(field)));
    end when;
  end for;
  corba/container/create-enum(container, id, name, version, members);
end method;

define method load-definition (back-end :: <ir-back-end>, value :: <ast-enum-value>, #key container)
 => (irobject :: corba/<IRObject>)
  make-nil(corba/<IRObject>);
end method;

