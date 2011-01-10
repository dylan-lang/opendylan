Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-structure>, container :: corba/<container>)
 => (irobject :: corba/<StructDef>)
  let id = declarator-repository-id(node);
  let name = identifier-label(declarator-local-name(node));
  let version = declarator-repository-id-version(node);
  let members = make(corba/<StructMemberSeq>);
  corba/container/create-struct(container, id, name, version, members);
end method;

define method load-deferred-contents (back-end :: <ir-back-end>, node :: <ast-structure>, def :: corba/<StructDef>)
 => ()
  let members = make(corba/<StructMemberSeq>);
  for (field in node.scope-declarators)
    when (emit-declarator?(back-end, field))
      add!(members, make(corba/<StructMember>,
			 name: identifier-label(declarator-local-name(field)),
			 type: CORBA/$void-typecode,
			 type-def: load-definition(back-end, field.field-type)));
    end when;
  end for;
  def.corba/StructDef/members := members;
end method;

