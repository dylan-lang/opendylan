Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-union>, container :: CORBA/<container>)
 => (irobject :: CORBA/<irobject>)
  let id = declarator-repository-id(node);
  let name = identifier-label(declarator-local-name(node));
  let version = declarator-repository-id-version(node);
  let discriminator-type = load-definition(back-end, node.union-discriminator-type);
  let members = make(CORBA/<UnionMemberSeq>);
  CORBA/Container/create-union(container, id, name, version, discriminator-type, members);
end method;

define method label-as-any (tc :: CORBA/<TypeCode>, branch-label :: <ast-union-branch-label>)
  let value = expression-value(branch-label.union-branch-label-value);
  if (CORBA/TypeCode/kind(tc) = #"tk-enum")
    value := as(<symbol>, CORBA/TypeCode/member-name(tc, value));
  end if;
  make(CORBA/<any>, type: tc, value: value);
end method;

define method label-as-any (tc :: CORBA/<TypeCode>, branch-label :: <ast-default-union-branch-label>)
  make(CORBA/<any>, type: CORBA/$octet-typecode, value: 0);
end method;

define method load-deferred-contents (back-end :: <ir-back-end>, node :: <ast-union>, def :: CORBA/<UnionDef>)
 => ()
  let tc = CORBA/UnionDef/discriminator-type(def);
  let members = make(corba/<UnionMemberSeq>);
  for (field in node.scope-declarators)
    when (emit-declarator?(back-end, field))
      let name = identifier-label(declarator-local-name(field));
      let type-def = load-definition(back-end, field.field-type);
      for (label in field.union-branch-labels)
        add!(members, make(CORBA/<UnionMember>,
                           name: name,
                           label: label-as-any(tc, label),
                           type: CORBA/$void-typecode,
                           type-def: type-def));
      end for;
    end when;
  end for;
  def.corba/UnionDef/members := members;
end method;

