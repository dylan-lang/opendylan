Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define scepter-back-end <ir-back-end>
  command-line-syntax: "ir",
  filename-extension: ""
  slot ir-back-end-repository :: corba/<repository> = make-nil(corba/<repository>);
end scepter-back-end;

define generic definition-irobject (back-end :: <ir-back-end>, node :: <ast-declarator>) => (irobject :: corba/<irobject>);
define generic definition-exists? (back-end :: <ir-back-end>, node :: <ast-declarator>) => (exists? :: <boolean>);
define generic destroy-definition (back-end :: <ir-back-end>, node :: <ast-declarator>) => ();
define generic load-definition (back-end :: <ir-back-end>, node :: <ast-declarator>, #key container) => (irobject :: corba/<irobject>);
define generic load-definition-in-container (back-end :: <ir-back-end>, node :: <ast-declarator>, container :: corba/<container>) => (irobject :: corba/<irobject>);
define generic create-definition-in-container (back-end :: <ir-back-end>, node :: <ast-declarator>, container :: corba/<container>) => (irobject :: corba/<irobject>);
define generic load-interface-repository (back-end :: <ir-back-end>, root :: <ast-root>) => ();

define constant $ir-version = "1.0";

define method scepter-back-end-banner (back-end :: subclass(<ir-back-end>), stream :: <stream>)
 => ()
  let scepter = get-scepter();
  format(stream, "\n%s, Interface Repository version %s", scepter.scepter-program-name, $ir-version);
end method;

define method scepter-back-end-emit (back-end :: <ir-back-end>, root :: <ast-root>, front-end :: <scepter-front-end>, source :: <scepter-source>)
 => (result :: <scepter-back-end-result>)
  load-interface-repository(back-end, root);
  make(<scepter-back-end-result>, success?: #t);
end method;

define method definition-irobject (back-end :: <ir-back-end>, node :: <ast-declarator>)
 => (irobject :: corba/<irobject>)
  let id = declarator-repository-id(node);
  let repository = ir-back-end-repository(back-end);
  corba/repository/lookup-id(repository, id);
end method;

define method definition-exists? (back-end :: <ir-back-end>, node :: <ast-declarator>)
 => (exists? :: <boolean>)
  let irobject = definition-irobject(back-end, node);
  ~corba/object/is-nil(irobject);
end method;

define method destroy-definition (back-end :: <ir-back-end>, node :: <ast-declarator>)
 => ()
  let irobject = definition-irobject(back-end, node);
  unless (corba/object/is-nil(irobject))
    corba/irobject/destroy(irobject);
  end unless;
end method;

define variable *override?* :: <boolean> = #t;

define method load-definition-in-container (back-end :: <ir-back-end>, node :: <ast-declarator>, container :: corba/<container>)
 => (irobject :: corba/<irobject>)
  if (definition-exists?(back-end, node))
    if (*override?*)
      destroy-definition(back-end, node);
    else
      error("Definition for %s already exists in IR", node);
    end if;
  end if;
  let irobject = create-definition-in-container(back-end, node, container);
  load-deferred-contents(back-end, node, irobject);
  irobject;
end method;

define method load-definition (back-end :: <ir-back-end>, node :: <ast-declarator>,
                               #key container :: false-or(corba/<Container>))
 => (irobject :: corba/<irobject>)
  let irobject = definition-irobject(back-end, node);
  if (CORBA/Object/is-nil(irobject))
    unless (container)
      let scope = declarator-scope(node);
      container := find-container(back-end.ir-back-end-repository, scope);
    end unless;
    load-definition-in-container(back-end, node, container);
  else
    irobject;
  end if;
end method;

define method load-deferred-contents (back-end :: <ir-back-end>, node :: <ast-declarator>, def :: corba/<IRObject>)
 => ()
end method;

define method load-interface-repository (back-end :: <ir-back-end>, root :: <ast-root>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let ir = as(corba/<Repository>, corba/orb/resolve-initial-references(orb, "InterfaceRepository"));
  if (corba/object/is-nil(ir))
    error("Could not get reference to Interface Repository");
  end if;
  back-end.ir-back-end-repository := ir;
  for (declarator in root.scope-declarators)
    when (emit-declarator?(back-end, declarator))
      load-definition(back-end, declarator);
    end when;
  end for;
end method;

define method find-container (repository :: corba/<Repository>, scope :: <scope>)
 => (container :: corba/<IRObject>)
  let id = declarator-repository-id(scope);
  let container = corba/Repository/lookup-id(repository, id);
  assert(~corba/object/is-nil(container), "Repository search for %s failed", scope);
  container;
end method;

define method find-container (repository :: corba/<Repository>, scope :: <ast-root>)
 => (container :: corba/<Repository>)
  repository;
end method;

