Module:    naming-service
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method name-invalid? (name :: CosNaming/<Name>)
 => (invalid? :: <boolean>)
  empty?(name);
end method;

define method validate-name (name :: CosNaming/<Name>)
 => ()
  if (name-invalid?(name))
    error(make(CosNaming/NamingContext/<InvalidName>));
  end if;
end method;

define method name-last-component (name :: CosNaming/<Name>)
 => (new-name :: CosNaming/<Name>)
  assert(name.size > 0, "Empty name passed to name-last-component");
  add!(make(CosNaming/<Name>), name.last);
end method;

define method name-all-but-first-components (name :: CosNaming/<Name>)
 => (new-name :: CosNaming/<Name>)
  assert(name.size > 0, "Empty name passed to name-all-but-first-components");
  copy-sequence(name, start: 1);
end method;

define method name-all-but-last-components (name :: CosNaming/<Name>)
 => (new-name :: CosNaming/<Name>)
  assert(name.size > 0, "Empty name passed to name-all-but-last-components");
  copy-sequence(name, end: (name.size - 1));
end method;


define class <naming-service> (<object>)
  slot naming-service-orb :: CORBA/<Orb>;
  slot naming-service-poa :: PortableServer/<POA>;
end class;

define sealed domain make(singleton(<naming-service>));

define sealed method initialize (object :: <naming-service>, #key)
  next-method();
  object.naming-service-orb := CORBA/orb-init(make(CORBA/<arg-list>), "Functional Developer ORB");
  let root-poa = CORBA/orb/resolve-initial-references(object.naming-service-orb, "RootPOA");
  object.naming-service-poa := PortableServer/POA/create-poa(root-poa, "Naming Context POA", #f);
  let poa-manager = PortableServer/POA/the-poamanager(object.naming-service-poa);
  PortableServer/POAManager/activate(poa-manager);
end method;

define variable *naming-ior-file* :: false-or(<string>) = #f;

define method start-naming-service () => ()
  let naming-service = make(<naming-service>);
  let context-servant = make(<naming-context>, service: naming-service);
  let context-reference = PortableServer/POA/servant-to-reference(naming-service.naming-service-poa, context-servant);
  if (*naming-ior-file*)
    block ()
      corba/orb/object-to-file(naming-service.naming-service-orb, *naming-ior-file*, context-reference);
    exception (<error>)
      format-out("%s: Unable to write IOR to %s\n", application-name(), *naming-ior-file*);
      abort();
    end block;
  end if;
  corba/orb/run(naming-service.naming-service-orb);
end method;

