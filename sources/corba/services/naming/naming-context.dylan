Module:    naming-service
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <name-component-table> (<table>)
end class;
define sealed domain make (singleton(<name-component-table>));
define sealed domain initialize (<name-component-table>);

define method name-component-hash (component :: CosNaming/<NameComponent>, initial-state :: <hash-state>)
 => (id, result-state)
  let id = CosNaming/NameComponent/id(component);
  let kind = CosNaming/NameComponent/kind(component);
  values-hash(string-hash, initial-state, id, kind);
end method;

define method table-protocol (table :: <name-component-table>)
 => (test-fn :: <function>, hash-fn :: <function>)
  values(\=, name-component-hash);
end method;

define sealed class <naming-context> (CosNaming/<NamingContext-servant>)
  constant slot naming-context-service :: <naming-service>, required-init-keyword: service:;
  constant slot naming-context-objects :: <name-component-table>, init-function: curry(make, <name-component-table>);
  constant slot naming-context-contexts :: <name-component-table>, init-function: curry(make, <name-component-table>);
end class;
define sealed domain make (singleton(<naming-context>));
define sealed domain initialize (<naming-context>);

define method as-reference (object :: <object>)
 => (object :: <object>)
  object;
end method;

define method as-reference (object :: <naming-context>)
 => (reference :: CosNaming/<NamingContext>)
    let poa = object.naming-context-service.naming-service-poa;
    as(CosNaming/<NamingContext>, PortableServer/POA/servant-to-reference(poa, object));
end method;

define method naming-context-name-bound? (context :: <naming-context>, name :: CosNaming/<NameComponent>)
 => (bound? :: <boolean>)
  if (element(context.naming-context-objects, name, default: #f)
    | element(context.naming-context-contexts, name, default: #f))
    #t
  else
    #f
  end if;
end method;

define method naming-context-resolve-object (context :: <naming-context>, name :: CosNaming/<Name>)
 => (object :: CORBA/<object>)
  let object = element(context.naming-context-objects, name[0], default: #f)
             | element(context.naming-context-contexts, name[0], default: #f);
  let reference = as-reference(object);
  unless (instance?(reference, CORBA/<Object>))
    error(make(CosNaming/NamingContext/<NotFound>,
               why: #"not-object",
               rest-of-name: name));
  end;
  reference;
end method;

define method naming-context-resolve-context (context :: <naming-context>, name :: CosNaming/<Name>)
 => (context :: CosNaming/<NamingContext>)
  let object = element(context.naming-context-contexts, name[0], default: #f);
  unless (instance?(object, CosNaming/<NamingContext>))
    error(make(CosNaming/NamingContext/<NotFound>,
               why: #"not-context",
               rest-of-name: name));
  end unless;
  object;
end method;

define method CosNaming/NamingContext/bind
    (context :: <naming-context>, name :: CosNaming/<Name>, object :: CORBA/<Object>)
 => ()
  validate-name(name);
  if (name.size = 1)
    let component = name[0];
    if (naming-context-name-bound?(context, component))
      error(make(CosNaming/NamingContext/<AlreadyBound>));
    else
      context.naming-context-objects[component] := object;
    end if;
  else
    let new-context = CosNaming/NamingContext/resolve(context, name-all-but-last-components(name));
    CosNaming/NamingContext/bind(new-context, name-last-component(name), object);
  end if;
end method;

define method CosNaming/NamingContext/rebind
    (context :: <naming-context>, name :: CosNaming/<Name>, object :: CORBA/<Object>)
 => ()
  validate-name(name);
  if (name.size = 1)
    let objects = context.naming-context-objects;
    let component = name[0];
    objects[component] := object;
  else
    let new-context = CosNaming/NamingContext/resolve(context, name-all-but-last-components(name));
    CosNaming/NamingContext/rebind(new-context, name-last-component(name), object);
  end if;
end method;

define sealed method CosNaming/NamingContext/bind-context
    (context :: <naming-context>, name :: CosNaming/<Name>, object :: CosNaming/<NamingContext>)
 => ()
  validate-name(name);
  if (name.size = 1)
    let component = name[0];
    if (naming-context-name-bound?(context, component))
      error(make(CosNaming/NamingContext/<AlreadyBound>));
    else
      context.naming-context-contexts[component] := object;
    end if;
  else
    let new-context = CosNaming/NamingContext/resolve(context, name-all-but-last-components(name));
    CosNaming/NamingContext/bind-context(new-context, name-last-component(name), object);
  end if;
end method;

define method CosNaming/NamingContext/rebind-context
    (context :: <naming-context>, name :: CosNaming/<Name>, object :: CosNaming/<NamingContext>)
 => ()
  validate-name(name);
  if (name.size = 1)
    let contexts = context.naming-context-contexts;
    let component = name[0];
    contexts[component] := object;
  else
    let new-context = CosNaming/NamingContext/resolve(context, name-all-but-last-components(name));
    CosNaming/NamingContext/rebind-context(new-context, name-last-component(name), object);
  end if;
end method;


define sealed method CosNaming/NamingContext/resolve
    (context :: <naming-context>, name :: CosNaming/<Name>)
 => (result :: CORBA/<Object>)
  validate-name(name);
  if (name.size = 1)
    naming-context-resolve-object(context, name);
  else
    let next-context = naming-context-resolve-context(context, name);
    CosNaming/NamingContext/resolve(next-context, name-all-but-first-components(name));
  end if;
end method;

define method CosNaming/NamingContext/unbind
    (context :: <naming-context>, name :: CosNaming/<Name>)
 => ()
  validate-name(name);
  if (name.size = 1)
    remove-key!(context.naming-context-objects, name[0])
    | remove-key!(context.naming-context-contexts, name[0]);
  else
    let new-context = CosNaming/NamingContext/resolve(context, name-all-but-last-components(name));
    CosNaming/NamingContext/unbind(new-context, name-last-component(name));
  end if;
end method;

define sealed method CosNaming/NamingContext/new-context
    (context :: <naming-context>)
 => (new-context :: CosNaming/<NamingContext>);
  make(<naming-context>, service: context.naming-context-service);
end method;

define method CosNaming/NamingContext/bind-new-context
    (context :: <naming-context>, name :: CosNaming/<Name>)
 => (new-context :: CosNaming/<NamingContext>)
  let new-context = CosNaming/NamingContext/new-context(context);
  CosNaming/NamingContext/bind-context(context, name, new-context);
  new-context;
end method;

define method CosNaming/NamingContext/destroy
    (context :: <naming-context>)
 => ()
  if (~empty?(context.naming-context-objects) | ~empty?(context.naming-context-contexts))
    error(make(CosNaming/NamingContext/<NotEmpty>));
  else
    let poa = context.naming-context-service.naming-service-poa;
    let objectid = PortableServer/POA/servant-to-id(poa, context);
    PortableServer/POA/deactivate-object(poa, objectid);
  end if;
end method;

define method CosNaming/NamingContext/list
    (context :: <naming-context>, how-many :: CORBA/<unsigned-long>)
 => (bl :: CosNaming/<BindingList>, bi :: CosNaming/<BindingIterator>)

  local method make-object-binding (component :: CosNaming/<NameComponent>)
          let name = make(CosNaming/<Name>, size: 1, fill: component);
          make(CosNaming/<Binding>, binding-name: name, binding-type: #"nobject");
        end method;

  local method make-context-binding (component :: CosNaming/<NameComponent>)
          let name = make(CosNaming/<Name>, size: 1, fill: component);
          make(CosNaming/<Binding>, binding-name: name, binding-type: #"ncontext");
        end method;

  let bindings :: CosNaming/<BindingList> = make(CosNaming/<BindingList>);

  for (object keyed-by name-component in context.naming-context-objects)
    bindings := add!(bindings, make-object-binding(name-component));
  end for;

  for (object keyed-by name-component in context.naming-context-contexts)
    bindings := add!(bindings, make-context-binding(name-component));
  end for;

  if (size(bindings) <= how-many)
    values(bindings, make-nil(CosNaming/<BindingIterator>));
  else
    let returned-bindings :: CosNaming/<BindingList> = make(CosNaming/<BindingList>);
    let position :: <integer> = 0;
    how-many := min(how-many, size(bindings));
    while (how-many > 0)
      returned-bindings := add!(returned-bindings, bindings[position]);
      position := position + 1;
      how-many := how-many - 1;
    end;
    let poa = context.naming-context-service.naming-service-poa;
    let iterator = make(<binding-iterator>, poa: poa, bindings: bindings, position: position);
//    let reference = PortableServer/POA/servant-to-reference(poa, servant);
//    let iterator = as(CosNaming/<BindingIterator>, reference);
    values(returned-bindings, iterator);
  end if;
end method;

