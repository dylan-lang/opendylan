Module:    naming-service
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <binding-iterator> (CosNaming/<BindingIterator-servant>)
  constant slot binding-iterator-poa :: PortableServer/<POA>, required-init-keyword: poa:;
  constant slot binding-iterator-bindings :: CosNaming/<BindingList>, required-init-keyword: bindings:;
  slot binding-iterator-position :: CORBA/<unsigned-long>, required-init-keyword: position:;
end class;
define sealed domain make (singleton(<binding-iterator>));
define sealed domain initialize (<binding-iterator>);

define constant $dummy-binding
  = make(CosNaming/<Binding>,
         binding-name: make(CosNaming/<Name>),
         binding-type: #"nobject");

define method CosNaming/BindingIterator/next-one
    (iterator :: <binding-iterator>)
 => (result :: CORBA/<boolean>, binding :: CosNaming/<Binding>)
  let bindings = iterator.binding-iterator-bindings;
  let position = iterator.binding-iterator-position;
  if (position < size(bindings))
    iterator.binding-iterator-position := position + 1;
    values(#t, iterator.binding-iterator-bindings[position]);
  else
    values(#f, $dummy-binding);
  end if;
end method;

define method CosNaming/BindingIterator/next-n
    (iterator :: <binding-iterator>, how-many :: CORBA/<unsigned-long>)
 => (result :: CORBA/<boolean>, bl :: CosNaming/<BindingList>)
  let bindings = iterator.binding-iterator-bindings;
  let position = iterator.binding-iterator-position;
  let success? = position < size(bindings);
  let return = make(CosNaming/<BindingList>);
  while (how-many > 0 & position < size(bindings))
    return := add!(return, bindings[position]);
    position := position + 1;
    how-many := how-many - 1;
  end;
  iterator.binding-iterator-position := position;
  values(success?, return);
end method;

define method CosNaming/BindingIterator/destroy
    (iterator :: <binding-iterator>)
 => ()
  let poa = iterator.binding-iterator-poa;
  let objectid = PortableServer/POA/servant-to-id(poa, iterator);
  PortableServer/POA/deactivate-object(poa, objectid);
end method;

