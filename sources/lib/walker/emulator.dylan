module:    walker
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method remove-all-keys! (table :: <table>) 
  do (curry(remove-key!, table), key-sequence(table));
  table;
end method;

define method walker-repeated-slot? (class :: subclass(<array>))
  #t
end method;

define constant initialized-slot-element = slot-element;

/// BOOLEAN

define method walker-compute-deep-slot-descriptors 
    (walker :: <walker>, class == <boolean>)
  #()
end method;

define method walker-compute-lazy-slot-descriptors 
    (walker :: <walker>, class == <boolean>)
  #()
end method;

define method walker-compute-weak-slot-descriptors 
    (walker :: <walker>, class == <boolean>)
  #()
end method;

/// LIST

define method walker-compute-deep-slot-descriptors 
    (walker :: <walker>, class :: subclass(<list>))
  vector(0, 1)
end method;

define method do-deep-copy
    (copier :: <copier>, object :: <object-table>) => (value)
  let size = size(object);
  let copy = make(<table>, size: size);
  copier-register-copied(copier, object, copy); 
  let entries = tail(object.table-values);
  for (val/key/back in entries)
    let key   = head(tail(val/key/back));
    let value = head(val/key/back);
    copy[deep-copy(copier, key)] := deep-copy(copier, value);
  end for;
  copy
end method;

define method do-deep-walk
    (walker :: <walker>, function :: <function>, 
     parent, object :: <object-table>) 
  walker-register-walked(walker, parent, object, object); 
  let entries = tail(object.table-values);
  for (val/key/back in entries)
    deep-walk(walker, function, object, head(tail(val/key/back)));
    deep-walk(walker, function, object, head(val/key/back));
  end for;
end method;

define method walker-instance-size (element :: <object-table>)
  next-method() + size(element) * 2 // lower bound
end method;

define method do-deep-walk
    (walker :: <walker>, function :: <function>, 
     parent, object :: <stretchy-vector>)
  do(curry(deep-walk, walker, function, parent), object)
end method;

define method walker-allocate-object (class :: subclass(<array>), size)
  make(class, size: size)
end method;

define method walker-allocate-object (class == <object-table>, size)
  make(<object-table>)
end method;

define method walker-allocate-object (class :: subclass(<pair>), size)
  pair(#f, #f)
end method;

define method walker-allocate-object (class :: <class>, size)
  allocate(class, size: size)
end method;

define function walker-allocate-simple-object (class)
  walker-allocate-object(class, 0);
end function;

define constant walker-allocate-repeated-object = walker-allocate-object;
