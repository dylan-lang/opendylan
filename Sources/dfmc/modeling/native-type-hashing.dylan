Module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $the-empty-vector = as(<simple-vector>, #[]);


// TODO : make hashing more like the emulator version - using object hash
// won't work!!!


// NB. to be sure this works we really have to intern all
// types, or else descend into compound types. The former is
// preferable, since it is a one shot at point of creation
// rather than points of use and relies on a more generic
// table type.

// But we are not currently interning all types, so we have to work harder here

define generic type-hash(t :: <&type>, hash-state :: <hash-state>)
     => (id :: <integer>, state :: <hash-state>);

define method type-hash(type :: <&type>, hash-state :: <hash-state>)
     => (id :: <integer>, state :: <hash-state>)
  object-hash(type, hash-state)
end;

define method type-hash(type :: <&singleton>, hash-state :: <hash-state>)
     => (id :: <integer>, state :: <hash-state>)
  let singleton-object = type.^singleton-object;
  if (instance?(singleton-object, <&type>))
    type-hash(singleton-object, hash-state)
  else
    object-hash(singleton-object, hash-state)
  end;
end;

define method type-hash(type :: <&union>, hash-state :: <hash-state>)
     => (id :: <integer>, state :: <hash-state>)
  type-hash(type.^union-members[0], hash-state)
end;


define function simple-vector-type-key-hash 
  (key :: <simple-vector>, hash-state :: <hash-state>)
     => (id :: <integer>, state :: <hash-state>)
  let current-id = 0;
  for (elt in key)
    let id = type-hash(elt, hash-state);
    current-id := merge-hash-ids(current-id, id, ordered: #t);
  end for;
  values(current-id, hash-state);
end function simple-vector-type-key-hash;


define function object-and-simple-vector-type-key-hash 
  (p :: <pair>, hash-state :: <hash-state>)
     => (id :: <integer>, state :: <hash-state>)
  let id-1 = object-hash(p.head, hash-state);
  let id-2 = simple-vector-type-key-hash(p.tail, hash-state);
  values(merge-hash-ids(id-1, id-2, ordered: #t), hash-state);
end;


define function values-type-table-hash 
  (p :: <pair>, hash-state :: <hash-state>)
     => (id :: <integer>, state :: <hash-state>)
  let id-1 = simple-vector-type-key-hash (p.head, hash-state);
  if (p.tail)
    let id-2 = object-hash(p.tail, hash-state);
    values(merge-hash-ids(id-1, id-2, ordered: #t), hash-state);
  else
    values(id-1, hash-state);
  end;
end;







