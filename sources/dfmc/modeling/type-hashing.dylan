Module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $my-=hash-mash-rotate-bits = 7;
define constant $my-=hash-mash-total-bits = 26;
define constant $my-=hash-mash-mask =
  ash(1, $my-=hash-mash-total-bits - $my-=hash-mash-rotate-bits) - 1;

define inline function my-merge-hash-ids 
    (dst :: <integer>, src :: <integer>, #key ordered)
  if (ordered)
    logxor(
      ash(src, $my-=hash-mash-rotate-bits - $my-=hash-mash-total-bits),
      ash(logand(src, $my-=hash-mash-mask), $my-=hash-mash-rotate-bits),
      dst)
  else
    logxor(dst, src)
  end
end;
	  
define inline function my-merge-hash-states 
    (state-1 :: false-or(<integer>),
     state-2 :: false-or(<integer>))
  if (state-1)
    if (state-2)
      if (state-1 > state-2) state-1 else state-2 end;
    else
      state-1
    end
  else
    state-2
  end
end;


define inline function my-merge-hash-codes 
    (id-1 :: <integer>, state-1 :: false-or(<integer>),
     id-2 :: <integer>, state-2 :: false-or(<integer>),
     #key ordered)
  values (my-merge-hash-ids(id-1, id-2, ordered: ordered),
          my-merge-hash-states(state-1, state-2))
end;

define constant $the-empty-vector = as(<simple-vector>, #[]);

// NB. to be sure this works we really have to intern all
// types, or else descend into compound types. The former is
// preferable, since it is a one shot at point of creation
// rather than points of use and relies on a more generic
// table type.

// But we are not currently interning all types, so we have to work harder here

define generic type-hash(t :: <&type>) 
  => (hash :: <integer>, hash-state :: <integer>);

define method type-hash(type :: <&type>)
    => (hash :: <integer>, hash-state :: <integer>);
  object-hash(type)
end;

define method type-hash(type :: <&singleton>)
    => (hash :: <integer>, hash-state :: <integer>);
  let singleton-object = type.^singleton-object;
  if (instance?(singleton-object, <&type>))
    type-hash(singleton-object)
  else
    object-hash(singleton-object)
  end;
end;

define method type-hash(type :: <&union>)
    => (hash :: <integer>, hash-state :: <integer>);
  type-hash(type.^union-members[0])
end;

define function simple-vector-type-key-hash 
  (key :: <simple-vector>)
  let k-size = key.size;
  if (k-size = 0)
    object-hash($the-empty-vector);
  else
    let (id-1, state-1) = type-hash(key[0]);
    for (i from 1 below key.size)
      let ith-key = key[i];
      if (instance?(ith-key, <&class>))
        let (id-2, state-2) = type-hash(ith-key);
        let (id-3, state-3) = my-merge-hash-codes(id-1, state-1,
                                               id-2, state-2,
                                               ordered: #t);
        id-1 := id-3;
        state-1 := state-3;
      end;
    end;
    values(id-1, state-1);
  end;
end;

define function object-and-simple-vector-type-key-hash 
  (p :: <pair>)
  let (id-1, state-1) = object-hash(p.head);
  let (id-2, state-2) = simple-vector-type-key-hash(p.tail);
  my-merge-hash-codes(id-1, state-1, id-2, state-2, ordered: #t);
end;


define function values-type-table-hash 
  (p :: <pair>)
  let (id-1, state-1) = simple-vector-type-key-hash (p.head);
  if (p.tail)
    let (id-2, state-2) = object-hash(p.tail);
    my-merge-hash-codes(id-1, state-1, id-2, state-2, ordered: #t);
  else
    values(id-1, state-1);
  end;
end;







