Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// NB. to be sure this works we really have to intern all
// types, or else descend into compound types. The former is
// preferable, since it is a one shot at point of creation
// rather than points of use and relies on a more generic
// table type.
define function pair-type-key-hash 
    (key :: <pair>, hash-state :: <hash-state>) 
        => (id :: <integer>, state :: <hash-state>)
  let id-1 = object-hash(key.head, hash-state);
  let id-2 = object-hash(key.tail, hash-state);
  values(merge-hash-ids(id-1, id-2, ordered: #t), hash-state);
end;

