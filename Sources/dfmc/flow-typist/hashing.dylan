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
  (key :: <pair>)
  let (id-1, state-1) = object-hash(key.head);
  let (id-2, state-2) = object-hash(key.tail);
  merge-hash-codes(id-1, state-1, id-2, state-2, ordered: #t);
end;

