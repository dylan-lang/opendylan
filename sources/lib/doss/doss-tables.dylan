Module:    emulator-doss
Author:    Eliot Miranda
Synopsis:  Emulator DOSS table support
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* Frig for emulator id tables.

 <lisp-object-table> has a table-table slot that holds onto a LispWorks
 eq-table.  We intercept the access of this slot to copy across values
 from the LispWorks table into our vector of elements, etc.
*/

// First ensure that table-table and table-values are eliminated from list of
// slots. (we don't need them to be dumped, they're LispWorks thingies anyway).
define method doss-dumpable-slots
    (class == <object-table>, policy :: <doss-policy>) => (slots :: <sequence>)
  choose(method (desc)
	   (desc.slot-getter ~== table-table)
	   & (desc.slot-getter ~== table-values)
	 end,
	 next-method())
end method doss-dumpable-slots;

// Frig access to the tally; we simply return the size.
define method doss-slot-value
    (getter == tally, obj :: <object-table>, dd :: <doss-dumper>) => (size :: <integer>)
  obj.size
end method doss-slot-value;

// Frig access to elements; we make a fake elements vector and copy
// our key-value pairs into it and then return this fake vector.
define method doss-slot-value
    (getter == elements, obj :: <object-table>, dd :: <doss-dumper>) => (elements :: <vector>)
  let i = 8;	// ensure elements is an adequatly large power of two in size
  while (i <= (obj.size * 2)) i := i + i end;
  let fake-elements = make(<vector>, size: i, fill: obj.void-element);
  let index = 0;
  /*
  do(method(key)
       fake-elements[index] := key;
       fake-elements[index + 1] := obj[key];
       index := index + 2;
     end,
     obj.key-sequence);
  */
  let (state, limit, next-state, finished-state?, current-key, current-element)
    = obj.forward-iteration-protocol;
  while (~finished-state?(obj,state,limit))
    fake-elements[index] := current-key(obj,state);
    fake-elements[index + 1] := current-element(obj,state);
    state := next-state(obj,state);
    index := index + 2
  end;
  fake-elements
end method doss-slot-value;

// On loading all we need to do is to initialize the table (which initializes
// table-table & table-values) and copy the key-value pairs into table-table.
define method post-load-cleanup (obj :: <object-table>) => ()
  let index = 0;
  let limit = obj.tally * 2;
  let vec   = obj.elements;
  initialize(obj);
  while (index < limit)
    obj[vec[index]] := vec[index + 1];
    index := index + 2
  end;
  obj
end method post-load-cleanup;


/// Bug fix methods:

define method shallow-copy
    (coln :: <explicit-key-collection>) => (new-coln :: <explicit-key-collection>)
  let new-coln = make(coln.class-for-copy, size: coln.size);
  do(method (key) new-coln[key] := coln[key] end, coln.key-sequence);
  new-coln
end method shallow-copy;
