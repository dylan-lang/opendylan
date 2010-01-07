Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define sealed class <pointer-id-table> (<table>)
end class;

define sealed domain make (singleton(<pointer-id-table>));
define sealed domain initialize (<pointer-id-table>);

// COPIED HERE FROM MACHINE-WORD-LOWLEVEL
define inline-only function machine-word-as-hash-index (x :: <machine-word>)
 => (hi :: <integer>)
  interpret-machine-word-as-integer(force-integer-tag(x))
end function;

define function dood-pointer-id-hash 
    (object :: <object>, hash-state :: <hash-state>) 
 => (hi :: <integer>, hash-state :: <hash-state>)
  primitive-mps-ld-add(hash-state, object);
  values(machine-word-as-hash-index(address-of(object)), hash-state)
end function;

define function dood-pointer-id? (x, y) => (res :: <boolean>)
  primitive-id?(x, y)
end function;

define sealed method table-protocol 
    (table :: <pointer-id-table>) => (test :: <function>, hash :: <function>)
  values(dood-pointer-id?, dood-pointer-id-hash)
end method;

define constant <dood-table> = <pointer-id-table>;
*/

define constant <dood-table> = <object-table>;
define constant <dood-word>  = <machine-word>;

define inline-only function dood-word-shift-right
    (x :: <dood-word>, y :: <integer>) => (z :: <dood-word>)
  machine-word-shift-right(x, y)
end function;

define inline-only function dood-word-shift-left
    (x :: <dood-word>, y :: <integer>) => (z :: <dood-word>)
  machine-word-shift-left-with-overflow(x, y)
end function;

define inline-only function dood-word-logior
    (x :: <dood-word>, y :: <dood-word>) => (z :: <dood-word>)
  machine-word-logior(x, y)
end function;

define inline-only function dood-word-logand
    (x :: <dood-word>, y :: <dood-word>) => (z :: <dood-word>)
  machine-word-logand(x, y)
end function;

define constant dood-instance-slot-descriptors
  = instance-slot-descriptors;

define method dood-repeated-slot?  (dood :: <dood>, class :: <class>)
  repeated-slot-descriptor(class)
end method;

define method dood-repeated-byte-slot?  (dood :: <dood>, class :: <class>)
  let sd = repeated-slot-descriptor(class);
  sd & slot-type(sd) == <byte-character>
end method;

/*
define method dood-reinitialize
    (dood :: <dood>, object :: <object-table>) => ()
  next-method();
  rehash-table(object);
end method;
*/

define inline function dood-standard-object (dood :: <dood>, object)
  object
end function;

/// DOUBLE-INTEGER

define method read-object-using-class-at
    (dood :: <dood>, class == <double-integer>, address :: <address>)
 => (res :: <double-integer>)
  let low  = dood-read-machine-word-at(dood, address + 1);
  let high = dood-read-machine-word-at(dood, address + 2);
  make(<double-integer>, low: low, high: high);
end method;

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, object :: <double-integer>) => ()
  walk-slot(dood, info, object, <double-integer>);
  if (walk-info-commit?(info))
    dood-write-machine-word(dood, %double-integer-low(object));
    dood-write-machine-word(dood, %double-integer-high(object));
  end if;
end method;

define inline method dood-compute-instance-size
    (dood :: <dood>, object == <double-integer>) => (address :: <address>)
  dood-compute-instance-size(dood, <machine-word>)
    + dood-compute-instance-size(dood, <machine-word>)
end method;

/// BIG-SMALL-INTEGER

define class <big-small-integer> (<object>)
  constant slot big-value :: <machine-word>, required-init-keyword: value:;
end class;

define method read-object-using-class-at
    (dood :: <dood>, class == <big-small-integer>, address :: <address>)
 => (res :: <integer>)
  as(<integer>, dood-read-machine-word-at(dood, address + 1));
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object == <big-small-integer>) => (address :: <address>)
  dood-compute-instance-size(dood, <machine-word>)
end method;

define method walk-slots
    (dood :: <dood>, info :: <walk-info>, object :: <big-small-integer>) => ()
  walk-slot(dood, info, object, <big-small-integer>);
  if (walk-info-commit?(info))
    dood-write-machine-word(dood, big-value(object));
  end if;
end method;

/// SYMBOL

define inline function dood-symbol-class (object :: <symbol>) <symbol> end;

/// INTEGER

define inline function small-integer? (object) => (res :: <boolean>)
  let object :: <integer> = object;
  object <= $max-dood-integer & object >= $min-dood-integer
end function;

// define constant $indirect-kind       = 0;
// define constant $integer-kind        = 1;
// define constant $byte-character-kind = 2;
// 
// define inline function object-kind (object) => (res :: <integer>)
//   raw-as-integer
//    (primitive-machine-word-logand
//      (primitive-cast-pointer-as-raw(object), integer-as-raw(3)))
// end function;

define method dood-disk-object 
    (dood :: <dood>, object :: <integer>) => (disk-object)
  make(<big-small-integer>, value: as(<machine-word>, object))
end method;

define inline function dood-integer-disk-pointer+object
    (dood :: <dood>, object :: <integer>)
 => (pointer :: <pointer>, disk-object)
  dood-format("MAKING INTEGER %=\n", object);
  if (object > $max-dood-integer | object < $min-dood-integer)
    dood-disk-pointer+object(dood, dood-disk-object(dood, object))
  else
    values(tag-as-integer(object), object)
  end if
end function;

define inline function dood-character-disk-pointer+object
    (dood :: <dood>, object :: <byte-character>)
 => (pointer :: <pointer>, disk-object)
  dood-format("WRITING BYTE-CHARACTER %=\n", object);
  values(tag-as-byte-character(as(<integer>, object)), object)
end function;

define inline function address-as-integer
    (x :: <integer>) => (res :: <integer>)
  x
end function;
