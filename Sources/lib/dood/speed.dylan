Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// PAIR

define method read-object-using-class-at
    (dood :: <dood>, class == <pair>, address :: <address>)
 => (res :: <pair>)
  let object = pair(#f, #());
  dood-register-read-object(dood, object, address); 
  let hd = read-object(dood);
  let tl = read-object(dood);
  head(object) := hd;
  tail(object) := tl;
  object
end method;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <pair>)
  unless ($tag-pairs?)
    walk-slot(dood, info, object, <pair>);
  end unless;
  walk-slot(dood, info, object, head(object));
  let parent
    = if (walk-info-parents?(info))
	// keep non-pair parent for better stats
	element(dood-back-pointers(dood), object, default: object)
      else
	object
      end if;
  walk-slot(dood, info, parent, tail(object));
end method;

define method dood-compute-instance-size
    (dood :: <dood>, class == <pair>) => (address :: <address>)
  if ($tag-pairs?) 2 else dood-compute-standard-instance-size(dood, class) end;
end method;

/// VECTOR

define method read-object-using-class-at
    (dood :: <dood>, class == <simple-object-vector>, address :: <address>)
 => (res :: <simple-object-vector>)
  let size :: <integer>             = read-object(dood);
  let vec :: <simple-object-vector> = make(<vector>, size: size);
  dood-register-read-object(dood, vec, address); 
  for (i :: <integer> from 0 below size)
    vec[i] := read-object(dood);
  end for;
  vec
end method;

define inline function walk-flat-sequence-slots
    (dood :: <dood>, info :: <walk-info>, 
     class :: <class>, object :: <sequence>)
  let size = size(object);
  walk-slot(dood, info, object, class);
  walk-slot(dood, info, object, size);
  for (e in object)
    walk-slot(dood, info, object, e);
  end for;
end function;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <simple-object-vector>)
  walk-flat-sequence-slots(dood, info, <simple-object-vector>, object);
end method;

/// STRETCHY-VECTOR

define method read-object-using-class-at
    (dood :: <dood>, class == <stretchy-object-vector>, address :: <address>)
 => (res :: <stretchy-object-vector>)
  let size :: <integer>
    = read-object(dood);
  let vec :: <stretchy-object-vector> 
    = make(<stretchy-object-vector>, size: size);
  dood-register-read-object(dood, vec, address); 
  for (i :: <integer> from 0 below size)
    vec[i] := read-object(dood);
  end for;
  vec
end method;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <stretchy-object-vector>)
  walk-flat-sequence-slots(dood, info, <stretchy-object-vector>, object);
end method;

define method dood-repeated-size 
    (dood :: <dood>, object :: <stretchy-object-vector>) => (res :: <integer>)
  size(object)
end method;

define method dood-repeated-slot? 
    (dood :: <dood>, class :: subclass(<stretchy-object-vector>))
  #t
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object :: subclass(<stretchy-object-vector>)) 
 => (address :: <address>)
  1
end method;

/// DEQUE

define method read-object-using-class-at
    (dood :: <dood>, class == <object-deque>, address :: <address>)
 => (res :: <object-deque>)
  let size :: <integer>        = read-object(dood);
  let object :: <object-deque> = make(<object-deque>);
  dood-register-read-object(dood, object, address); 
  for (i :: <integer> from 0 below size)
    push-last(object, read-object(dood));
  end for;
  object
end method;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <object-deque>)
  walk-flat-sequence-slots(dood, info, <object-deque>, object);
end method;

define method dood-repeated-size 
    (dood :: <dood>, object :: <object-deque>) => (res :: <integer>)
  size(object)
end method;

define method dood-repeated-slot? 
    (dood :: <dood>, class :: subclass(<object-deque>))
  #t
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object :: subclass(<object-deque>)) 
 => (address :: <address>)
  1
end method;

/// STRING

define inline function walk-byte-string-slots
    (dood :: <dood>, info :: <walk-info>, class :: <class>, object)
  walk-slot(dood, info, object, class);
  walk-slot(dood, info, object, size(object));
  if (walk-info-commit?(info))
    dood-write-string(dood, object);
  end if;
end function;

define inline function read-byte-string-object-using-class-at
    (dood :: <dood>, class :: <class>, address :: <address>)
 => (res)
  let size :: <integer> = read-object(dood);
  let object            = make(class, size: size);
  dood-register-read-object(dood, object, address); 
  dood-read-string-into!(dood, size, object);
  object
end function;

define method read-object-using-class-at
    (dood :: <dood>, class == <byte-string>, address :: <address>)
 => (res :: <byte-string>)
  read-byte-string-object-using-class-at(dood, <byte-string>, address)
end method;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <byte-string>)
  walk-byte-string-slots(dood, info, <byte-string>, object);
end method;

//// BYTE-VECTOR

define method read-object-using-class-at
    (dood :: <dood>, class == <byte-vector>, address :: <address>)
 => (res :: <byte-vector>)
  read-byte-string-object-using-class-at(dood, <byte-vector>, address)
end method;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <byte-vector>)
  walk-byte-string-slots(dood, info, <byte-vector>, object);
end method;

define method dood-repeated-byte-slot? (dood :: <dood>, class == <byte-vector>)
  #t
end method;

//// SYMBOLS

define method read-object-using-class-at
    (dood :: <dood>, class == <symbol>, address :: <address>)
 => (res :: <symbol>)
  let size :: <integer>       = read-object(dood);
  let string :: <byte-string> = make(<byte-string>, size: size);
  dood-read-string-into!(dood, size, string);
  let sym :: <symbol> = as(<symbol>, string);
  dood-register-read-object(dood, sym, address); 
  sym
end method;

define method walk-slots 
    (dood :: <dood>, info :: <walk-info>, object :: <symbol>)
  if (object == #()) // HACK: EMU
    next-method()
  else
    let string :: <byte-string> = as(<byte-string>, object);
    walk-byte-string-slots(dood, info, dood-symbol-class(object), string);
  end if
end method;

define method dood-repeated-size 
    (dood :: <dood>, object :: <symbol>) => (res :: <integer>)
  let string :: <byte-string> = as(<byte-string>, object);
  size(string)
end method;

define method dood-repeated-slot? (dood :: <dood>, class == <symbol>)
  #t
end method;

define method dood-repeated-byte-slot?  (dood :: <dood>, class == <symbol>)
  #t
end method;

define method dood-compute-instance-size
    (dood :: <dood>, object == <symbol>) => (address :: <address>)
  1
end method;

// eof
