Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// IO SUPPORT

define constant <dood-word> = <integer>;

define function dood-word-shift-right (x, amount) 
  ash(x, -amount)
end function;

define constant dood-word-shift-left = ash;

define constant dood-word-logior     = logior; 

define constant dood-word-logand     = logand;

/// SLOT-DESCRIPTORS

define constant dood-instance-slot-descriptors
  = slot-descriptors;

/// BOOLEAN

define method dood-compute-deep-slot-descriptors (class == <boolean>)
  #()
end method;

define method dood-compute-lazy-slot-descriptors (class == <boolean>)
  #()
end method;

define method dood-compute-weak-slot-descriptors (class == <boolean>)
  #()
end method;

/// LIST

define method dood-compute-deep-slot-descriptors (class :: subclass(<list>))
  vector(0, 1)
end method;

/// OBJECT-TABLE

// TODO:  Is this needed anymore?  The emulator now defines this.
define method remove-all-keys! (table :: <table>) 
  do (curry(remove-key!, table), key-sequence(table));
  table;
end method;

/// REPEATED DEFAULTS

define method dood-repeated-slot? (dood :: <dood>, class :: <class>)
  #f
end method;

define method dood-repeated-byte-slot? (dood :: <dood>, class :: <class>)
  #f
end method;

define method dood-repeated-slot? (dood :: <dood>, class :: subclass(<array>))
  #t
end method;

define method dood-repeated-byte-slot? (dood :: <dood>, class == <byte-string>)
  #t
end method;

/// EMULATOR DOESN'T ALWAYS RETURN THE EXACT SAME EMPTY VECTOR OBJECT

define method dood-standard-object
    (dood :: <dood>, object) => (object)
  object
end method;

// We wouldn't have to do this if the runtime did...
define method dood-standard-object
     (dood :: <dood>, object :: <simple-object-vector>) => (object)
  if (empty?(object)) $dood-empty-vector else object end
end method;

// EMULATOR: in emulator, "" is a <simple-object-vector>, so need this
// to shadow above.
define method dood-standard-object
    (dood :: <dood>, object :: <string>) => (object)
  object
end method;

define class <dood-keyword-proxy> (<dood-proxy>) end;

define method read-object-using-class-at
    (dood :: <dood>, class == <dood-keyword-proxy>, address :: <address>)
  let size   = read-object(dood);
  let string = make(<byte-string>, size: size);
  dood-read-string-into!(dood, size, string);
  as-keyword(string)
end method;

define function dood-symbol-class (object :: <symbol>)
  if (keyword?(object)) // HACK: EMULATOR
    <dood-keyword-proxy>
  else
    <symbol>
  end
end function;

define function dood-integer-disk-pointer+object
    (dood :: <dood>, object :: <integer>)
 => (pointer :: <pointer>, disk-object)
  dood-format("MAKING INTEGER %=\n", object);
  values(tag-as-integer(object), object)
end function;

define method dood-disk-object 
    (dood :: <dood>, object :: <class>)
 => (proxy :: <dood-program-binding-proxy>)
  dood-as-proxy(dood, object, dood-make-program-binding-proxy)
end method;

define inline function small-integer? (object :: <integer>) => (res :: <boolean>)
  #t
end function;

define inline function dood-character-disk-pointer+object
    (dood :: <dood>, object :: <byte-character>)
 => (pointer :: <pointer>, disk-object)
  dood-format("WRITING BYTE-CHARACTER %=\n", object);
  // HACK: EMULATOR ONLY FIX
  with-unbound-caught
    values(tag-as-byte-character(as(<integer>, object)), object)
  unbound
    // format-out("UNBOUND SLOT\n");
    values(tag-as-address(#f, $dood-false-id), #f)
  end
end function;

define constant $max-tagged-integer 
  =  (2 ^ ($tagged-word-size - 1)) - 1;
define constant $negative-tagged-integer-correction
  =  2 ^ $tagged-word-size;

define inline function address-as-integer
    (x :: <integer>) => (res :: <integer>)
  if (x > $max-tagged-integer)
    x - $negative-tagged-integer-correction
  else 
    x
  end if
end function;

define method dood-compute-instance-size
    (dood :: <dood>, object :: subclass(<simple-object-vector>)) 
 => (address :: <address>)
  2
end method;

// eof
