Module:    emulator-doss
Author:    Eliot Miranda
Synopsis:  Emulator DOSS dumper patches
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
 The emulator has a few idiosyncrasies due to its implementation of
 the Dylan object system over LispWorks CLOS.  For efficiency reasons
 a number of Dylan classes are represented via CLOS classes.  Because
 of this instances of certain classes (notably <object-table>) are not
 dumpable by the vanilla DOSS system.
 This DOSS dumper attempts to gloss-over the differences and produce files
 which contain structures that look as if they are all native DylanWorks
 objects.

 This is now arranged as a series of patches to the default classes
 <doss-dumper>, <doss-loader>, and <basic-doss-policy>, in order to
 hide the differences between running native versus emulated. 
*/

/*
define class <emulator-doss-dumper> (<doss-dumper>)
end class <emulator-doss-dumper>;

define class <emulator-doss-policy> (<basic-doss-policy>)
end class <emulator-doss-policy>;

define method make
    (class == <doss-dumper>, #rest args, #key policy = make(<emulator-doss-policy>), #all-keys)
 => (dd :: <emulator-doss-dumper>)
  apply(make, <emulator-doss-dumper>, policy: policy, args)
end method make;
*/

define constant <emulator-doss-dumper> = <doss-dumper>;
define constant <emulator-doss-policy> = <basic-doss-policy>;

/*
// Redundant - modules aren't first class, so we don't need special
// support for doss-dumping them. 
define method dump-variable 
    (obj :: <translator-module>, dd  :: <emulator-doss-dumper>, dp  :: <doss-policy>) => ()
  let module-name = module-name(obj);
  my-format("dump-variable %= -> %= %= %=\n", obj, #f, module-name, #f);
  dump-object(#f, dd);
  dump-object(module-name, dd);
  dump-object(#f, dd)
end method dump-variable;
*/

// problems in current emulator with keyword & symbol will disappear

define method dump-object 
    (symbol :: <symbol>, dd :: <emulator-doss-dumper>) => ()
  // my-format("dump-object(symbol) %= \n", symbol);
  let code = if (keyword?(symbol)) $keyword-code else $symbol-code end;
  unless (check-dump-value-object-id(symbol, code, dd))
    dump-string(as(<string>, symbol), dd)
  end
end method dump-object;

define method all-dumpable-slot-descriptors
    (class) => (slot-descs :: <vector>)
  local method dumpable-slot-descriptor? (slot-desc)
    // Be robust in the emulator, not all slot descs work!!
    block () 
      let allocation = slot-allocation(slot-desc);
      (allocation == #"instance" | allocation == #"constant")
    exception (<error>)
      // assume anything else is not interesting, so class will do.
      #f
    end
  end;
  choose(dumpable-slot-descriptor?, class.slot-descriptors)
end method all-dumpable-slot-descriptors;


/// Special-case objects in the emulator:

define method doss-dumpable-slots 
    (class, policy :: <emulator-doss-policy>) => (s :: <sequence>)
  all-dumpable-slot-descriptors(class)
end method doss-dumpable-slots;

define method doss-dumpable-slots 
    (class == <stretchy-vector>, policy :: <doss-policy>) => (s :: <sequence>)
  #[]
end method doss-dumpable-slots;

define method doss-dumpable-slots 
    (class == <byte-vector>, policy :: <doss-policy>) => (s :: <sequence>)
  #[]
end method doss-dumpable-slots;

/* Wots this?  No slot descriptors here:
define method doss-dumpable-slots (class == <pair>, policy :: <doss-policy>)
  vector(head-setter, tail-setter)
end method doss-dumpable-slots;
*/


/// Policy stuff

define method put-specially
    (obj :: <class>, policy :: <emulator-doss-policy>, dd :: <doss-dumper>)
 => (object-dumped? :: <boolean>)
  put-class-description(obj, dd);
  #t
end method put-specially;

define method put-specially 
    (obj :: <function>, policy :: <emulator-doss-policy>, dd :: <doss-dumper>)
 => (object-dumped? :: <boolean>)
  put-reference(obj, dd);
  #t
end method put-specially;

define method put-specially 
    (obj :: <translator-module>, policy :: <basic-doss-policy>, dd :: <doss-dumper>)
 => (dumped? :: <boolean>)
  put-reference(obj, dd);
  #t
end method put-specially;

// Bug fix methods:

define method =hash (module :: <translator-module>) => (hash :: <integer>)
  =hash(module-name(module));
end method =hash;

// eof
