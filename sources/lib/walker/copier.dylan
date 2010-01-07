module:    walker
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function shallow-copy-instance (object) => (copy);
  let class  = object-class(object);
  let sz     = if (walker-repeated-slot?(class)) size(object) end;
  let copy   = walker-allocate-object(class, sz);
  let slotds = slot-descriptors(class);
  for (slotd in slotds)
    let sd = as-walker-slot-descriptor(class, slotd);
    walker-slot-value(copy, sd) := walker-slot-value(object, sd);
  end;
  if (sz)
    let sz :: <integer> = sz;
    for (i :: <integer> from 0 below sz) copy[i] := object[i] end;
  end if;
  copy
end function;

define macro dont-copy-slots-definer
  { define dont-copy-slots ?class:name using ?walker:name = { ?entries } }
    => { define method walker-shallow-getters
	     (walker_ :: ?walker, x_ :: subclass(?class)) => (res :: <sequence>)
           concatenate(?=next-method(), list(?entries))
	 end method }
entries:
  { } 
    => { }
  { ?getter:name, ... } 
    => { ?getter, ... }
  { ?getter:name => ?default:expression, ... } 
    => { pair(?getter, method (?=self) ?default end), ... }
end macro;

define macro dont-copy-object-definer 
  { define dont-copy-object ?:name ?copier }
    => { define method deep-copy (copier :: ?copier, object :: ?name) => (value)
	   object
	 end method }
copier:
  { }              => { <copier> }
  { using ?:name } => { ?name }
end macro;

define open class <copier> (<walker>)
end class;

define dont-copy-object <empty-list>   using <copier>;
define dont-copy-object <number>       using <copier>;
define dont-copy-object <boolean>      using <copier>;
define dont-copy-object <symbol>       using <copier>;
define dont-copy-object <character>    using <copier>;
define dont-copy-object <machine-word> using <copier>;

define inline function copier-register-copied
    (copier :: <copier>, object, copy)
  walker-walked(copier)[object] := copy
end function;

define open generic do-deep-copy (copier :: <copier>, object) => (copy);

/*
define macro do-copy-slots
  { do-copy-slots () (?object:name, ?copy:name, ?start:expression) } 
    => { }
  { do-copy-slots (?slot:name, ...) (?object:name, ?copy:name, ?start:expression) } 
    => { walker-slot-value(?copy, ?index) := walker-slot-value(?object, ?index);
         do-copy-slots (...) (?start + 1) }
end macro;

define macro copy-slots-definer
  { define copy-slots (?slots:*) (?:name) } 
    => { define function "copy-slots-" ## "?name" (copier :: <copier>, object)
	   let class = object-class(object);
	   let copy  = walker-allocate-object(class, 0);
	   copier-register-copied(copier, object, copy);
	   do-copy-slots (?slots) (?object, ?copy, 0);
	 end function }
end macro;

define copy-slots (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) (10);
*/

/*
define method do-deep-copy (copier :: <copier>, object) => (value)
  let class     = object-class(object);
  let repeated? = walker-repeated-slot?(class);
  let sz        = if (repeated?) size(object) end;
  let copy      = walker-allocate-object(class, sz);
  copier-register-copied(copier, object, copy); 
  let (shallow-slotds, defaulted-slotds, deep-slotds)
    = walker-all-slot-descriptors(copier, class);
  for (slotd in shallow-slotds)
    walker-slot-value(copy, slotd) := walker-slot-value(object, slotd);
  end for;
  for (slotd/default in defaulted-slotds)
    let slotd         = walker-default-slot-descriptor(slotd/default);
    let default-thunk = walker-default-thunk(slotd/default);
    walker-slot-value(copy, slotd) := default-thunk(object);
  end for;
  for (slotd in deep-slotds)
    walker-slot-value(copy, slotd) 
      := deep-copy(copier, walker-slot-value(object, slotd));
  end for;
  if (repeated?)
    for (i :: <integer> from 0 below sz)
      copy[i] := deep-copy(copier, object[i]);
    end for;
  end if;
  copy
end method;
*/

define inline function do-deep-copy-simple 
    (copier :: <copier>, object, class :: <class>, walker-class :: <walker-class>)
 => (value)
  let copy = walker-allocate-simple-object(class);
  copier-register-copied(copier, object, copy); 
  let deep-slotds = walker-class-deep-slot-descriptors(walker-class);
  for (slotd :: <walker-slot-descriptor> in deep-slotds)
    walker-slot-value(copy, slotd) 
      := deep-copy(copier, walker-slot-value(object, slotd));
  end for;
  copy
end function;

define function do-deep-copy-repeated 
    (copier :: <copier>, object, class :: <class>, walker-class :: <walker-class>)
 => (value)
  let sz   = size(object);
  let copy = walker-allocate-repeated-object(class, sz);
  copier-register-copied(copier, object, copy); 
  let deep-slotds = walker-class-deep-slot-descriptors(walker-class);
  for (slotd :: <walker-slot-descriptor> in deep-slotds)
    walker-slot-value(copy, slotd) 
      := deep-copy(copier, walker-slot-value(object, slotd));
  end for;
  for (i :: <integer> from 0 below sz)
    copy[i] := deep-copy(copier, object[i]);
  end for;
  copy
end function;

define function do-deep-copy-complex 
    (copier :: <copier>, object, class :: <class>, walker-class :: <walker-class>)
 => (value)
  let copy = walker-allocate-simple-object(class);
  copier-register-copied(copier, object, copy); 
  let shallow-slotds   = walker-class-shallow-slot-descriptors(walker-class);
  let defaulted-slotds = walker-class-defaulted-slot-descriptors(walker-class);
  let deep-slotds      = walker-class-deep-slot-descriptors(walker-class);
  for (slotd :: <walker-slot-descriptor> in shallow-slotds)
    walker-slot-value(copy, slotd) := walker-slot-value(object, slotd);
  end for;
  for (slotd/default :: <walker-defaulted-descriptor> in defaulted-slotds)
    let slotd         = walker-default-slot-descriptor(slotd/default);
    let default-thunk = walker-default-thunk(slotd/default);
    walker-slot-value(copy, slotd) := default-thunk(object);
  end for;
  for (slotd :: <walker-slot-descriptor> in deep-slotds)
    walker-slot-value(copy, slotd) 
      := deep-copy(copier, walker-slot-value(object, slotd));
  end for;
  copy
end function;

define method do-deep-copy (copier :: <copier>, object) => (value)
  let class        = object-class(object);
  let walker-class = walker-class(copier, class);
  select (walker-class-kind(walker-class))
    $walker-simple   => do-deep-copy-simple(copier, object, class, walker-class);
    $walker-complex  => do-deep-copy-complex(copier, object, class, walker-class);
    $walker-repeated => do-deep-copy-repeated(copier, object, class, walker-class);
  end select;
end method;

define method do-deep-copy
    (copier :: <copier>, object :: <pair>) => (value)
  let copy :: <pair> = pair(#f, #f);
  copier-register-copied(copier, object, copy); 
  head(copy) := deep-copy(copier, head(object));
  tail(copy) := deep-copy(copier, tail(object));
  copy
end method;

define method do-deep-copy
    (copier :: <copier>, object :: <simple-object-vector>) => (value)
  let size :: <integer> = size(object);
  let copy :: <simple-object-vector> = make(<simple-object-vector>, size: size);
  copier-register-copied(copier, object, copy); 
  for (i :: <integer> from 0 below size)
    copy[i] := deep-copy(copier, object[i]);
  end for;
  copy
end method;

define method do-deep-copy
    (copier :: <copier>, object :: <stretchy-object-vector>) => (value)
  let size :: <integer> = size(object);
  let copy :: <stretchy-object-vector> = make(<stretchy-object-vector>, size: size);
  copier-register-copied(copier, object, copy); 
  for (i :: <integer> from 0 below size)
    copy[i] := deep-copy(copier, object[i]);
  end for;
  copy
end method;

define method do-deep-copy
    (copier :: <copier>, object :: <table>) => (value)
  let size :: <integer> = size(object);
  let copy :: <table> = make(object-class(object), size: size);
  copier-register-copied(copier, object, copy); 
  for (val keyed-by key in object)
    copy[key] := val;
  end for;
  copy
end method;

define method do-deep-copy
    (copier :: <copier>, object :: <byte-string>) => (value)
  let copy = copy-sequence(object);
  copier-register-copied(copier, object, copy); 
  copy
end method;

define inline function maybe-do-deep-copy
     (copier :: <copier>, object) => (value)
  let copy = walked(copier, object, default: not-found());
  if (found?(copy))
    copy
  else
    do-deep-copy(copier, object);
  end if
end function;

define open generic deep-copy (copier :: <copier>, object) => (value);

define method deep-copy (copier :: <copier>, object) => (value)
  maybe-do-deep-copy(copier, object)
end method;

define method copier-reset
    (copier :: <copier>, #rest all-keys, #key capacity) => (copier :: <copier>)
  apply(walker-reset, copier, all-keys)
end method;
