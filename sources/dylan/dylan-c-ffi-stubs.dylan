module: internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define inline-only function box-c-signed-char (w :: <raw-machine-word>)
 => (result :: <machine-word>)
  let raw-result
    = if (logand(raw-as-integer(w), #x80) = 0)
	primitive-machine-word-logand(w, integer-as-raw(#xff));
      else
	primitive-machine-word-logior
	  (w, primitive-unwrap-machine-word(as(<machine-word>, #xffffff00)));
      end if;
  primitive-wrap-machine-word(raw-result);
end function;

define inline-only function box-c-unsigned-char (w :: <raw-machine-word>)
 => (result :: <machine-word>)
  let raw-result = primitive-machine-word-logand(w, integer-as-raw(#xff));
  primitive-wrap-machine-word(raw-result);
end function;

define inline-only function box-c-signed-short (w :: <raw-machine-word>)
 => (result :: <machine-word>)
  let raw-result
    = if (logand(raw-as-integer(w), #x8000) = 0)
	primitive-machine-word-logand(w, integer-as-raw(#xffff));
      else
	primitive-machine-word-logior
	  (w, primitive-unwrap-machine-word(as(<machine-word>, #xffff0000)));
      end if;
  primitive-wrap-machine-word(raw-result);
end function;

define inline-only function box-c-unsigned-short (w :: <raw-machine-word>)
 => (result :: <machine-word>)
  let raw-result = primitive-machine-word-logand(w, integer-as-raw(#xffff));
  primitive-wrap-machine-word(raw-result);
end function;


define open generic pointer-value (p :: <c-pointer>, #key index)
 => (o :: <object>);
define open generic pointer-value-setter
    (n :: <object>, p :: <c-pointer>, #key index)
 => (o :: <object>);

define open generic pointer-value-address (p :: <c-pointer>, #key index)
 => (o :: <c-pointer>);


// TODO: PERFORMANCE: Maybe add back inlining when we can do it under
// more control.

define /* inline */ function make-c-pointer-internal
  (class :: <designator-class>,
   address :: <machine-word>,
   init-args :: <simple-object-vector>)
 => (v :: <C-pointer>);
  let instance :: <C-pointer> = allocate-c-pointer-instance(class, init-args);
  let raw-address :: <raw-pointer> = primitive-unwrap-machine-word(address);
  let init-args 
    = concatenate-2(init-args, class.defaulted-initialization-arguments);
  apply(default-initialize, class, instance,
        raw-pointer-address: raw-address, init-args);
  apply(initialize, instance, init-args);
  instance
end;

// TODO: CORRECTNESS: The FFI seems to allocate instances of abstract
// classes all over the place, hence this hack. Fix!!!

define function allocate-c-pointer-instance
    (class :: <class>, init-args :: <simple-object-vector>) => (instance)
  let iclass :: <implementation-class> = class-implementation-class(class);
  /*
  if (class-abstract?(class))
    error("Cannot instantiate an abstract class - %=", class);
  end;
  */
  for (i from 0 below size(class-slot-descriptors(iclass)))
    find-or-create-class-slot-storage(iclass, i, #t)
  end for;
  let (instance-size :: <integer>,
       repeated-slot? :: <boolean>, repeated-slot-type :: <type>,
       repeated-size :: <integer>, fill)
    = allocation-attributes(iclass, init-args);

  if (repeated-slot?)
    system-allocate-repeated-instance
      (class, repeated-slot-type, unbound(), repeated-size, fill)
  else
    system-allocate-simple-instance-i(iclass)
  end if;
end function;


// this is not quite right because it is signed on some platforms, and
// not on others, but nobody should really be depending on that
define constant <C-raw-char> = <C-raw-signed-char>;
define constant <C-raw-char*> = <C-raw-signed-char*>;

define inline method concrete-class 
    (class :: <designator-class>) 
 => (cclass :: false-or(<designator-class>))
  class
end method;

define inline method concrete-class-setter
    (cclass :: false-or(<designator-class>), class :: <designator-class>)
 => (cclass :: false-or(<designator-class>))
  cclass
end method;
