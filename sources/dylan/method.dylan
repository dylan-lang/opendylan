Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// BOOTED: define ... class <lambda> ... end;
// BOOTED: define ... class <simple-method> ... end;
// BOOTED: define ... class <closure-method> ... end;
// BOOTED: define ... class <keyword-method> ... end;
// BOOTED: define ... class <keyword-closure-method> ... end;

define sealed method make (class == <method>, #rest all-keys, #key) => (res)
  error("Cannot instantiate %=, it is not an instantiable type.", class)
end method;

define inline function simple-method-iep (m :: <simple-method>) => (res)
  mep(m)
end function;

define inline function function-next? (f :: <lambda>) => (res :: <boolean>)
  signature-next?(function-signature(f))
end function;


// define method sealed-domain? (l :: <lambda>) => (res :: <boolean>)
//   signature-sealed-domain?(function-signature(l))
// end method;


define method type-complete? (l :: <lambda>) => (well? :: <boolean>)
  type-complete?(function-signature(l))
end method;


define method recompute-type-complete! (l :: <lambda>) => (well? :: <boolean>)
  recompute-type-complete!(function-signature(l))
end method;


define method map-congruency-classes (f :: <function>, l :: <lambda>) => ()
  map-congruency-classes(f, function-signature(l))
end method;


define method reduce-incomplete-classes (f :: <function>,l :: <lambda>, ans)
 => (ans)
  reduce-incomplete-classes(f, function-signature(l), ans)
end method;
