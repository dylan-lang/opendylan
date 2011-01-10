Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// Conversion methods

define sideways sealed inline method make
    (t == <machine-word>, #key value :: <abstract-integer>)
 => (result :: <machine-word>)
  // ignore(t);
  coerce-abstract-integer-to-machine-word(value)
end method make;

//////////////////////////////////////////////////////////////////////////////
// Numeric Predicates

define sealed sideways inline method odd? (m :: <machine-word>)
 => (result :: <boolean>)
  %logbit?(0, m)
end method odd?;

define sealed sideways inline method even? (m :: <machine-word>)
 => (result :: <boolean>)
  ~%logbit?(0, m)
end method even?;

define inline constant $machine-word-zero :: <machine-word> = as(<machine-word>, 0);
//  = coerce-integer-to-machine-word(0);

define sealed sideways inline method zero? (m :: <machine-word>)
 => (result :: <boolean>)
  m = $machine-word-zero
end method zero?;

define sealed sideways inline method positive? (m :: <machine-word>)
 => (result :: <boolean>)
  m > $machine-word-zero
end method positive?;

define sealed sideways inline method negative? (m :: <machine-word>)
 => (result :: <boolean>)
  m < $machine-word-zero
end method negative?;
