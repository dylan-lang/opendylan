Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// shift-left (x :: <machine-word>, count :: <integer>)
//   => low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>

define sealed generic %shift-left (x :: <object>, count :: <integer>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);

define inline method %shift-left (x :: <machine-word>, count :: <integer>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);
  check-shift-quantity(count);
  machine-word-shift-left-with-overflow(x, count);
end method;

define inline method %shift-left (x :: <abstract-integer>, count :: <integer>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);
  check-shift-quantity(count);
  machine-word-shift-left-with-overflow
      (coerce-abstract-integer-to-machine-word(x), count);
end method;

//////////////////////////////////////////////////////////////////////////////
// shift-right (x :: <machine-word>, count :: <integer>)
//   => result :: <machine-word>

define sealed generic %shift-right (x :: <object>, count :: <integer>)
  => result :: <machine-word>;

define method %shift-right (x :: <machine-word>, count :: <integer>)
  => result :: <machine-word>;
  check-shift-quantity(count);
  machine-word-shift-right(x, count);
end method;

define method %shift-right (x :: <abstract-integer>, count :: <integer>)
  => result :: <machine-word>;
  check-shift-quantity(count);
  machine-word-shift-right(coerce-abstract-integer-to-machine-word(x), count);
end method;
