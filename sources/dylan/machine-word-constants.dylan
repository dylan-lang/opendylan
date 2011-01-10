Module:    internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// Machine word size

define inline constant $machine-word-size :: <integer> = word-size();

//////////////////////////////////////////////////////////////////////////////
// Machine word constants

define inline constant $maximum-signed-machine-word :: <machine-word> =
  machine-word-unsigned-shift-right(coerce-integer-to-machine-word(-1), 1);

define inline constant $minimum-signed-machine-word :: <machine-word> =
  machine-word-shift-left-signal-overflow
    (coerce-integer-to-machine-word(-1), $machine-word-size - 1);

define inline constant $maximum-unsigned-machine-word :: <machine-word> =
  coerce-integer-to-machine-word(-1);

define inline constant $minimum-unsigned-machine-word :: <machine-word> =
  coerce-integer-to-machine-word(0);
