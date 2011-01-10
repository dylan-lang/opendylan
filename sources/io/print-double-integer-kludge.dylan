Module:       print-internals
Author:       Gary Palter
Synopsis:     KLUDGE: Temporary method until division is implemented for <double-integer>
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed method print-object
    (object :: <double-integer>, stream :: <stream>) => ()
  write(stream, "#ex");
  write(stream,
	copy-sequence(machine-word-to-string(%double-integer-high(object)), start: 2));
  write(stream, copy-sequence(machine-word-to-string(%double-integer-low(object)), start: 2))
end method;
