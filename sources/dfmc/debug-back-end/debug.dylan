Module: dfmc-debug-back-end
Author: Jonathan Bachrach, Keith Playford, and Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// generic formatting (should use pretty printer)

define thread variable *offset* = 0;

define method indent (stream, offset :: <integer>)
  for (i from 0 below offset)
    format(stream, "  ");
  end for;
end method;


//// FUNCTIONS

define thread variable *lambdas-in-progress* = #();

define compiler-sideways method print-object
    (o :: <&iep>, stream :: <stream>) => ()
  format(stream, "(iep of)%=", o.function);
end method;

define compiler-sideways method print-object
    (o :: <&engine-node-ep>, stream :: <stream>) => ()
  format(stream, "{%= %=}", debug-name(object-class(o)), ^entry-point-name(o));
end method;


