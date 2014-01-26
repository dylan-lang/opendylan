Module: dfmc-debug-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// !@#$ might not be nec now that c-functions are primitives

define compiler-sideways method print-object
    (o :: <&c-function>, stream :: <stream>) => ()
  format(stream, "&[C-FUNCTION %s]", o.binding-name);
end method;

// !@#$ perhaps this should be split into function-name-string

define compiler-sideways method print-object
    (o :: <&c-callable-function>, stream :: <stream>) => ()
  format(stream, "[%s $s]", "C-CALLABLE-FUNCTION", o.iep);
end method print-object;
