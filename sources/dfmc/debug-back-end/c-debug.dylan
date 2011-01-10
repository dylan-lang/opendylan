Module: dfmc-debug-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// !@#$ might not be nec now that c-functions are primitives

define compiler-sideways method print-object (o :: <&c-function>, stream :: <stream>) => ()
  format(stream, "&[C-FUNCTION %s]", o.binding-name);
  // next-method();
  /*
  format(stream, "c-function %s (", o.c-name);
  for (type in o.parameter-types, first? = #t then #f)
    unless (first?)
      format(stream, ", ");
    end unless;
    format(stream, "%=", type);
  end for;
  format(stream, ") => (%=)", o.return-type);
  */
end method;

// !@#$ perhaps this should be split into function-name-string

define compiler-sideways method print-object
    (o :: <&c-callable-function>, stream :: <stream>) => ()
  // let format-string = if (*verbose-objects?*) "[%s %=]" else "[%s %s]" end;
  format(stream, "[%s $s]", "C-CALLABLE-FUNCTION", o.iep);
end method print-object;
