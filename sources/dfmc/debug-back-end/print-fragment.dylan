Module: dfmc-debug-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define compiler-sideways method print-object
    (o :: <variable-name-fragment>, stream :: <stream>) => ()
  format(stream, if (*sexp?*) "{ %s }" else "%s" end, fragment-identifier(o))
end method;

define method print-contents (frag :: <function-call-fragment>, stream) => ()
  print-contents(fragment-function(frag), stream);
  format(stream, "(");
  for (arg in fragment-arguments(frag), sep? = #f then #t)
    if (sep?) format(stream, ", ") end;
    print-contents(arg, stream);
  end;
  format(stream, ")");
end method;

define method print-contents 
    (frag :: <literal-constant-fragment>, stream) => ()
  format(stream, if (*sexp?*) "%=" else "{ %s }" end, fragment-value(frag));
end method;
