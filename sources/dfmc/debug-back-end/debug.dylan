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

define compiler-sideways method print-object (o :: <&iep>, stream :: <stream>) => ()
  format(stream, "(iep of)%=", o.function);
end method;

/*
define method indented-format (stream, format-string, #rest arguments)
  indent(stream, *offset*); 
  apply(format, stream, format-string, arguments);
end method;

define method print-named-object (stream :: <stream>, o)
  case
    o.named?  => format(stream, "%s ", o.name);
    otherwise => #f;
  end case;
end method;

define method print-parameter (stream, p :: <lexical-required-variable>)
  format(stream, "%s", p);
end method print-parameter;

define method print-parameter (stream, p :: <lexical-rest-variable>)
  format(stream, "#rest %s", p);
end method print-parameter;

define method print-parameter (stream, p :: <lexical-keyword-variable>)
  format(stream, "#key %s", p);
end method print-parameter;


define method print-object (o :: <&iep>, stream :: <stream>) => ()
  format(stream, "lambda ");
  // ??? print-named-object(stream, o);
  // ??? format(stream, "%= [%=]", o.id, o.environment.id);
  if (*verbose-computations?* & ~member?(o, *lambdas-in-progress*))
    dynamic-bind (*lambdas-in-progress* = pair(o, *lambdas-in-progress*))
      label!(o);
      format(stream, " (");
      for (parameter in o.parameters, first? = #t then #f)
	unless (first?)
	  format(stream, ", ");
	end unless;
        print-parameter(stream, parameter);
      end for;
      format(stream, ")\n");
      dynamic-bind (*offset* = *offset* + 1)
        print-computations(stream, o.body);
      end dynamic-bind;
      indented-format(stream, "end lambda");
    end dynamic-bind;
  end if;
end method;
*/

define compiler-sideways method print-object (o :: <&engine-node-ep>, stream :: <stream>) => ();
  format(stream, "{%= %=}", debug-name(object-class(o)), ^entry-point-name(o))
end method;


