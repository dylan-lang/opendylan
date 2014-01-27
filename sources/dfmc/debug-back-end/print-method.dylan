Module:   dfmc-debug-back-end
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Printing of flow-graph classes.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method indentd (stream :: <stream>, depth :: <integer>)
  for (i from 0 below depth)
    write(stream, "  ");
  end for;
end method;

define method indentd-format
    (stream :: <stream>, depth :: <integer>,
     format-string :: <byte-string>, #rest arguments)
  indentd(stream, depth);
  apply(format, stream, format-string, arguments);
end method;

define method output-lambda-header
    (stream :: <stream>, o :: <&lambda>)
  format(stream, "METHOD");
  if (o.named?)
    format(stream, " %s", o.debug-string);
  end if;
  format(stream, " ");
  print-contents(signature-spec(o), stream);
end method;

define method output-lambda-computations
    (stream :: <stream>, depth :: <integer>, o :: <&lambda>)
  output-lambda-header(stream, o);
  format(stream, "\n");
  let first? = #t;
  for-used-lambda (sub-f in o)
    if (first?)
      indentd-format(stream, depth + 1, "LOCAL ");
      first? := #f;
    else
      indentd(stream, depth + 4);
    end if;
    output-lambda-computations(stream, depth + 4, sub-f);
  end;
  if (o.body)
    output-computations(stream, depth + 1, o.body.next-computation, #f);
  end if;
  indentd-format(stream, depth, "END\n");
end method;

define compiler-sideways method print-method
    (stream :: <stream>, o :: <&lambda>, #key css)
  output-lambda-computations(stream, 0, o);
end method;

define compiler-sideways method print-method-out (o :: <&lambda>, #key css)
  print-method(*standard-output*, o)
end method;

define method output-computations
    (stream :: <stream>, depth :: <integer>, c :: <computation>, last)
  iterate loop (c = c)
    if (c & c ~== last)
      output-computation(stream, depth, c);
      loop(next-computation(c))
    end if;
  end iterate;
end method;

define method output-computation
    (stream :: <stream>, depth :: <integer>, c :: <computation>)
  indentd(stream, depth);
  if (c.temporary & c.temporary.used?)
    format(stream, "%s := ", c.temporary);
  end if;
  print-computation(stream, c);
  format(stream, "\n");
end method;

define method output-computation
    (stream :: <stream>, depth :: <integer>, c :: <loop>)
  indentd-format(stream, depth, "LOOP ()\n");
  output-computations
    (stream, depth + 1, loop-body(c), last);
  indentd-format(stream, depth, "END LOOP\n");
end method;

define method output-computation
    (stream :: <stream>, depth :: <integer>, c :: <if>)
  indentd-format(stream, depth, "IF (%=)\n", c.test);
  output-computations(stream, depth + 1, c.consequent, c.next-computation);
  indentd-format(stream, depth, "ELSE\n");
  output-computations(stream, depth + 1, c.alternative, c.next-computation);
  indentd-format(stream, depth, "END IF\n");
end method;

define method output-computation
    (stream :: <stream>, depth :: <integer>, c :: <bind-exit>)
  indentd-format(stream, depth, "BIND-EXIT (%=)\n", c.entry-state);
  output-computations(stream, depth + 1, c.body, c.next-computation);
  indentd-format(stream, depth, "END BIND-EXIT\n");
end method;

define method output-computation
    (stream :: <stream>, depth :: <integer>, c :: <unwind-protect>)
  indentd-format(stream, depth, "UNWIND-PROTECT (%=)\n", c.entry-state);
  output-computations(stream, depth + 1, c.body, c.next-computation);
  indentd-format(stream, depth, "CLEANUP\n");
  output-computations(stream, depth + 1, c.cleanups, c.next-computation);
  indentd-format(stream, depth, "END UNWIND-PROTECT\n");
end method;
