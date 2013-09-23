Module: dfmc-back-end
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define compiler-open class <code-walker> (<object>)
  constant slot visited = make(<table>);
end class;

define compiler-open generic maybe-walk
    (walker :: <code-walker>, c :: <computation>) => ();

define compiler-open generic walk
    (walker :: <code-walker>, c :: <computation>) => ();

define compiler-open generic subsequent-walk
    (walker :: <code-walker>, c :: <computation>) => ();

define compiler-open generic before-walk
    (walker :: <code-walker>, c :: <computation>) => ();

define compiler-open generic do-walk
    (walker :: <code-walker>, c :: <computation>) => ();

define method maybe-walk
    (walker :: <code-walker>, c :: <computation>) => ()
  if (element(walker.visited, c, default: #f))
    subsequent-walk(walker, c);
  else
    walker.visited[c] := #t;
    before-walk(walker, c);
    walk(walker, c)
  end if
end method;

define method walk
    (walker :: <code-walker>, c :: <computation>) => ()
  do-walk(walker, c);
  maybe-walk(walker, c.next-computation);
end method;

define method walk
    (walker :: <code-walker>, c :: <if>) => ()
  do-walk(walker, c);
  maybe-walk(walker, c.consequent);
  maybe-walk(walker, c.alternative);
end method;

define method walk
    (walker :: <code-walker>, c :: <loop-call>) => ()
  do-walk(walker, c);
  maybe-walk(walker, c.loop-call-loop);
end method;

define method walk
    (walker :: <code-walker>, c :: <bind-exit>) => ()
  do-walk(walker, c);
  maybe-walk(walker, c.body);
  maybe-walk(walker, c.next-computation);
end method;

define method walk
    (walker :: <code-walker>, c :: <unwind-protect>) => ()
  do-walk(walker, c);
  maybe-walk(walker, c.body);
  maybe-walk(walker, c.cleanups);
  maybe-walk(walker, c.next-computation);
end method;

define method walk
    (walker :: <code-walker>, c :: <end>) => ()
  do-walk(walker, c);
end method;

define method walk (walker :: <code-walker>, c :: <end-exit-block>) => ()
  do-walk(walker, c);
  // The following new code forces a jump to be emitted over any
  // following code that isn't actually next in the flow.
  maybe-walk(walker, c.next-computation);
end method;

define method walk (walker :: <code-walker>, c :: <end-protected-block>) => ()
  do-walk(walker, c);
  // The following new code forces a jump to be emitted over any
  // following code that isn't actually next in the flow.
  maybe-walk(walker, c.entry-state.me-block.cleanups);
end method;

define method walk (walker :: <code-walker>, c :: <end-cleanup-block>) => ()
  do-walk(walker, c);
  maybe-walk(walker, c.entry-state.me-block.next-computation);
end method;

define method walk (walker :: <code-walker>, c :: <nop-computation>) => ()
  maybe-walk(walker, c.next-computation);
end method;
