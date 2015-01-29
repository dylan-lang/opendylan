module: dfmc-flow-graph
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method walk-all-lambda-computations
    (walk :: <function>, o :: <&method>, #key previous?)
  for-lambda (sub-f in o)
    walk-all-lambda-computations(walk, sub-f);
  end;
  walk-computations(walk, o.body, #f, previous?: previous?);
end method;

define method walk-lambda-computations
    (function :: <function>, first :: <computation>, #key before, previous?) ;
  walk-computations(function, first, before, previous?: previous?)
end method;

define method walk-lambda-computations
    (function :: <function>, first == #f, #key before, previous?) ;
end method;

define inline function lambda-walker
    (walk :: <function>, previous?) => (res :: <function>)
  if (previous?)
    walk
  else
    method (pc, c) ignore(pc); walk(c) end
  end if;
end function;

define function walk-computations
    (walk :: <function>, c :: <computation>, last, #key previous?)
  do-walk-computations(lambda-walker(walk, previous?), #f, c, last)
end function;

define function do-walk-computations
    (walk :: <function>, pc :: false-or(<computation>), c :: <computation>, last)
  iterate loop (pc = pc, c = c)
    unless (c == last)
      walk-computation(walk, pc, c);
      let pc = c;
      let cc = c.next-computation;
      when (cc)
        loop(pc, cc);
      end when;
    end unless;
  end iterate;
end function;

define method walk-computation
    (walk :: <function>, pc :: false-or(<computation>), c :: <computation>)
  walk(pc, c);
end method;

define method walk-computation
    (walk :: <function>, pc :: false-or(<computation>), c :: <if>)
  walk(pc, c);
  do-walk-computations(walk, c, c.consequent,  c.next-computation);
  do-walk-computations(walk, c, c.alternative, c.next-computation);
end method;

define method walk-computation
    (walk :: <function>, pc :: false-or(<computation>), c :: <loop>)
  walk(pc, c);
  do-walk-computations(walk, c, loop-body(c), c.next-computation);
end method;

define method walk-computation
    (walk :: <function>, pc :: false-or(<computation>), c :: <bind-exit>)
  walk(pc, c);
  do-walk-computations(walk, c, c.body, c.next-computation);
end method;

define method walk-computation
    (walk :: <function>, pc :: false-or(<computation>), c :: <unwind-protect>)
  walk(pc, c);
  do-walk-computations(walk, c, c.body,     c.next-computation);
  do-walk-computations(walk, c, c.cleanups, c.next-computation);
end method;

//// Computation reference walker.

define method walk-all-lambda-references
    (walk :: <function>, o :: <&method>) => ()
  for-lambda (sub-f in o)
    walk-all-lambda-references(walk, sub-f);
  end;
  walk-lambda-references(walk, o);
end method;

define method walk-lambda-references
    (walk :: <function>, m :: <&method>) => ()
  if (m.body)
    for-computations (c in m)
      walk-computation-references(walk, c);
    end;
  end;
end method;

define method dereference (ref :: <object>) => (ref :: <object>)
  ref
end method;

define method dereference
    (ref :: <defined-constant-reference>) => (ref :: <object>)
  ref.referenced-binding
end method;

define method dereference
    (ref :: <object-reference>) => (ref :: <object>)
  ref.reference-value
end method;

define method walk-computation-references
    (walk :: <function>, c :: <computation>) => ()
  do-used-value-references
    (method (ref)
       if (~instance?(ref, <temporary>))
         walk(c, ref, dereference(ref))
       end
     end, c);
end method;

define method walk-computation-references
    (walk :: <function>, c :: <variable-reference>) => ()
  next-method();
  walk(c, #f, c.referenced-binding);
end method;

define method walk-computation-references
    (walk :: <function>, c :: <make-closure>) => ()
  next-method();
  walk(c, #f, function(c.computation-closure-method));
end method;

define method walk-computation-references
    (walk :: <function>, c :: <set!>) => ()
  next-method();
  walk(c, #f, c.assigned-binding);
end method;

define method walk-computation-references
    (walk :: <function>, c :: <redefinition>) => ()
  next-method();
  walk(c, #f, c.assigned-binding);
end method;

define method walk-computation-references
    (walk :: <function>, c :: <stack-vector>) => ()
  next-method();
  let empty-vector = dylan-value(#"%empty-vector");
  let size = c.temporary.number-values;
  if (size = 0)
    walk(c, #f, empty-vector);
  else
    let class = &object-class(empty-vector);
    walk(c, #f, class);
    walk(c, #f, size);
  end if;
end method;

// !@#$ seems like a wart

define method walk-computation-references
    (walk :: <function>, c :: <primitive-call>) => ()
  next-method();
  walk(c, #f, c.primitive);
end method;

// !@#$ seems like a wart

define method walk-computation-references
    (walk :: <function>, c :: <c-variable-pointer-call>) => ()
  next-method();
  walk(c, #f, c.c-variable); // !@#$ external
end method;
