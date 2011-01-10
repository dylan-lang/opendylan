Module:   dfmc-flow-graph
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: DFM optimizations that shouldn't be used
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Inlining tail recursion as jumps

// This optimization should only be considered if the DFM is really
// turned into an SSA-like representation which can stand cycles in
// the program graph.  As it is right now, it can't.  This sort of
// optimization must be done at the harp or harp generation level.

define method try-inlining-calls-as-jumps (c :: <function-call>)
  let called-temporary = c.function;
  let called-object = called-temporary.temporary-value;
  if (instance?(called-object, <&lambda>))
    if (c.tail-position? & c.environment == called-object.environment)
      inline-jump(c, called-object);
    end if;
  end if;
end method try-inlining-calls-as-jumps;

define macro append-computation!
  { append-computation!(?name:name) }
  => { if (?name ## "-first")
	 (?name ## "-last").next-computations := vector(?name ## "-c")
       else
	 ?name ## "-first" := ?name ## "-c"
       end if;
       ?name ## "-last" := ?name ## "-c" }
end macro;

// The "copy-" computations created by this routine are needed to keep
// the register allocator for the DFM execute machinery from breaking.
// They are in there only because the allocator can't deal with cycles
// in the flow graph, which is going to cause us more grief down the road.

define method inline-jump (c :: <function-call>, f :: <&lambda>)
  let bind-c = check-type(f.body, <bind>);
  let env = bind-c.environment;

  local method make* (class, previous, #rest keys)
	          => (computation, temporary);
	  apply(make-with-temporary,
		env, class, previous-computations: vector(previous),
		keys)
	end make*;

  let (load-first, load-last) = values(#f, bind-c);
  let (copy-first, copy-last) = values(#f, #f);
  let (merge-first, merge-last) = values(#f, #f);

  for (parameter in f.parameters, argument in c.arguments)
    let (load-c, load-t)
      = make*(<temporary-transfer>, load-last, value: parameter);
    let (copy-c, copy-t)
      = make*(<temporary-transfer>, copy-last, value: argument);
    let (merge-c, merge-t)
      = make*(<merge>, merge-last, sources: vector(load-t, copy-t));
    append-computation!(merge);
    append-computation!(load);
    append-computation!(copy);
    replace-temporary-in-users!
      (parameter, merge-t, exclude: curry(\==, load-c));
  end for;

  let replacement
    = if (merge-first)
        merge-first.previous-computations := vector(load-last, copy-last);
        merge-last.next-computations := bind-c.next-computations;
	redirect-next-computations!(bind-c, merge-last);
	bind-c.next-computations := vector(load-first);
	load-last.completion-computation := merge-first;
        copy-last.next-computations := vector(merge-first);
	copy-first
      else
	bind-c.next-computation
      end if;

  replacement.previous-computations := c.previous-computations;
  redirect-previous-computations!(c, replacement);
  c.previous-computations := #[];

  // TODO: split this out in a new function
  for (follow = c.next-computation then follow.next-computation,
       while: instance?(follow, <merge>))
    follow.sources := remove(follow.sources, c.temporary);
    // TODO: special case sources == 0 and sources == 1
  end for;

  delete-computation!(c);
end method inline-jump;
