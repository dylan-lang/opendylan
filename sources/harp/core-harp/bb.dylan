module:    base-harp
Synopsis:  Support for HARP basic blocks
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//


//// The result of code generation is a list of basic blocks

//// **********Thought of the day, Make the special slot share with the opcode
//// But we do not want to allocate conses.
//// Cases: 
//// branch instructions (opcode . tag)
//// we make jst instructions end the block and update the others slot
//// there and then.



define method empty-bb?
    (backend :: <harp-back-end>, bb :: <basic-block>) => (b :: <boolean>)
  bb.bb-start == backend.variables.fp-instructions;
end;

define method find-bb
     (backend :: <harp-back-end>, taag :: <tag>) => (bb :: <basic-block>)
  let bbtag = taag.tag-bb;
  if (bbtag)
    bbtag;
  else
    let bb = make-bb(backend);
    bb.bb-taags := list(taag);
    taag.tag-bb := bb;
    bb;
  end if;
end;

define method block-synonym (bb :: <basic-block>, taag :: <tag>) => ()
  pushnew!(taag, bb.bb-taags);
  taag.tag-bb := bb;
end;

define method finish-bb (backend :: <harp-back-end>) => ()
  let vars = backend.variables;
  let current = vars.current-bb;
  if (current)
    current.bb-end := vars.fp-instructions;
  end if;
end;


define method make-current-bb (backend :: <harp-back-end>) => ()
  let bb =  make-bb(backend);
  make-current-this-bb(backend, bb);
end;

define method make-current-this-bb
   (backend :: <harp-back-end>, bb :: <basic-block>) => ()
  let vars = backend.variables;
  finish-bb(backend);
  bb.bb-start := vars.fp-instructions;
  bb.bb-end := vars.fp-instructions;
  vars.current-bb := bb;
end;


define method taag-out (backend :: <harp-back-end>, taag :: <tag>) => ()
  let bb = taag.tag-bb;
  let vars :: <harp-variables> = backend.variables;
  let current :: <basic-block> = vars.current-bb;
  let empty-current :: <boolean> = empty-bb?(backend, current);
  if (~ bb & empty-current)
    block-synonym(current, taag);
  else
    if (empty-current)
      // We have an empty bb but we have found this one
      // before, but it is empty so the current bbs remains
      // the current one
      bb-redirect(bb, current);
    else
      // Just start a new basic block
      let nbb :: <basic-block> = bb | find-bb(backend, taag);
      pushnew!(nbb, current.bb-next-set);
      current.bb-fall-thru := nbb;
      pushnew!(current, nbb.bb-prev-set);
      make-current-this-bb(backend, nbb);
    end if;
  end if;
end;


define method conditional-branch-windup
   (backend :: <harp-back-end>, taag :: <tag>) => ()
  let bb :: <basic-block> = find-bb(backend, taag);
  let current :: <basic-block> = backend.variables.current-bb;
  push!(bb, current.bb-next-set);
  pushnew!(current, bb.bb-prev-set);
  make-fall-thru-bb(backend);
end;


define method make-fall-thru-bb (backend :: <harp-back-end>) => ()
  let bb :: <basic-block> = make-bb(backend);
  let current :: <basic-block> = backend.variables.current-bb;
  push!(bb, current.bb-next-set);
  current.bb-fall-thru := bb;
  push!(current, bb.bb-prev-set);
  make-current-this-bb(backend, bb);
end;


define method add-destination-tags-to-bb 
    (backend :: <harp-back-end>, tags :: <list>) => ()
  unless (tags.empty?)
    let current :: <basic-block> = backend.variables.current-bb;
    for (tag in tags)
      let bb = find-bb(backend, tag);
      pushnew!(bb, current.bb-next-set);
      pushnew!(current, bb.bb-prev-set);
    end for;
  end unless;
end;


define inline function replace-old-with-new!
    (new, old, seq :: <list>) => (seq :: <list>)
  iterate replace(nseq :: <list> = seq)
    if (nseq == #()) seq
    else
      let e = nseq.head;
      if (e == old)
	nseq.head := new;
      end;
      let nseq :: <list> = nseq.tail;
      replace(nseq);
    end;
  end;
end function;

define method fast-nsubstitute (new, old, seq :: <list>) => ()
  replace-old-with-new!(new, old, seq);
end;


define method bb-redirect (old :: <basic-block>, new :: <basic-block>) => ()
  for (p :: <basic-block> in old.bb-prev-set)
    pushnew!(p, new.bb-prev-set);
    fast-nsubstitute(new, old, p.bb-next-set);
    fast-nsubstitute(new, old, p.bb-other-set);
    if (p.bb-fall-thru == old)
      p.bb-fall-thru := new;
    end if;
  end for;
  for (taag :: <tag> in old.bb-taags)
    block-synonym(new, taag);
  end for;
end;



define method output-unconditional-branch-instruction 
    (backend :: <harp-back-end>, taag :: <tag>) => ()
  let bb :: <basic-block> = find-bb(backend, taag);
  let current :: <basic-block> = backend.variables.current-bb;
  if (empty-bb?(backend, current) & 
      ~ (bb == current & 
         begin harp-warning(backend, "An infinite loop"); #t end))
    bb-redirect(current, bb);
  else
    pushnew!(bb, current.bb-next-set);
    current.bb-fall-thru := bb;
    pushnew!(current, bb.bb-prev-set);
  end if;
  make-current-bb(backend);
end;


define method ensure-room-in-array
   (vec :: <stretchy-vector>, limit :: <integer>) => ()
  let length = vec.size;
  if (length < limit)
    vec.size := limit + limit;
  end if;
end;

define method ensure-room-in-vector
   (vec :: <simple-object-vector>, limit :: <integer>)
 => (new-vec :: <simple-object-vector>)
  let length = vec.size;
  if (length < limit)
    let new-size = limit + limit;

    // copy-sequence(vec, size: new-size);

    let new-vec :: <simple-object-vector> =
      make(<simple-object-vector>, size: new-size);

    primitive-replace!
      (new-vec, primitive-repeated-slot-offset(new-vec), integer-as-raw(0),
       vec, primitive-repeated-slot-offset(vec), integer-as-raw(0),
       integer-as-raw(length));
    new-vec
  else
    vec
  end if;
end;

define method ensure-room-in-vector
   (vec :: <stretchy-vector>, limit :: <integer>)
 => (vec :: <stretchy-vector>)
  ensure-room-in-array(vec, limit);
  vec
end;

define method ensure-room-in-vector
   (vec :: <vector-32bit>, limit :: <integer>)
 => (new-vec :: <vector-32bit>)
  let length = vec.size * $bit-unit-size$;
  if (length < limit)
    make-bit-set(limit);
  else
    clear-bit-set(vec, end: ceiling/(limit, $bit-unit-size$));
    vec
  end if;
end;


define method instruction-uses
   (instruction :: <integer>, sv-ins :: <instructions-vector>) => (uses :: <list>)
  let uses = #();
  for-instruction-uses (uze in sv-ins at instruction)
    push!(uze, uses);
  end;
  uses;
end;

define method instruction-defs
   (instruction :: <integer>, sv-ins :: <instructions-vector>) => (defs :: <list>)
  let defs = #();
  for-instruction-defs (def in sv-ins at instruction)
    push!(def, defs);
  end;
  defs;
end;




	 
