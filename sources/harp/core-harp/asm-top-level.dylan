module:    main-harp
Synopsis:  The top level for HARP code generation.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// This is the code generator top level driver. Given a block, it produces a
/// list of numbers representing the instruction stream (for whichever
/// processor) from the transitive closure of blocks reachable from that top
/// one. The n-locals argument is provided by the compiler back end to say how
/// many words of gc local storage are wanted in the current frame. The
/// allocate-local-area instruction must take account of this when building the
/// gc spill area. The result is an mv of the code, spill statistics and a
/// vector of blocks with starting addresses for the benefit of the debugger.



define method code-gen-from-block 
    (backend :: <harp-back-end>, top-blck :: <basic-block>, print-harp) => ()
  let regs = backend.registers;
  let vars = backend.variables;
  let state = vars.vreg-state;

  // !@#$ For future reference - may need this for register 
  // preserving around NLXs. TBD!
  let alloc-reals = if (backend.bind-exit-frame?)
                      let preserved 
                        = if (vars.compiling-call-in)
                            regs.c-preserved-register-vector 
                          else regs.preserved-register-vector 
                          end if;
                      preserved.r-union-of-real-regs;
                    else empty-rset
                    end;
  state.allocated-reals := alloc-reals;

  // state.allocated-reals := empty-rset;

  vars.pgm-vect := build-pgm-vector(backend, top-blck);

  if (~ print-harp) compress-vreg-usage(backend, vars.pgm-vect) end;

  // allocation -> vars
  colour-graph(backend, int-graph(backend, vars.pgm-vect, top-blck)); 
  state.raw-size := adjusted-raw-size(backend);

  if (vars.optimize-leaf-case)
    optimize-leaf-case-2(backend, vars.pgm-vect);
  else
    // Now fix load-stack-arg-n even if not optimizing (TonyM 19/12/91)
    // fix-prolog-stack-ops(backend, vars.pgm-vect);
  end if;

  let allocated-regs :: <list> = list-from-rset(backend, state.allocated-reals);
  for (reg :: <real-register> in allocated-regs)
    let mask :: <integer> = real-preserved-mask(backend, reg);
    unless (mask == 0)
      let known-preserved = state.allocated-preserved;
      let new-preserved = r-union(known-preserved, mask);
      unless (new-preserved == known-preserved)
        state.allocated-preserved := new-preserved;
        inc!(state.number-preserved);
      end unless;
    end unless;
  end for;

  let (blk-vector :: <simple-basic-block-vector>, blk-num :: <integer>) =
     linearise(backend, top-blck);

  if (print-harp)
    print-linearised-harp(backend, print-harp, blk-vector, blk-num);
  end if;
      
  splat-colours(backend, blk-vector, blk-num);

  code-select-program(backend, blk-vector, blk-num);
end;


define method adjusted-raw-size
     (backend :: <harp-back-end>) => (i :: <integer>)
  let state = backend.variables.vreg-state;
  let raw-size = state.next-ng-spill + state.next-sf-spill +
                 state.next-df-spill + state.next-df-spill;
  raw-size;
end;




/// After a short period of absence this function returns as it seems more
/// efficient to get this chore over and done with in one go rather than
/// cluttering up the template and auxiliary functions with it. CIM 23/12/88

define method splat-colours 
    (backend :: <harp-back-end>, pgm :: <simple-basic-block-vector>,
     blk-num :: <integer>)
  let sv-ins :: <instructions-vector> = backend.variables.sv-instructions;
  for (x from 0 below blk-num, bb :: <basic-block> in pgm)
    for-instructions-in-basic-block (ins in bb)
      let the-op :: <op> = ins-op(sv-ins, ins);

      unless (the-op.op-keep-virtual)
        for-instruction-defs (d in sv-ins at ins)
          if (instance?(d, <virtual-register>))
            let colour = d.virtual-register-colour;
            set-def(if (instance?(colour, <integer>)) #f else colour end);
          end if;
        end for-instruction-defs;
        for-instruction-uses (u in sv-ins at ins)
	  if (instance?(u, <virtual-register>))
            let colour = u.virtual-register-colour;
            set-use(if (instance?(colour, <integer>)) #f else colour end);
          end if;
        end for-instruction-uses;
      end unless;

      if (the-op.op-is-move)
        with-du (sv-ins at ins)
          if (du-def(1) == du-uze(1)) eliminate-instruction(backend, ins) end;
        end with-du;
      end if;
    end for-instructions-in-basic-block;
  end for;
end;

define method build-pgm-vector
     (backend :: <harp-back-end>, top-block :: <basic-block>)
     => (v :: <stretchy-basic-block-vector>)
  really-build-pgm-vector(backend, top-block);
end;


// new version, markt, 21/jul/90
// this uses a list of (cons bb next-other-list) instead of explicit recursion
// and hence does not blow up the stack.  It does more allocation, and it would be 
// nice to get rid of the calls to append!
// at least, there is an assaumption that other will usually be null
// and so the append can be bypassed as unnecessary (in both calls -
// nick - 26.7.90)
define method  really-build-pgm-vector
     (backend :: <harp-back-end>, top-block :: <basic-block>)
     => (v :: <stretchy-basic-block-vector>)
  let next = top-block.bb-next-set;
  let other = top-block.bb-other-set;
  let the-rest :: <list> =  
    if (~ (other == #())) concatenate(next, other) else next end;
  let current = pair(top-block, the-rest);
  let route-list :: <list> =  #();
  let result :: <stretchy-basic-block-vector> =
    make(<stretchy-basic-block-vector>,
	 capacity: 1 + backend.variables.vreg-state.next-vreg-id);
  top-block.bb-seen := #t;
  until (current == #())
    if  (current.tail == #())
      push!(current.head, result);
      current := list-pop!(route-list);
    else
      let new :: <basic-block> = list-pop!(current.tail);
      unless  (new.bb-seen)
	new.bb-seen := #t;
	push!(current, route-list);
        let next = new.bb-next-set;
        let other = new.bb-other-set;
        let new-tail :: <list> = 
          if (~ (other == #())) concatenate(next, other) else next end;
	current := pair(new, new-tail);
      end unless;
    end if;
  end until;
  result;
end;
