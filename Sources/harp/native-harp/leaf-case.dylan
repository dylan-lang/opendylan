module:    native-main-harp
Synopsis:  HARP leaf-case analysis.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// started 15/2/89 cim

/// 10/12/90 #"self" has now disappeared as self-calls no longer go through a
/// stub and there is no *self-rejoin-tag* anymore

/// A set of functions to optimize leaf calls. The basic idea is to identify
/// those basic blocks which lie on a direct line between function entry and
/// exit without any function calls. These blocks will be called "green". All
/// the other blocks are "red". The boundaries (and note there usually will
/// more than one - or more importantly in tak there are) are "brown" (green ->
/// red) or "yellow (red -> green).
/// 
/// At each boundary we split the live range of any virtuals live at that point
/// so that we effectively have different virtuals for the two regions. This
/// has the advantage that in the green blocks (if the preferencing is done
/// properly) we may be able to do all the work in the argument registers. Also
/// we should be able to delay the stack building stuff (though this is
/// trickier) to the "brown" boundaries.

/// a handy function for splicing in new blocks

define method with-spliced-block 
    (backend :: <harp-back-end>, 
     preds :: <list>, succ :: <basic-block>, 
     body :: <function>) => (bb :: <basic-block>)
  let sv-ins = backend.variables.sv-instructions;
  let tag :: <tag> = make-tag(backend);
  let new-bb :: <basic-block> = find-bb(backend, tag);
  for (pred :: <basic-block> in preds)
    pred.bb-next-set := safe-substitute(pred.bb-next-set, succ, new-bb);
    if (pred.bb-fall-thru == succ)
      pred.bb-fall-thru := new-bb;
    end if;
    let tag-index :: <integer> = pred.bb-end - instruction-size;
    if (member?(ins-tag(sv-ins, tag-index), succ.bb-taags))
      ins-tag(sv-ins, tag-index) := tag;
    end if;
  end for;
  new-bb.bb-prev-set := preds;
  make-current-this-bb(backend, new-bb);
  body();
  finish-bb(backend);
  let last-bb :: <basic-block> = backend.variables.current-bb;
  for (pred :: <basic-block> in preds)
    succ.bb-prev-set := safe-substitute(succ.bb-prev-set, pred, last-bb);
  end for;
  last-bb.bb-next-set := list(succ);
  last-bb.bb-fall-thru := succ;
  new-bb;
end method;
       

define method safe-substitute (l :: <list>, old, new) => (l :: <list>)
  let safe = copy-sequence(l);
  replace-old-with-new!(new, old, safe);
end method;


// first we find those blocks which involve calls

define method find-red-blocks 
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let sv-ins = backend.variables.sv-instructions;
  for (bb :: <basic-block> in pgm)
    bb.bb-colour := 
      block (scan-bb)
	for-instructions-in-basic-block-backwards (ins in bb)
          if (stack-op?(ins-op(sv-ins, ins)))
	    scan-bb($red-colour);
          end if;
        end for-instructions-in-basic-block-backwards;
	$green-colour;
      end block;
  end for;
end method;


/// now we need to do some dataflow to propogate this information


define method find-real-preds (bb :: <basic-block>) => (l :: <list>)
  let real-preds :: <list> = #();
  for (pred :: <basic-block> in bb.bb-prev-set)
    if (member?(bb, pred.bb-next-set)) // check-for-phonies
      push!(pred, real-preds);
    end if;
  end for;
  real-preds;
end method;


define method none-green? (l :: <list>) => (b :: <boolean>)
  if (l == #())
    #f
  else
    every?(method (bb :: <basic-block>) => (b :: <boolean>)
             bb.bb-colour ~= $green-colour
           end,
           l)
  end if;
end method;

define method should-be-red? (bb :: <basic-block>) => (b :: <boolean>)
  none-green?(bb.bb-next-set) |
    none-green?(find-real-preds(bb));
end method;


define method propogate-red-blocks (pgm :: <stretchy-basic-block-vector>)
  let count :: <integer> = 1;
  let changed :: <boolean> = #t;
  while (changed)
    changed := #f;
    for (bb :: <basic-block> in pgm)
      if (bb.bb-colour == $green-colour)
	if (should-be-red?(bb))
          bb.bb-colour := $red-colour;
          changed := #t;
        end if;
      end if;
    end for;
    inc!(count);
  end while;
end method;


/// now the splitting of the live ranges

define method split-red-green 
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>) => (b :: <boolean>)
  let splits :: <integer> = 0;
  let rgs :: <list> = #();
  let grs :: <list> = #();
  for (bb :: <basic-block> in pgm)
    if (bb.bb-colour == $red-colour)
      let greens :: <list> = #();
      for (pred :: <basic-block> in bb.bb-prev-set)
        if (member?(bb, pred.bb-next-set) & (pred.bb-colour == $green-colour))
          push!(pred, greens);
        end if;
      end for;
      unless (greens == #())
        unless (grs == #()) inc!(splits) end;      //// not first split
	push!(pair(bb, greens), grs);
      end unless;
    else
      let reds :: <list> = #();
      for (pred :: <basic-block> in bb.bb-prev-set)
        if (member?(bb, pred.bb-next-set) & (pred.bb-colour == $red-colour))
          push!(pred, reds);
        end if;
      end for;
      unless (reds == #())
        unless (rgs == #()) inc!(splits) end;      //// not first split
	push!(pair(bb, reds), rgs);
      end unless;
    end if;
  end for;
  if (splits <= backend.variables.optimize-leaf-case)
    for (gr :: <pair> in grs)
      add-green-red(backend, gr.tail, gr.head);
    end for;
    for (rg :: <pair> in rgs)
      add-red-green(backend, rg.tail, rg.head);
    end for;
    ~ empty?(grs) | ~ empty?(rgs)
  else
    #f;
  end if;
end method;

define method add-coloured-block
    (backend :: <harp-back-end>, preds :: <list>, 
     succ :: <basic-block>, reverse :: <boolean>)
    => (b :: <basic-block>)
  with-spliced-block
    (backend, preds, succ,
     method ()
       do-bit-set (index in succ.bb-live-entry.set-thingy-vect)
         red-green-vrs(backend, index, reverse);
       end do-bit-set;
     end method);
end method;

define method add-green-red 
    (backend :: <harp-back-end>, preds :: <list>, succ :: <basic-block>)
  let bb = add-coloured-block(backend, preds, succ, #f);
  bb.bb-colour := $brown-colour;
end method;


define method add-red-green
    (backend :: <harp-back-end>, preds :: <list>, succ :: <basic-block>)
  let bb = add-coloured-block(backend, preds, succ, #t);
  bb.bb-colour := $yellow-colour;
end method;


define method red-green-vrs
    (backend :: <harp-back-end>, index :: <integer>, reverse :: <boolean>)
  let state = backend.variables.vreg-state;
  let red-vr :: <virtual-register> = state.vr-vect[index];
  unless (member?(red-vr, state.unique-registers) 
          | red-vr.possible-arg-spill?)
    let green-vr :: <virtual-register> =
      begin
	let vr = state.green-vr-vect[index];
	if (vr.invalid-virtual-register?)
	  state.green-vr-vect[index] := 
	    make-register(backend, 
			  reg-class: object-class(red-vr),
			  name: red-vr.virtual-register-name,
			  indirections: red-vr.virtual-register-named-indirections);
	else vr
	end if;
      end;
    if (reverse)
      let temp :: <virtual-register> = red-vr;
      red-vr := green-vr;
      green-vr := temp;
    end if;
    move-reg(backend, red-vr, green-vr);
  end unless;
end method;


define method possible-arg-spill? (reg :: <virtual-register>)
  instance?(reg.virtual-register-colour, <integer>);
end method;


/// here we rename all the vrs in the green/blue blocks

define method rename-green-vrs 
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let sv :: <instructions-vector> = backend.variables.sv-instructions;
  let green-vrs = backend.variables.vreg-state.green-vr-vect;
  let new-scl-vrs :: <simple-object-vector> = #[];

  for (bb :: <basic-block> in pgm)
    if (bb.bb-colour == $green-colour)
      for-instructions-in-basic-block (ins in bb)
	let the-op :: <op> = ins-op(sv, ins);
        if (the-op.op-is-scl)
	  // For SCL instructions, duplicate and replace vectors of SCLs as a
          // whole unit, to ensure that the vectors are shared throughout the block
          // NOTE! This introduces the assumption that uze(2) is a lambda constant;
          //       this is true for the compiler(parameters of the lambda), but may
          //       not be so in general -- revisit
          // [Nosa  Jan 25, 1999]
	  with-uu (sv at ins)
	    if (new-scl-vrs == #[])

	    let scl-vrs :: <simple-object-vector> = uu-uze(2);

	    for (i :: <integer> from 0 below scl-vrs.size)
	      let u :: <virtual-register> = scl-vrs[i];
	      let green-vr :: <virtual-register> = green-vrs[u.virtual-register-id];
	      unless (green-vr.invalid-virtual-register?)
		if (new-scl-vrs == #[])
		  new-scl-vrs := copy-sequence(scl-vrs);
	          uu-uze(2) := new-scl-vrs;
		end if;
		new-scl-vrs[i] := green-vr
	      end;
	    end for;

	    else
	      uu-uze(2) := new-scl-vrs;
	    end if;
	  end with-uu;
	else

        for-instruction-defs (d in sv at ins)
	  if (instance?(d, <virtual-register>))
	    let green-vr = green-vrs[d.virtual-register-id];
	    unless (green-vr.invalid-virtual-register?) set-def(green-vr) end;
          end if;
        end for-instruction-defs;
	for-instruction-uses (u in sv at ins)
	  if (instance?(u, <virtual-register>))
	    let green-vr = green-vrs[u.virtual-register-id];
	    unless (green-vr.invalid-virtual-register?) set-use(green-vr) end;
          end if;
        end for-instruction-uses;

        end if;
      end for-instructions-in-basic-block;
    end if;
  end for;
end method;

/// the next batch of functions deal with the stack building stuff. Note that
/// there is no direct connection between #"red" and #"green" blocks and those
/// which have a stack frame as I had originally thought. ie #"red" blocks may
/// appear before the stack is built and #"green" blocks after. This is to do
/// with problems encountered with (a) tail recursive calls, (b) running out of
/// real registers in #"green" blocks, and (c) the constants register

/// stack state can be one of #"before" #"with" or nil (was #"after"). #"with" means
/// that either this bb will always need a stack frame (ie it contains a stack
/// or constants reg reference etc) or in any possible route through the bb's a
/// stack frame is needed. nil means that this bb doesn't need a stack frame
/// nor do it's or any of their successors etc etc need stack frames. #"before" 
/// means that this bb doesn't need a stack frame but some of it successor or
/// their succ... need stack frames. 

/// stack state can also be #"prolog" which means that this bb mustn't have a
/// stack (ie a special kind of #"before")

/// A problem - what do we do if we find a register that is coloured to the
/// function register. If the block in which this is found becomes of stack
/// state #"constants" we are well and truly buggered because reg::function will
/// be playing at being the constants register...

define method colour-needs-stack? 
    (preserved :: <vector>, colour :: <spill>) => (b :: <boolean>)
  ~ arg-spill?(colour);
end method;

define method colour-needs-stack? 
    (preserved :: <vector>, colour :: <real-register>) => (b :: <boolean>)
  member?(colour, preserved);
end method;

define method colour-needs-stack? 
    (preserved :: <vector>, colour :: <object>) => (b :: <boolean>)
  #f;
end method;


/// For some architectures (notably the Pentium), instructions may
/// clobber registers which are preserved by C, and yet which are
/// not allocatable HARP registers. In this case, the block must be
/// with stack, and the clobbered registers must be saved. We check for 
/// this here.

define method block-uses-unknown-c-preserved-reg?
    (backend :: <harp-back-end>, bb :: <basic-block>) => (res :: <boolean>)
  let vars = backend.variables;
  let state = vars.vreg-state;
  let sv   = vars.sv-instructions;
  let res = #f;
  for-instructions-in-basic-block-backwards (ins in bb)
    let the-op = ins-op(sv, ins);
    let clobbers-fn = the-op.op-c-preserved-destroys-fn;
    unless (clobbers-fn == nil-fn)
      let clobbered = prset-from-list(backend, clobbers-fn(backend, ins));
      unless (clobbered == 0)
        res := #t;
        let known-preserved = state.allocated-preserved;
        let new-preserved = r-union(known-preserved, clobbered);
        unless (new-preserved == known-preserved)
          state.allocated-preserved := new-preserved;
          state.number-preserved := new-preserved.logcount;
        end unless;
      end unless;
    end unless;
  end for-instructions-in-basic-block-backwards;
  res;
end method;


define method find-with-stack 
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let regs = backend.registers;
  let vars = backend.variables;
  let sv   = vars.sv-instructions;
  let call-in = vars.compiling-call-in;
  let preserved  = if (call-in) 
                       regs.c-preserved-register-vector 
                     else 
                       regs.preserved-register-vector 
                     end if;
  for (bb :: <basic-block> in pgm)
    block (done)
      if (call-in)
        if (block-uses-unknown-c-preserved-reg?(backend, bb))
          bb.bb-stack-state := $with-stack-state;
          done(#f);
        end if;
      end if;
      for-instructions-in-basic-block-backwards (ins in bb)
	if (stack-op?(ins-op(sv, ins)))
	  bb.bb-stack-state := $with-stack-state;
	  done(#f);
        end if;
	for-instruction-defs (d in sv at ins)
	  if (d == regs.reg-stack)
	    bb.bb-stack-state := $with-stack-state;
            done(#f);
          end if;
	  if (instance?(d, <virtual-register>))
	    let colour = d.virtual-register-colour;
	    if (colour-needs-stack?(preserved, colour))
	      bb.bb-stack-state := $with-stack-state;
              done(#f);
            end if;
          end if;
        end for-instruction-defs;
	for-instruction-uses (u in sv at ins)
	  if (instance?(u, <virtual-register>))
	    let colour = u.virtual-register-colour;
	    if (colour-needs-stack?(preserved, colour))
	      bb.bb-stack-state := $with-stack-state;
              done(#f);
            end if;
          end if;
        end for-instruction-uses;
      end for-instructions-in-basic-block-backwards;
    end block;
  end for;
end method;



//// instead of checking all the time for sparc, just redefine this



// this function propogates the above "with-stack" information so that for
// every block we can tell whether it has a stack built or not

// In the next function the various checks for changing a bb's stack state are
// done one after the other. This is possible because any change of a bb's
// stack state must adhere to the ordering nil -> #"before" -> #"after" -> #"with"
// It may have been more efficient to work out the bb's new state from it's
// successors and preds all in "one go" (as in propogate-red-blocks) and indeed
// at one point the code was written this way. This was ok when only #"with" and
// nil existed, but after the addition of #"before" and #"self" (which admittedly
// is more or less independent) the code was getting out of hand and any
// comments would have been useless because I hardly understood the code. With
// the addition of #"constants" I decided to simplify things slightly (ie rewrite
// the function + half of the rest of the file) _cim

define method propogate-stack-state 
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let vars = backend.variables;
  let count :: <integer> = 1;
  let changed :: <boolean> = #t;
  while (changed)
    changed := #f;
    for (bb :: <basic-block> in pgm)

      // first we check for "#"after"" blocks ie nil. For a block to be after
      // the stack exit then it can't have any successors that aren't #"after"

      if (bb.bb-stack-state == $no-stack-state & ~ empty?(bb.bb-next-set))
        // want to make sure that the last bb of
        // a function is preceded by a stack-exit
	if (should-be-before?(bb))
	  bb.bb-stack-state := $before-stack-state;
          changed := #t;
        end if;
      end if;

      // now we look for the #"before" -> #"with" transition. 
      if (bb.bb-stack-state == $before-stack-state)
	if (should-be-with?(bb))
	  bb.bb-stack-state := $with-stack-state;
	  changed := #t;
        end if;
      end if;

    end for;
    inc!(count);
  end while;
  vars.top-block.bb-stack-state := $before-stack-state;
end method;


define method none-unset? (l :: <list>) => (b :: <boolean>)
  if (l == #())
    #f
  else
    every?(method (bb :: <basic-block>) => (b :: <boolean>)
             ~ (bb.bb-stack-state == $no-stack-state);
           end,
           l)
  end if;
end method;


define method should-be-before? (bb :: <basic-block>) => (b :: <boolean>)
  if (any?(method (bb :: <basic-block>) bb.bb-stack-state ~== $no-stack-state end, 
           bb.bb-next-set))
    #t;
  else
    none-unset?(find-real-preds(bb));
  end if;
end method;

define method none-before? (l :: <list>) => (b :: <boolean>)
  if (l == #())
    #f
  else
    every?(method (bb :: <basic-block>) => (b :: <boolean>)
             let state = bb.bb-stack-state;
             ~ (state == $no-stack-state) & ~ (state == $before-stack-state)
           end,
           l)
  end if;
end method;

define method should-be-with? (bb :: <basic-block>) => (b :: <boolean>)
  unless (bb.bb-needs-leaf)
    none-before?(bb.bb-next-set) |
      none-before?(find-real-preds(bb));
  end unless;
end method;



// things have got a bit more complicated now  with the #"constants" bb's. But
// the basic idea is still the same ie if you want to go from somewhere that
// has a stack in a particular state to somewhere where the stack is in a
// "less developed" state you can't do it -> duplicate-bbs

/// in a little more detail see this table. The vertical column is the pred bb
/// and the horiz is the succ.
///
/// imposs -> this should never occur if the code is working properly
/// -      -> don't bother to do anything
/// dupl   -> duplicate the succ and set the stack-state of the copy to that of
/// 	      the pred
/// lf     -> ins::load-symbol-constants reg::function reg::function
/// rf     -> ins::move reg::function reg::constants
/// st-ext -> stack exit code
/// st-ent -> stack entry including setting reg::constants thus...
/// 		0 -> ins::load-symbol-constants reg::constants reg::function
///   		1 -> ins::move reg::constants reg::function
///   		()-> ins::move reg::constants reg::constants ie nop
/// 
///
///         *TO*   nil     #"before"  #"constants"#"with"    #"self"
/// *FROM*
/// nil	           -       imposs     imposs      imposs     imposs
/// #"before"      -       -          lf          st-ent(1)  imposs
/// #"constants"   -       dupl       -           st-ent(0)  imposs
/// #"with"	   st-ext  dupl       dupl,rf     -          only through bsr
/// #"self"	   -       dupl       dupl,rf     st-ent(()) -
///
///       



define method maybe-duplicate-bbs 
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let vars = backend.variables;
  let pgmvect :: <stretchy-basic-block-vector> = vars.pgm-vect;
  let count :: <integer> = 1;
  let dups :: <integer> = 0;
  let changed :: <boolean> = #t;
  while (changed)
    changed := #f;
    for (bb :: <basic-block> in pgm)
      if (bb.bb-stack-state == $before-stack-state)
	let withs :: <list> = #();
	let others :: <list> = #();
	       
	for (pred :: <basic-block> in bb.bb-prev-set)
	  if (~ (pred.bb-colour == $no-colour) & member?(bb, pred.bb-next-set))
	    // filter out the phonies
            if (pred.bb-stack-state == $with-stack-state)
              push!(pred, withs);
            else
              push!(pred, others)
            end if;
          end if;
        end for;
		 
	if (~ empty?(withs))
	  changed := #t;
	  let clone-bb :: false-or(<basic-block>) = bb.bb-copy-of;
          while (clone-bb & ~( clone-bb.bb-stack-state == $with-stack-state))
            clone-bb := clone-bb.bb-copy-of;
          end while;

          let clone-tag :: <tag> = 
            if (clone-bb) first(clone-bb.bb-taags) else make-tag(backend) end;
	  unless (clone-bb)
	    clone-bb := copy-bb(bb);
	    inc!(dups);
	    bb.bb-copy-of := clone-bb;
            add!(pgmvect, clone-bb);

	    make-current-this-bb(backend, clone-bb);
	    let len :: <integer> = bb.bb-end - bb.bb-start;
	    let dest :: <integer> = vars.fp-instructions;
	    let start :: <integer> = bb.bb-start;
	    let sv :: <instructions-vector> =
	      ensure-room-in-vector(vars.sv-instructions, len + dest);
	    vars.sv-instructions := sv;
            until (len == 0)
              sv[dest] := sv[start];
              inc!(dest); inc!(start); dec!(len);
            end until;
	    vars.fp-instructions := dest;
	    finish-bb(backend);
		       
	    clone-bb.bb-stack-state := $with-stack-state;

            if (clone-bb.bb-fall-thru & clone-bb.bb-fall-thru.bb-taags == #())
	      let fall-thru-tag :: <tag> = make-tag(backend);
              let fall-thru-bb :: <basic-block> = clone-bb.bb-fall-thru;
	      fall-thru-tag.tag-bb := fall-thru-bb;
	      push!(fall-thru-tag, fall-thru-bb.bb-taags);
            end if;

	    clone-tag.tag-bb := clone-bb;
	    clone-bb.bb-taags := list(clone-tag);
	    clone-bb.bb-prev-set := #();
          end unless;

	  clone-bb.bb-prev-set := concatenate(withs, clone-bb.bb-prev-set);
          bb.bb-prev-set := others;
		 
	  for (succ :: <basic-block> in bb.bb-next-set)
            push!(clone-bb, succ.bb-prev-set);
          end for;
		 
	  let sv :: <instructions-vector> = vars.sv-instructions;
	  for (pred :: <basic-block> in withs)
	    pred.bb-next-set := safe-substitute(pred.bb-next-set, bb, clone-bb);
	    if (pred.bb-fall-thru == bb)
	      pred.bb-fall-thru := clone-bb;
            end if;
            let tag-index :: <integer> = pred.bb-end - instruction-size;
	    if (member?(ins-tag(sv, tag-index), bb.bb-taags))
	      ins-tag(sv, tag-index) := clone-tag;
            end if;
          end for;
        end if;
      end if;
    end for;
    inc!(count);
  end while;
end method;


define method insert-stack-code-etc
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let entries :: <integer> = 0;
  let exits :: <integer> = 0;
  for (bb :: <basic-block> in pgm)
    if (bb.bb-stack-state == $with-stack-state)
      let befores :: <list> = #();
      let others :: <list> = #();
      for (pred :: <basic-block> in bb.bb-prev-set)
	if (member?(bb, pred.bb-next-set))
          if (pred.bb-stack-state == $before-stack-state)
            push!(pred, befores);
          end if;
        else
	  // this push handles the bsr for self tags
	  push!(pred, others);
        end if;
      end for;
      if (~ empty?(others))
	add-stack-entry(backend, others, bb);
        inc!(entries);
      end if;
      if (~ empty?(befores))
	add-stack-entry(backend, befores, bb); 
        inc!(entries);
      end if;
    else
      // no stack built
      if (bb.bb-stack-state == $no-stack-state)
	let withs :: <list> = #();
	for (pred :: <basic-block> in bb.bb-prev-set)
	  if (member?(bb, pred.bb-next-set))
	    if (pred.bb-stack-state == $with-stack-state)
	      push!(pred, withs);
            end if;
          else
	    // this traps for self calls jumping to a point after which
	    // the stack has been built -- this should be impossible
	    error("Internal leaf case error with self call");
          end if;
        end for;
	if (~ empty?(withs))
          add-stack-exit(backend, withs, bb);
          inc!(exits);
        end if;
      end if;

      // lets fudge the load-stack-arg-n stuff here... this is getting
      // sicker and more sick
      modify-stack-dependent-ins(backend, bb);
    end if;
  end for;
end method;


// Modify-stack-dependent-ins corrects the behaviour of
// ins::load-stack-arg-n when there is no frame.

define method modify-stack-dependent-ins
    (backend :: <harp-back-end>, bb :: <basic-block>)
  let sv :: <instructions-vector> = backend.variables.sv-instructions;
  for-instructions-in-basic-block (ins in bb)
    let op :: <op> = ins-op(sv, ins);
    if (op.op-stack-dependent)
      ins-tag(sv, ins) := #f;
    end if;
  end for-instructions-in-basic-block;
end method;



define method add-stack-entry 
    (backend :: <harp-back-end>, pred-list :: <list>, succ :: <basic-block>)
    => (b :: <basic-block>)
  with-spliced-block 
   (backend, pred-list, succ,
    method ()
      ins-stack-entry(backend);
    end method);
end method;

define method add-stack-exit
    (backend :: <harp-back-end>, pred-list :: <list>, succ :: <basic-block>)
    => (b :: <basic-block>)
  if (dangling-block?(backend, succ))
    succ;
  else
    with-spliced-block 
     (backend, pred-list, succ,
      method ()
        ins-stack-exit(backend);
      end method);
  end if;
end method;


/// In some circumstances (e.g. after an end-cleanup) a block may be 
/// encountered which has no instructions associated with it (e.g.
/// the block is empty and has no successors). There is no point
/// inserting stack-exit code before such a block (TonyM 27/9/94)

define method dangling-block? 
   (backend :: <harp-back-end>, bb :: <basic-block>)
  empty?(bb.bb-next-set) & (bb.bb-start = bb.bb-end);
end method;


/// finally these two functions make up the interface to the rest of the
/// back-end

/// this is called after dataflow but before graph colouring

define method optimize-leaf-case-1 
    (backend :: <harp-native-back-end>, pgm :: <stretchy-basic-block-vector>,
     top-block :: <basic-block>) => (b :: <boolean>)
  let vars = backend.variables;
  let state = vars.vreg-state;
  unless (vars.compiling-defasm)
    let vr-len :: <integer> = size(state.vr-vect);
    state.green-vr-vect
      := make(<simple-virtual-register-vector>, size: vr-len, fill: invalid-virtual-register());
    find-red-blocks(backend, pgm);
    propogate-red-blocks(pgm);
    if (split-red-green(backend, pgm))
      rename-green-vrs(backend, pgm);
      // and now we redo the dataflow stuff all over again - probably
      // should be a bit more intelligent here and avoid redoing the work
      // but... 
      vars.pgm-vect := build-pgm-vector(backend, top-block);
      for (bb :: <basic-block> in vars.pgm-vect)
	bb.bb-last-entry := -1;
      end for;
      set-use-defs(backend, vars.pgm-vect);
      set-liveness(backend, vars.pgm-vect);
      #t;
    end if;
  end unless;
end method;

/// this is called after graph colouring - here we try to move the stack
/// building stuff 

define method optimize-leaf-case-2
    (backend :: <harp-native-back-end>, pgm :: <stretchy-basic-block-vector>) => ()
  let vars = backend.variables;
  unless (vars.compiling-defasm)

    // first some tidying up
    for (bb :: <basic-block> in pgm)
      bb.bb-copy-of := #f;
    end for;

    find-with-stack(backend, pgm);
    propogate-stack-state(backend, pgm);
    maybe-duplicate-bbs(backend, pgm);
    insert-stack-code-etc(backend, pgm);
  end unless;
end method;


define method ins-stack-entry (backend :: <harp-back-end>)
  call-instruction(preserve-registers-entry, backend);
  call-instruction(allocate-local-area, backend);
  call-instruction(allocate-raw-area, backend);
end method;



/// changed stack-exit to be a default callable. (TonyM 27/11/90)

define method ins-stack-exit (backend :: <harp-back-end>)
  call-instruction(preserve-registers-exit, backend);
end method;

///
