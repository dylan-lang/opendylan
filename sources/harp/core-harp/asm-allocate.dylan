module:    main-harp
Synopsis:  The HARP register allocator.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Yet another re-working of allocation, this time to support iteratable
/// bitsets. Also, remove almost as amny special refs as poss.

/// The basic operations available are test membership, make member, remove,
/// test set intersection, iterate over members. Reals & virtuals still have
/// different sets, and the reals are still represented as fixnums.

/// Representing the clash graph in this way means an end to v/v preferencing.
/// Well, it was never anything very special.

/// A file in the asm package to do the register allocation, using the
/// bignum-free allocator.
///
/// Begun AJW 14/1/88, from the original done in late 87. Still much to do ...
///       AJW 25/2/88  Simplify Package structure
///       AJW 19/3/88  Soup up dataflow
///       AJW 20/3/88  Spot loop-free case for fast dataflow, v/v preferencing
///       AJW 25/3/88  Better v/v preferencing ... (y-asm-allocate version)
///       AJW  1/4/88  Add RA phase timings
///       AJW  8/4/88  Add floating point stuff
///       AJW 18/4/88  More intelligent graph building
///       AJW 19/4/88  Just replace all hashtables with bitsets
///       AJW  2/5/88  Let's take it that spill is always coloured ...
///       AJW  5/5/88  Work on blocks with instruction sequences reversed.
///       AJW  9/5/88  Less garbageful use/def info



/// Having given up on splitting as a way of getting decent allocation, in a
/// caller-saves environment, a fast graph colourer to do the allocation that
/// spur-graf does with splitting turned off, only faster.

/// The other string to this new bow is a phase of local fix-up after the
/// global allocation, but I haven't really worked out how to do this yet.

/// Mods AJW 11/12/87 to colour the spill area.

// some generic functions to be specialized by the back end:


define open generic optimize-leaf-case-1 
       (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>, top-block :: <basic-block>) 
    => (b :: <boolean>);

define method optimize-leaf-case-1 
       (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>, top-block :: <basic-block>) 
    => (b :: <boolean>)
  #f;
end method;

define open generic optimize-leaf-case-2
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>) => ();

define method optimize-leaf-case-2
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>) => ()
end method;



define open generic allowable-colours  
    (backend :: <harp-back-end>, vr :: <virtual-register>) => (i :: <integer>);

define open generic make-pref-vector
    (backend :: <harp-back-end>) => (v :: <simple-integer-vector>);

define inline method make-reg-bit-set 
    (backend :: <harp-back-end>) => (n :: <vector-32bit>)
  let vars = backend.variables;
  make(<vector-32bit>, size: vars.word-size-of-bit-set, fill: $bit-set-zero$);
end;


define method select-central 
    (backend :: <harp-back-end>, vr :: <greg>) => (c :: <central-spill>)
  backend.variables.vreg-state.gc-spill-central;
end;

define method select-central
    (backend :: <harp-back-end>, vr :: <nreg>) => (c :: <central-spill>)
  backend.variables.vreg-state.ng-spill-central;
end;

define method select-central
    (backend :: <harp-back-end>, vr :: <sfreg>) => (c :: <central-spill>)
  backend.variables.vreg-state.sf-spill-central;
end;

define method select-central
    (backend :: <harp-back-end>, vr :: <dfreg>) => (c :: <central-spill>)
  backend.variables.vreg-state.df-spill-central;
end;

define method uniquely-spill
    (backend :: <harp-back-end>, vr :: <virtual-register>)
  let vars = backend.variables;
  push!(vr, vars.vreg-state.unique-registers);
end;

define inline method gc-maker (x :: <integer>) => (s :: <gspill>)
  make(<gspill>, offset: x)
end;

define inline method ng-maker (x :: <integer>) => (s :: <nspill>)
  make(<nspill>, offset: x)
end;

define inline method sf-maker (x :: <integer>) => (s :: <sfspill>)
  make(<sfspill>, offset: x)
end;

define inline method df-maker (x :: <integer>) => (s :: <dfspill>)
  make(<dfspill>, offset: x)
end;

define inline method init-spill (backend :: <harp-back-end>)
  let state :: <vreg-state> = backend.variables.vreg-state;
  state.gc-spill-central := make(<central-spill>, maker: gc-maker);
  state.ng-spill-central := make(<central-spill>, maker: ng-maker);
  state.sf-spill-central := make(<central-spill>, maker: sf-maker);
  state.df-spill-central := make(<central-spill>, maker: df-maker);
end;

define method next-spill (central :: <central-spill>) => (s :: <spill>)
  let maker :: <function> = central.central-spill-maker;
  let fp :: <integer> = size(central.central-spill-holder);
  let the-object = maker(fp);
  add!(central.central-spill-holder, the-object);
  the-object;
end;

// MJS 10/10/91: spilled arguments are left in call position.
// Marked by having a negative spill-offset.
define inline method arg-spill-offset-to-arg-number
     (spill-offset :: <integer>) => (i :: <integer>)
  -1  - spill-offset;
end;

define inline method arg-number-to-arg-spill-offset
     (spill-offset :: <integer>) => (i :: <integer>)
  -1  - spill-offset;
end;


define method make-arg-spill
    (vr :: <greg>, old-colour :: <integer>) => (s :: <gspill>)
  let arg-number :: <integer> = arg-number-to-arg-spill-offset(old-colour);
  gc-maker(arg-number);
end;

define method make-arg-spill
    (vr :: <nreg>, old-colour :: <integer>) => (s :: <nspill>)
  let arg-number :: <integer> = arg-number-to-arg-spill-offset(old-colour);
  ng-maker(arg-number);
end;




define method arg-spill? (x :: <spill>) => (b :: <boolean>)
  x.spill-offset < 0;
end;

define method arg-spill? (x :: <object>) => (b :: <boolean>)
  #f;
end;

/* currently unused
define method get-me-a-permanent-slot (central :: <central-spill>) => (s :: <spill>)
  let next = central.next-spill;
  central.central-spill-lower-limit := size(central.central-spill-holder);
  next;
end;
*/

define method advance-mark (vr :: <virtual-register>) => (s :: <spill>)
  let ss = vr.virtual-register-spill-set;
  let central = ss.spill-set-central;
  let limit = ss.spill-set-limit;
  let lower-limit = central.central-spill-lower-limit;

  if (limit < lower-limit)
    ss.spill-set-limit := limit := lower-limit;
  end if;

  if (limit >= size(central.central-spill-holder))
    next-spill(central);
  else
    let spill :: <spill> = central.central-spill-holder[limit];
    inc!(ss.spill-set-limit);
    spill;
  end if;
end;

define class <set-thingy> (<object>)
  constant slot set-thingy-vect :: <vector-32bit>, required-init-keyword: vect:;
  slot set-thingy-mask :: <integer>, init-value: 0, init-keyword: mask:;
end;

/* Define temporary bit-subsets for virtual-register ranges

   All virtual-registers defined by a basic-block are likely
   to be in close proximity of age; most of the time, only
   one machine word of data is sufficient to encode this

   This cuts down significantly on consing for huge lambdas

   All bits in subsets are initially set, until explicitly unset

   Nosa  Jan 25, 1999 */

define class <subset-thingy> (<object>)
  slot subset-thingy-vect :: <bit-subset>, required-init-keyword: vect:;
  slot subset-thingy-mask :: <integer>, init-value: -1, init-keyword: mask:;
end;

define inline method make-set-thingy (vect) => (t :: <set-thingy>)
  make(<set-thingy>, vect: vect);
end;

define inline method make-subset-thingy (size :: <integer>) => (t :: <subset-thingy>)
  make(<subset-thingy>,
       vect: if (size = 0) $empty-bit-subset
	     else
	       make-bit-subset(size: size, fill: $bit-set-not-zero$)
	     end);
end;

define constant $empty-subset-thingy :: <subset-thingy> =
  make(<subset-thingy>,
       vect: $empty-bit-subset);

define inline method empty-subset-thingy? (set :: <subset-thingy>)
 => (empty? :: <boolean>)
  set == $empty-subset-thingy
end;

define inline method empty-subset-thingy ()
 => (set :: <subset-thingy>)
  $empty-subset-thingy
end;

define inline method set-thingy-size (set :: <set-thingy>) => (i :: <integer>)
  logcount(set.set-thingy-vect) + logcount(set.set-thingy-mask);
end;


define inline method add-set-thingy 
    (item :: <virtual-register>, set :: <set-thingy>)
  set-bit-in-set(set.set-thingy-vect, item.virtual-register-id);
end;

define inline method add-set-thingy 
    (item :: <real-register>, set :: <set-thingy>)
  add-set-thingy(item.real-register-mask, set);
end;

define inline method add-set-thingy 
    (mask :: <integer>, set :: <set-thingy>)
  set.set-thingy-mask := r-union(set.set-thingy-mask, mask);
end;

define inline method remove-set-thingy 
    (item :: <virtual-register>, set :: <set-thingy>)
  unset-bit-in-set(set.set-thingy-vect, item.virtual-register-id);
end;

define inline method remove-set-thingy 
    (item :: <real-register>, set :: <set-thingy>)
  remove-set-thingy(item.real-register-mask, set);
end;

define inline method remove-set-thingy 
    (mask :: <integer>, set :: <set-thingy>)
  set.set-thingy-mask := r-set-diff(set.set-thingy-mask, mask);
end;

define generic remove-subset-thingy 
    (item, set :: <subset-thingy>)
 => (set :: <subset-thingy>);

define inline method remove-subset-thingy 
    (item :: <virtual-register>, set :: <subset-thingy>)
 => (set :: <subset-thingy>)
  let set :: <subset-thingy> =
    case
      empty-subset-thingy?(set) => make-subset-thingy(1);
      (set.subset-thingy-vect == $empty-bit-subset) =>
	set.subset-thingy-vect := make-bit-subset(size: 1, fill: $bit-set-not-zero$);
	set;
      otherwise => set;
    end;
  unset-bit-in-subset(set.subset-thingy-vect, item.virtual-register-id);
  set
end;

define inline method remove-subset-thingy 
    (item :: <real-register>, set :: <subset-thingy>)
 => (set :: <subset-thingy>)
  remove-subset-thingy(item.real-register-mask, set);
end;

define inline method remove-subset-thingy 
    (mask :: <integer>, set :: <subset-thingy>)
 => (set :: <subset-thingy>)
  let set :: <subset-thingy> =
    case
      empty-subset-thingy?(set) => make-subset-thingy(0);
      otherwise => set;
    end;
  set.subset-thingy-mask := r-set-diff(set.subset-thingy-mask, mask);
  set
end;

define macro update-set-thingies
  { update-set-thingies() }
    =>
  {
   remove-set-thingy(?=register, ?=set1);
   remove-subset-thingy(?=register, ?=set2);
   }
end macro;


define generic update-register-sets
    (item, set1 :: <set-thingy>, set2 :: <subset-thingy>)
 => (set :: <subset-thingy>);

define method update-register-sets
    (register :: <virtual-register>, set1 :: <set-thingy>, set2 :: <subset-thingy>)
 => (set :: <subset-thingy>)
  update-set-thingies();
end;

define method update-register-sets
    (register :: <real-register>, set1 :: <set-thingy>, set2 :: <subset-thingy>)
 => (set :: <subset-thingy>)
  update-set-thingies();
end;

define method update-register-sets
    (item, set1 :: <set-thingy>, set2 :: <subset-thingy>)
 => (set :: <subset-thingy>)
  set2
end;


define inline method remove-register-from-live-sets
    (item, live-set :: <set-thingy>, live-names-set :: <vector-32bit>,
     clash-count :: <integer>) => ()
  if (instance?(item, <virtual-register>))
    let num :: <integer> = item.virtual-register-id;
    let offset :: <integer> = bit-set-offset(num);
    let the-bit :: <machine-word> = machine-word-lognot(bit-set-mask(num));
    let live-set :: <vector-32bit> = live-set.set-thingy-vect;

    unset-bit-in-word(live-set, offset, the-bit);
    if (item.virtual-register-name)
      unset-bit-in-word(live-names-set, offset, the-bit);
    end;
    inc!(item.virtual-register-clash-count, clash-count);
  elseif (instance?(item, <real-register>))
    remove-set-thingy(item, live-set);
  end;
end;

// define inline method remove-register-from-live-set
//     (item, live-set :: <set-thingy>) => ()
//   if (instance?(item, <virtual-register>))
//     remove-set-thingy(item, live-set);
//   elseif (instance?(item, <real-register>))
//     remove-set-thingy(item, live-set);
//   end;
// end;

define inline method add-register-to-live-sets
    (item,
     live-set :: <set-thingy>, live-names-set :: <vector-32bit>, lr-table :: <vector-32bit>,
     clash-count :: <integer>) => ()
  if (instance?(item, <virtual-register>))
    let num :: <integer> = item.virtual-register-id;
    let offset :: <integer> = bit-set-offset(num);
    let the-bit :: <machine-word> = bit-set-mask(num);
    let live-set :: <vector-32bit> = live-set.set-thingy-vect;

    set-bit-in-word(live-set, offset, the-bit);
    set-bit-in-word(lr-table, offset, the-bit);
    if (item.virtual-register-name)
      set-bit-in-word(live-names-set, offset, the-bit);
    end;
    inc!(item.virtual-register-clash-count, clash-count);
  elseif (instance?(item, <real-register>))
    add-set-thingy(item, live-set);
  end;
end;

define inline method add-register-to-live-set
    (item, live-set :: <set-thingy>) => ()
  if (instance?(item, <virtual-register>))
    add-set-thingy(item, live-set);
  elseif (instance?(item, <real-register>))
    add-set-thingy(item, live-set);
  end;
end;


/* currently unused
define method union-set-thingy-reals
     (set1 :: <set-thingy>, set2 :: <set-thingy>) => (i :: <integer>)
  set1.set-thingy-mask := r-union(set1.set-thingy-mask, set2.set-thingy-mask);
end;

define method set-thingy-member
    (item :: <virtual-register>, set :: <set-thingy>)
  get-bit-from-set(set.set-thingy-vect, item.virtual-register-id);
end;

define method set-thingy-member
    (item :: <real-register>, set :: <set-thingy>)
  r-membr(item, set.set-thingy-mask);
end;
*/

define method set-thingy-union (set1 :: <set-thingy>, set2 :: <set-thingy>)
  bit-set-or(set1.set-thingy-vect, set2.set-thingy-vect);
  set1.set-thingy-mask := r-union(set1.set-thingy-mask, set2.set-thingy-mask);
end;

// define method set-thingy-diff (set1 :: <set-thingy>, set2 :: <set-thingy>)
//   bit-set-andc2(set1.set-thingy-vect, set2.set-thingy-vect);
//   set1.set-thingy-mask :=
//      r-set-diff(set1.set-thingy-mask, set2.set-thingy-mask);
// end;


define method set-thingy-copy (dst :: <set-thingy>, src :: <set-thingy>)
  copy-bit-set(dst.set-thingy-vect, src.set-thingy-vect);
  dst.set-thingy-mask := src.set-thingy-mask;
end;

define inline method set-thingy-emptyify (set :: <set-thingy>)
  clear-bit-set(set.set-thingy-vect);
  set.set-thingy-mask := 0;
end;

define method set-thingy-live(set1 :: <set-thingy>, set2 :: <set-thingy>, set3 :: <subset-thingy>) => ()

  bit-set-or-andc2(set1.set-thingy-vect,
		   set2.set-thingy-vect,
		   set3.subset-thingy-vect);
  set1.set-thingy-mask :=
     logior(set1.set-thingy-mask, logand(set2.set-thingy-mask, set3.subset-thingy-mask));

end method;


/// AJW 19/3/88 - alter the dataflow persuant to a suggestion by CER. Given
/// that the sets can only ever grow in size, changed info decided by size of
/// set. Add a pre-pass to set things up and cache this info.
/// bb-last-entry is now the hash table count last time round.

define method set-liveness
     (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  // a program is a vector of basic blocks
  let changed :: <boolean> = #F;     // re-use this all over the place
  let loop-depth :: <integer> = 1;

  // First the pre-pass - live entry is use-before-def, do next set
  // actions, set last entry as hash table count. After all done, look at the
  // not-done flag to see if we must iterate.

  for (bb :: <basic-block> in pgm)
    let live-entry :: <set-thingy> = bb.bb-live-entry;
    for (next-bb :: <basic-block> in bb.bb-next-set)  // for every successor
      if (next-bb.bb-last-entry = -1)     // now also serves as vis flag
        changed := #t;		           // no ht -> back edge
        inc!(loop-depth);
      end if;
      set-thingy-live(live-entry, next-bb.bb-live-entry, bb.bb-defs);
    end for;

    bb.bb-last-entry := set-thingy-size(live-entry);
    bb.bb-live-entry := live-entry;
    // Upper limit of 7 on loop-depths
    bb.bb-loop-depth := min(max(loop-depth, 0), 8);

    for (prev :: <basic-block> in bb.bb-prev-set)
      unless (prev.bb-last-entry = -1)
	dec!(loop-depth);
      end unless;
    end for;
  end for;

  // Now do the actions as many times as needed ...
  while (changed)
    changed := #f;                 // the changed flag starts false
     for (bb :: <basic-block> in pgm)
        // new-live-entry = use-before-def U (live-exit - defines-before-use)
        //
        // live-exit is U(successors) entry-set
       
        for (next-bb :: <basic-block> in bb.bb-next-set)  // for successors
	  set-thingy-live(bb.bb-live-entry, next-bb.bb-live-entry, bb.bb-defs);
        end for;

        // Now do the changed flag business and update record of set size

        let this-times-count = set-thingy-size(bb.bb-live-entry);
	changed := changed | (~ (this-times-count = bb.bb-last-entry));
	bb.bb-last-entry := this-times-count;
    end for;
  end while;

  // Having done, throw away stuff we don't need - the whole of
  // bb-defs

  for (bb :: <basic-block> in pgm)
    bb.bb-defs := #f;
  end for;

  // and lets check for virtuals with no def

end;

/// The bb-uses (use before define) and the bb-defines
/// (defines before use) hash tables are set up here, but (as of 19/3/88),
/// not the liveness tables.

/// AJW 5/5/88 The instruction sequeneces are now reversed here.

/// TonyM 27/7/93 Merge in the PC compiler changes which used to be in
/// a private copy in ~pcl/pcharp.

define method set-use-defs
     (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let vars = backend.variables;
  let vstate = vars.vreg-state;
  let unique-regs :: <list> = vstate.unique-registers;
  let sv :: <instructions-vector> = vars.sv-instructions;
  let args-bit-set = #f;
  vars.word-size-of-bit-set := calculate-word-size-of-bit-set(backend);
  if (~ unique-regs == #())
    args-bit-set := make-reg-bit-set(backend);
    for (vr :: <virtual-register> in unique-regs)
      set-bit-in-set(args-bit-set, vr.virtual-register-id);
    end for;
  end if;

  for (bb :: <basic-block> in pgm)
    let uses-bit-set :: <vector-32bit> = make-reg-bit-set(backend);
    let uses :: <set-thingy> = make-set-thingy(uses-bit-set);
    if (args-bit-set & ~ (bb.bb-colour == $before-move-home-colour))
      let ins :: <op> = ins-op(sv, bb.bb-start);
      unless (ins.op-does-jump) bit-set-or(uses-bit-set, args-bit-set) end;
    end if;
    bb.bb-live-entry := uses;
    compute-live-entry-exclusive(backend, bb);
  end for;
end;

define method compute-live-entry-exclusive
    (backend :: <harp-back-end>, bb :: <basic-block>)
 => ()
    let vars = backend.variables;
    let sv :: <instructions-vector> = vars.sv-instructions;
    let uses :: <set-thingy> = bb.bb-live-entry;
    let defs :: <subset-thingy> = empty-subset-thingy();

    for-instructions-in-basic-block-backwards (ins in bb)
      for-instruction-defs (d in sv at ins)
        defs := update-register-sets(d, uses, defs);
      end for-instruction-defs;

      for-instruction-uses (u in sv at ins)
        add-register-to-live-set(u, uses);
      end for-instruction-uses;

      let this-op :: <op> = ins-op(sv, ins);
      let implicit-fn = this-op.op-implicit-uses;
      unless (implicit-fn == nil-fn)
	let implicit-uses :: <integer> = implicit-fn(backend, ins);
	add-set-thingy(implicit-uses, uses);
      end unless;
    end for-instructions-in-basic-block-backwards;

    bb.bb-defs := defs;

end method;



define method warn-on-live-on-entry (backend :: <harp-back-end>)
  let vars = backend.variables;
  let vstate = vars.vreg-state;
  let unique-regs :: <list> = vstate.unique-registers;
  let vrs = vstate.vr-vect;
  let thingy :: <set-thingy> = vars.top-block.bb-live-entry;
  let live-entry :: <vector-32bit> = thingy.set-thingy-vect;
  let live-count :: <integer> = logcount(live-entry);
  unless (live-count = 0)
    let live-regs :: <list> = #();
    do-bit-set (id in live-entry)
      let vr :: <virtual-register> = element-no-bounds-check(vrs, id);
      unless (member?(vr, unique-regs))
        live-regs := pair(vr, live-regs);
      end unless;
    end do-bit-set;
    unless (size(live-regs) = 0)
      harp-warning(backend, "%= are live on entry to lambda", live-regs);
    end unless;
  end unless;
end;

/// Something to take a mixed collection of reals & virtuals, & return the
/// original list with all reals removed and turned into a bitset.
/// Probably a good idea to do this non-destructively.

define method calculate-word-size-of-bit-set (backend :: <harp-back-end>)
 => (word-size-of-bit-set :: <integer>)
  let start :: <integer> = 1 + backend.variables.vreg-state.next-vreg-id;
  for (word-length :: <integer> from 0 by $bit-set-word-size$,
       length :: <integer> from start to 0 by - $bit-unit-size$)
    finally word-length;
  end for;
end;


/// Now for the graph builder. The graph is a list of Virtual Registers ...
/// AJW 29/2/88 int-graph now needs to know number of locals for when we
///             colour spills.

define method int-graph
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>, 
    top-block :: <basic-block>)

  let vars = backend.variables;
  vars.word-size-of-bit-set := 0;

  // First off, calculate all sorts of useful data about the graph ...
  set-use-defs(backend, pgm);	// set up all use-def information
  set-liveness(backend, pgm);   // then do the live variable analysis
  warn-on-live-on-entry(backend);
  let do-leaf-case = vars.optimize-leaf-case & 
                     optimize-leaf-case-1(backend, pgm, top-block);
      
  // AJW 14/1/88 - initialise the slots in all the VRs.
  // All this should really be done at vr creation time ...
  build-the-graph(backend, vars.pgm-vect, do-leaf-case);
end;

define method eliminate-instruction
     (backend :: <harp-back-end>, ins :: <integer>)
  let vars = backend.variables;
  let sv :: <instructions-vector> = vars.sv-instructions;
  ins-op(sv, ins) := op-element(backend.instructions, rem);
  ins-tag(sv, ins) := "eliminated";
  for (i :: <integer> from 0 below instruction-size - instruction-defs-index,
       index :: <integer> from ins + instruction-defs-index)
    sv[index] := #f;
  end for;
end;
	  
define method init-vrs (backend :: <harp-back-end>)
  let state = backend.variables.vreg-state;
  let vr-vect :: <stretchy-virtual-register-vector> = state.vr-vect;
  let fp = size(vr-vect);
  let i :: <integer> = state.next-vreg-id;
  let factori :: <integer> = factorial-add(i);
  let factor :: <integer> = factori;

  backend.variables.virtual-register-clashes :=
    ensure-room-in-vector
    (backend.variables.virtual-register-clashes,
     factori + i + 1);

  for (j :: <integer> from 0 below fp)
    let vr :: <virtual-register> = vr-vect[j];
    let central :: <central-spill> = select-central(backend, vr); 
    // the appropriate spill type

    vr.virtual-register-available-colours := allowable-colours(backend, vr);
    vr.virtual-register-spill-set := make-spill-set(central);
    vr.virtual-register-clash-count := 0;

    vr.virtual-register-clash-start := factor - factori;
    factori := factori - i;  i := i - 1;

    if (empty?(vr.virtual-register-colour-pref))
      vr.virtual-register-colour-pref := make-pref-vector(backend);
    end if;
  end for;
    
  // Now a quick post-pass to set up the spill set limits 
  for (j :: <integer> from 0 below fp)
    let vr :: <virtual-register> = vr-vect[j];
    let spill-set :: <spill-set> = vr.virtual-register-spill-set;
    spill-set.spill-set-limit := 
      spill-set.spill-set-central.central-spill-lower-limit;
  end for;
end;

// 2^*loop-factor* gives the factor by which inner loops are weighted for
// register allocation

define variable loop-factor :: <integer> = 3;
//  "Used to control weighting of ra in inner loops"


define method build-the-graph
     (backend :: <harp-back-end>,  pgm :: <stretchy-basic-block-vector>,
     optimize-leaf-case :: <boolean>)

  let vars = backend.variables;
  // First, init the spills
  init-spill(backend);

  // Now initialise the virtual registers
  init-vrs(backend);

  // Now we walk over the basic blocks
  let live-set = make-set-thingy(make-reg-bit-set(backend));
  let lr-table = make-reg-bit-set(backend);
  let live-cache :: <live-cache> =
    make(<live-cache>, live-set: make-reg-bit-set(backend));
      
  for (blk :: <basic-block> in pgm)
    let bb-next-set :: <list> = blk.bb-next-set;
    if (bb-next-set.empty?)
      set-thingy-emptyify(live-set);
    else
      let succ :: <basic-block> = bb-next-set.head;
      let bb-next-set :: <list> = bb-next-set.tail;
      set-thingy-copy(live-set, succ.bb-live-entry);
      for (succ :: <basic-block> in bb-next-set) // for every successor
	set-thingy-union(live-set, succ.bb-live-entry);
      end for;
    end;
    walk-block(backend, blk, live-set, lr-table, live-cache);
  end for;
  lr-table;
end;

define variable vv-prefer :: <boolean> = #t;

// Update the set of live named registers from the set
// of all live registers

define method update-live-names-set
    (backend :: <harp-back-end>,
     live-names-set :: <vector-32bit>,
     live-set :: <vector-32bit>) => ()
  copy-bit-set(live-names-set, live-set);

  let vr-vect :: <stretchy-virtual-register-vector> =
    backend.variables.vreg-state.vr-vect;
  do-bit-set (bit-id :: <integer> in live-names-set)
    let reg :: <virtual-register> =
      element-no-bounds-check(vr-vect, bit-id);
    unless (reg.virtual-register-name)
      unset-bit-in-word(live-names-set, $word$,
			machine-word-lognot($bit-mask$));
    end;
  end;

end method;

define method walk-block
    (backend :: <harp-back-end>, blk :: <basic-block>,
     live-set :: <set-thingy>, lr-table :: <vector-32bit>,
     live-cache :: <live-cache>)
  let vars = backend.variables;
  let sv :: <instructions-vector> = vars.sv-instructions;
  let arg-count = backend.registers.reg-arg-count;
  let vr-vect :: <stretchy-virtual-register-vector> =
    vars.vreg-state.vr-vect;

  // mark cache as not up-to-date for this block
  live-cache.cache-up-to-date? := #f;

  // Now we walk backwards over each block doing the detailed clash
  // detection. Two vrs clash if one is live at the point that the other is
  // defined with the exception of the move instruction which clashes its
  // defined vr with all those that are live at that point except for the
  // source of the move.
  // Real registers are only removed from the colour sets, not added into the
  // graph. 

  // for the loop weighting we set a limit of 2^21 on the assumption that the
  // maximum preference (unweighted) doesn't exceed 127 (should check this)

  let to-shift :: <integer> = (blk.bb-loop-depth - 1) * loop-factor;
  let loop-weighting :: <integer> = 
      ash(1, if (to-shift > 21) 21 else to-shift end);
  let is-green :: <boolean> = blk.bb-colour == $green-colour;

  for-instructions-in-basic-block-backwards (ins in blk)
						   
    // AJW 30/1/88. If, at this point, we find that any of the
    // defined virtual registers are not live here (that is,
    // immediately after the instruction), then we must go in and
    // change the defining reference to that VR to NIL. This is to
    // avoid the multiple-definition-partial-live-range bug that
    // nasty imperative programmers can generate.
    
    // Since this doesn't happen very often, we can probably afford to
    // be rather inefficient. Check all the members of i-def for
    // membership of entry-set, and call zap-defining-references for
    // any that aren't. We'll also remove offending entries from the
    // i-def list, so that everything in both it and i-use is
    // hereafter guaranteed live.   Rehacked AJW 2/3/88.
    
    // Something else that we could do here is to note what used vrs
    // are dead after the instruction, since for somewthing like BIT
    // this could help generate better code.
	  
    let zapped-def :: <boolean> = #f;
    let unzapped-def :: <boolean> = #f;
    let the-op :: <op> = ins-op(sv, ins);
    for-instruction-defs (d in sv at ins)
      // markt, noticed that we eliminate if ANY
      // def was zapped, not when ALL defs are zapped!
      if (instance?(d, <virtual-register>))
	if (get-bit-from-set(live-set.set-thingy-vect,
			     d.virtual-register-id))
	  unzapped-def := #t;
	else
	  set-def(#f);
	  zapped-def := #t;
	end if;
      elseif (instance?(d, <real-register>)) // Allow for PC having a dest
				            // which is a constant (TonyM)
	// this is a very deep hack.
	if (d == arg-count & the-op.op-is-move &
	      ~ r-membr(d, live-set.set-thingy-mask))
	  set-def(#f);
	  zapped-def := #t;
        else
	  unzapped-def := #t;
	end if;
      end if;
    end for-instruction-defs;

    if (zapped-def & ~ unzapped-def & the-op.op-eliminatable)
      // note mustn't zap instructions with no defs at all!
      eliminate-instruction(backend, ins);

    else
      // the op specific preferences
      the-op.op-prefer-fn(backend, ins);

      // Given the use & def sets, do the preferred register
      // calculations. This is easy - prefer all used and defined to
      // be same with a couple of small exceptions - back to
      // virtual/real only here! 19/4/88
  
      // with the new implicit-uses we don't do any preferencing
      // simply because by definition these will be live and hence
      // will clash (cim)

      for-instruction-uses (uze in sv at ins)
	if (instance?(uze, <real-register>))
	    let num = uze.real-register-number;
	    for-instruction-defs (def in sv at ins)
              if (instance?(def, <virtual-register>))
                let prefs = def.virtual-register-colour-pref;
		inc!(prefs[num], 4);
              end if;
            end for-instruction-defs;

        elseif (instance?(uze, <virtual-register>))
	    let uze :: <virtual-register> = uze;
	    let prefs = uze.virtual-register-colour-pref;
	    for-instruction-defs (def in sv at ins)
              if (instance?(def, <real-register>))
		let num = def.real-register-number;
		//avoid preferencing of out regs on
                //sparc, screw up leaf-case
		if (~ is-green | 
                    ~ vars.optimize-leaf-case |
		    num < 8 | num > 11)
		  inc!(prefs[num], 4);
                end if;
              else
		if (vv-prefer)
		  if (instance?(def, <virtual-register>))
		    // make this PC safe (TonyM 27/7/93)
		    push!(def, uze.virtual-register-virtual-pref);
		    push!(uze, def.virtual-register-virtual-pref);
		  end if;
		end if;
	      end if;
            end for-instruction-defs;

        end if;
      end for-instruction-uses;

      // Clash detection time - with bitsets, clash the defines with all
      // the lives except for ins::move in in which the use isn't clashed
      // with the def.

      let live-virtuals = live-set.set-thingy-vect;
      if (the-op.op-is-move)
        with-du (sv at ins)
	  let src = du-uze(1);
	  let dst = du-def(1);
	  if (instance?(dst, <virtual-register>))
	    let dst-id :: <integer> = dst.virtual-register-id;
            do-bit-set (bit-id :: <integer> in live-virtuals)
              let key :: <virtual-register> =
	        element-no-bounds-check(vr-vect, bit-id);
	      unless (key == src)
		virtual-registers-clash(vars, key, bit-id, dst, dst-id);
              end unless;
            end do-bit-set;
          end if;
        end with-du;
      else
        for-instruction-defs (d in sv at ins)
          if (instance?(d, <virtual-register>))
            let this-id = d.virtual-register-id;
            do-bit-set (bit-id in live-virtuals)
	      let key :: <virtual-register> = element-no-bounds-check(vr-vect, bit-id);
	      virtual-registers-clash(vars, key, bit-id, d, this-id);
            end do-bit-set;
          end if;
        end for-instruction-defs;
      end if;

      // Take steps to do something special with SCL. These instructions
      // may be very common, so there's an efficiency gain to be made by
      // assuming they will have little effect on allocation.
      // But there's also a potential necessity to avoid allowing for them
      // in the clash count, because they are SO frequent that they can 
      // cause overflows.
      // We assume here that SCL instructions do not have destroys functions,
      // implicit functions, disallow functions, clash functions, or
      // any definitions, 

      if (the-op.op-is-scl)
        let live-names-set :: <vector-32bit> = live-cache.live-bit-set;

	with-uu (sv at ins)
	  let scl-vrs :: <simple-object-vector> = uu-uze(2);

	  for (u :: <virtual-register> in scl-vrs)
	    add-register-to-live-sets(u, live-set, live-names-set, lr-table, 0);
	  end;
	end with-uu;

        let disallowed-reals :: <integer> = live-set.set-thingy-mask;
        let live-virtuals = live-set.set-thingy-vect;
        do-bit-set (bit-id in live-virtuals)
  	  let key :: <virtual-register> = element-no-bounds-check(vr-vect, bit-id);
          key.virtual-register-available-colours := 
  	    r-set-diff(key.virtual-register-available-colours, disallowed-reals);
        end do-bit-set;
  
        // For SCL instructions, we must annotate the parameters with the 
        // named live registers

        unless (live-cache.cache-up-to-date?)
	  update-live-names-set(backend,
				live-cache.live-bit-set,
				live-set.set-thingy-vect);
	  live-cache.cache-up-to-date? := #t;
	end;

        add-live-registers-for-scl(backend, ins, live-cache);


      else // Not an SCL instruction
	// MJS 04/04/91: Can now describe the case where real registers are
	// corrupted by an instruction, but can still be used as arguments.
	// Useful for JSR-ALIEN.
	// MJS 31/05/91: Now on all machines.
	let destroys-fn =  the-op.op-destroys-fn;
	unless (destroys-fn == nil-fn)
	  let destr-reals :: <integer> = rset-from-list(destroys-fn(backend, ins));
	  let live-virtuals = set-thingy-vect(live-set);
	  do-bit-set (bit-id in live-virtuals)
	    let key :: <virtual-register> = element-no-bounds-check(vr-vect, bit-id);
  	    key.virtual-register-available-colours := 
	      r-set-diff(key.virtual-register-available-colours, destr-reals);
	  end do-bit-set;
        end unless;

        let live-names-set :: <vector-32bit> = live-cache.live-bit-set;

        for-instruction-defs (d in sv at ins)
          remove-register-from-live-sets(d, live-set, live-names-set, loop-weighting);
        end for-instruction-defs;

        for-instruction-uses (u in sv at ins)
	  add-register-to-live-sets(u, live-set, live-names-set, lr-table, loop-weighting);
        end for-instruction-uses;
	  
        let implicit-fn =  the-op.op-implicit-uses;
        unless (implicit-fn == nil-fn)
	  let implicit-uses :: <integer> = implicit-fn(backend, ins);
          add-set-thingy(implicit-uses, live-set);
        end unless;

        let disallow-reals :: <integer> = rset-from-list(the-op.op-disallow-fn(backend, ins));
        let disallowed-reals :: <integer> =
          r-union(live-set.set-thingy-mask, disallow-reals);
        let live-virtuals = live-set.set-thingy-vect;
  	    
        do-bit-set (bit-id in live-virtuals)
  	  let key :: <virtual-register> = element-no-bounds-check(vr-vect, bit-id);
          key.virtual-register-available-colours := 
  	    r-set-diff(key.virtual-register-available-colours, disallowed-reals);
        end do-bit-set;
  
        // Now for the clash fn - this bit will usually be skipped, as
        // few instructions actually have anything to say on this
        // subject.
  
        let clashes :: <list> = the-op.op-clash-fn(backend, ins);
        for (c :: <list> in clashes)	// for each clash set
          let the-reals :: <integer> = empty-rset;
          let filter-from :: <list> = c;
  
          c := #();
          for (x in filter-from)
	    if (instance?(x, <real-register>))
	      the-reals := r-union(the-reals, x.real-register-mask);
	    elseif (instance?(x, <virtual-register>)) push!(x, c);
	    elseif (instance?(x, <abstract-integer>)) #();
	    elseif (instance?(x, <constant-reference>)) #();
            elseif (x ~== #f)
	      // Allow #f because that's how we eliminate defs
	      harp-warning(backend, "Unexpected operand while colouring: %=.\n", x);
	    end if;
          end for;
  
          iterate clash-registers(c :: <list> = c)
	    unless (c.empty?)
	      let x :: <virtual-register> = c.head;
	      x.virtual-register-available-colours :=
		r-set-diff(x.virtual-register-available-colours, the-reals);
	      let c-tail :: <list> = c.tail;
	      for (y :: <virtual-register> in c-tail)
		virtual-registers-clash
		  (vars, x, x.virtual-register-id, y, y.virtual-register-id);
	      end;
	      clash-registers(c-tail);
	    end;
	  end;

        end for;

      end if;

    end if;
  end for-instructions-in-basic-block-backwards;
end;



// define inline method named-arg-spill? (reg :: <virtual-register>) => (nas? :: <boolean>)
//   if (reg.virtual-register-name & reg.virtual-register-colour) #t else #f end;
// end method;

// define inline method named-non-arg-spill? (reg :: <virtual-register>) => (nnas? :: <boolean>)
//   if (reg.virtual-register-name & ~ reg.virtual-register-colour) #t else #f end;
// end method;


// define method named-arg-spills 
//     (vr-vect :: <stretchy-virtual-register-vector>) => (res :: <simple-object-vector>) 
//   as(<simple-object-vector>, choose(named-arg-spill?, vr-vect));
// end method;


// A live-cache of live named registers and vectors of
// source-code-locators

define class <live-cache>(<object>)
  // current live named set
  constant slot live-bit-set :: <vector-32bit>, required-init-keyword: live-set:;

  slot cache-up-to-date? :: <boolean> = #f;

  // previous snapshot of live name set
  slot cache-bit-set :: <vector-32bit> = $empty-bit-set;

  // cached vector of SCLs
  slot cache-vector :: <simple-object-vector>;
end class;

// make a bit-set out of a vector of virtual-registers

define method make-bit-set-from-virtuals
    (backend :: <harp-back-end>, virtuals :: <simple-object-vector>)
 => (bit-set :: <vector-32bit>)
  let bit-set :: <vector-32bit> = make-reg-bit-set(backend);
  for (vr :: <virtual-register> in virtuals)
    set-bit-in-set(bit-set, vr.virtual-register-id);
  end for;
  bit-set
end method;

// make a vector of virtual-registers from a bit-set

define method make-virtuals-from-bit-set
    (backend :: <harp-back-end>, bit-set :: <vector-32bit>)
 => (virtuals :: <simple-object-vector>)
  let vr-vect :: <stretchy-virtual-register-vector> =
    backend.variables.vreg-state.vr-vect;
  let virtuals :: <simple-object-vector> =
    make(<vector>, size: logcount(bit-set));
  let index :: <integer> = 0;

  do-bit-set (bit-id in bit-set)
    without-bounds-checks
      let reg :: <virtual-register> = vr-vect[bit-id];
      virtuals[index] := reg;
      index := index + 1;
    end;
  end do-bit-set;
  virtuals
end method;

define method add-live-registers-for-scl
    (backend :: <harp-back-end>, 
     ins :: <integer>, 
     live-cache :: <live-cache>) => ()

  // This is an SCL instruction, so we must determine the set of live
  // named registers, and insert that set into instruction operands.
  // 
  // We include all arg-spills too, because they are always live, and often useful
  // debugging aids
  //

  //
  // Vectors of live named registers are cached using a very simple scheme;
  // only the last vector is remembered, and this is dropped and updated if
  // there has been any change in liveness since the last time;
  // experiments show the cache to be effective 80% of the time for the Dylan
  // compiler
  //
  // Nosa  Jan 25, 1999

  if (live-cache.cache-bit-set == $empty-bit-set)
    
    with-uu (backend.variables.sv-instructions at ins)
      let live-vrs :: <simple-object-vector> = uu-uze(2);
      live-cache.cache-bit-set := make-bit-set-from-virtuals(backend, live-vrs);
      live-cache.cache-vector := live-vrs;
    end with-uu;

  end if;

  let changed? = bit-set-update(live-cache.cache-bit-set, live-cache.live-bit-set);

  if (changed?)
    live-cache.cache-vector :=
      make-virtuals-from-bit-set(backend, live-cache.live-bit-set);
  end if;

  let all-live-regs :: <simple-object-vector> = live-cache.cache-vector;
      
    
  with-uu (backend.variables.sv-instructions at ins)
    let use1 :: <pair> = uu-uze(1);
    use1.tail := all-live-regs;
  end with-uu;

end method;



// MJS 05/12/92: new stuff for removing unreferenced vregs from *vr-vect*
//               DANGER: the vregs ids are changed
define method compress-vreg-usage 
    (backend :: <harp-back-end>, pgm :: <stretchy-basic-block-vector>)
  let vars = backend.variables;
  let sv :: <instructions-vector> = vars.sv-instructions;
  let state = vars.vreg-state;
  let vrvect = state.vr-vect;
  let nvregs :: <integer> = size(vrvect);
  let used-vregs = make(<simple-object-vector>, size: nvregs, fill: #f);
  for (bb :: <basic-block> in pgm)
    for-instructions-in-basic-block (ins in bb)
      for-instruction-defs (d in sv at ins)
        if (instance?(d, <virtual-register>))
	  used-vregs[d.virtual-register-id] := #t;
        end if;
      end for-instruction-defs;
      for-instruction-uses (u in sv at ins)
        if (instance?(u, <virtual-register>))
	  used-vregs[u.virtual-register-id] := #t;
        end if;
      end for-instruction-uses;
    end for-instructions-in-basic-block;
  end for;
  let out-index :: <integer> = 0;
  for (index :: <integer> from 0, 
       ureg :: <boolean> in used-vregs,
       vreg :: <virtual-register> in vrvect)
    if (ureg)
      unless (index = out-index)
	vrvect[out-index] := vreg;
	vreg.virtual-register-id := out-index;
      end unless;
      inc!(out-index);
    else
      vreg.virtual-register-id := -1;
    end if;
  end for;

  note-vrvect-truncated(state, out-index); //MJS 17May93: now in a function
  state.unique-registers :=
    choose(vreg-dead?, state.unique-registers);
end;

define method vreg-dead? (vreg :: <virtual-register>) => (b :: <boolean>)
  vreg.virtual-register-id >= 0;
end;


// define method emit-clashes-data(vars :: <harp-variables>) => ()
//   for (v :: <virtual-register> in vars.vreg-state.vr-vect)
//     do-virtual-register-clashes(v, v2 in vars)
//       format-out("\n### %= clashes with %=\n",
// 		 v.virtual-register-id,
// 		 v2.virtual-register-id);
//     end;
//   end;
// end method;


