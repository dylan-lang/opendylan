module:    main-harp
Synopsis:  The HARP graph colourer
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// copyright Functional Objects, Inc. 1994

/// Split off the graph colourer, as asm-allocate getting too big.
/// This useful macro does the business on local spill sets when a neighbour
/// has been coloured as spill

/// Added stuff to delay colouring a virtual register which has no preference
/// so that it doesn't use a real register which something lower down in the
/// vr-vect might have a definite preference to use (cim)

define method  mangle-vr-spill-set
     (set :: <spill-set>, colour :: <spill>)
  let spill-num :: <integer> = colour.spill-offset;
  if (spill-num < set.spill-set-limit)
    set.spill-set-scatter := remove!(set.spill-set-scatter, spill-num);
  else
    set.spill-set-scatter :=
       conc!(set.spill-set-scatter, seq(set.spill-set-limit, spill-num));
    set.spill-set-limit := 1 + spill-num;
  end if;
end;



define method conc! (x :: <list>, y :: <list>) => (res :: <list>)
  if (x == #())
    y;
  else
    iterate zap-end (p :: <list> = x)
      let next :: <list> = p.tail;
      if (next == #())
        p.tail := y;
      else zap-end(next);
      end if;
    end iterate;
    x;
  end if;
end method;


define method seq 
     (incl-lower :: <integer>, excl-upper :: <integer>) => (l :: <list>)
  let lst :: <list> = #();
  for (count from excl-upper - 1 to incl-lower by -1)
    push!(count, lst);
  end for;
  lst
end;
      
define method select-spill (vr :: <virtual-register>) => (s :: <spill>)
  let ss :: <spill-set> = vr.virtual-register-spill-set;
  if (ss.spill-set-scatter == #())
    advance-mark(vr);
  else
    let holder = ss.spill-set-central.central-spill-holder;
    let scatter :: <integer> = list-pop!(ss.spill-set-scatter);
    holder[scatter];
  end if;
end;

define method select-spill-by-number
     (vr :: <virtual-register>, number :: <integer>) => (s :: <spill>)
  let ss :: <spill-set> = vr.virtual-register-spill-set;
  let central = ss.spill-set-central;
  let holder = central.central-spill-holder;
  let scatter = ss.spill-set-scatter;
  if (number >= ss.spill-set-limit)
    for (spill :: <spill> = advance-mark(vr) then advance-mark(vr),
         until: (spill.spill-offset == number))
       ss.spill-set-scatter := concatenate(scatter, list(spill));
    end for;
  elseif (member?(number, scatter))
    ss.spill-set-scatter := remove!(scatter, number);
  else
    // It looks as though someone else already has this spill
    harp-error("Failed to select spill by number");
  end if;
  holder[number];
end;


define method colour-graph
    (backend :: <harp-back-end>, lr-table :: <vector-32bit>)
  let ranges :: <integer> = logcount(lr-table);
  let state = backend.variables.vreg-state;
  if (ranges == 0)
    state.next-gc-spill := 0;
    state.next-ng-spill := 0;
    state.next-sf-spill := 0;
    state.next-df-spill := 0;
  else
    really-colour-graph(backend, lr-table, ranges);

    // Now splatter the central spill registries to be instead the number of
    // spills, like what they used to be, and hence compatible with bits of the
    // backend.

    state.next-gc-spill := size(state.gc-spill-central.central-spill-holder);
    state.next-ng-spill := size(state.ng-spill-central.central-spill-holder);
    state.next-sf-spill := size(state.sf-spill-central.central-spill-holder);
    state.next-df-spill := size(state.df-spill-central.central-spill-holder);
  end if;
end;


/// added the pref-all vector which is added to the preferences of the per
/// register pref-vector - it is currently used to ensure that as few preserved
/// registers as possible are used (cim 17/3/89) and that non-preserved
/// register are used in preference to preserved if poss.

/// we set things up so as to achieve the affect that preserved registers are
/// initially only used on account of lack of available non-preserved colours.
/// Once a preserved has been allocated then it should be be used in preference
/// to other preserved registers (except maybe on the sparc where it might not
/// make any difference ?) but still a non-preserved register should be used
/// in preference if available and the vr in question has nothing to say simply
/// because preserved registers are valued commodities (cim)

/// MJS 08/02/93: changed pref-all adjustment for preserved regs to be -2
/// initially and to become -1 when first allocated.  This prevents preserved
/// regs being used after first preserved reg is allocated if a non-preserved
/// reg will do.  It used to work like this, but rs6k has preserved regs
/// numbered backwards, so > was changed to >= in the preferencing code.


define variable delay-colouring :: <boolean> = #t;
// "whether we should bother trying to delay colouring vrs with no preference"

define method real-preserved-mask 
    (backend :: <harp-back-end>, reg :: <real-register>) => (mask :: <integer>)
  if (backend.variables.compiling-call-in)
    reg.real-register-c-preserved-mask
  else reg.real-register-preserved-mask
  end if;
end method;

define method init-pref-vector
    (backend :: <harp-back-end>, pref-all :: <simple-integer-vector>)
  let regs = backend.registers;
  let r-r-vector = regs.real-register-vector;
  for (index from 0 below size(r-r-vector),
       reg in r-r-vector)
    unless (real-preserved-mask(backend, reg) == 0)
      pref-all[index] := -2; //MJS 08/02/93: was -1 (see above)
    end unless;
  end for;
end;

define method really-colour-graph
    (backend :: <harp-back-end>, 
     lr-table :: <vector-32bit>, ranges :: <integer>)
  let state = backend.variables.vreg-state;
  let cache = state.vr-vect;
  let regs = backend.registers;
  let clashes :: <sort-groups> = make(<sort-groups>);
  let clashes2 :: <sort-groups> = clashes;

  do-bit-set (id :: <integer> in lr-table)
    let key :: <virtual-register> = element-no-bounds-check(cache, id);
    let (grps, grps2) =
      insert-virtual-register-by-block-clash(clashes, clashes2, key);
    clashes := grps; clashes2 := grps2;
  end do-bit-set;

  let vrc-vect :: <simple-virtual-register-vector> =
     make(<simple-virtual-register-vector>, 
	  size: ranges, fill: invalid-virtual-register());

  fill-vector-from-sort-groups(clashes, vrc-vect);

  let pref-all :: <simple-integer-vector> = make-pref-vector(backend);
  init-pref-vector(backend, pref-all);

  let i :: <integer> = 0;
  let j :: <integer> = 1;
  let force-colour :: <boolean> = #f;
  until (i = ranges)
    let vr :: <virtual-register> = element-no-bounds-check(vrc-vect, i);
    let old-colour = vr.virtual-register-colour;
    if (old-colour & ~ instance?(old-colour, <integer>))
      i := 1 + i;
      j := 1 + i;
    else
      let avail-colours :: <integer> = vr.virtual-register-available-colours;
      let number-avail :: <integer> = logcount(avail-colours);
      if (number-avail == 0)
        let selected-colour :: <spill> = 
          select-spill-by-colour(backend, vr, old-colour);

	// Colour as spill

	vr.virtual-register-colour := selected-colour;

	let central = vr.virtual-register-spill-set.spill-set-central;
	let vars = backend.variables;
	do-virtual-register-clashes (vr, key in vars)
	  unless (instance?(old-colour, <integer>) & (old-colour >= 0))
	    let key-spill-set = key.virtual-register-spill-set;
	    let key-central = key-spill-set.spill-set-central;
	    if (key-central == central)
	      mangle-vr-spill-set(key-spill-set, selected-colour);
	    end if;
	  end unless;
	  unset-bit-in-set(vars.virtual-register-clashes, $num$);
        end do-virtual-register-clashes;

	// note that this is not a psetq
	i := 1 + i;
        j := 1 + i;

      else

        // Otherwise looks like we can select a real register. Do
	// the preferencing to find out which one.
		   
	let r-r-vector :: <simple-object-vector> = regs.real-register-vector;
	let selected-reg :: <real-register> = r-r-vector[0]; // dummy init
	let selected-prf :: <integer> = -1000;
	let pref-vector :: <simple-integer-vector> = 
            vr.virtual-register-colour-pref;
		       
        for (this-reg :: <real-register> in r-r-vector, 
             pref :: <integer> in pref-vector)
          let this-prf :: <integer> = pref + pref + pref;
          if (r-membr(this-reg, avail-colours) & this-prf >= selected-prf)
	    selected-reg := this-reg;
	    selected-prf := this-prf;
          end if;
        end for;

        if (vv-prefer & selected-prf <= 0)
	  for (vv :: <virtual-register> in vr.virtual-register-virtual-pref)
            let col = vv.virtual-register-colour;
	    if (col)
	      if (instance?(col, <real-register>))
		let inx :: <integer> = col.real-register-number;
		inc!(pref-vector[inx]);
	      end if;
	    end if;
          end for;
          for (this-reg :: <real-register> in r-r-vector,
               half-pref :: <integer> in pref-vector,
               this-all :: <integer> in pref-all)
	    let this-prf :: <integer> = 
                half-pref + half-pref + half-pref + this-all;
	    if (this-prf >= selected-prf & r-membr(this-reg, avail-colours))
	      selected-reg := this-reg;
	      selected-prf := this-prf;
            end if;
          end for;
        end if;
        
        if (delay-colouring & (~  force-colour) & selected-prf <= 0)
			
	  without-bounds-checks

	  if (number-avail > (j - i) & ~ (j == ranges))
	      let vri :: <virtual-register> = vrc-vect[i];
	      let vrj :: <virtual-register> = vrc-vect[j];
	      vrc-vect[i] := vrj;
              vrc-vect[j] := vri;
              j := 1 + j;
          else

	    // here we have decided to give up because of no
	    // positive preferencing. we could just colour the
	    // top vr but this could (and has) caused worst
	    // code to be generated. so instead we restore the
	    // ordering to the original and force the orig top
	    // vr to be coloured next time round

            for (k :: <integer> from 1 + i below j)
	      vrc-vect[k - 1] := vrc-vect[k];
            finally
	      vrc-vect[k - 1] := vr;
	      force-colour := #t;
            end for;
          end if;

	end without-bounds-checks;

        else
	  // And do the actual colouring
	  // if we're colouring a floating point vr then make
	  // sure we choose the correct real reg ie single or double

	  vr.virtual-register-colour :=
            if (instance?(vr, <dfreg>))
	      the-real-dfreg(backend, selected-reg);
            else
	      selected-reg;
            end if;
		       
	  let colour-mask :: <integer> = selected-reg.real-register-mask;
	  state.allocated-reals := r-union(state.allocated-reals, colour-mask);

          unless (real-preserved-mask(backend, selected-reg) == 0)
	    pref-all[selected-reg.real-register-number] := -1;
            //MJS 08/02/93: was 0 (see above)
          end unless;
	  let vars = backend.variables;
	  do-virtual-register-clashes (vr, key in vars)
	    key.virtual-register-available-colours :=
	      r-set-diff(key.virtual-register-available-colours, colour-mask);
	    unset-bit-in-set(vars.virtual-register-clashes, $num$);
	  end do-virtual-register-clashes;

	  i := 1 + i;
	  j := 1 + i;
          force-colour := #f;

        end if;
      end if;
    end if;
  end until;
end;


define open generic select-spill-by-colour 
    (backend :: <harp-back-end>, vr :: <virtual-register>, 
     old-colour)
    => (spill :: <spill>);

define method select-spill-by-colour 
    (backend :: <harp-back-end>, vr :: <virtual-register>, 
     old-colour :: <integer>)
    => (spill :: <spill>)
  make-arg-spill(vr, old-colour);
end method;

define method select-spill-by-colour 
    (backend :: <harp-back-end>, vr :: <virtual-register>, 
     old-colour)
    => (spill :: <spill>)
  select-spill(vr);
end method;



define open generic sort-by-block-clashes
    (backend :: <harp-back-end>, vrc-vect :: <simple-virtual-register-vector>)
    => (sorted-vect :: <simple-object-vector>);


define method sort-by-block-clashes
    (backend :: <harp-back-end>, vrc-vect :: <simple-virtual-register-vector>)
    => (sorted-vect :: <simple-object-vector>)
  sort(vrc-vect, test: compare-block-clashes);
end method;

/* Introduce sort-groups for virtual-register-clashes to reduce sorting
   overhead by a factor of 6; often virtual-registers share the same 
   clash-count; registers are inserted into their groupings by clash-count
   and then copied in place into a vector in a post pass

   There is a 50-50 chance that the group last visited will be visited
   again immediately after, so pass & return that group as well

   Nosa  Jan 25, 1999 */

define abstract class <abstract-sort-groups>(<object>)
end;

define class <empty-sort-groups>(<abstract-sort-groups>)
end;

define constant $empty-sort-groups = make(<empty-sort-groups>);

define class <sort-groups>(<abstract-sort-groups>)
  slot sort-group-id :: <integer> = -1;
  slot sort-group :: <list> = #();
  slot sort-groups :: <abstract-sort-groups> = $empty-sort-groups;
end;

// TO-DO: make this generic so that other harp-back-ends can specialize

define inline method insert-virtual-register-by-block-clash
    (groups :: <sort-groups>, current-groups :: <sort-groups>,
     vr :: <virtual-register>)
 => (groups :: <sort-groups>, current-groups :: <sort-groups>)

  let vr-clash :: <integer> = vr.virtual-register-clash-count;

  if (vr-clash = current-groups.sort-group-id)
    current-groups.sort-group := pair(vr, current-groups.sort-group);
    values(groups, current-groups);
  else

  iterate insert-element
    (grps :: <sort-groups> = groups, prev-grps :: <sort-groups> = groups)
    let grps-id :: <integer> = grps.sort-group-id;
    if (grps-id = -1)
      grps.sort-group-id := vr-clash;
      grps.sort-group := pair(vr, #());
      grps.sort-groups := make(<sort-groups>);
      values(groups, grps);
    elseif (grps-id = vr-clash)
      grps.sort-group := pair(vr, grps.sort-group);
      values(groups, grps);
    elseif (vr-clash > grps-id)
      let new-grps = make(<sort-groups>);
      new-grps.sort-group-id := vr-clash;
      new-grps.sort-group := pair(vr, #());
      new-grps.sort-groups := grps;
      if (grps == prev-grps)
	values(new-grps, new-grps);
      else
	prev-grps.sort-groups := new-grps;
	values(groups, new-grps);
      end;
    else
      let next-grps :: <sort-groups> = grps.sort-groups;
      insert-element(next-grps, grps);
    end;
  end;

  end;
end method;


define method fill-vector-from-sort-groups
    (groups :: <sort-groups>, vect :: <simple-virtual-register-vector>)
  let index :: <integer> = 0;
  iterate fill-vector(grps :: <sort-groups> = groups)
    let grps-id :: <integer> = grps.sort-group-id;
    unless (grps-id = -1)
      for (e in grps.sort-group)
	without-bounds-checks
	  vect[index] := e;
	  index := index + 1;
        end;
      end;
      let next-grps :: <sort-groups> = grps.sort-groups;
      fill-vector(next-grps);
    end;
  end;
end method;

// define method print-sort-groups
//     (groups :: <sort-groups>) => (p :: <list>)
//   let p :: <list> = #();
//   iterate print(grps :: <sort-groups> = groups)
//     let grps-id :: <integer> = grps.sort-group-id;
//     if (grps-id = -1) reverse(p)
//     else
//       p := pair(pair(grps.sort-group-id, list(grps.sort-group)), p);
//       let next-grps :: <sort-groups> = grps.sort-groups;
//       print(next-grps);
//     end;
//   end;
// end;


define method compare-block-clashes
    (x :: <virtual-register>, y :: <virtual-register>) => (b :: <boolean>)
  x.virtual-register-clash-count > y.virtual-register-clash-count;
end;



define open generic the-real-dfreg (be :: <harp-back-end>, reg) => (reg);

define method the-real-dfreg (be :: <harp-back-end>, reg) => (reg)
  reg;
end method;
