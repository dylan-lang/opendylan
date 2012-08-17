module:    x86-harp
Synopsis:  Pentium FP branch instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




define constant mc-fcomp = #b011000;   // compare top with mem and pop

/*

define method compare-no-pop (be :: <harp-x86-back-end>)
  emit(be, #b11011000);
  emit(be, #b11010001);
  make-flags-avail(be);
end method;


define method compare-2-pop (be :: <harp-x86-back-end>)
  emit(be, #b11011110);
  emit(be, #b11011001);
  make-flags-avail(be);
end method;
*/


with-ops-in pentium-instructions (fbeq, dbeq) info := bhi-x end; 
        // see later frig with flags

with-ops-in pentium-instructions (fbne, dbne) info := bne-x end;

with-ops-in pentium-instructions (fblt, dblt) info := blo-x end;
	// this really is signed -
	// see 4-14 of the 387 manual

with-ops-in pentium-instructions (fbge, dbge) info := bhs-x end;

with-ops-in pentium-instructions (fble, dble) info := bls-x end;
 	// ... ditto for bls



/// We assume no canonicalisation here, as for clipper branches.

/// the argument frig to make-flags-avail allows aribitary flipping of bits
/// within the status word before doing the conditional branch. This is used so
/// that both equality and "orderedness" can be tested in one instruction

define method make-flags-avail 
    (be :: <harp-x86-back-end>, #key frig)
  emit(be, flt-esc + #b111, #b11100000);	// fstsw ax
  if (frig)
    harp-out (be) eor(be, eax, eax, frig) end;
  end if;
  emit(be, #x9e);				// sahf
end method;


define pentium-template (fbeq)
  options (self);
  pattern (be, i :: <integer> by op-info, d, s1, s2 :: <sf-ic/spill-ref> by colour)
    push-single(be, s1);
    emit(be, flt-esc + mf-single);
    emit-f-c-spill-operand(be, s2, mc-fcomp);
    make-flags-avail(be, frig: #x4000);		// flip the zero flag
    emit-branch-sdi(be, i, d);
end pentium-template;


define pentium-template (fbne, fble, fblt, fbge)
  options (self);
  pattern (be, i :: <integer> by op-info, d, s1, s2 :: <sf-ic/spill-ref> by colour)
    push-single(be, s1);
    emit(be, flt-esc + mf-single);
    emit-f-c-spill-operand(be, s2, mc-fcomp);
    make-flags-avail(be);
    emit-branch-sdi(be, i, d);
end pentium-template;


define pentium-template (dbeq)
  options (self);
  pattern (be, i :: <integer> by op-info, d, s1, s2 :: <df-ic/spill-ref> by colour)
    push-double(be, s1);
    emit(be, flt-esc + mf-double);
    emit-f-c-spill-operand(be, s2, mc-fcomp);
    make-flags-avail(be, frig: #x4000);		// flip the zero flag
    emit-branch-sdi(be, i, d);
end pentium-template;


define pentium-template (dbne, dble, dblt, dbge)
  options (self);
  pattern (be, i :: <integer> by op-info, d, s1, s2 :: <df-ic/spill-ref> by colour)
    push-double(be, s1);
    emit(be, flt-esc + mf-double);
    emit-f-c-spill-operand(be, s2, mc-fcomp);
    make-flags-avail(be);
    emit-branch-sdi(be, i, d);
end pentium-template;



