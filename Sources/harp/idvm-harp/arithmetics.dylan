module:    idvm-harp
Synopsis:  The IDVM arithmetic operations
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The arithmetic templates sort out some basic canonicalisation
/// and then invoke the local canon-op template which is closely matched
/// to the IDVM. Canonicalisation is in 2 forms - for commutative
/// and non-commutative OPs.


with-ops-in idvm-instructions (add)
  info := vector(emit-res-loc-add,
                 emit-res-lit-add,
                 emit-loc-add,
                 emit-lit-add);
end with-ops-in;

with-ops-in idvm-instructions (sub)
  info := vector(emit-res-loc-sub,
                 emit-res-lit-sub,
                 emit-loc-sub,
                 emit-lit-sub);
end with-ops-in;

/* Although the following are tentatively in the IDVM instruction set 
   there are no harp instructions to access them yet.  The harp style 
   is to use conditional branches.

with-ops-in idvm-instructions (lt)
  info := vector(emit-res-loc-lt,
                 emit-res-lit-lt,
                 emit-loc-lt,
                 emit-lit-lt);
end with-ops-in;

with-ops-in idvm-instructions (gt)
  info := vector(emit-res-loc-gt,
                 emit-res-lit-gt,
                 emit-loc-gt,
                 emit-lit-gt);
end with-ops-in;

with-ops-in idvm-instructions (le)
  info := vector(emit-res-loc-le,
                 emit-res-lit-le,
                 emit-loc-le,
                 emit-lit-le);
end with-ops-in;

with-ops-in idvm-instructions (ge)
  info := vector(emit-res-loc-ge,
                 emit-res-lit-ge,
                 emit-loc-ge,
                 emit-lit-ge);
end with-ops-in;

with-ops-in idvm-instructions (eq)
  info := vector(emit-res-loc-eq,
                 emit-res-lit-eq,
                 emit-loc-eq,
                 emit-lit-eq);
end with-ops-in;

with-ops-in idvm-instructions (ne)
  info := vector(emit-res-loc-ne,
                 emit-res-lit-ne,
                 emit-loc-ne,
                 emit-lit-ne);
end with-ops-in;

with-ops-in idvm-instructions (ideq)
  info := vector(emit-res-loc-ideq,
                 emit-res-lit-ideq,
                 emit-loc-ideq,
                 emit-lit-ideq);
end with-ops-in;

with-ops-in idvm-instructions (idne)
  info := vector(emit-res-loc-id,
                 emit-res-lit-id,
                 emit-loc-id,
                 emit-lit-id);
end with-ops-in;
*/


/// First, the commutative instructions
// 
// Clash S1 and S2 both res

with-ops-in idvm-instructions  (add /* , eq, ne, ideq, idne */)
  destroys-fn := res-fn;
  // Don't know how to clash S! and S2 against res. !@#$
  clash-fn :=  idvm-method (duu)
                 list(list(uze(2), res));
               end idvm-method;
end with-ops-in;

define idvm-template (add /* , eq, ne, ideq, idne */)
  options(self);

  // Move the destination
  pattern (be, i, d by not-res-ref, s1, s2)
    harp-reapply(be, i, res, s1, s2);
    harp-out (be) move(be, d, res) end;

  // Reverse the operands if the second is result
  pattern (be, i, d by res-ref, s1, s2 by res-ref)
    harp-reapply(be, i, d, s2, s1);

  // Unless S1 is a literal, we can use the local template now
  pattern (be, emitters by op-info, d by res-ref, s1 by m/spill-ref, s2)
    call-local(canon-op, be, emitters, s1, s2);
    harp-out (be) move(be, d, res) end;

  // If S1 is a literal and S2 is not, then reverse
  pattern (be, i, d by res-ref, s1, s2 by m/spill-ref)
    harp-reapply(be, i, d, s2, s1);

  // If S1 and S2 are both literals, then move S1 into result first
  pattern (be, i, d by res-ref, s1, s2)
    harp-out (be) move(be, res, s1) end;
    harp-reapply(be, i, d, res, s2);

end idvm-template;



/// Now the non-commutative
//
// Disallow S2 from being result.

with-ops-in idvm-instructions  (sub /* , gt, lt, ge, le */)
  destroys-fn := res-fn;
  clash-fn :=  idvm-method (duu)
                 list(list(uze(2), res));
               end idvm-method;
end with-ops-in;

define idvm-template (sub /* , gt, lt, ge, le */)
  options(self);

  // Move the destination
  pattern (be, i, d by not-res-ref, s1, s2)
    harp-reapply(be, i, res, s1, s2);
    harp-out (be) move(be, d, res) end;

  // Unless S1 is a literal, we can use the local template now
  pattern (be, emitters by op-info, d by res-ref, s1 by m/spill-ref, s2)
    call-local(canon-op, be, emitters, s1, s2);
    harp-out (be) move(be, d, res) end;

  // If S1 is a literal, then move it into result first
  pattern (be, i, d by res-ref, s1, s2)
    harp-out (be) move(be, res, s1) end;
    harp-reapply(be, i, d, res, s2);

end idvm-template;


