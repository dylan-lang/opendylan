Module:    idvm-harp
Language:  infix-dylan
Synopsis:  IDVM branch instructions
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/* For derinitions of vector indices see operations.dylan:

define constant idvm-harp-res-loc = 0;
define constant idvm-harp-res-lit = 1;
define constant idvm-harp-loc-loc = 2;
define constant idvm-harp-loc-lit = 3;
*/

with-ops-in idvm-instructions (blt)
    info := vector(emit-res-loc-br-lt,
                   emit-res-lit-br-lt,
                   emit-loc-br-lt,
                   emit-lit-br-lt)
end;
with-ops-in idvm-instructions (bgt)
    info := vector(emit-res-loc-br-gt,
                   emit-res-lit-br-gt,
                   emit-loc-br-gt,
                   emit-lit-br-gt)
end;
with-ops-in idvm-instructions (ble)
    info := vector(emit-res-loc-br-le,
                   emit-res-lit-br-le,
                   emit-loc-br-le,
                   emit-lit-br-le)
end;
with-ops-in idvm-instructions (bge)
    info := vector(emit-res-loc-br-ge,
                   emit-res-lit-br-ge,
                   emit-loc-br-ge,
                   emit-lit-br-ge)
end;
with-ops-in idvm-instructions (beq)
    info := vector(emit-res-loc-br-ideq,
                   emit-res-lit-br-ideq,
                   emit-loc-br-ideq,
                   emit-lit-br-ideq)
end;
with-ops-in idvm-instructions (bne)
    info := vector(emit-res-loc-br-idne,
                   emit-res-lit-br-idne,
                   emit-loc-br-idne,
                   emit-lit-br-idne)
end;


/*

with-ops-in idvm-instructions (bequal)
    info := vector(emit-res-loc-br-eq,
                   emit-res-lit-br-eq,
                   emit-loc-br-eq,
                   emit-lit-br-eq)
end;
with-ops-in idvm-instructions (bnequal)
    info := vector(emit-res-loc-br-ne,
                   emit-res-lit-br-ne,
                   emit-loc-br-ne,
                   emit-lit-br-ne)
end;

*/



// Disallow S2 from being result.

with-ops-in idvm-instructions (bgt, blt, bge, ble, beq, bne 
                               /*, bequal, bnequal */)
  destroys-fn := res-fn;
  clash-fn :=  idvm-method (uu)
                 list(list(uze(2), res));
               end idvm-method;
end with-ops-in;

define idvm-template (bgt, blt, bge, ble, beq, bne /*, bequal, bnequal */)
  options(self);

  // Unless S1 is a literal, we can use the local template now
  pattern (be, emitters by op-info, tag, s1 by m/spill-ref, s2)
    call-local(canon-branch, be, emitters, tag, s1, s2);

  // If S1 is a literal, then move it into result first
  pattern (be,  emitters by op-info, tag, s1, s2)
    harp-out (be) move(be, res, s1) end;
    call-local(canon-branch, be, emitters, tag, res, s2);

end idvm-template;



/* jump-true & jump-false !@#$$ */


define idvm-template bra
  pattern (be, tag)
    emit-jump(be, tag);
end idvm-template;

