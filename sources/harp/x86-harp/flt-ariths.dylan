module:    x86-harp
Synopsis:  Pentium FP arithmetic instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Basic FP instructions


with-ops-in pentium-instructions (fadd, dadd) info := #b000000 end;
with-ops-in pentium-instructions (fsub, dsub) info := #b100000 end;
with-ops-in pentium-instructions (fmul, dmul) info := #b001000 end;
with-ops-in pentium-instructions (fdiv, ddiv) info := #b110000 end;


define pentium-template (fadd, fsub, fmul, fdiv)  // it is known that all floats are spilt
  options (self);

  pattern (be, i :: <integer> by op-info, d, s1 :: <sf-ic/spill-ref> by colour, s2 :: <sf-ic/spill-ref> by colour)
    push-single(be, s1);
    emit(be, flt-esc + mf-single);
    emit-f-c-spill-operand(be, s2, i);
    pop-single(be, d);

  // permit the first argument to be an integer, giving implicit conversion.
  // This is used for negate.
  pattern (be, i :: <integer> by op-info, d, s1 :: <integer>, s2 :: <sf-ic/spill-ref> by colour)
    push-integer(be, s1);
    emit(be, flt-esc + mf-single);
    emit-f-c-spill-operand(be, s2, i);
    pop-single(be, d);

end pentium-template;


define pentium-template (dadd, dsub, dmul, ddiv)  // it is known that all floats are spilt
  options (self);

  pattern (be, i :: <integer> by op-info, d, s1 :: <df-ic/spill-ref> by colour, s2 :: <df-ic/spill-ref> by colour)
    push-double(be, s1);
    emit(be, flt-esc + mf-double);
    emit-f-c-spill-operand(be, s2, i);
    pop-double(be, d);

  // permit the first argument to be an integer, giving implicit conversion.
  // This is used for negate.
  pattern (be, i :: <integer> by op-info, d, s1 :: <integer>, s2 :: <df-ic/spill-ref> by colour)
    push-integer(be, s1);
    emit(be, flt-esc + mf-double);
    emit-f-c-spill-operand(be, s2, i);
    pop-double(be, d);

end pentium-template;




