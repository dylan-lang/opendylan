module:    powerpc-harp
Synopsis:  PowerPC load/store instructions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Note: the order of the two address-specifying operands has been
/// canonicalised: r>s>c

/// The RS6000 has thoughtfully provided indexed forms of the load and store
/// instructions for byte, half and word forms. These forms add the contents of 
/// two registers together to form an address, which is useful for the general
/// case of loads and stores.  Hence we pass two possible opcodes to our local
/// templates for load and store: the first is for addressing with base reg
/// and 16 bit signed offset, the second is for addressing with 2 registers.


with-ops-in powerpc-instructions (st, sth, stb)
  disallow-fn := tmp3-fn;
end;


define powerpc-template ld

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-ld), d, r, s, lwz-op, lwzx-op);

end powerpc-template;


define powerpc-template ldh

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-ld), d, r, s, lhz-op, lhzx-op);

end powerpc-template;

define powerpc-template ldh-signed

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-ld), d, r, s, lha-op, lhax-op);

end powerpc-template;


define powerpc-template ldb

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-ld), d, r, s, lbz-op, lbzx-op);

end powerpc-template;

define powerpc-template ldb-signed

  pattern (be, d, r, s)
    harp-out(be)
      ldh-signed(be, d, r, s);
      asr(be, d, d, 8);
    end;

end powerpc-template;


define powerpc-template st

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-st), d, r, s, stw-op, stwx-op);

end powerpc-template;


define powerpc-template sth

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-st), d, r, s, sth-op, sthx-op);

end powerpc-template;

define powerpc-template stb

  pattern (be, d, r, s)
    canon(be, local-fn(ppc-st), d, r, s, stb-op, stbx-op);

end powerpc-template;


define method canon
    (be :: <powerpc-back-end>, i :: <function>, d, s, m, l, lx)
  if (const-ref(s)
      | (address-constant-ref(s) & ~ const-ref(m))
      | (ic/spill-ref(s) & m-ref(m)))
    i(be, d, m, s, l, lx);
  else
    i(be, d, s, m, l, lx)
  end
end method canon;


define local-powerpc-template ppc-ld
  // xrc (canonicalised xcr)
  pattern (be, d, b, o :: <integer> of signed-16bit-const-ref, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp1);
    emit-d-via-tmp1-dest1(be, ins, d, bb, o)

  // Put a trap for the constant-constant case here
  pattern (be, d, r :: <integer>, s :: <integer>, ins, index-ins)
    let bb = emit-make-reg(be, r + s, reg--tmp1);
    emit-d-via-tmp1-dest1(be, ins, d, bb, 0)

  pattern (be, d, b, o, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp1);
    let oo = emit-make-reg(be, o, reg--tmp2);
    emit-x-via-tmp1-dest1(be, index-ins, d, bb, oo)

end local-powerpc-template;


define local-powerpc-template ppc-st

  pattern (be, s, b, o :: <integer> of signed-16bit-const-ref, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp2);
    let ss = emit-make-reg(be, s, reg--tmp1);
    emit-d(be, ins, ss, bb, o)

  pattern (be, s, b, o, ins, index-ins)
    let bb = emit-make-reg(be, b, reg--tmp3);
    let oo = emit-make-reg(be, o, reg--tmp2);
    let ss = emit-make-reg(be, s, reg--tmp1);
    emit-x(be, index-ins, ss, bb, oo)

end local-powerpc-template;


