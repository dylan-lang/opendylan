module:    powerpc-harp
Synopsis:  PowerPC move instructions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define powerpc-template move

   // rr/c is an ORIL with 0 (good as anything else).

  pattern (be, d, s is d)  // discard the eliminatable case
    #f;

  // the general case is simple use of emit-make-reg/dst-place/dst-move
  pattern (be, d, s)
    // if s not a reg, may need tmp1
    let tmp-reg = m-ref(s) | reg--tmp1;
    // dst will be d or s if either is a reg
    let dst-reg = dst-place(d, tmp-reg);
    // ditto for src
    let src-reg = emit-make-reg(be, s, dst-reg);

    unless (src-reg == dst-reg)
      emit-d-via-tmp1-dest2(be, ori-op, dst-reg, src-reg, 0)
    end;
    dst-move(be, d, dst-reg)

end powerpc-template;


/// CONDITIONAL-MOVE
/// Use the LWARX, STWCX! instructions to guarantee atomicity
/// even with multiprocessors


with-ops-in powerpc-instructions (conditional-move)
  disallow-fn := tmp345-fn;
end;

define powerpc-template (conditional-move)

  /// For this instruction, constant references are treated as direct
  /// (address) constant references even if they are actually 
  /// indirect. This is designed to make life easier for harp-cg
  /// to avoid having to allocate a new direct constant.

  pattern (be, tag, var :: <i-constant-reference>, new-val, comp)
    let var = emit-make-reg-direct(be, var, reg--tmp1);
    harp-out (be) 
      conditional-move(be, tag, var, new-val, comp);
    end;

  pattern (be, tag, var, new-val, comp)
    let var = emit-make-reg(be, var, reg--tmp1);
    let comp = emit-make-reg(be, comp, reg--tmp2);
    let new-val = emit-make-reg(be, new-val, reg--tmp3);

    emit-d(be, addi-op, reg--tmp5, r0, 0);

    // LOOP
    emit-x(be, lwarx-op, reg--tmp4, r0, var);
    emit-drr(be, cmp-op, 0, comp, reg--tmp4);
    emit-branch(be, bc-op, beq-cc, 3 * 4);

    emit-d(be, addi-op, reg--tmp5, r0, 1);
    emit-branch(be, bc-op, bra-cc, 3 * 4);

    emit-x(be, stwcx!-op, new-val, r0, var);

    // TEST
    emit-branch(be, bc-op, bne-cc, -6 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp5, 0);
    emit-branch-sdi(be, bne-cc, tag);

end powerpc-template;




define method op--store-thread-local
    (be :: <linux-powerpc-back-end>, data, offset :: <integer>) => ()
  harp-out(be)
    st(be, data, reg--teb, offset);
  end harp-out;
end method;

define method op--load-thread-local
    (be :: <linux-powerpc-back-end>, dest :: <register>, offset :: <integer>) => ()
  harp-out(be)
    ld(be, dest, reg--teb, offset);
  end harp-out;
end method;

define method op--tlb-base-register 
    (be :: <powerpc-back-end>, dest :: <register>) => ()
  op--load-thread-local(be, dest, #x14);
end method;



/// Now the templates


with-ops-in powerpc-instructions (get-teb, set-teb) info := #x14 end;
with-ops-in powerpc-instructions (get-seh, set-seh) info := 0 end;


define powerpc-template (get-teb, get-seh)
  options (self);

  pattern (be, index :: <integer> by op-info, dest :: <real-register> by colour)
    op--load-thread-local(be, dest, index);

  pattern (be, index :: <integer> by op-info, dest)
    op--load-thread-local(be, reg--tmp1, index);
    harp-out (be) move(be, dest, reg--tmp1) end;
end powerpc-template;


define powerpc-template (set-teb, set-seh)
  options (self);

  pattern (be, index :: <integer> by op-info, data :: <spill> by colour)
    harp-out (be) move(be, reg--tmp1, data) end;
    op--store-thread-local(be, reg--tmp1, index);

  pattern (be, index :: <integer> by op-info, data)
    op--store-thread-local(be, data, index);

end powerpc-template;



define powerpc-template (ld-teb)

  pattern (be, dest, index :: <integer> of signed-16bit-const-ref)
    op--tlb-base-register(be, reg--tmp1);
    emit-d-via-tmp1-dest1(be, lwz-op, dest, reg--tmp1, index)

end powerpc-template;


define powerpc-template (ld-teb-address)

  pattern (be, dest, index :: <integer> of signed-16bit-const-ref)
    op--tlb-base-register(be, reg--tmp1);
    unless (index == 0)
      emit-d-via-tmp1-dest1(be, addi-op, dest, reg--tmp1, index);
    end;

end powerpc-template;


define powerpc-template (st-teb)

  pattern (be, data, index :: <integer> of signed-16bit-const-ref)
    op--tlb-base-register(be, reg--tmp1);
    let data = emit-make-reg(be, data, reg--tmp2);
    emit-d(be, stw-op, data, reg--tmp1, index);

end powerpc-template;
