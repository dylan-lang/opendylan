module:    native-instructions
Synopsis:  The definition of the native HARP instruction set.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// This is the "standard" instruction set, for use with all native 
/// back ends.




define instruction-set <instruction-set>
     (<core-instruction-set>, <harp-native-back-end>)
  create default-instructions, inheriting: default-core-instructions;

  none op allocate-local-area, allocate-raw-area,
           preserve-registers-entry, preserve-registers-exit;
  uuu  op st, sth, stb, fill-words, copy-words-up, copy-words-down,
           fill-words-w, copy-words-down-w, copy-words-up-w,
           copy-bytes-down, copy-bytes-up, fill-bytes;
  u    op push, flag: #t;
  u    op set-teb, set-seh;
  d    op get-teb, get-seh;
  u    op force-u;
  d    op force-d;
  tduu op taddv, addv, divv, mulv, subv, tsubv, eliminatable: #t;
  tuuu op conditional-move;
  du   op not, eliminatable: #t;
  du   op ld-teb, ld-teb-address, eliminatable: #t;
  uu   op st-teb;
  uu   op add-into-stack-pointer, flag: #t;
  uu   op store-stack-arg-n;
  duu  op and, asl, asr, divs, divu, eor,
          ld, ldh, ldh-signed, ldb, ldb-signed,
          lsr, muls, and-not, mulu, or, mods, modu,
          set-bit, unset-bit,
          eliminatable: #t;
  d    op pop, flag: #t;
  t    op bmvunset, bmvset, control-flow-link;
  tuu  op bhi, bhs, blo, bls, band, bnand, dynamic-bit, dynamic-nbit,
          beq-byte, bne-byte, bge-byte, bgt-byte, ble-byte, blt-byte;
  tuuu op bit, nbit, bne-mem, beq-mem, bne-words, bne-bytes;
  duuu op ldbits, extract-bits, eliminatable: #t;
  duuuu op set-bits, eliminatable: #t;
  uuuu op stbits;
       op t-pop, spread: spread-t-pop;
       op t-push, remove-optionals, spread: spread-uu;
       op rts-and-drop, adjust-stack, spread: spread-u;
       op load-count-adjusting-stack, spread: spread-tduu, stack-dependent: #t;
 
       op rts, spread: spread-none;
       op end-cleanup, spread: spread-td, flag: #t;
       op pea, spread: spread-tu, flag: #t, for-effective-address: #t;
       op lea, spread: spread-tdu, for-effective-address: #t;
       op load-nlx-address, spread: spread-td, for-effective-address: #t;
       op call, call-alien, spread: spread-uu, flag: #t; // disallow leaf case
       op call-indirect, spread: spread-uuu, flag: #t;   // disallow leaf case
       op jmp, jmp-alien, spread: spread-uuuuu, does-jump: #t;
       op jmp-indirect, spread: spread-uuuuuu, does-jump: #t;
       op load-stack-arg-half-signed, load-stack-arg-half-unsigned, 
          load-stack-arg-byte-signed, load-stack-arg-byte-unsigned, 
            spread: spread-tdu, eliminatable: #t, stack-dependent: #t;
       op load-address-of-stack-arg-n,
            spread: spread-tdu, eliminatable: #t, stack-dependent: #t;
       op load-address-of-stack-arg,
            spread: spread-td, eliminatable: #t, stack-dependent: #t;

  uuuu op and2-mem, or2-mem, add2-mem, sub2-mem, 
          add2-mem-locked, sub2-mem-locked, 
	  eor2-mem;

  duuu  op xadd-mem-locked;

  duu  op move-return-address;


  // New (for Dylan) ops for machine word support

  duu  op ror, rol, eliminatable: #t;

  duu  op add-trap, sub-trap, muls-trap, asl-trap, eliminatable: #t;

  tduu op addc, subc;

  dduu op addcx, subcx, mulx, mulux, divx, divux, aslx, lslx, truncatex;

  tdduu op mulxv, aslxv;

  dduuu op divxx, divuxx, lslxx, lsrxx, truncatexx;

  none  op reset-values, set-values, halt;


  // some floating point instructions ...

  du   op f10tox, f2tox, fabs, facos, fasin, fatan,
	   fatanh, fcos, fcosh, fetox, fetoxm1,
	   fgetexp, fgetman, fint, flog10, flog2,
	   floge, flogep1, fmove, fneg, fsin,
	   fsinh, fsqrt, ftan, ftanh,
           d10tox, d2tox, dabs, dacos, dasin, datan,
	   datanh, dcos, dcosh, detox, detoxm1,
	   dgetexp, dgetman, dint, dlog10, dlog2,
	   dloge, dlogep1, dmove, dneg, dsin,
	   dsinh, dsqrt, dtan, dtanh, eliminatable: #t;
  duu  op fadd, fdiv, fld, fmul, fsub, ld-double, 
           dmul, ddiv, dadd, dsub, dld, eliminatable: #t;
  tuu  op fbeq, fbge, fbgt, fble, fblt, fbne, fbnge,  
           dble, dbge, dbne, dblt, dbeq, fbngt, fbnle, fbnlt;
  uuu  op fst, dst, st-double;
  du   op move-to-sfreg, move-from-sfreg,
	   convert-to-single-float, convert-to-double-float,
	   double-to-single-float, single-to-double-float,
	   convert-from-single-float, convert-from-double-float;
  ddu  op convert-from-single-float-x, convert-from-double-float-x,
          move-from-dfreg;
  duu  op convert-to-single-float-x, convert-to-double-float-x,
          move-to-dfreg;
  u    op set-rounding-mode, restore-frame;
  none op init-fpu, init-control-word, clear-float-exceptions;
  du   op classify-float;
end;


