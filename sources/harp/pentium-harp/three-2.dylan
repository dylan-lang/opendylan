module:    pentium-harp
Synopsis:  Three - Two address translator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Well, here we are, perpetuating the same old mistakes for a brand new
/// processor, namely the 386. It looks a bit like a 68k in its CISCyness -
/// however, the move instruction doesn't have so many modes, so cannot get us
/// out of trouble as easily.

/// argument canonicalisation for this processor is eax > reg > spill > const


// First of all, the support for C preserved registers

with-ops-in pentium-instructions (add, sub, and, or, eor,
                                  asl, asr, asr-unsafe, lsr, rol, ror,
                                  muls, mulu, divs, divu, mods,
                                  addv, subv, addc, subc, mulv, divv, 
                                  add-trap, sub-trap, muls-trap, 
                                  ld, ldh, ldb, ldh-signed, ldb-signed)
  c-preserved-destroys-fn
    :=  pentium-method (duu)
          destroys-tmp1-if(ic/spill-ref(duu-def(1)) 
                           | ic/spill-ref(duu-uze(1)) 
                           | ic/spill-ref(duu-uze(2))) // conservative
        end pentium-method;
end with-ops-in;


with-ops-in pentium-instructions (and2-mem, or2-mem, add2-mem, sub2-mem, 
                                  add2-mem-locked, sub2-mem-locked, 
                                  eor2-mem, and2-byte-mem, or2-byte-mem,
                                  xadd-mem-locked,
                                  ld-index, ldb-index, ldh-index,
                                  ldb-index-signed, ldh-index-signed,
                                  ld-index-scaled, ldh-index-scaled,
                                  ldh-index-scaled-signed,
                                  set-teb, ld-teb, ld-teb-address, st-teb,
                                  st-index, stb-index, sth-index,
                                  st-index-scaled, sth-index-scaled,
                                  st, sth, stb, mulx, mulux, mulxv,
                                  addcx, subcx, divx, divux, truncatex,
                                  divxx, divuxx, truncatexx,
                                  aslx, aslxv, lslx, lslxx, lsrxx, asl-trap)
  c-preserved-destroys-fn := tmp1-fn;  // not worth being smarter
end with-ops-in;


/// Now the clashes



with-ops-in pentium-instructions (xadd-mem-locked)
  clash-fn := pentium-method (duuu)
                list(list(duuu-def(1), duuu-uze(1)),
                     list(duuu-def(1), duuu-uze(2)),
                     list(duuu-def(1), duuu-uze(3)));
              end pentium-method;
end;

with-ops-in pentium-instructions (ldb)
  clash-fn :=  pentium-method (duu)
                 list(list(duu-def(1), esi, edi));
               end pentium-method;
end;

with-ops-in pentium-instructions (ldb-index)
  clash-fn :=  pentium-method (duuu)
                 list(list(duuu-def(1), esi, edi));
               end pentium-method;
end;

with-ops-in pentium-instructions (stb)
  clash-fn :=  pentium-method (uuu)
                 list(list(uuu-uze(1), esi, edi));
               end pentium-method;
end;

with-ops-in pentium-instructions (stb-index)
  clash-fn :=  pentium-method (uuuu)
                 list(list(uuuu-uze(1), esi, edi));
               end pentium-method;
end;

with-ops-in pentium-instructions (sth, st)
  clash-fn :=  pentium-method (uuu)
                 list(list(uuu-uze(1), esi));
               end pentium-method;
end;


with-ops-in pentium-instructions (asl, asr, asr-unsafe, lsr, rol, ror)

  destroys-fn := pentium-method (duu)
                   if (const-ref(duu-uze(2)))
                     #[];
                   else $vector-ecx;
                   end if;
                 end pentium-method;

  clash-fn := pentium-method (duu)
                if (const-ref(duu-uze(2)))
                  #();
                else
		  list(list(duu-def(1), duu-uze(2)),
                       // Since cl is used in the template
                       list(duu-def(1), ecx));
                end if;
              end pentium-method;
end;


with-ops-in pentium-instructions (sub, sub-trap)
  clash-fn := pentium-method (duu)
                if (instance?(duu-uze(2), <register>))
		  list(list(duu-def(1), duu-uze(2)));
                else
                  #();
                end if;
              end pentium-method;
end;



with-ops-in pentium-instructions (subcx)
  clash-fn := pentium-method (dduu)
                if (instance?(dduu-uze(2), <register>))
		  list(list(dduu-def(1), dduu-uze(2)));
                else
                  #();
                end if;
              end pentium-method;
end;


with-ops-in pentium-instructions (subv, subc)
  clash-fn := pentium-method (duu)
                concatenate
                  (default-overflow-function-clashes(backend, ins),
                   if (instance?(duu-uze(2), <register>))
                     list(list(duu-def(1), duu-uze(2)));
                   else
                     #();
                   end if);
              end pentium-method;
end;


with-ops-in pentium-instructions (mulv, addv, addc)
  clash-fn := pentium-method (duu)
                default-overflow-function-clashes(backend, ins);
              end pentium-method;
end;



with-ops-in pentium-instructions (fbeq, fbne, fblt, fble, fbge,
                                  dbeq, dbne, dblt, dble, dbge)
  disallow-fn := eax-fn;
end;


with-ops-in pentium-instructions (copy-words-up,   copy-words-down, 
                                  copy-words-up-w, copy-words-down-w)
  destroys-fn := constant-fn(vector(esi, edi, ecx));
  clash-fn := pentium-method (uuu)
	        list(list(uuu-uze(1), esi, ecx),
                     list(edi, uuu-uze(2), ecx),
                     list(edi, esi, uuu-uze(3)));
              end pentium-method;
end;

with-ops-in pentium-instructions (copy-bytes-up, copy-bytes-down)
  destroys-fn := constant-fn(vector(esi, edi, ecx));
  clash-fn := pentium-method (uuu)
	        list(list(uuu-uze(1), esi, ecx),
                     list(edi, uuu-uze(2), ecx),
                     list(edi, esi, ecx, uuu-uze(3)));
              end pentium-method;
end;

with-ops-in pentium-instructions (fill-words, fill-words-w)
  destroys-fn := constant-fn(vector(edi, ecx, eax));
  clash-fn := pentium-method (uuu)
	        list(list(uuu-uze(1), ecx, eax),
                     list(edi, uuu-uze(2), eax),
                     list(edi, ecx, uuu-uze(3)));
              end pentium-method;
end;

with-ops-in pentium-instructions (fill-bytes)
  destroys-fn := constant-fn(vector(edi, ecx, eax));
  clash-fn := pentium-method (uuu)
	        list(list(uuu-uze(1), ecx, eax),
                     list(edi, uuu-uze(2), ecx, eax),
                     list(edi, ecx, uuu-uze(3)));
              end pentium-method;
end;


with-ops-in pentium-instructions (st, sth, stb, set-teb, st-teb, 
                                  st-index, stb-index, sth-index,
                                  ld-index-scaled, ldh-index-scaled,
                                  ldh-index-scaled-signed,
	                          extract-bits, ldbits, fld, fst, dld, dst, 
                                  fld-index, dld-index,
                                  fld-index-scaled, dld-index-scaled, dld-index-scale-2,
                                  fst-index, dst-index,
                                  fst-index-scaled, dst-index-scaled, dst-index-scale-2,
                                  bits-mem, bitc-mem, and2-mem, or2-mem, 
                                  eor2-mem, add2-mem, sub2-mem, 
                                  add2-mem-locked, sub2-mem-locked, 
                                  and2-byte-mem, or2-byte-mem,
                                  xadd-mem-locked)
  disallow-fn := tmp2-fn;
end;


with-ops-in pentium-instructions (st-index-scaled, sth-index-scaled)
  disallow-fn := constant-fn(vector(reg--tmp2, reg--tmp3));
end;


with-ops-in pentium-instructions (and2-byte-mem, or2-byte-mem)
  clash-fn := pentium-method (uuuu)
                list(list(uuuu-uze(4), esi, edi));
              end pentium-method; 		  
end;





define constant eax-and-edx = vector(eax, edx);

define constant byte-addressable-regs = vector(eax, ebx, ecx, edx);



with-ops-in pentium-instructions (mulx, mulux)

  destroys-fn := constant-fn(eax-and-edx);

  prefer-fn := pentium-method (dduu)
		 prefer(dduu-def(1), $vector-eax);
		 prefer(dduu-def(2), $vector-edx);
		 prefer(dduu-uze(1), $vector-eax);
               end pentium-method; 
end with-ops-in;



with-ops-in pentium-instructions (divs, divu, mods)
  clash-fn := pentium-method (duu)
                list(list(duu-uze(2), eax, edx));
              end pentium-method; 		  
end;


with-ops-in pentium-instructions (divx, divux, truncatex)
  clash-fn := pentium-method (dduu)
                list(list(dduu-uze(2), eax, edx),
                     list(dduu-def(1), edx),
                     list(dduu-def(2), eax));
              end pentium-method; 		  
end;

with-ops-in pentium-instructions (divxx, divuxx, truncatexx)
  clash-fn := pentium-method (dduuu)
                list(list(dduuu-uze(3), eax, edx),
                     list(dduuu-uze(2), eax),
                     list(dduuu-uze(1), edx),
                     list(dduuu-def(1), edx),
                     list(dduuu-def(2), eax));
              end pentium-method; 		  
end;


with-ops-in pentium-instructions (divv)
  clash-fn := pentium-method (duu)
                concatenate
                  (default-overflow-function-clashes(backend, ins),
                   list(list(duu-uze(2), eax, edx)));
              end pentium-method; 		  
end;


with-ops-in pentium-instructions (divs, divu, divv, mods,
                                  divx, divux, truncatex,
                                  divxx, divuxx, truncatexx)
  destroys-fn := constant-fn(eax-and-edx);
end;


with-ops-in pentium-instructions (divs, divu, divv)
  prefer-fn := pentium-method (duu)
		 prefer(duu-def(1), $vector-eax);
		 prefer(duu-uze(1), $vector-eax);
               end pentium-method; 		  
end;


with-ops-in pentium-instructions (divx, divux, truncatex)
  prefer-fn := pentium-method (dduu)
		 prefer(dduu-def(1), $vector-eax);
		 prefer(dduu-def(2), $vector-edx);
		 prefer(dduu-uze(1), $vector-eax);
               end pentium-method; 		  
end;


with-ops-in pentium-instructions (divxx, divuxx, truncatexx)
  prefer-fn := pentium-method (dduuu)
		 prefer(dduuu-def(1), $vector-eax);
		 prefer(dduuu-def(2), $vector-edx);
		 prefer(dduuu-uze(1), $vector-eax);
		 prefer(dduuu-uze(2), $vector-edx);
               end pentium-method; 		  
end;


with-ops-in pentium-instructions (mods)
  prefer-fn := pentium-method (duu)
		 prefer(duu-def(1), $vector-edx);
		 prefer(duu-uze(1), $vector-eax);
               end pentium-method; 		  
end;


with-ops-in pentium-instructions (copy-words-up, copy-words-down, 
                                  copy-words-up-w, copy-words-down-w)
  prefer-fn := pentium-method (uuu)
		 prefer(uuu-uze(1), $vector-edi);
		 prefer(uuu-uze(2), $vector-esi);
		 prefer(uuu-uze(3), $vector-ecx);
               end pentium-method;
end;

with-ops-in pentium-instructions (copy-bytes-up, copy-bytes-down)
  prefer-fn := pentium-method (uuu)
		 prefer(uuu-uze(1), $vector-edi);
		 prefer(uuu-uze(2), $vector-esi);
               end pentium-method;
end;


with-ops-in pentium-instructions (fill-words, fill-words-w)
  prefer-fn := pentium-method (uuu)
		 prefer(uuu-uze(1), $vector-edi);
		 prefer(uuu-uze(2), $vector-ecx);
		 prefer(uuu-uze(3), $vector-eax);
               end pentium-method;
end;

with-ops-in pentium-instructions (fill-bytes)
  prefer-fn := pentium-method (uuu)
		 prefer(uuu-uze(1), $vector-edi);
		 prefer(uuu-uze(3), $vector-eax);
               end pentium-method;
end;



/// The register-destination pattern for 3-2 is largely lifted from the 68k.
/// As an experiment, catch the reverse-op case with an error message - the
/// allocator should avoid generating these.

define method three-2 
    (be :: <pentium-back-end>, ins :: <function>, 
     d :: <real-register>, s1, s2)
  call-local(rdest, be, ins, d, s1, s2);
end method;

define method three-2 
    (be :: <pentium-back-end>, ins :: <function>, 
     d :: <object>, s1, s2)
  call-local(sdest, be, ins, d, s1, s2);
end method;

define method commut (x)
  if (x == local-fn(and2) | x == local-fn(or2) | 
      x == local-fn(add2) | x == local-fn(xor2))
    x;
  end if;
end method;

define method noncom (x)
  if (x == local-fn(sub2)) x end;
end method;


define local-pentium-template rdest

  // two addressable, same dest and first arg
  pattern (be, i, d, r is d, s)
    i(be, d, s);

  // commutative, source and destination in 'wrong' order
  pattern (be, i by commut, d, s, r is d)
    i(be, d, s);

  // non-commutative and operands in 'wrong' order - moan.
  pattern (be, i by noncom, d, s, r is d)
    harp-error("Reverse operation detected ! %=", list(i, d, s, d));
					       
  // now we know the three operands are distinct. In generating 2
  // address code for 3 address instructions, we will have to do a
  // move. Uniquely, this processor has instructions that better
  // address constants than move. All these instructions are in commut.

  pattern (be, i by commut, d, r :: <ac/const-ref> by colour, s)
    harp-out (be) move(be, d, s) end harp-out;
    i(be, d, r);

  // All others just go fwd

  pattern (be, i, d, r, s)
    harp-out (be) move(be, d, r) end harp-out;
    i(be, d, s);

end local-pentium-template;


/// Spill destinations

define local-pentium-template sdest

  // two addressable, same dest and first arg, second register/const
  pattern (be, i, d, r is d, s :: <m/const-ref> by colour)
    i(be, d, s);
	
  // commutative, source and destination in 'wrong' order, other 'easy'
  pattern (be, i by commut, d, s :: <m/const-ref> by colour, r is d)
    i(be, d, s);
	
  // commutative but both source and destination are spill
  pattern (be, i by commut, d, r is d, s)
    harp-out (be) move(be, reg--tmp1, s) end harp-out;
    i(be, d, reg--tmp1);
	
  pattern (be, i by commut, d, s, r is d)
    harp-out (be) move(be, reg--tmp1, s) end harp-out;
    i(be, d, reg--tmp1);
	
  // non-commutative and operands in 'wrong' order - whinge.
  pattern (be, i by noncom, d, s, r is d)
    harp-error("Reverse operation detected ! %=", list(i, d, s, d));
	 
  // now we know the three operands are distinct. In generating 2
  // address code for 3 address instructions, we will have to do a move.
  // Got to try and put a register/constant operand in the operation ...
  // No memory/memory move means it doesn't really matter what we do here.

  pattern (be, i, d, r, s)
    harp-out (be) move(be, reg--tmp1, r) end harp-out;
    i(be, reg--tmp1, s);
    harp-out (be) move(be, d, reg--tmp1) end harp-out;

end local-pentium-template;


/// Lastly, canonicalisation patterns, used here for ld, st, multiplies.

define method canon
    (be :: <pentium-back-end>, i :: <function>, d, s, m)
  if (const-ref(s)
      | (address-constant-ref(s) & ~ const-ref(m))
      | (ic/spill-ref(s) & m-ref(m))
      | (m-ref(s) & eax-ref(m)))
    i(be, d, m, s);
  else
    i(be, d, s, m);
  end if;
end method;



