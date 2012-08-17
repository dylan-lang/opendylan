module:    x86-harp
Synopsis:  Pentium Shift instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Shifts on this machine code the direction as part of the instruction.
/// The length for a variable-length shift has to be in CL, the byte register
/// at the bottom of C. Small mercy - the 386 can actually do a memory shift
/// sensibly. Almost compensates for having to put the count in C. Mmmm.
/// When doing the memory/memory case we could spend one extra register and
/// save time by doing it in registers. Should we?

with-ops-in pentium-instructions (asl)  info := asl2  end;
with-ops-in pentium-instructions (asr)  info := asr2  end;
with-ops-in pentium-instructions (lsr)  info := lsr2  end;
with-ops-in pentium-instructions (rol)  info := rol2  end;
with-ops-in pentium-instructions (ror)  info := ror2  end;


define pentium-template (asl, asr, lsr, rol, ror)
  options (self);

  // All very easy - just move src -> dest, and do shift on dest
  pattern (be, i :: <integer> by op-info, d, s, c)
    harp-out (be) move(be, d, s) end;
    do-the-shift(be, i, d, c);

end pentium-template;



/// ASR-UNSAFE allows us to leave junk in the upper bits after the shift.
/// Here we optimise the case where we are shifting by 8, 16 or 24 bits, and
/// the source is an indirect constant reference. In this case we can just 
/// indirect to an address with the appropriate offset.

define method byte-shift-offset (x)
  select (x)
    8  => 1;
    16 => 2;
    24 => 3;
    otherwise => #f;
  end select;
end method;

define method shift-indirection 
    (be :: <harp-x86-back-end>, 
     ind-const :: <constant-reference>, 
     offset :: <integer>) => (c :: <constant-reference>)

  call-instruction(constant-ref, be,  ind-const.cr-refers-to-object,
		   offset: offset + ind-const.cr-const-offset,
                   mode: #"indirect");
end method;



define pentium-template (asr-unsafe)

  pattern (be, d, s :: <i-indirect-constant-reference>, c by byte-shift-offset)
    harp-out (be) move(be, d, shift-indirection(be, s, c)) end;

  // Catch-all case
  pattern (be, d, s, c)
    harp-out (be) asr(be, d, s, c) end;

end pentium-template;


// NB. DO-THE-SHIFT uses ecx (tmp2)
define method do-the-shift 
    (be :: <harp-x86-back-end>, ins :: <integer>, dest, count :: <integer>)
  unless (zero?(count))
    emit(be, #xc1);
    emit-m-spill-dest(be, dest, ins);
    emit-one-byte(be, count);
  end unless;
end method;


define method do-the-shift 
    (be :: <harp-x86-back-end>, ins :: <integer>, dest, count :: <object>)
  harp-out (be) move(be, ecx, count) end;
  emit(be, #xd3);
  emit-m-spill-dest(be, dest, ins);
end method;


//// 64bit shifting


// NB. DOUBLE-SHIFT-LEFT uses ecx (tmp2), eax and edx
// The low and high words to be shifted are in eax and edx respectively

define method double-shift-left
    (be :: <harp-x86-back-end>, low, high, count :: <integer>)
  // first, optionally shift the low bits into the high bits
  if (high)
    unless (zero?(count))
      emit(be, #x0f);
      emit(be, #xa4);
      emit-reg-direct(be, edx, ex-reg(eax));
      emit-one-byte(be, count);
    end unless;
    harp-out (be) move(be, high, edx) end;
  end if;
  // now optionally shift just the low bits
  if (low)
    unless (zero?(count))
      emit(be, #xc1);
      emit-m-spill-dest(be, eax, asl2);
      emit-one-byte(be, count);
    end unless;
    harp-out (be) move(be, low, eax) end;
  end if;
end method;


define method double-shift-left
    (be :: <harp-x86-back-end>, low, high, count :: <object>)
  harp-out (be) move(be, ecx, count) end;
  // first, optionally shift the low bits into the high bits
  if (high)
    emit(be, #x0f);
    emit(be, #xa5);
    emit-reg-direct(be, edx, ex-reg(eax));
    harp-out (be) move(be, high, edx) end;
  end if;
  // now optionally shift just the low bits
  if (low)
    emit(be, #xd3);
    emit-reg-direct(be, eax, asl2);
    harp-out (be) move(be, low, eax) end;
  end if;
end method;



// NB. DOUBLE-SHIFT-RIGHT uses ecx (tmp2), eax and edx
// The low and high words to be shifted are in eax and edx respectively

define method double-shift-right
    (be :: <harp-x86-back-end>, low, high, count :: <integer>)
  // first, optionally shift the high bits into the low bits
  if (low)
    unless (zero?(count))
      emit(be, #x0f);
      emit(be, #xac);
      emit-reg-direct(be, eax, ex-reg(edx));
      emit-one-byte(be, count);
    end unless;
    harp-out (be) move(be, low, eax) end;
  end if;
  // now optionally shift just the high bits
  if (high)
    unless (zero?(count))
      emit(be, #xc1);
      emit-m-spill-dest(be, edx, lsr2);
      emit-one-byte(be, count);
    end unless;
    harp-out (be) move(be, high, edx) end;
  end if;
end method;


define method double-shift-right
    (be :: <harp-x86-back-end>, low, high, count :: <object>)
  harp-out (be) move(be, ecx, count) end;
  // first, optionally shift the high bits into the low bits
  if (low)
    emit(be, #x0f);
    emit(be, #xad);
    emit-reg-direct(be, eax, ex-reg(edx));
    harp-out (be) move(be, low, eax) end;
  end if;
  // now optionally shift just the high bits
  if (high)
    emit(be, #xd3);
    emit-reg-direct(be, edx, lsr2);
    harp-out (be) move(be, high, edx) end;
  end if;
end method;

  


define pentium-template (aslx)
  pattern (be, low :: any, high :: any, s, count)
    harp-out (be) move(be, eax, s) end;
    emit(be, cdq); // 64bit sign-extend EAX into EDX
    double-shift-left(be, low, high, count);
end pentium-template;



define pentium-template (lslx)
  pattern (be, low :: any, high :: any, s, count)
    harp-out (be) 
      move(be, eax, s);
      move(be, edx, 0); // 64bit zero-extend EAX into EDX
    end;
    double-shift-left(be, low, high, count);
end pentium-template;



define pentium-template (lslxx)
  pattern (be, low :: any, high :: any, s-low, s-high, count)
    harp-out (be) 
      move(be, eax, s-low);
      move(be, edx, s-high);
    end;
    double-shift-left(be, low, high, count);
end pentium-template;



define pentium-template (lsrxx)
  pattern (be, low :: any, high :: any, s-low, s-high, count)
    harp-out (be) 
      move(be, eax, s-low);
      move(be, edx, s-high);
    end;
    double-shift-right(be, low, high, count);
end pentium-template;



with-ops-in pentium-instructions (aslx, lslx)

  destroys-fn := constant-fn(vector(eax, edx, ecx));

  prefer-fn := pentium-method (dduu)
		 prefer(dduu-def(1), $vector-eax);
		 prefer(dduu-def(2), $vector-edx);
		 prefer(dduu-uze(1), $vector-eax);
		 prefer(dduu-uze(2), $vector-ecx);
               end pentium-method; 
		  
  clash-fn := pentium-method (dduu)
                if (instance?(dduu-uze(2), <register>))
		  list(list(dduu-uze(2), eax, edx),
                       list(dduu-def(2), eax, ecx));
                else
                  list(list(dduu-def(2), eax));
                end if;
              end pentium-method;
end with-ops-in;



with-ops-in pentium-instructions (lslxx, lsrxx)

  destroys-fn := constant-fn(vector(eax, edx, ecx));

  prefer-fn := pentium-method (dduuu)
		 prefer(dduuu-def(1), $vector-eax);
		 prefer(dduuu-def(2), $vector-edx);
		 prefer(dduuu-uze(1), $vector-eax);
		 prefer(dduuu-uze(2), $vector-edx);
		 prefer(dduuu-uze(3), $vector-ecx);
               end pentium-method; 
		  
  clash-fn := pentium-method (dduuu)
                if (instance?(dduuu-uze(3), <register>))
		  list(list(dduuu-uze(2), eax),
                       list(dduuu-uze(3), eax, edx),
                       list(dduuu-def(2), eax, ecx));
                else
                  list(list(dduuu-def(2), eax),
                       list(dduuu-uze(2), eax));
                end if;
              end pentium-method;
end with-ops-in;


