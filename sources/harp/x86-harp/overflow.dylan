module:    x86-harp
Synopsis:  Pentium overflow-sensitive instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// updated for the new compiler format + callee saves (cim 2/5/89)

/// Glory hallelujah! At last a processor that does NOT reset overflow on move.
/// On the other hand, I'd like to speak to the bozo who made signed multiply
/// only ever reset the flags for a good result, never set them for a bad one.
/// There's no set overflow flag either ... Not sure I can believe this as the
/// book contradicts itself.

define constant bvs-x = #x70;   // JO, Jump if no overflow
define constant bno-x = #x71;   // JNO, Jump if no overflow
define constant bov-x = #x72;   // JB, Jump if below (carry flag set)


with-ops-in pentium-instructions (addv, subv)
  info := bvs-x;
end;

with-ops-in pentium-instructions (addc, subc)
  info := bov-x;
end;


define pentium-template (addv, addc)
  options (self);

  pattern (be, flag-test :: <integer> by op-info, tag, d, r, s)
    call-local(add-setting-flags, be, d, r, s);
    emit-branch-sdi(be, flag-test, tag);
end pentium-template;


define pentium-template (subv, subc)
  options (self);

  pattern (be, flag-test :: <integer> by op-info, tag, d, r, s)
    harp-out (be) sub(be, d, r, s) end;
    emit-branch-sdi(be, flag-test, tag);
end pentium-template;


define pentium-template addcx
  pattern (be, sum, carry, r, s)
    call-local(add-setting-flags, be, sum, r, s);
    do-the-shift(be, rcl2, carry, 1); // shift in the carry bit
    harp-out (be) and(be, carry, carry, 1) end; // mask all else

  pattern (be, sum, carry :: any, r, s)
    harp-out (be) add(be, sum, r, s) end;
end pentium-template;


define pentium-template subcx
  pattern (be, diff, carry, r, s)
    harp-out (be) sub(be, diff, r, s) end;
    do-the-shift(be, rcl2, carry, 1); // shift in the carry bit
    harp-out (be) and(be, carry, carry, 1) end; // mask all else

  pattern (be, diff, carry :: any, r, s)
    harp-out (be) sub(be, diff, r, s) end;
end pentium-template;



define pentium-template mulv
  pattern (be, tag, d, r, s)
    harp-out (be) muls(be, d, r, s) end;
    emit-branch-sdi(be, bvs-x, tag);
end pentium-template;



define pentium-template mulxv
  pattern (be, tag, low :: any, high :: any, r, s)
    harp-out (be) mulx(be, low, high, r, s) end;
    emit-branch-sdi(be, bvs-x, tag);
end pentium-template;



with-ops-in pentium-instructions (mulxv)
  clash-fn := pentium-method (dduu)
                default-double-overflow-function-clashes(backend, ins);
              end pentium-method;
end with-ops-in;



/// We ignore the possibility of divisive (!) overflow, partly because I don't
/// think it'll ever happen, and partly because it's impossible to detect.

define pentium-template divv
  pattern (be, tag, d, r, s)
    harp-out (be) divs(be, d, r, s) end;
    emit-branch-sdi(be, bvs-x, tag);
end pentium-template;



define pentium-template (aslxv)
  pattern (be, ov-tag, low, high :: any, s, count)
    let hi = high | reg--tmp1; // need a real register for the result
    // First do the shift
    harp-out (be) aslx(be, low, hi, s, count) end;
    // clashes ensure that low & high not in edx, high not in eax
    // the design of aslx ensures that eax has a copy of low
    //
    // Now test for overflow by sign extending the new EAX
    // and comparing the results with hi
    emit(be, cdq); // 64bit sign-extend EAX into EDX again
    harp-out (be) bne(be, ov-tag, edx, hi) end;
end pentium-template;


with-ops-in pentium-instructions (aslxv)

  destroys-fn := constant-fn(vector(eax, edx, ecx));

  prefer-fn := pentium-method (dduu)
		 prefer(dduu-def(1), $vector-eax);
		 prefer(dduu-uze(1), $vector-eax);
		 prefer(dduu-uze(2), $vector-ecx);
               end pentium-method; 
		  
  clash-fn := pentium-method (dduu)
                concatenate
                  (default-double-overflow-function-clashes(backend, ins),
                   if (instance?(dduu-uze(2), <register>))
                     list(list(dduu-uze(2), eax, edx),
                     list(dduu-def(1), edx),
                     list(dduu-def(2), eax, ecx, edx));
                   else
                     list(list(dduu-def(1), edx),
                     list(dduu-def(2), eax, edx));
                   end if);
              end pentium-method;
end with-ops-in;



//// Trapping versions



define pentium-template add-trap
  pattern (be, d, r, s)
    call-local(add-setting-flags, be, d, r, s);
    trap-on-overflow(be);
end pentium-template;


define pentium-template sub-trap
  pattern (be, d, r, s)
    harp-out (be) sub(be, d, r, s) end;
    trap-on-overflow(be);
end pentium-template;


define pentium-template muls-trap
  pattern (be, d, r, s)
    harp-out (be) muls(be, d, r, s) end;
    trap-on-overflow(be);
end pentium-template;



define pentium-template (asl-trap)
  pattern (be, low, s, count)
    let hi = reg--tmp1; // need a register for the high result
    // First do the shift
    harp-out (be) aslx(be, low, hi, s, count) end;
    // clashes ensure that low not in edx
    // the design of aslx ensures that eax has a copy of low
    //
    // Now test for overflow by sign extending the new EAX
    // and comparing the results with hi
    emit(be, cdq); // 64bit sign-extend EAX into EDX again
    call-local(cmp2, be, edx, hi);
    // jump over the trap instruction if there's no overflow
    emit(be, beq-x);
    emit(be, 2); // 2 byte instruction
    trap-always(be);
end pentium-template;


with-ops-in pentium-instructions (asl-trap)

  destroys-fn := constant-fn(vector(eax, edx, ecx));

  prefer-fn := pentium-method (duu)
		 prefer(duu-def(1), $vector-eax);
		 prefer(duu-uze(1), $vector-eax);
		 prefer(duu-uze(2), $vector-ecx);
               end pentium-method; 
		  
  clash-fn := pentium-method (duu)
                if (instance?(duu-uze(2), <register>))
		  list(list(duu-uze(2), eax, edx),
                       list(duu-def(1), edx));
                else
                  list(list(duu-def(1), edx));
                end if;
              end pentium-method;
end with-ops-in;


define method trap-on-overflow (be :: <harp-x86-back-end>) => ()
  // INTO instruction, calls INT 4 on overflow
  emit(be, #xce);
end method;

define method trap-always (be :: <harp-x86-back-end>) => ()
  // just manually call INT 4
  emit(be, #xcd);
  emit(be, 4);
end method;
