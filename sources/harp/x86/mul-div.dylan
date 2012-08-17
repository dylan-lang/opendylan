module:    harp-x86
Synopsis:  Pentium multiply/divide instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// These will be almost the worst instructions in the set, because of the
/// funny register requirements. To compensate, though, there is a real
/// three address multiply. Odd.

///                                   (signed)
///               DIV       IDIV       IMUL       MUL    
/// D/A,r/m32    f7 /6     f7 /7      f7 /5      f7 /4
/// r32,r/m32                         0f af /r
/// r32,r/m32,i8                      6b /r ib
/// r32,r/m32,i32                     69 /r id

/// For multiply, the argument order will have been canonicalised. We fail
/// to distinguish between signed and unsigned operands here, since truncating
/// both the signed and unsigned products to 32 bits gives the same result.
/// However, this is 'really' the signed variant.

define pentium-template (muls, mulu)
  pattern (be, d, r, s)
    canon(be, local-fn(can-mul), d, r, s);
end pentium-template;


define local-pentium-template (can-mul)

  // detect the cases where we're multiplying const by r/m into reg:
  pattern (be, d :: <real-register> by colour, r :: <m/spill-ref> by colour, s :: <integer> of eight-bit-const-ref)
    emit(be, #x6b);
    emit-m-spill-dest(be, r, d.ex-reg);
    emit-one-byte(be, s);

  pattern (be, d :: <real-register> by colour, r :: <m/spill-ref> by colour, s :: <ac/const-ref> by colour)
    emit(be, #x69);
    emit-m-spill-dest(be, r, d.ex-reg);
    emit-immediate-constant(be, s);

  // Now isolate any two address cases where constants aren't involved
  pattern (be, d :: <real-register> by colour, s :: <real-register> by colour, r is d)
    emit(be, #x0f, #xaf);
    emit-reg-direct(be, s, d.ex-reg);

  pattern (be, d :: <real-register> by colour, r is d, s :: <ic/m/spill-ref> by colour)
    emit(be, #x0f, #xaf);
    emit-m-c-spill-dest(be, s, d.ex-reg);

  // So now to do the 3-2 stuff. For register destinations this just means
  // putting one (either) source to the destination and multiplying in the
  // other.

  pattern (be, d :: <real-register> by colour, r, s)
    harp-out (be) move(be, d, r) end;
    call-local(can-mul, be, d, d, s);

  // For spill destination, use the temporary register.
  pattern (be, d :: <ispill> by colour, r, s)
    harp-out (be) move(be, reg--tmp1, r) end;
    call-local(can-mul, be, reg--tmp1, reg--tmp1, s);
    harp-out (be) move(be, d, reg--tmp1) end;
  
end local-pentium-template;


/// 64 bit multiplication. There are far less options for register usage compared
/// with the 32 bit counterpart. There is no instruction support for immediate
/// constants. We're obliged to make heavy use of EAX / EDX


with-ops-in pentium-instructions (mulx)  info := #b101000 end;
with-ops-in pentium-instructions (mulux) info := #b100000 end;


define pentium-template (mulx, mulux)
  options (self);

  // A commutative op - so canonicalize EAX first
  pattern (be, i, low :: any, high :: any, x, y by eax-ref)
    harp-reapply(be, i, low, high, y, x);

  // This pattern does all the real work
  pattern (be, i :: <integer> by op-info, low :: any, high :: any, x by eax-ref, y :: <ic/m/spill-ref> by colour)
    emit(be, #xf7);
    emit-m-c-spill-dest(be, y, i);
    if (low)  harp-out (be) move(be, low,  eax) end end;
    if (high) harp-out (be) move(be, high, edx) end end;

  // Move any constant 2nd arg into EDX
  pattern (be, i, low :: any, high :: any, x by eax-ref, y)
    harp-out (be) move(be, edx, y) end;
    harp-reapply(be, i, low, high, x, edx);

  // General cases where no arg is in eax. Move at least one arg.

  // Move the first arg if the second is a convenient r/m32
  pattern (be, i, low :: any, high :: any, x, y :: <ic/m/spill-ref> by colour)
    harp-out (be) move(be, eax, x) end;
    harp-reapply(be, i, low, high, eax, y);

  // Otherwise, move the second arg and the first just might be a r/m32
  pattern (be, i, low :: any, high :: any, x, y)
    harp-out (be) move(be, eax, y) end;
    harp-reapply(be, i, low, high, eax, x);

end pentium-template;


/// And now (drumroll) division. No quarter given here. However, we can at
/// least deal with divs and divu with the same template, modulo a little
/// chicanery to load EDX with the sign extend of EAX for signed divide and
/// zero for the unsigned. In order to handle division by a constant we need
/// a temporary, but this case can

define constant divu = #b110000;
define constant divs = #b111000;


with-ops-in pentium-instructions (divu, divux, divuxx) 
  info := divu 
end with-ops-in;


with-ops-in pentium-instructions (divs, mods, divx, divxx, truncatex, truncatexx) 
  info := divs 
end with-ops-in;


define method emit-div-preamble (be :: <harp-x86-back-end>, i :: <integer>)
  select (i)
    divu => harp-out (be) move(be, edx, 0) end;
    divs => emit(be, cdq);
  end select;
end method;


define pentium-template (divu, divs)
  options (self);

  pattern (be, i :: <integer> by op-info, d, r, s :: <ic/m/spill-ref> by colour)
    harp-out (be) move(be, eax, r) end;	// gets eliminated if r = eax
    emit-div-preamble(be, i);
    emit(be, #xf7);
    emit-m-c-spill-dest(be, s, i);
    harp-out (be) move(be, d, eax) end;
   
  // Need an extra temporary register for the /constant case
  pattern (be, i :: <integer> by op-info, d, r, s :: <ac/const-ref> by colour)
    harp-out (be)
      move(be, reg--tmp1, s);
      move(be, eax, r);
    end harp-out;
    emit-div-preamble(be, i);
    emit(be, #xf7);
    emit-m-spill-dest(be, reg--tmp1, i);
    harp-out (be) move(be, d, eax) end;

end pentium-template;


define pentium-template (mods)
  options (self);

  pattern (be, i :: <integer> by op-info, d, r, s :: <ic/m/spill-ref> by colour)
    harp-out (be) move(be, eax, r) end;	// gets eliminated if r = eax
    emit-div-preamble(be, i);
    emit(be, #xf7);
    emit-m-c-spill-dest(be, s, i);
    harp-out (be) move(be, d, edx) end;
   
  // Need an extra temporary register for the /constant case
  pattern (be, i :: <integer> by op-info, d, r, s :: <ac/const-ref> by colour)
    harp-out (be)
      move(be, reg--tmp1, s);
      move(be, eax, r);
    end harp-out;
    emit-div-preamble(be, i);
    emit(be, #xf7);
    emit-m-spill-dest(be, reg--tmp1, i);
    harp-out (be) move(be, d, edx) end;

end pentium-template;


define pentium-template (divux, divx, truncatex)
  options (self);

  pattern (be, i :: <integer> by op-info, quot :: any, rem :: any, r, s :: <ic/m/spill-ref> by colour)
    harp-out (be) move(be, eax, r) end;	// gets eliminated if r = eax
    emit-div-preamble(be, i);
    emit(be, #xf7);
    emit-m-c-spill-dest(be, s, i);
    if (rem)  harp-out (be) move(be,  rem, edx) end end;
    if (quot) harp-out (be) move(be, quot, eax) end end;
   
  // Need an extra temporary register for the /constant case
  pattern (be, i, quot :: any, rem :: any, r, s :: <ac/const-ref> by colour)
    harp-out (be) move(be, reg--tmp1, s) end;
    harp-reapply(be, i, quot, rem, r, reg--tmp1);

end pentium-template;


define pentium-template (divuxx, divxx, truncatexx)
  options (self);

  pattern (be, i :: <integer> by op-info, quot :: any, rem :: any, low, high, s :: <ic/m/spill-ref> by colour)
    harp-out (be) move(be, eax, low) end;	// gets eliminated if low = eax
    harp-out (be) move(be, edx, high) end;	// gets eliminated if high = edx
    emit(be, #xf7);
    emit-m-c-spill-dest(be, s, i);
    if (rem)  harp-out (be) move(be,  rem, edx) end end;
    if (quot) harp-out (be) move(be, quot, eax) end end;
   
  // Need an extra temporary register for the /constant case
  pattern (be, i, quot :: any, rem :: any, low, high, s :: <ac/const-ref> by colour)
    harp-out (be) move(be, reg--tmp1, s) end;
    harp-reapply(be, i, quot, rem, low, high, reg--tmp1);

end pentium-template;
    
