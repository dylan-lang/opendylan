module:    pentium-harp
Synopsis:  Pentium floating point move instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method fspill/st0 (x)
  if (instance?(x, <fspill>))
    x;
  else
    x.st-ref;
  end if;
end method;



with-ops-in pentium-instructions (fld, dld,
				  fld-index, fld-index-scaled,
				  dld-index, dld-index-scaled, dld-index-scale-2,
                                  fst, dst,
				  fst-index, fst-index-scaled,
				  dst-index, dst-index-scaled, dst-index-scale-2,
                                  move-to-sfreg, move-from-sfreg)
  c-preserved-destroys-fn := tmp1-fn;  // not worth being smarter
end;


define pentium-template (fld)
  pattern (be, d by fspill/st0, r, s)
    canon(be, local-fn(can-fld), d, r, s);
end pentium-template;

define pentium-template (dld)
  pattern (be, d by fspill/st0, r, s)
    canon(be, local-fn(can-dld), d, r, s);
end pentium-template;


define constant can-fld = mf-single;
define constant can-dld = mf-double;

define method fld-pop-ins (i :: <integer>) => (f :: <function>)
  if (i == can-fld)
    pop-single;
  else
    pop-double;
  end if;
end method;

define method fld-reapply-fn (ins) => (f :: <function>)
  select (ins)
    can-fld => local-fn(can-fld);
    can-dld => local-fn(can-dld);
  end select;
end method;

define local-pentium-template (can-fld, can-dld)
  options (self);

  pattern (be, i, d, base :: <real-register> by colour, offset :: <ac/const-ref> by colour)
    emit(be, flt-esc + i + 1);
    emit-reg-offset(be, base, offset, mc-fld);
    unless (d.st-ref) i.fld-pop-ins(be, d) end;

  pattern (be, i, d, base :: <real-register> by colour, offset :: <real-register> by colour)
    emit(be, flt-esc + i + 1);
    emit-reg-indexed(be, base, offset, mc-fld);
    unless (d.st-ref) i.fld-pop-ins(be, d) end;
     
  pattern (be, i, d, base :: <real-register> by colour, offset :: <ispill> by colour)
    harp-out (be) move(be, reg--tmp1, offset) end;
    i.fld-reapply-fn(be, d, base, reg--tmp1);

  pattern (be, i, d, base :: <ic/spill-ref> by colour, offset :: <ac/const-ref> by colour)
    harp-out (be) move(be, reg--tmp1, base) end;
    i.fld-reapply-fn(be, d, reg--tmp1, offset);
     
  pattern (be, i, d, base :: <ic/spill-ref> by colour, offset :: <ispill> by colour)
    harp-out (be) 
      move(be, reg--tmp1, base);
      move(be, reg--tmp2, offset);
    end harp-out;
    i.fld-reapply-fn(be, d, reg--tmp1, reg--tmp2);

  // Address constant with fixed constant:
  pattern (be, i, d, base :: <i-address-constant-reference>, offset :: <integer>)
    let new = coerce-constant-with-offset(be, base, offset);
    emit(be, flt-esc + i + 1);
    emit-reg-constant-offset(be, new, mc-fld);
    unless (d.st-ref) i.fld-pop-ins(be, d) end;

end local-pentium-template;


with-ops-in pentium-instructions (fld-index) info := mf-single end;
with-ops-in pentium-instructions (fld-index-scaled) info := pair(mf-single, 4) end;
with-ops-in pentium-instructions (dld-index) info := mf-double end;
with-ops-in pentium-instructions (dld-index-scaled) info := pair(mf-double, 8) end;
with-ops-in pentium-instructions (dld-index-scale-2) info := pair(mf-double, 2) end;

define pentium-template (fld-index, dld-index)
  options (self);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, r, s :: <ac/const-ref> by colour, o :: <integer>)
    let base = coerce-constant-with-offset(be, s, o);
    i.fld-reapply-fn(be, d, r, base);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, r :: <ac/const-ref> by colour, s, o :: <integer>)
    let base = coerce-constant-with-offset(be, r, o);
    i.fld-reapply-fn(be, d, s, base);

  // The main case where we can make use of the 386 addressing modes.
  pattern (be, i :: <integer> by op-info, d, r, s, o :: <integer>)
    let tmps = list(reg--tmp1, reg--tmp2);
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    emit(be, flt-esc + i + 1);
    emit-double-indexed(be, rn, sn, o, mc-fld);
    unless (d.st-ref) i.fld-pop-ins(be, d) end;

end pentium-template;



// define method f-scale-factor
//     (index-op :: <integer>) => (scale :: <integer>)
//   select (index-op)
//     mf-single  => 4;
//     mf-double  => 8;
//   end select;
// end method;


define pentium-template (fld-index-scaled, dld-index-scaled, dld-index-scale-2)
  options (self);

  // Handle the case where the scale arg is constant
  pattern (be, i :: <pair> by op-info, d, r, s :: <integer>, o :: <integer>)
    let scale :: <integer> = i.tail;
    let i :: <integer> = i.head;
    i.fld-reapply-fn(be, d, r, (s * scale) + o);

  // Handle the case where the unscaled base arg is constant
  pattern (be, i :: <pair> by op-info, d, r :: <ac/const-ref> by colour, s, o :: <integer>)
    let scale :: <integer> = i.tail;
    let i :: <integer> = i.head;
    let tmps = list(reg--tmp1);
    let sn = ensure-mreg(be, s, tmps);
    let base = coerce-constant-with-offset(be, r, o);
    emit(be, flt-esc + i + 1);
    emit-reg-offset-scaled(be, sn, scale, base, mc-fld);
    unless (d.st-ref) i.fld-pop-ins(be, d) end;

  // The main case where we can make use of the 386 addressing modes.
  pattern (be, i :: <pair> by op-info, d, r, s, o :: <integer>)
    let scale :: <integer> = i.tail;
    let i :: <integer> = i.head;
    let tmps = list(reg--tmp1, reg--tmp2);
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    emit(be, flt-esc + i + 1);
    emit-double-index-scaled(be, sn, scale, rn, o, mc-fld);
    unless (d.st-ref) i.fld-pop-ins(be, d) end;

end pentium-template;



define pentium-template (fst)
  pattern (be, d by fspill/st0, r, s)
    canon(be, local-fn(can-fst), d, r, s);
end pentium-template;

define pentium-template (dst)
  pattern (be, d by fspill/st0, r, s)
    canon(be, local-fn(can-dst), d, r, s);
end pentium-template;


define constant can-fst = mf-single;
define constant can-dst = mf-double;

define method fst-push-ins (i :: <integer>) => (f :: <function>)
  if (i == can-fst)
    push-single;
  else
    push-double;
  end if;
end method;


define method fst-reapply-fn (ins) => (f :: <function>)
  select (ins)
    can-fst => local-fn(can-fst);
    can-dst => local-fn(can-dst);
  end select;
end method;



define local-pentium-template (can-fst, can-dst)
  options (self);

  pattern (be, i, s, base :: <real-register> by colour, offset :: <ac/const-ref> by colour)
    unless (s.st-ref) i.fst-push-ins(be, s) end;
    emit(be, flt-esc + i + 1);
    emit-reg-offset(be, base, offset, mc-fstp);
     
  pattern (be, i, s, base :: <real-register> by colour, offset :: <real-register> by colour)
    unless (s.st-ref) i.fst-push-ins(be, s) end;
    emit(be, flt-esc + i + 1);
    emit-reg-indexed(be, base, offset, mc-fstp);
     
  pattern (be, i, s, base :: <real-register> by colour, offset :: <ispill> by colour)
    harp-out (be) move(be, reg--tmp1, offset) end;
    i.fst-reapply-fn(be, s, base, reg--tmp1);

  pattern (be, i, s, base :: <ispill> by colour, offset :: <ac/const-ref> by colour)
    harp-out (be) move(be, reg--tmp1, base) end;
    i.fst-reapply-fn(be, s, reg--tmp1, offset);
     
  pattern (be, i, s, base :: <ispill> by colour, offset :: <ispill> by colour)
    harp-out (be) 
      move(be, reg--tmp1, base);
      move(be, reg--tmp2, offset);
    end harp-out;
    i.fst-reapply-fn(be, s, reg--tmp1, reg--tmp2);
     
  pattern (be, i, s, base :: <i-address-constant-reference>, offset :: <integer>)
    let new = coerce-constant-with-offset(be, base, offset);
    unless (s.st-ref) i.fst-push-ins(be, s) end;
    emit(be, flt-esc + i + 1);
    emit-reg-constant-offset(be, new, mc-fstp);

end local-pentium-template;



with-ops-in pentium-instructions (fst-index) info := mf-single end;
with-ops-in pentium-instructions (fst-index-scaled) info := pair(mf-single, 4) end;
with-ops-in pentium-instructions (dst-index) info := mf-double end;
with-ops-in pentium-instructions (dst-index-scaled) info := pair(mf-double, 8) end;
with-ops-in pentium-instructions (dst-index-scale-2) info := pair(mf-double, 2) end;


define pentium-template (fst-index, dst-index)
  options (self);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, r, s :: <ac/const-ref> by colour, o :: <integer>)
    let base = coerce-constant-with-offset(be, s, o);
    i.fst-reapply-fn(be, d, r, base);

  // Handle the case where 2 of the args are constants
  pattern (be, i :: <integer> by op-info, d, r :: <ac/const-ref> by colour, s, o :: <integer>)
    let base = coerce-constant-with-offset(be, r, o);
    i.fst-reapply-fn(be, d, s, base);

  // The main case where we can make use of the 386 addressing modes.
  pattern (be, i :: <integer> by op-info, d, r, s, o :: <integer>)
    let tmps = list(reg--tmp1, reg--tmp2);
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    unless (d.st-ref) i.fst-push-ins(be, d) end;
    emit(be, flt-esc + i + 1);
    emit-double-indexed(be, rn, sn, o, mc-fstp);

end pentium-template;

define pentium-template (fst-index-scaled, dst-index-scaled, dst-index-scale-2)
  options (self);

  // Handle the case where the scale arg is constant
  pattern (be, i :: <pair> by op-info, d, r, s :: <integer>, o :: <integer>)
    let scale :: <integer> = i.tail;
    let i :: <integer> = i.head;
    i.fst-reapply-fn(be, d, r, (s * scale) + o);

  // Handle the case where the unscaled base arg is constant
  pattern (be, i :: <pair> by op-info, d, r :: <ac/const-ref> by colour, s, o :: <integer>)
    let scale :: <integer> = i.tail;
    let i :: <integer> = i.head;
    let tmps = list(reg--tmp1);
    let sn = ensure-mreg(be, s, tmps);
    let base = coerce-constant-with-offset(be, r, o);
    unless (d.st-ref) i.fst-push-ins(be, d) end;
    emit(be, flt-esc + i + 1);
    emit-reg-offset-scaled(be, sn, scale, base, mc-fstp);

  // The main case where we can make use of the 386 addressing modes.
  pattern (be, i :: <pair> by op-info, d, r, s, o :: <integer>)
    let scale :: <integer> = i.tail;
    let i :: <integer> = i.head;
    let tmps = list(reg--tmp1, reg--tmp2);
    let rn = ensure-mreg(be, r, tmps);
    let sn = ensure-mreg(be, s, tmps);
    unless (d.st-ref) i.fst-push-ins(be, d) end;
    emit(be, flt-esc + i + 1);
    emit-double-index-scaled(be, sn, scale, rn, o, mc-fstp);

end pentium-template;




define pentium-template convert-to-single-float
  pattern (be, d, s)
    push-integer(be, s);
    pop-single(be, d);
end pentium-template;

define pentium-template convert-to-double-float
  pattern (be, d, s)
    push-integer(be, s);
    pop-double(be, d);
end pentium-template;


define pentium-template convert-from-single-float
  pattern (be, d, s)
    push-single(be, s);
    pop-integer(be, d);
end pentium-template;


define pentium-template convert-from-double-float
  pattern (be, d, s)
    push-double(be, s);
    pop-integer(be, d);
end pentium-template;


define pentium-template convert-to-single-float-x
  pattern (be, d, low, high)
    push-double-integer(be, low, high);
    pop-single(be, d);
end pentium-template;

define pentium-template convert-to-double-float-x
  pattern (be, d, low, high)
    push-double-integer(be, low, high);
    pop-double(be, d);
end pentium-template;


define pentium-template convert-from-single-float-x
  pattern (be, low, high, s)
    push-single(be, s);
    pop-double-integer(be, low, high);
end pentium-template;


define pentium-template convert-from-double-float-x
  pattern (be, low, high, s)
    push-double(be, s);
    pop-double-integer(be, low, high);
end pentium-template;


define pentium-template single-to-double-float
  pattern (be, d, s)
    push-single(be, s);
    pop-double(be, d);
end pentium-template;


define pentium-template double-to-single-float
  pattern (be, d, s)
    push-double(be, s);
    pop-single(be, d);
end pentium-template;


define constant move-to-freg-low    = 0;
define constant move-to-freg-high   = 4;
define constant move-from-freg-low  = 0;
define constant move-from-freg-high = 4;


define local-pentium-template (move-to-freg-low, move-to-freg-high)
  options (self);

  pattern (be, hilo, d :: <f-ic/spill-ref> by colour, data :: <real-register> by colour)
    emit(be, #x89);
    emit-f-c-spill-operand(be, d, data.ex-reg, hilo: hilo);

  pattern (be, hilo, d :: <f-ic/spill-ref> by colour, data :: <ac/const-ref> by colour)
    emit(be, #xc7);
    emit-f-c-spill-operand(be, d, 0, hilo: hilo);
    emit-immediate-constant(be, data);

  pattern (be, hilo, d :: <f-ic/spill-ref> by colour, data)
    harp-out (be) move(be, reg--tmp1, data) end;
    emit(be, #x89);
    emit-f-c-spill-operand(be, d, reg--tmp1.ex-reg, hilo: hilo);
end local-pentium-template;


define local-pentium-template (move-from-freg-low, move-from-freg-high)
  options (self);

  pattern (be, hilo, d :: <real-register> by colour, s :: <f-ic/spill-ref> by colour)
    emit(be, #x8b);
    emit-f-c-spill-operand(be, s, d.ex-reg, hilo: hilo);

  pattern (be, hilo, d, s :: <f-ic/spill-ref> by colour)
    emit(be, #x8b);
    emit-f-c-spill-operand(be, s, reg--tmp1.ex-reg, hilo: hilo);
    harp-out (be) move(be, d, reg--tmp1) end;

  pattern (be, hilo, d :: any, s :: <f-ic/spill-ref> by colour)
    #f;
end local-pentium-template;


define pentium-template move-to-sfreg
  pattern (be, d :: <sf-ic/spill-ref> by colour, s)
    call-local(move-to-freg-low, be, d, s);
end pentium-template;


define pentium-template move-from-sfreg
  pattern (be, d :: any, s :: <sf-ic/spill-ref> by colour)
    call-local(move-from-freg-low, be, d, s);
end pentium-template;


define pentium-template move-to-dfreg
  pattern (be, d :: <df-ic/spill-ref> by colour, low, high)
    call-local(move-to-freg-low, be, d, low);
    call-local(move-to-freg-high, be, d, high);
end pentium-template;


define pentium-template move-from-dfreg
  pattern (be, low :: any, high :: any, s :: <df-ic/spill-ref> by colour)
    call-local(move-from-freg-low, be, low, s);
    call-local(move-from-freg-high, be, high, s);
end pentium-template;


with-ops-in pentium-instructions (fmove) info := #f end;
with-ops-in pentium-instructions (dmove) info := #t end;


/// Preserving stack sanity for the stack of FP registers:
///
/// It's important that every stack push has a corresponding pop.
/// Normally this would happen as a matter of course for a FP operation
/// - but if a value is returned in ST, then we had better make sure
/// that the corresponding move of the value out of the register by the 
/// caller doesn't get eliminated. Hence, for the Pentium, fmove and dmove
/// are not eliminatable instructions - and if the destination register is
/// unallocated, then we just pop the FP stack. A corresponding rule is that
/// the harp-cg layer must ensure that ST is moved somewhere (with fmove or
/// dmove) after any call to a function which returns a result in ST.

with-ops-in pentium-instructions (fmove, dmove) 
  eliminatable := #f;
end with-ops-in;


define pentium-template (fmove, dmove)
  options (self);

  // first check for eliminatable case
  pattern (be, i :: <boolean> by op-info, d, s is d)
    #f;

  pattern (be, i :: <boolean> by op-info, d by st-ref, s)
   let double = i | instance?(s, <dfspill>);
   push-float(be, double, s);

  pattern (be, i :: <boolean> by op-info, d, s by st-ref)
   let double = i | instance?(d, <dfspill>);
   pop-float(be, double, d);

  pattern (be, i :: <boolean> by op-info, d, s)
   let double = i | instance?(d, <dfspill>);
   push-float(be, double, s);
   pop-float(be, double, d);

  pattern (be, i :: <boolean> by op-info, d :: any, s by st-ref)
   let double = i;
   pop-float(be, double, f-st);

  pattern (be, i :: <boolean> by op-info, d :: any, s)
   // The normal eliminatable case - do nothing
   #f;

end pentium-template;



define method rounding-mode (val) => (res)
  select (val)
    #"default"  => #x0000;
    #"round"    => #x0000;
    #"truncate" => #x0c00;
    #"floor"    => #x0400;
    #"ceiling"  => #x0800;
    otherwise   => #f;
  end select;
end method;


define constant mc-fstcw = #b111000;
define constant mc-fldcw = #b101000;


define pentium-template set-rounding-mode
  pattern (be, mode by rounding-mode)
    harp-out (be) push(be, reg--tmp1) end; // make some memory space
    emit(be, flt-esc + 1); // get the current rounding mode
    emit-reg-offset(be, reg--stack, 0, mc-fstcw);
    harp-out (be)
      and2-mem(be, reg--stack, 0, 0, #xf3ff);
      or2-mem(be, reg--stack, 0, 0, mode);
    end harp-out;
    emit(be, flt-esc + 1); // set the new rounding mode
    emit-reg-offset(be, reg--stack, 0, mc-fldcw);
    harp-out (be) pop(be, reg--tmp1) end;
end pentium-template;


// Suitably initialize the FPU control-word
//   unmask exceptions overflow, underflow, zero-divide
//   set rounding to nearest
//   set precision to 64 bits

define constant dylan-control-word = #x0363;

define pentium-template init-control-word
  pattern (be)
    emit(be, #x68);
    emit-immediate-constant(be, dylan-control-word);
    emit(be, flt-esc + 1);
    emit-reg-offset(be, reg--stack, 0, mc-fldcw);
    harp-out (be) add(be, reg--stack, reg--stack, 4) end;
end pentium-template;


// Suitably initialize the FPU

define pentium-template init-fpu
  pattern (be)
    emit(be, flt-esc + #b011, #b11100011);   // fninit
    harp-out (be) init-control-word(be) end;
end pentium-template;



// Clear exception bits in status-word

define pentium-template clear-float-exceptions
  pattern (be)
    emit(be, flt-esc + #b011, #b11100010);  // fnclex
end pentium-template;


define constant mc-fstsw = #b111000;


/*  Classify floats as follows:

    #x0      Unsupported
    #x100    Nan
    #x400    Normal finite number
    #x500    Infinity
    #x4000   Zero
    #x4100   Empty
    #x4400   Denormal number

*/


define pentium-template classify-float
  pattern (be, d, s)
    let double = instance?(s, <dfspill>);
    unless (st-ref(s))
      push-float(be, double, s);
    end;
    emit(be, flt-esc + 1, #b11100101);                    // fxam
    harp-out (be) push(be, 0) end;
    emit(be, flt-esc + #b101);                            // fnstsw dword ptr [esp]
    emit-reg-offset(be, reg--stack, 0, mc-fstsw);
    harp-out (be)
      and2-mem(be, reg--stack, 0, 0, #x4500);
    end harp-out;
    harp-out (be) pop(be, d) end;
    unless (st-ref(s))
      pop-float(be, double, s);
    end;
end pentium-template;


