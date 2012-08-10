module:    x86-harp
Synopsis:  Pentium floating point transcendental instructions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method fpu-duplicate (be :: <x86-back-end>)
  emit(be, #b11011001, #b11000000);
end method;
						       
// define method fpu-push-0 (be :: <x86-back-end>)
//   emit(be, #b11011001, #b11101110);
// end method;

define method fpu-push-1 (be :: <x86-back-end>)
  emit(be, #b11011001, #b11101000);
end method;

// define method fpu-push-pi (be :: <x86-back-end>)
//   emit(be, #b11011001, #b11101011);
// end method;

// define method fpu-push-log2-10 (be :: <x86-back-end>)
//   emit(be, #b11011001, #b11101001);
// end method;

define method fpu-push-log2-e (be :: <x86-back-end>)
  emit(be, #b11011001, #b11101010);
end method;

// define method fpu-push-loge-10 (be :: <x86-back-end>)
//   emit(be, #b11011001, #b11101100);
// end method;

define method fpu-push-loge-2 (be :: <x86-back-end>)
  emit(be, #b11011001, #b11101101);
end method;

define method fpu-push-2 (be :: <x86-back-end>)
  fpu-push-1(be); fpu-duplicate(be); fpu-add(be);
end method;
					  
define method fpu-add (be :: <x86-back-end>)
  emit(be, #b11011110, #b11000001);
end method;

define method fpu-sub (be :: <x86-back-end>)
  emit(be, #b11011110, #b11101001);
end method;

define method fpu-subr (be :: <x86-back-end>)
  emit(be, #b11011110, #b11100001);
end method;

define method fpu-mul (be :: <x86-back-end>)
  emit(be, #b11011110, #b11001001);
end method;

define method fpu-div (be :: <x86-back-end>)
  emit(be, #b11011110, #b11111001);
end method;

define method fpu-divr (be :: <x86-back-end>)
  emit(be, #b11011110, #b11110001);
end method;

define method fpu-halve (be :: <x86-back-end>)
  fpu-push-2(be); fpu-div(be);
end method;
					  
define method fpu-sqrt (be :: <x86-back-end>)
  emit(be, #b11011001, #b11111010);
end method;

define method fpu-discard (be :: <x86-back-end>)
  emit(be, #b11011101, #b11011000);
end method;

define method fpu-pop-down (be :: <x86-back-end>)
  emit(be, #b11011101, #b11011001);
end method;

define method fpu-exch (be :: <x86-back-end>)
  emit(be, #b11011001, #b11001001);
end method;

define method fpu-exch2 (be :: <x86-back-end>)
  emit(be, #b11011001, #b11001010);
end method;

define method fpu-round (be :: <x86-back-end>)
  emit(be, #b11011001, #b11111100);
end method;

define method fpu-extract (be :: <x86-back-end>)
  emit(be, #b11011001, #b11110100);
end method;

define method fpu-scale (be :: <x86-back-end>)
  emit(be, #b11011001, #b11111101);
end method;

define method fpu-raise-mant (be :: <x86-back-end>)
  emit(be, #b11011001, #b11110000);
end method;

define method fpu-logarithm (be :: <x86-back-end>)
  emit(be, #b11011001, #b11110001);
end method;

define method fpu-raw-tan (be :: <x86-back-end>)
  emit(be, #b11011001, #b11110010);
end method;

define method fpu-raw-arctan (be :: <x86-back-end>)
  emit(be, #b11011001, #b11110011);
end method;
					  
define method fpu-tan (be :: <x86-back-end>)
  fpu-raw-tan(be); fpu-discard(be);
end method;

define method fpu-atan (be :: <x86-back-end>)
  fpu-push-1(be);  fpu-raw-arctan(be);
end method;
					  
define method fpu-raise (be :: <x86-back-end>)
  fpu-duplicate(be); fpu-round(be); fpu-duplicate(be);
  fpu-exch2(be); fpu-subr(be); fpu-raise-mant(be); 
  fpu-push-1(be); fpu-add(be); fpu-scale(be); 
  fpu-pop-down(be);
end method;

define method fpu-raise-e (be :: <x86-back-end>)
  fpu-push-log2-e(be); fpu-mul(be); fpu-raise(be);
end method;

define method fpu-log-e (be :: <x86-back-end>)
  fpu-push-loge-2(be); fpu-exch(be); fpu-logarithm(be);
end method;
					  
define method fpu-one-minus-sq (be :: <x86-back-end>)
  fpu-duplicate(be); fpu-mul(be); fpu-push-1(be); 
  fpu-subr(be); fpu-sqrt(be);
end method;
					  
define method fpu-sinh (be :: <x86-back-end>)
  fpu-raise-e(be); fpu-duplicate(be); fpu-push-1(be); 
  fpu-divr(be); fpu-sub(be); fpu-halve(be);
end method;

define method fpu-cosh (be :: <x86-back-end>)
  fpu-raise-e(be); fpu-duplicate(be); fpu-push-1(be); 
  fpu-divr(be); fpu-add(be); fpu-halve(be);
end method;

define method fpu-tanh (be :: <x86-back-end>)
  fpu-duplicate(be); fpu-add(be); fpu-raise-e(be); 
  fpu-duplicate(be); fpu-push-1(be); fpu-sub(be);
  fpu-exch(be); fpu-push-1(be); fpu-add(be); fpu-div(be);
end method;

define method fpu-atanh (be :: <x86-back-end>)
  fpu-duplicate(be); fpu-push-1(be); fpu-add(be); 
  fpu-exch(be); fpu-push-1(be); fpu-subr(be); fpu-div(be);
  fpu-log-e(be); fpu-halve(be);
end method;


define method push-float (be :: <x86-back-end>, d, s)
  if (d) push-double(be, s) else push-single(be, s) end;
end method;

define method pop-float (be :: <x86-back-end>, d, s)
  if (d) pop-double(be, s) else pop-single(be, s) end;
end method;


//// For the following ops, the info field is encoded as the 387 trans code.

define constant abs-code = #b11100001;
with-ops-in pentium-instructions (dabs, fabs)
  info := abs-code;  // abs x
end with-ops-in;

define constant neg-code = #b11100000;
with-ops-in pentium-instructions (dneg, fneg)
  info := neg-code;  // neg x
end with-ops-in;

define constant cos-code = #b11111111;
with-ops-in pentium-instructions (dcos, fcos)
  info := cos-code;  // cos x
end with-ops-in;

define constant sin-code = #b11111110;
with-ops-in pentium-instructions (dsin, fsin)
  info := sin-code;  // sin x
end with-ops-in;

define constant sqrt-code = #b11111010;
with-ops-in pentium-instructions (dsqrt, fsqrt)
  info := sqrt-code;  // sqrt x
end with-ops-in;

define constant tan-code = #b11110010;
with-ops-in pentium-instructions (dtan, ftan)
  info := tan-code;  // tan x  (also pushes 1.0)
end with-ops-in;


//// For these ops, the 387 trans code is defined as a constant in the same 
//// way - but the info field is encoded with these constants:


define constant i387-one =     #b11101000;
// define constant i387-zero =    #b11101110;
// define constant i387-pi =      #b11101011;
define constant i387-log2-10 = #b11101001;
define constant i387-log2-e =  #b11101010;
define constant i387-log10-2 = #b11101100;
define constant i387-loge-2 =  #b11101101;


define constant log2-code = #b11110001;  // y log2 x
with-ops-in pentium-instructions (dlog2, flog2)
  info := i387-one;
end with-ops-in;

define constant log10-code = #b11110001;  // y log2 x also used for these bases
with-ops-in pentium-instructions (dlog10, flog10)
  info := i387-log10-2;
end with-ops-in;

define constant loge-code = #b11110001;  // y log2 x    // y pushed first
with-ops-in pentium-instructions (dloge, floge)
  info := i387-loge-2;
end with-ops-in;

define constant logep1-code = #b11111001;  // y log2 (x + 1)
with-ops-in pentium-instructions (dlogep1, flogep1)
  info := i387-loge-2;
end with-ops-in;

// define constant c2tox-code = #b11110000;  // 2^x - 1
with-ops-in pentium-instructions (d2tox, f2tox)
  info := i387-one;
end with-ops-in;

// define constant c10tox-code = #b11110000;  // 2^x - 1
with-ops-in pentium-instructions (d10tox, f10tox)
  info := i387-log2-10;
end with-ops-in;

// define constant etox-code = #b11110000;  // 2^x - 1
with-ops-in pentium-instructions (detox, fetox)
  info := i387-log2-e;
end with-ops-in;

// define constant etoxm1-code = #b11110000;  // 2^x - 1
with-ops-in pentium-instructions (detoxm1, fetoxm1)
  info := i387-log2-e;
end with-ops-in;



//// For these ops, the info field is encoded with #t or #f to 
//// indicate whether the operands are doubles.


//define constant atan-code = #b11110011; // atan (y/x) // y pushed first
with-ops-in pentium-instructions (fatan) info := #f end;
with-ops-in pentium-instructions (datan) info := #t end;

//define constant getman-code = #b11110100;  // FEXTRACT exponent and mantissa
with-ops-in pentium-instructions (dgetman) info := #f end;
with-ops-in pentium-instructions (fgetman) info := #t end;

//define constant getexp-code = #b11110100;  // FEXTRACT exponent and mantissa
with-ops-in pentium-instructions (dgetexp) info := #f end;
with-ops-in pentium-instructions (fgetexp) info := #t end;

// define constant int-code = #b11111100;  // FRNDINT
with-ops-in pentium-instructions (dint) info := #f end;
with-ops-in pentium-instructions (fint) info := #t end;


with-ops-in pentium-instructions (fasin, facos, fsinh, fcosh, ftanh, fatanh)
  info := #f;
end with-ops-in;

with-ops-in pentium-instructions (dasin, dacos, dsinh, dcosh, dtanh, datanh)
  info := #t;
end with-ops-in;





define pentium-template (fneg, fabs, fcos, fsin, fsqrt)
  options (self);

  pattern (be, i :: <integer> by op-info, d :: <sf-ic/spill-ref> by colour, s :: <sf-ic/spill-ref> by colour)
    push-single(be, s);
    emit(be, flt-esc + #b001, i);
    pop-single(be, d);

end pentium-template;


define pentium-template (dneg, dabs, dcos, dsin, dsqrt)
  options (self);

  pattern (be, i :: <integer> by op-info, d :: <df-ic/spill-ref> by colour, s :: <df-ic/spill-ref> by colour)
    push-double(be, s);
    emit(be, flt-esc + #b001, i);
    pop-double(be, d);

end pentium-template;


/*
define method pop-and-discard (be :: <x86-back-end>)
  emit(be, flt-esc + 5, #b11011000); // store-pop to top, that is ST(1)
end method;
*/


define pentium-template (ftan)
  pattern (be, d :: <sf-ic/spill-ref> by colour, s :: <sf-ic/spill-ref> by colour)
    push-single(be, s);
    fpu-tan(be);
    pop-single(be, d);
end pentium-template;


define pentium-template (dtan)
  pattern (be, d :: <df-ic/spill-ref> by colour, s :: <df-ic/spill-ref> by colour)
    push-double(be, s);
    fpu-tan(be);
    pop-double(be, d);
end pentium-template;


define method push-const (be :: <x86-back-end>, i387-bits :: <integer>)
  emit(be, flt-esc + 1,  i387-bits);
end method;

define method i387-trans-code (opinfo :: <integer>) => (i :: <integer>)
  select (opinfo)
    i387-one       => log2-code;
    i387-log10-2   => log10-code;
    i387-loge-2    => loge-code;
  end select;
end method;


define pentium-template (flog2, flog10, floge)
  options (self);

  // The op-info field contains i387-log10-2 etc.
  pattern (be, i :: <integer> by op-info, d :: <sf-ic/spill-ref> by colour, s :: <sf-ic/spill-ref> by colour)
    push-const(be, i);
    push-single(be, s);
    emit(be, flt-esc + #b001,  i.i387-trans-code);
    pop-single(be, d);

end pentium-template;


define pentium-template (dlog2, dlog10, dloge)
  options (self);

  // The op-info field contains i387-log10-2 etc.
  pattern (be, i :: <integer> by op-info, d :: <df-ic/spill-ref> by colour, s :: <df-ic/spill-ref> by colour)
    push-const(be, i);
    push-double(be, s);
    emit(be, flt-esc + #b001,  i.i387-trans-code);
    pop-double(be, d);

end pentium-template;



define pentium-template (flogep1)
  pattern (be, d :: <sf-ic/spill-ref> by colour, s :: <sf-ic/spill-ref> by colour)
    push-const(be, i387-loge-2);
    push-single(be, s);
    emit(be, flt-esc + #b001, logep1-code);
    pop-single(be, d);
end pentium-template;


define pentium-template (dlogep1)
  pattern (be, d :: <df-ic/spill-ref> by colour, s :: <df-ic/spill-ref> by colour)
    push-const(be, i387-loge-2);
    push-double(be, s);
    emit(be, flt-esc + #b001, logep1-code);
    pop-double(be, d);
end pentium-template;


/*
define method emit-fpu-duplicate (be :: <x86-back-end>)
  emit(be, flt-esc + #b001, #b11000000);
end method;
*/


define pentium-template (f2tox, f10tox, fetox)
  options (self);

  // The op-info field contains i387-log10-2 etc.
  pattern (be, i :: <integer> by op-info, d :: <sf-ic/spill-ref> by colour, s :: <sf-ic/spill-ref> by colour)
    emit-fpu-reset(be);
    /// thus an exponential operation will restore sanity for now!
    push-single(be, s);
    push-const(be, i);
    fpu-mul(be);
    fpu-raise(be);
    pop-single(be, d);

end pentium-template;


define pentium-template (d2tox, d10tox, detox)
  options (self);

  // The op-info field contains i387-log10-2 etc.
  pattern (be, i :: <integer> by op-info, d :: <df-ic/spill-ref> by colour, s :: <df-ic/spill-ref> by colour)
    emit-fpu-reset(be);
    /// thus an exponential operation will restore sanity for now!
    push-double(be, s);
    push-const(be, i);
    fpu-mul(be);
    fpu-raise(be);
    pop-double(be, d);

end pentium-template;



define pentium-template (fetoxm1)
  pattern (be, d :: <sf-ic/spill-ref> by colour, s :: <sf-ic/spill-ref> by colour)
    push-single(be, s);
    push-const(be, i387-log2-e);
    fpu-mul(be);
    fpu-raise-mant(be);
    pop-single(be, d);
end pentium-template;


define pentium-template (detoxm1)
  pattern (be, d :: <df-ic/spill-ref> by colour, s :: <df-ic/spill-ref> by colour)
    push-double(be, s);
    push-const(be, i387-log2-e);
    fpu-mul(be);
    fpu-raise-mant(be);
    pop-double(be, d);
end pentium-template;


define pentium-template (datan, fatan)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-atan(be);
    pop-float(be, i, d);

end pentium-template;


define pentium-template (dasin, fasin)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-duplicate(be);
    fpu-one-minus-sq(be);
    fpu-raw-arctan(be);
    pop-float(be, i, d);

end pentium-template;


define pentium-template (dacos, facos)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-duplicate(be);
    fpu-one-minus-sq(be);
    fpu-exch(be);
    fpu-raw-arctan(be);
    pop-float(be, i, d);

end pentium-template;



define pentium-template (fint, dint)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-round(be);
    pop-float(be, i, d);

end pentium-template;


define pentium-template (fgetman, dgetman)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-extract(be);
    pop-float(be, i, d);
    fpu-discard(be);

end pentium-template;


define pentium-template (fgetexp, dgetexp)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-extract(be);
    fpu-discard(be);
    pop-float(be, i, d);

end pentium-template;


define pentium-template (fsinh, dsinh)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-sinh(be);
    pop-float(be, i, d);

end pentium-template;


define pentium-template (fcosh, dcosh)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-cosh(be);
    pop-float(be, i, d);

end pentium-template;


define pentium-template (ftanh, dtanh)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-tanh(be);
    pop-float(be, i, d);

end pentium-template;


define pentium-template (fatanh, datanh)
  options (self);

  // The op-info field contains #t or #f to indicate a double
  pattern (be, i :: <boolean> by op-info, d :: <f-ic/spill-ref> by colour, s :: <f-ic/spill-ref> by colour)
    push-float(be, i, s);
    fpu-atanh(be);
    pop-float(be, i, d);

end pentium-template;





