module:    x86-harp
Synopsis:  Emitter functions for the Pentium backend.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// (c) Copyright Functional Objects, Inc. 1988
///
/// Begun AJW 19/7/88  
/// Modified for PC, TonyM 11/11/91

//// Basic utilities to support the Pentium backend.

// ex-reg returns the number of its register argument shifted up by 3

define method ex-reg (reg :: <real-register>) => (i :: <integer>)
  ash(reg.real-register-number, 3);
end method;

// ex-hi-byte-reg returns the number of its H register argument shifted
// up by 3 

// define method ex-hi-byte-reg (reg :: <real-register>) => (i :: <integer>)
//   ash(reg.real-register-number + 4, 3);
// end method;

define method one-byte (x :: <integer>) => (i :: <integer>)
  logand(x, #xff);
end method;

define method emit-one-byte (be :: <harp-x86-back-end>, x :: <integer>) 
  emit(be, one-byte(x));
end method;

/// Detect if this is one of the magic 4 registers that we can use as byte
/// operands.

define method byte-reg-ref (x)
  if (x == eax | x == ebx | x == ecx | x == edx)
    x;
  else
    #f;
  end if;
end method;

    
/// The code emission stuff has been changed to avoid ever generating
/// raw values, since these are not supported on the PC. (TonyM 11/10/91)



define method emit-four-bytes (backend :: <harp-x86-back-end>, x :: <abstract-integer>)
  emit(backend,
       generic-logand(x, #xff),
       logand(generic-ash(x, -8), #xff),
       logand(generic-ash(x, -16), #xff),
       logand(generic-ash(x, -24), #xff));
end method;


define method emit-four-bytes (backend :: <harp-x86-back-end>, x :: <integer>)
  emit(backend,
       logand(x, #xff),
       logand(ash(x, -8), #xff),
       logand(ash(x, -16), #xff),
       logand(ash(x, -24), #xff));
end method;


define method four-bytes (x :: <abstract-integer>) => (l :: <list>)
  list(generic-logand(x, #xff),
       logand(generic-ash(x, -8), #xff),
       logand(generic-ash(x, -16), #xff),
       logand(generic-ash(x, -24), #xff));
end method;



define method four-bytes (x :: <integer>) => (l :: <list>)
  list(logand(x, #xff),
       logand(ash(x, -8), #xff),
       logand(ash(x, -16), #xff),
       logand(ash(x, -24), #xff));
end method;


define method emit-two-bytes (backend :: <harp-x86-back-end>, x :: <integer>)
  emit(backend,
       logand(x, #xff),
       logand(ash(x, -8), #xff));
end method;


// define method two-bytes (x :: <integer>) => (l :: <list>)
//   list(logand(x, #xff),
//        logand(ash(x, -8), #xff));
// end method;


/// Here are the encodings for the mod r/m and sib bytes to reflect the
/// addressing modes that we want to use. Basically we use [reg], [reg,8],
/// [reg,32], reg32, reg16 (used to store halves, A B C D only), [reg,reg],
/// reg8 (used to store bytes, A B C D only).

/// I'm assuming that we're operating in 32 bit mode, which I think means that
/// we have to ensure that some bits are set in the segment table. It looks as
/// though this stuff is all set up for us by SunOs (muted cheering).

define constant mod00 = #b00000000;
define constant mod01 = #b01000000;
define constant mod10 = #b10000000;
define constant mod11 = #b11000000;

define constant scale1 = #b00000000;
define constant scale2 = #b01000000;
define constant scale4 = #b10000000;
define constant scale8 = #b11000000;


define method scale-index (scale :: <integer>) => (index :: <integer>)
  select (scale)
    1 => scale1;
    2 => scale2;
    4 => scale4;
    8 => scale8;
  end select;
end method;


/// Note - in this file the extension triplets are assumed already to be
/// aligned into the right place for putting into the word. No masking or
/// shifting is done.

define method emit-reg-indir 
    (backend :: <harp-x86-back-end>, reg :: <real-register>, ex :: <integer>)
  let rn :: <integer> = reg.real-register-number;
  select (rn)                                // Can you say 'Orthogonal'?
    4 => emit(backend, ex + 4, 36);          // [sp] with SIB and no index
    5 => emit(backend, mod01 + ex + 5, 0);   // [bp,8] with zero offset
    otherwise => emit(backend, ex + rn);
  end select;
end method;

define method emit-constant-operand 
    (backend :: <harp-x86-back-end>, 
     const-ref :: <constant-reference>, 
     ex :: <integer>)
// PC specific: Emit an indirect constant reference
  emit(backend, mod00 + ex + 5);
  emit-constant-ref(backend, const-ref);
end method;
	      
define method emit-spill-operand 
    (backend :: <harp-x86-back-end>, spill :: <spill>, ex :: <integer>)
  let arg-spill :: <boolean> = arg-spill?(spill);
  let offset :: <integer> = 
    if (arg-spill)
      arg-offset(backend, spill)
    else
      signed-frame-pointer-offset(backend, spill);
    end if;

   case
     (arg-spill & ~ backend.variables.with-stack) =>
       emit-reg-offset(backend, backend.registers.reg-stack, offset, ex);
     (signed-eight-bits?(offset)) =>
       emit(backend, mod01 + ex + 5);
       emit-one-byte(backend, offset);
     otherwise =>
       emit(backend, mod10 + ex + 5);
       emit-four-bytes(backend, offset);
   end case;
end method;
	      

/// EMIT-REG-OFFSET supports address constants as offsets too now.

define method emit-reg-offset
   (backend :: <harp-x86-back-end>, reg :: <real-register>,
    offset == 0, ex :: <integer>)
  emit-reg-indir(backend, reg, ex);
end method;

define method emit-reg-offset
   (backend :: <harp-x86-back-end>, reg :: <real-register>,
    offset :: <constant-reference>, ex :: <integer>)
  let rn :: <integer> = reg.real-register-number;
  if (rn == 4)
    emit(backend, mod10 + ex + 4, 36);
    emit-constant-ref(backend, offset);
  else
    emit(backend, mod10 + ex + rn);
    emit-constant-ref(backend, offset);
  end if;
end method;


define method emit-reg-offset
   (backend :: <harp-x86-back-end>, reg :: <real-register>,
    offset :: <abstract-integer>, ex :: <integer>)
  let rn :: <integer> = reg.real-register-number;
  if (signed-eight-bits?(offset))
    if (rn == 4)           // SP is the exception
      emit(backend, mod01 + ex + 4, 36);
    else
      emit(backend, mod01 + ex + rn);
    end if;
    emit-one-byte(backend, offset);
  else
    if (rn == 4)           // SP is the exception
      emit(backend, mod10 + ex + 4, 36);
    else
      emit(backend, mod10 + ex + rn);
    end if;
    emit-four-bytes(backend, offset);
  end if;
end method;


define method emit-reg-offset-scaled
   (backend :: <harp-x86-back-end>, 
    reg :: <real-register>, scale :: <integer>,
    offset, ex :: <integer>)
  emit(backend, mod00 + ex + 4);
  emit(backend, scale.scale-index + reg.ex-reg + 5);
  emit-immediate-constant(backend, offset);
end method;


define method emit-reg-constant-offset 
    (backend :: <harp-x86-back-end>, offset, ex :: <integer>)
  emit(backend, mod00 + ex + 5);
  emit-immediate-constant(backend, offset);
end method;


/// reg-direct assumes that the w bit (ls bit of the instruction) is 1 (for
/// full 32 bit operation.

define method emit-reg-direct
    (backend :: <harp-x86-back-end>, reg :: <real-register>, ex :: <integer>)
  emit(backend, mod11 + ex + reg.real-register-number);
end method;

/// reg-indexed gives us one register offset by the other, no scaling.
/// WARNING - for speed, this code doesn't dothe check to see if we're asking
/// for SP,SP or BP,BP. These are the two cases that we cannot do without a
/// little extra chicanery.

define method emit-reg-indexed
    (backend :: <harp-x86-back-end>, 
     reg :: <real-register>, index :: <real-register>,
     ex :: <integer>)
  let r-n :: <integer> = reg.real-register-number;
  let i-n :: <integer> = index.real-register-number;
  if (r-n == 5)    // if base is BP then reverse base and index
    emit(backend, ex + 4, ash(r-n, 3) + i-n);
  else
    emit(backend, ex + 4, ash(i-n, 3) + r-n);
  end if;
end method;


/// emit-double-indexed gives us one register offset by the other, no
/// scaling, with a possible integer offset. This is for memory
/// operands.  
        
define method emit-double-indexed
    (backend :: <harp-x86-back-end>, 
     reg1 :: <real-register>, reg2 :: <real-register>,
     offset :: <abstract-integer>, ex :: <integer>)
  let mod :: <integer> =
    case
      offset == 0                => mod00;
      signed-eight-bits?(offset) => mod01;
      otherwise                  => mod10;
    end case;
  let rn1 :: <integer> = reg1.real-register-number;
  let rx2 :: <integer> = ex-reg(reg2);
  emit(backend, mod + ex + 4);
  emit(backend, rx2 + rn1);
  case
    mod == mod01 => emit-one-byte(backend, offset);
    mod == mod10 => emit-four-bytes(backend, offset);
  end case;
end method;


/// emit-double-index-scaled gives us one scaled register offset 
/// by another which is not scaled, with a possible integer offset. 
/// This is for array operands.  
        
define method emit-double-index-scaled
    (backend :: <harp-x86-back-end>, 
     reg1 :: <real-register>, scale :: <integer>, 
     reg2 :: <real-register>, offset :: <abstract-integer>, 
     ex :: <integer>)
  let mod :: <integer> =
    case
      offset == 0                => mod00;
      signed-eight-bits?(offset) => mod01;
      otherwise                  => mod10;
    end case;
  let rx1 :: <integer> = ex-reg(reg1);
  let rn2 :: <integer> = reg2.real-register-number;
  emit(backend, mod + ex + 4);
  emit(backend, scale.scale-index + rx1 + rn2);
  case
    mod == mod01 => emit-one-byte(backend, offset);
    mod == mod10 => emit-four-bytes(backend, offset);
  end case;
end method;


define method emit-m-spill-dest 
    (backend :: <harp-x86-back-end>, 
     x :: <real-register>, 
     extension :: <integer>)
  emit-reg-direct(backend, x, extension);
end method;

define method emit-m-spill-dest 
    (backend :: <harp-x86-back-end>, 
     x :: <spill>, 
     extension :: <integer>)
  emit-spill-operand(backend, x, extension);
end method;


/// The following function recognises the similarity between spill
/// references and PC indirect constant refs in terms of addressing
/// modes. 
define method emit-m-c-spill-dest 
    (backend :: <harp-x86-back-end>, x, extension :: <integer>)
  if (indirect-constant-ref(x))
    emit-constant-operand(backend, x, extension);
  else
    emit-m-spill-dest(backend, x, extension);
  end if;
end method;


/// The following function recognises the similarity between 32 bit
/// constants and PC address constants.
define method emit-immediate-constant
    (backend :: <harp-x86-back-end>, x :: <abstract-integer>)
  emit-four-bytes(backend, x);
end method;

define method emit-immediate-constant
    (backend :: <harp-x86-back-end>, x :: <constant-reference>)
  emit-constant-ref(backend, x);
end method;

  
/* currently unused ...

/// It looks as though the exchange instruction is so useful, I'm going to
/// code it up as a 386 template. Assume for the moment that this is only
/// between registers.

define method xcg
    (backend :: <harp-x86-back-end>, one == eax, two :: <real-register>)
  emit(backend, #x90 + two.real-register-number);
end method;

define method xcg
    (backend :: <harp-x86-back-end>, one :: <real-register>, two == eax)
  emit(backend, #x90 + one.real-register-number);
end method;

define method xcg
    (backend :: <harp-x86-back-end>, 
     one :: <real-register>, two :: <real-register>)
  emit(backend, #x87);
  emit-reg-direct(backend, one, ex-reg(two));
end method;
*/


// define method dec-register 
//     (backend :: <harp-x86-back-end>, reg :: <real-register>)
//   emit(backend, #x48 + reg.real-register-number);
// end method;



/// Random things


define method byte-addressable (r)
  byte-reg-ref(r) | ic/spill-ref(r);
end method;

define method code-item-increment 
     (backend :: <harp-x86-back-end>) => (i :: <integer>)
  1;
end method;


define sideways method return-address-on-stack?
    (be :: <harp-x86-back-end>) => (on-stack? :: <boolean>)
  #t
end method;

define method indirect-runtime-reference 
    (name :: <byte-string>) => (c :: <constant-reference>)
  make(<constant-reference>, 
       refers-to: name,
       address-mode: #"indirect",
       const-offset: 0);
end method;

define method thread-local-runtime-reference
(name :: <byte-string>) => (c :: <constant-reference>)
  make(<i-thread-constant-reference>, 
       refers-to: name,
       const-offset: 0);
end method;
    

define method output-implicit-externals
    (backend :: <harp-x86-unix-back-end>, outputter :: <harp-outputter>)
  output-external(backend, outputter, remove-optionals-runtime);
end method;
