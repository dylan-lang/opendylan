module:    powerpc-harp
Synopsis:  PowerPC register model
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// The register model definition for the PowerPC backend


define class <powerpc-register-model> (<register-model>)
end;


define class <integer-register> (<real-register>)
end class;

define class <float-register> (<real-register>)
end class;

define class <special-register> (<real-register>)
end class;


define method ppc-register-number (x)
  x.real-register-number;
/*
  let y = x.real-register-number;
  select (x by instance?)
    <integer-register> => y;
    <float-register> => y - 32;
    <special-register> => y - 64;
    otherwise =>
      error("powerpc-harp: %= is not a PowerPC register", x)
  end
*/
end method ppc-register-number;

define inline method make-integer-register
    (number :: <integer>, pname :: <string>, mask-bit :: <integer>, 
     #key pmask = -1, cpmask = -1) 
      => (new :: <integer-register>)
  make(<integer-register>, 
       number: number, pname: pname, mask: ash(1, mask-bit),
       preserved-mask: ash(1, pmask),
       c-preserved-mask: ash(1, cpmask));
end;

define inline method make-float-register
    (number :: <integer>, pname :: <string>, mask-bit :: <integer>, 
     #key pmask = -1, cpmask = -1) 
    => (new :: <float-register>)
  make(<float-register>, 
        number: number, pname: pname, mask: ash(1, mask-bit));
end;

define inline method make-special-register
    (number :: <integer>, pname :: <string>) 
    => (new :: <special-register>)
  make(<special-register>, 
        number: number, pname: pname, mask: 0);
end;



// define the registers and their numbers

define constant r0 =  make-integer-register(0, "r0", 27);  // arg-count
define constant r1 =  make-integer-register(1, "r1", -1, cpmask: 0); // stack
define constant r2 =  make-integer-register(2, "r2", -1, cpmask: 1); // TOC for aix
define constant r3 =  make-integer-register(3, "r3", 0);  // arg 1
define constant r4 =  make-integer-register(4, "r4", 1);  // arg 2
define constant r5 =  make-integer-register(5, "r5", 2);  // arg 3
define constant r6 =  make-integer-register(6, "r6", 3);  // arg 4
define constant r7 =  make-integer-register(7, "r7", -1); // tmp1
define constant r8 =  make-integer-register(8, "r8", -1); // tmp2
define constant r9 =  make-integer-register(9, "r9", 4); // tmp3
define constant r10 = make-integer-register(10, "r10", 5); // tmp4
define constant r11 = make-integer-register(11, "r11", 6); // tmp5
define constant r12 = make-integer-register(12, "r12", 7);
define constant r13 = make-integer-register(13, "r13", 8, cpmask: 2);
define constant r14 = make-integer-register(14, "r14", 9, cpmask: 3);
define constant r15 = make-integer-register(15, "r15", 10, cpmask: 4);
define constant r16 = make-integer-register(16, "r16", 11, cpmask: 5);
define constant r17 = make-integer-register(17, "r17", 12, cpmask: 6);
define constant r18 = make-integer-register(18, "r18", 13, cpmask: 7);
define constant r19 = make-integer-register(19, "r19", 14, cpmask: 8);
define constant r20 = make-integer-register(20, "r20", 15, cpmask: 9);
define constant r21 = make-integer-register(21, "r21", 16, cpmask: 10);
define constant r22 = make-integer-register(22, "r22", 17, cpmask: 11);  // funct
define constant r23 = make-integer-register(23, "r23", 18, cpmask: 12);  // mlist
define constant r24 = make-integer-register(24, "r24", -1, cpmask: 13);  // constants
define constant r25 = make-integer-register(25, "r25", -1, cpmask: 14);  // teb
define constant r26 = make-integer-register(26, "r26", 19, pmask: 0, cpmask: 15);
define constant r27 = make-integer-register(27, "r27", 20, pmask: 1, cpmask: 16);
define constant r28 = make-integer-register(28, "r28", 21, pmask: 2, cpmask: 17);
define constant r29 = make-integer-register(29, "r29", 22, pmask: 3, cpmask: 18);
// frame
define constant r30 = make-integer-register(30, "r30", -1, cpmask: 19);
// return address
define constant r31 = make-integer-register(31, "r31", -1, cpmask: 20);

define constant f0 =  make-float-register(0, "f0", -1);  // ftmp1
define constant f1 =  make-float-register(1, "f1", 23);
define constant f2 =  make-float-register(2, "f2", 24);
define constant f3 =  make-float-register(3, "f3", 25);
define constant f4 =  make-float-register(4, "f4", 26);
define constant f5 =  make-float-register(5, "f5", -1);
define constant f6 =  make-float-register(6, "f6", -1);
define constant f7 =  make-float-register(7, "f7", -1);  // ftmp2
define constant f8 =  make-float-register(8, "f8", -1);  // ftmp3
define constant f9 =  make-float-register(9, "f9", -1);  // ftmp4
define constant f10 = make-float-register(10, "f10", -1); // ftmp5
define constant f11 = make-float-register(11, "f11", -1); // ftmp6
define constant f12 = make-float-register(12, "f12", -1); // ftmp7
define constant f13 = make-float-register(13, "f13", -1);
define constant f14 = make-float-register(14, "f14", -1, cpmask: 21);
define constant f15 = make-float-register(15, "f15", -1, cpmask: 22);
define constant f16 = make-float-register(16, "f16", -1, cpmask: 23);
define constant f17 = make-float-register(17, "f17", -1, cpmask: 24);
define constant f18 = make-float-register(18, "f18", -1, cpmask: 25);
define constant f19 = make-float-register(19, "f19", -1, cpmask: 26);
define constant f20 = make-float-register(20, "f20", -1, cpmask: 27);
define constant f21 = make-float-register(21, "f21", -1, cpmask: 28);
define constant f22 = make-float-register(22, "f22", -1, cpmask: 29);
define constant f23 = make-float-register(23, "f23", -1, cpmask: 30);
define constant f24 = make-float-register(24, "f24", -1, cpmask: 31);
define constant f25 = make-float-register(25, "f25", -1, cpmask: 32);
define constant f26 = make-float-register(26, "f26", -1, cpmask: 33);
define constant f27 = make-float-register(27, "f27", -1, cpmask: 34);
define constant f28 = make-float-register(28, "f28", -1, cpmask: 35);
define constant f29 = make-float-register(29, "f29", -1, cpmask: 36);
define constant f30 = make-float-register(30, "f30", -1, cpmask: 37);
define constant f31 = make-float-register(31, "f31", -1, cpmask: 38);

// define constant mq =   make-special-register(0, "mq");
define constant xer =  make-special-register(1, "xer");
// define constant rtcu = make-special-register(4, "rtcu");
// define constant rtcl = make-special-register(5, "rtcl");
// define constant dec =  make-special-register(6, "dec");
define constant lr =   make-special-register(8, "lr");
define constant ctr =  make-special-register(9, "ctr");


define constant $ev = vector();

define method initialize
    (model :: <powerpc-register-model>, #key) 
     => (new :: <powerpc-register-model>)
  next-method();

  model.reg-frame := reg--frame;
  model.reg-stack := reg--stack;
  model.reg-environment := reg--function;
  model.reg-function := reg--function;
  model.reg-mlist := reg--mlist;
  model.reg-result-out := reg--arg0;
  model.reg-result := reg--arg0;
  model.reg-float-result := reg--float-arg0;
  model.reg-arg-count := reg--arg-count;
  model.reg-arg0 := reg--arg0;
  model.reg-float-arg0 := reg--float-arg0;
  model.reg-machine-arguments := powerpc-machine-arguments;
  model.reg-c-machine-arguments := powerpc-c-machine-arguments;
  model.reg-float-machine-arguments := powerpc-float-machine-arguments;
  model.reg-c-float-machine-arguments := powerpc-c-float-machine-arguments;
  model.reg-arg-masks := powerpc-arg-masks;
  model.reg-arg-masks-out := powerpc-arg-masks-out;
  model.reg-c-arg-masks := powerpc-c-arg-masks;
  model.reg-c-arg-masks-out := powerpc-c-arg-masks-out;
  model.reg-tmp1 := reg--tmp1;
  model.reg-tmp2 := reg--tmp2;
  model.reg-tmp3 := reg--tmp3;

  model.reg-c-result := reg--arg0;
  model.reg-c-frame := r2;  // N/A ???
  model.reg-c-float-result := reg--float-arg0;
  model.reg-c-stack := reg--stack;

  model.real-register-vector := powerpc-real-registers;
  model.preserved-register-vector := powerpc-preserved-registers;
  model.c-preserved-register-vector := powerpc-c-preserved-registers;
  model.savable-registers := powerpc-savable-registers;
  model.arguments-passed-in-registers := size(powerpc-machine-arguments);
  model.float-arguments-passed-in-registers := size(powerpc-float-machine-arguments);
  model.c-arguments-passed-in-registers := size(powerpc-c-machine-arguments);
  model.c-float-arguments-passed-in-registers := size(powerpc-c-float-machine-arguments);
  model.all-allocatable := powerpc-allocatable-registers;
  model.not-preserved := powerpc-not-preserved;
  model.c-not-preserved := powerpc-c-not-preserved;

  model

end;


define constant powerpc-real-registers =
  vector(r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13,
         r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25,
         r26, r27, r28, r29, r30, r31, f0, f1, f2, f3, f4, f5, f6,
         f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18,
         f19, f20, f21, f22, f23, f24, f25, f26, f27, f28, f29, f30,
         f31);


/// Sometimes we need the registers in their "correct" order ...

define constant ordered-register-vector =
  vector(r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13,
         r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25,
         r26, r27, r28, r29, r30, r31);

define method register-from-number (n)
  ordered-register-vector[n]
end method register-from-number;

define constant powerpc-preserved-registers =
  vector(r26, r27, r28, r29);  // last 4 saved regs


define constant powerpc-savable-registers =
  vector(r0, r1, r2, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14,
         r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26,
         r27, r28, r29, r30, r31);


define constant powerpc-c-preserved-registers =
  vector(r1, r2, r13, r14, r15,
	 r16, r17, r18, r19, r20,
	 r21, r22, r23, r24, r25,
	 r26, r27, r28, r29, r30, r31,
	 f14, f15, f16, f17, f18,
         f19, f20, f21, f22, f23,
	 f24, f25, f26, f27, f28,
	 f29, f30, f31);

define constant powerpc-c-not-preserved =
  vector(r0, r3, r4, r5, r6,
	 r7, r8, r9, r10, r11, r12,
	 f0, f1, f2, f3, f4, f5, f6,
         f7, f8, f9, f10, f11, f12, f13);



// added on 21/11/89

define method the-real-dfreg 
    (backend :: <powerpc-back-end>, x :: <real-register>) 
     => (reg :: <real-register>)
  x;
end;



define constant reg--arg0 = r3;
// define constant reg--arg1 = r4;
// define constant reg--arg2 = r5;
// define constant reg--arg3 = r6;

define constant reg--float-arg0  = f1;


define constant zero-args-mask = rset-from-args();

define constant powerpc-arg-masks = 
  as(<simple-integer-vector>,
     vector(zero-args-mask,
	    rset-from-args(r3),
	    rset-from-args(r3, r4),
	    rset-from-args(r3, r4, r5),
	    rset-from-args(r3, r4, r5, r6)));

define constant powerpc-arg-masks-out =
  as(<simple-integer-vector>,
     vector(zero-args-mask,
	    rset-from-args(r3),
	    rset-from-args(r3, r4),
	    rset-from-args(r3, r4, r5),
	    rset-from-args(r3, r4, r5, r6),
	    rset-from-args(r3, r4, r5, r6),
	    rset-from-args(r4, r5, r6),
	    rset-from-args(r5, r6),
	    rset-from-args(r6)));

define constant powerpc-c-arg-masks = 
  as(<simple-integer-vector>,
     vector(zero-args-mask,
	    zero-args-mask,
	    zero-args-mask,
	    zero-args-mask, 
	    zero-args-mask));

define constant powerpc-c-arg-masks-out = 
  as(<simple-integer-vector>,
     vector(zero-args-mask,
	    zero-args-mask,
	    zero-args-mask,
	    zero-args-mask, 
	    zero-args-mask,
	    zero-args-mask,
	    zero-args-mask,
	    zero-args-mask, 
	    zero-args-mask));


define constant powerpc-machine-arguments =
  vector(r3, r4, r5, r6);

define constant powerpc-float-machine-arguments =
  vector(f1, f2, f3, f4);

define constant powerpc-c-machine-arguments =
  vector(r3, r4, r5, r6, r7, r8, r9, r10);

define constant powerpc-c-float-machine-arguments =
  vector(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13);


define constant reg--tmp1 = r7;   // not allocatable
define constant reg--tmp2 = r8;   // not allocatable
define constant reg--tmp3 = r9;   // allocatable
define constant reg--tmp4 = r10;  // allocatable
define constant reg--tmp5 = r11;  // allocatable
define constant reg--tmp6 = r12;  // allocatable
define constant reg--tmp7 = r13;  // allocatable

define constant reg--arg-count = r0; // May want to move this ??
define constant reg--function = r22; // allocatable
define constant reg--mlist = r23; // allocatable

// our caller's info:

define constant reg--frame = r30; // a saved reg 
define constant reg--stack = r1;  // convention

define constant reg--toc = r2;

define constant reg--constants = r24;
define constant reg--teb = r25;
define constant reg--link = r31;

define constant reg--ftmp1 = f0;    /// we can afford to use several
define constant reg--ftmp2 = f7;    /// registers as FP temps
// define constant reg--ftmp3 = f8;
// define constant reg--ftmp4 = f9;
// define constant reg--ftmp5 = f10;
// define constant reg--ftmp6 = f11;
// define constant reg--ftmp7 = f12;

/// we only have 28 allocatable regs, but this is probably enough

define constant powerpc-allocatable-registers =
  vector(r3, r4, r5, r6, r9, r10, r11, r12, r13, r14, r15, r16, r17,
	 r18, r19, r20, r21, r22, r23, r26, r27, r28, r29, f1, f2, f3,
	 f4);

define constant powerpc-integer-allowable-colours =
  rset-from-args(r3, r4, r5, r6, r9, r10, r11, r12, r13, r14, r15,
                 r16, r17, r18, r19, r20, r21, r22, r23, r26,
                 r27, r28, r29);


/// We may want more allocatable floating registers - at the expense of GPRs

define constant powerpc-floating-allowable-colours =
  rset-from-args(f1, f2, f3, f4);


/*
define constant powerpc-ok-for-non-gc =
  rset-from-args(                 r3,   r4,   r5,   r6,
		       r9,  r10,  r11,  r12,  r13,  r14,  r15,
                 r16,  r17, r18,  r19,  r20,  r21,  r22,  r23);
*/

// a subset of all-allocatable

define constant powerpc-not-preserved =
  vector(              r3,   r4,   r5,   r6,
		       r9,  r10,  r11,  r12,  r13,  r14,  r15,
                 r16,  r17, r18,  r19,  r20,  r21,  r22,  r23,
		       f1,  f2,   f3,   f4);


define method allowable-colours
    (backend :: <powerpc-back-end>, vr :: <virtual-register>) 
 => (i :: <integer>)
  if (instance?(vr, <floating-virtual-register>))
    powerpc-floating-allowable-colours   // all floating allocatable
  else
    powerpc-integer-allowable-colours
  end if;
end;

define method make-pref-vector
    (backend :: <powerpc-back-end>) => (vec :: <simple-integer-vector>)
  let pref-size = powerpc-real-registers.size;
  make(<simple-integer-vector>, size: pref-size, fill: 0);
end;

// disallows

define constant tmp3-fn = constant-fn(vector(reg--tmp3));

define constant tmp7-fn = constant-fn(vector(reg--tmp7));

define constant tmp34-fn =
  constant-fn(vector(reg--tmp3, reg--tmp4));

define constant tmp45-fn =
  constant-fn(vector(reg--tmp4, reg--tmp5));

define constant tmp345-fn =
  constant-fn(vector(reg--tmp3, reg--tmp4, reg--tmp5));

define constant tmp34567-fn =
  constant-fn(vector(reg--tmp3, reg--tmp4, reg--tmp5, reg--tmp6, reg--tmp7));


// Some useful functions for c-preserved-destroys-fns

define constant all-c-preserved-fn = constant-fn(powerpc-c-preserved-registers);


define variable powerpc-registers = make(<powerpc-register-model>);



/// from the  Risc System 6000 Assembler Language Reference book
///
/// reg      Preserved     Convention        Preserved     Convention
///            in C          in C             in Dylan      in Dylan
///
/// r0         no          Prologs            no            Arg count
/// r1         yes         Stack Pointer      no            Stack Pointer
/// r2         yes         TOC                no            TOC
/// r3         no          Arg1 / Ret1        no            Arg1 / Ret1
/// r4         no          Arg2 / Ret2        no            Arg2
/// r5         no          Arg3 / Ret3        no            Arg3
/// r6         no          Arg4 / Ret4        no            Arg4
/// r7         no          Arg5 / Ret5        no            tmp1
/// r8         no          Arg6 / Ret6        no            tmp2
/// r9         no          Arg7 / Ret7        no            tmp3 / alloc
/// r10        no          Arg8 / Ret8        no            tmp4 / alloc
/// r11        no          Scratch            no            tmp5 / alloc
/// r12        no          Scratch            no            alloc
/// r13        yes         Non-volatile       no            alloc
/// r14        yes         Non-volatile       no            alloc
/// r15        yes         Non-volatile       no            alloc
/// r16        yes         Non-volatile       no            alloc
/// r17        yes         Non-volatile       no            alloc
/// r18        yes         Non-volatile       no            alloc
/// r19        yes         Non-volatile       no            alloc
/// r20        yes         Non-volatile       no            alloc
/// r21        yes         Non-volatile       no            alloc
/// r22        yes         Non-volatile       no            alloc
/// r23        yes         Non-volatile       no            Function/alloc
/// r24        yes         Non-volatile       yes           NIL
/// r25        yes         Non-volatile       yes           preserved
/// r26        yes         Non-volatile       yes           preserved
/// r27        yes         Non-volatile       yes           preserved
/// r28        yes         Non-volatile       yes           preserved
/// r29        yes         Non-volatile       yes           Frame
/// r30        yes         Non-volatile       yes           Constants
/// r31        yes         Non-volatile       yes           Return Address
///
/// f0         no          Scratch            no            ftmp1
/// f1         no          FP Arg1  / Ret1    no            alloc
/// f2         no          FP Arg2  / Ret2    no            alloc
/// f3         no          FP Arg3  / Ret3    no            alloc
/// f4         no          FP Arg4  / Ret4    no            alloc
/// f5         no          FP Arg5  / Ret5    no            alloc
/// f6         no          FP Arg6  / Ret6    no            alloc
/// f7         no          FP Arg7  / Ret7    no            ftmp2
/// f8         no          FP Arg8  / Ret8    no            -
/// f9         no          FP Arg9  / Ret9    no            -
/// f10        no          FP Arg10 / Ret10   no            -
/// f11        no          FP Arg11 / Ret11   no            -
/// f12        no          FP Arg12 / Ret12   no            -
/// f13        no          FP Arg13 / Ret13   no            -
/// f14 - f31  yes         Non-volatile       no            -
///
/// CR         part        Condition Reg
/// LR         no          Link Register
/// CTR        no          Count Reg    
/// MQ         no          Multiply Quot
/// XER        no          Exception Reg
/// FPSCR      no          FP Status Reg


/// MJS 31/03/91:
/// C passes arguments as a block of words. The first 8 words are in r3 to
/// r10, the remainder on the stack. Any floating point arguments are also
/// placed, in order, into f1 to f13. The documentation suggests that integer
/// registers are not used at all when passing a floating point argument, but
/// calls to printf use them so it safest to always do so.

/// For example ...
///
/// Function (int1, float2, int3, float4) has arguments placed as follows
///    int1   in r3
///    float2 in f1 and also in r4,r5
///    int3   in r6
///    float4 in f2 and also in r7,r8

/// TonyM 12/11/90:
/// Stack usage also seems odd. The rules say that space must be reserved by
/// the caller for the callee to preserve all the arguments if it wants to.
/// A double word must be reserved for floats, and a single word for singles.
/// However, the C compiler seems to make no effort to double-word align the
/// floats, even though the STFD opcode which it uses expects the address to be
/// double aligned unless alignment checking is enabled.
///
/// The best way to do things the way C expects is to first calculate the
/// total stack size, and register/stack positions - and then to generate code
/// to move them to the relevant place in reverse order, so that e.g. when
/// use 3 is moved to r6, the use already in r6 has already been placed.
/// Note that 6 words of space must also be reserved at the top of the stack 
/// for 6 Table-Of-Contents, Link-register ....
