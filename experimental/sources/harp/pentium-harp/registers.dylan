module:    pentium-harp
Synopsis:  The register model definition for the Pentium backend.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The register model definition for the Pentium backend


define class <pentium-register-model> (<register-model>)
end;



/// 80386 register model

define abstract class <pentium-register> (<real-register>)
end;

define class <pentium-integer-register> (<pentium-register>)
end;

define class <pentium-float-register> (<pentium-register>)
end;

define class <pentium-segment-register> (<pentium-register>)
end;

define method make-integer-register
    (number :: <integer>, pname :: <string>, mask-bit :: <integer>, 
     #key pmask = -1, cpmask = -1) 
      => (new :: <pentium-integer-register>)
  make(<pentium-integer-register>, 
       number: number, pname: pname, mask: ash(1, mask-bit),
       preserved-mask: ash(1, pmask),
       c-preserved-mask: ash(1, cpmask));
end;

define method make-float-register
    (number :: <integer>, pname :: <string>, mask-bit :: <integer>) 
    => (new :: <pentium-float-register>)
  make(<pentium-float-register>, 
        number: number, pname: pname, mask: ash(1, mask-bit));
end;

define method make-segment-register
    (number :: <integer>, pname :: <string>) 
    => (new :: <pentium-segment-register>)
  make(<pentium-segment-register>, 
        number: number, pname: pname, mask: 0);
end;


define constant eax = make-integer-register(0, "eax", 0);
define constant ecx = make-integer-register(1, "ecx", 1);
define constant edx = make-integer-register(2, "edx", 2);//
// As a temporary fix for a Windows NT 3.51 bug, we treat EBX as a C preserved 
// register, even though we know that it isn't.
// #!$%
define constant ebx = make-integer-register(3, "ebx", 3, cpmask: 3);
define constant esp = make-integer-register(4, "esp", 4);
define constant ebp = make-integer-register(5, "ebp", 5);
define constant esi = make-integer-register(6, "esi", 6, cpmask: 0);
define constant edi = make-integer-register(7, "edi", 7, cpmask: 1);

define constant f-st  = make-float-register(0, "ST",  8);
// define constant f-st1 = make-float-register(1, "ST1", 9);
// define constant f-st2 = make-float-register(2, "ST2", 10);
// define constant f-st3 = make-float-register(3, "ST3", 11);
// define constant f-st4 = make-float-register(4, "ST4", 12);
// define constant f-st5 = make-float-register(5, "ST5", 13);
// define constant f-st6 = make-float-register(6, "ST6", 14);
// define constant f-st7 = make-float-register(7, "ST7", 15);

define constant es = make-segment-register(0, "es");
define constant cs = make-segment-register(1, "cs");
define constant ss = make-segment-register(2, "ss");
define constant ds = make-segment-register(3, "ds");
define constant fs = make-segment-register(4, "fs");
define constant gs = make-segment-register(5, "gs");

define constant direction-flag
   = make-integer-register(16, "direction-flag", 16, cpmask: 2);

define constant reg--tmp1  = esi;
define constant reg--tmp2  = ecx;
define constant reg--tmp3  = edx;
define constant reg--stack = esp;
define constant reg--frame = ebp;
define constant reg--arg0  = eax;
define constant reg--mlist = edi;
define constant reg--function  = ebx;
define constant reg--arg-count = ecx;

define constant reg--float-arg0  = f-st;
define constant temps-list :: <list> = 
  list(reg--tmp1, reg--tmp2, reg--tmp3);

/// Some ref predicates for back end

define method eax-ref (x) => (x) eax == x & x end;
// define method ebx-ref (x) => (x) ebx == x & x end;
define method ecx-ref (x) => (x) ecx == x & x end;
// define method edx-ref (x) => (x) edx == x & x end;
// define method esp-ref (x) => (x) esp == x & x end;
define method esi-ref (x) => (x) esi == x & x end;
// define method edi-ref (x) => (x) edi == x & x end;

define method st-ref  (x) => (x) f-st == x & x end;

define method direction-flag-ref  (x) => (x) 
  direction-flag == x & x 
end method;


define constant zero-args-mask = rset-from-args();
define constant have-args-mask = rset-from-args(eax);

define constant pentium-arg-masks = 
  as(<simple-integer-vector>,
     vector(zero-args-mask, have-args-mask, have-args-mask, have-args-mask, 
	    have-args-mask, have-args-mask, have-args-mask, have-args-mask));

define constant pentium-c-arg-masks = 
  as(<simple-integer-vector>,
     vector(zero-args-mask, zero-args-mask, zero-args-mask, zero-args-mask, 
	    zero-args-mask, zero-args-mask, zero-args-mask, zero-args-mask));

define constant pentium-machine-arguments = vector(eax);

define constant pentium-float-machine-arguments = vector(reg--float-arg0);

define constant pentium-allocatable-registers = 
  vector(eax, ecx, edx, ebx, edi);

define constant pentium-real-registers =
  vector(eax, ecx, edx, ebx, esp, ebp, esi, edi, f-st, direction-flag);

define constant pentium-savable-registers =
  vector(edi, esi, edx, ecx, ebx);

//
// As a temporary fix for a Windows NT 3.51 bug, we treat EBX as a C preserved 
// register, even though we know that it isn't.
// #!$%
// define constant pentium-c-preserved-registers = vector(edi, esi, direction-flag);
define constant pentium-c-preserved-registers = vector(edi, esi, ebx, direction-flag);

define constant pentium-c-not-preserved = vector(eax, ecx, edx, ebx);

define constant $ev = vector();

define method initialize
    (model :: <pentium-register-model>, #key) 
     => (new :: <pentium-register-model>)
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
  model.reg-machine-arguments := pentium-machine-arguments;
  model.reg-c-machine-arguments := $ev;
  model.reg-float-machine-arguments := pentium-float-machine-arguments;
  model.reg-c-float-machine-arguments := $ev;
  model.reg-arg-masks := pentium-arg-masks;
  model.reg-arg-masks-out := pentium-arg-masks;
  model.reg-c-arg-masks := pentium-c-arg-masks;
  model.reg-c-arg-masks-out := pentium-c-arg-masks;
  model.reg-tmp1 := reg--tmp1;
  model.reg-tmp2 := reg--tmp2;
  model.reg-tmp3 := reg--tmp3;

  model.reg-c-result := reg--arg0;
  // Return of C structures across FFI barrier requires this additional machine register
  model.reg-c-result2 := edx;
  model.reg-c-frame := reg--frame;
  model.reg-c-float-result := reg--float-arg0;
  model.reg-c-stack := reg--stack;

  model.real-register-vector := pentium-real-registers;
  model.preserved-register-vector := $ev;
  model.c-preserved-register-vector := pentium-c-preserved-registers;
  model.savable-registers := pentium-savable-registers;
  model.arguments-passed-in-registers := 1;
  model.float-arguments-passed-in-registers := 1;
  model.c-arguments-passed-in-registers := 0;
  model.c-float-arguments-passed-in-registers := 0;
  model.all-allocatable := pentium-allocatable-registers;
  model.not-preserved := pentium-allocatable-registers;
  model.c-not-preserved := pentium-c-not-preserved;

  model

end;


define method make-pref-vector
    (backend :: <pentium-back-end>) => (vec :: <simple-integer-vector>)
 let pref-size = pentium-real-registers.size;
 make(<simple-integer-vector>, size: pref-size, fill: 0);
end;


define method the-real-dfreg 
    (backend :: <pentium-back-end>, x :: <real-register>) 
     => (reg :: <real-register>)
  x;
end;

define constant pentium-floating-registers =
  rset-from-args(); // yep thats right 

define constant pentium-allowable-colours =
  rset-from-list(pentium-allocatable-registers);

/// this is a bit simpler than for the other processors !

define method allowable-colours
   (backend :: <pentium-back-end>, vr :: <virtual-register>) 
    => (i :: <integer>)
  if (instance?(vr, <floating-virtual-register>))
    pentium-floating-registers;
  else
    pentium-allowable-colours  // no need for nreg check as no
			       // preserved registers
  end if;
end;

// Some useful functions for disallows

define constant $vector-eax = vector(eax);
// define constant $vector-ebx = vector(ebx);
define constant $vector-ecx = vector(ecx);
define constant $vector-edx = vector(edx);
define constant $vector-edi = vector(edi);
define constant $vector-esi = vector(esi);
define constant $vector-tmp1 = $vector-esi;
define constant $vector-tmp2 = vector(esi, ecx);
define constant $vector-flags = vector(direction-flag);
define constant $vector-c-preserved = pentium-c-preserved-registers;

define constant ecx-fn = constant-fn($vector-ecx);
define constant eax-fn = constant-fn($vector-eax);
// define constant ebx-fn = constant-fn($vector-ebx);
define constant tmp2-fn = constant-fn($vector-tmp2);

// Some useful functions for c-preserved-destroys-fns
define constant tmp1-fn = constant-fn($vector-tmp1);
define constant flags-fn = constant-fn($vector-flags);
define constant all-c-preserved-fn = constant-fn($vector-c-preserved);

define method destroys-tmp1-if (test) => (res :: <simple-object-vector>)
  if (test) $vector-tmp1 else $ev end if;
end method;

define variable pentium-registers = make(<pentium-register-model>);

