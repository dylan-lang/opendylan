module:    powerpc-harp
Synopsis:  PowerPC opcodes, and routines to emit them
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




define constant <opcode> = <machine-word>;

define macro opcode-definer
  { define opcode ?:name ?rest:*}
    =>
  {
    define opcode-aux ?name (?rest)
  }
end macro;

define macro opcode-aux-definer
  { define opcode-aux ?:name (?primary:expression) }
    =>
  {
    define constant ?name :: <opcode> = mw-ash(?primary, 26)
  }

  { define opcode-aux ?:name (?primary:expression ?extended:expression) }
    =>
  {
    define constant ?name :: <opcode> =
      machine-word-add-signal-overflow
        (mw-ash(?primary, 26), mw-ash(?extended, 1))
  }
end macro;



/// Here are the routines that actually emit code ...

/// There are 11 different formats in the book (XO D X I B XL A XFX XFL M SC).
/// Unfortunately things are not really that consistent as some instructions 
/// have dummy fields and some fields can be for either registers or data
/// within the same format.

/// The other major problem is that the destination field wanders from 
/// instruction to instruction (ADD and AND are different, for instance).
/// We leave the job of getting the ordering right to the templates themselves,
/// rather than handling the problem here.

/// MJS 24Feb98: rewrote to avoid pointlessly making and then spliting a bignum.


/// First some handy alignment macros


define macro reg-align
  { reg-align(?reg:expression, ?position:expression, ?hi-part?:expression) }
    => { field-align(?reg.ppc-register-number, ?position, ?hi-part?) }
end macro;

define macro field5-align
  { field5-align(?data:expression, ?position:expression, ?hi-part?:expression) }
    => { field-align(low-5(?data), ?position, ?hi-part?) }
end macro;

define macro field-align
  { field-align(?data:expression, ?position:expression, ?hi-part?:expression) }
    => { ash(?data,
	     if (?hi-part?) ?position - 16
	     else ?position end)
       }
end macro;


define inline method emit-ppc
    (be :: <powerpc-back-end>, op :: <opcode>,
     #key r1, r2, r3, r4,
     d1, d2, d3,
     fsf, low-part, condition, displacement) => ()
  let r1 :: <integer> = if (r1) reg-align(r1, 21, #t) else 0 end;
  let r2 :: <integer> = if (r2) reg-align(r2, 16, #t) else 0 end;
  let d1 :: <integer> = if (d1) field5-align(d1, 21, #t) else 0 end;
  let d2 :: <integer> = if (d2) reg-align(d2, 16, #t) else 0 end;
  let fsf :: <integer> = if (fsf) field-align(lo-part(fsf), 17, #t) else 0 end;
  let condition :: <integer> = if (condition) ash(condition, -16) else 0 end;

  let high-part :: <integer> =
    mw-hi-part(op) + r1 + r2 + d1 + d2 + fsf + condition;

  let r3 :: <integer> = if (r3) reg-align(r3, 11, #f) else 0 end;
  let r4 :: <integer> = if (r4) reg-align(r4, 6, #f) else 0 end;
  let d3 :: <integer> = if (d3) field5-align(d3, 11, #f) else 0 end;
  let low-part :: <integer> = if (low-part) lo-part(low-part) else 0 end;
  let displacement :: <integer> = if (displacement) displacement else 0 end;

  let low-part :: <integer> =
    mw-lo-part(op) + r3 + r4 + d3 + low-part + displacement;

  // BigEndian emitting - emit high-part followed by low-part
  emit(be, high-part, low-part)

end;

define inline method emit-high-part
    (be :: <powerpc-back-end>, op :: <opcode>,
     #key r1, r2,
     d1, d2,
     fsf, condition) => ()
  let r1 :: <integer> = if (r1) reg-align(r1, 21, #t) else 0 end;
  let r2 :: <integer> = if (r2) reg-align(r2, 16, #t) else 0 end;
  let d1 :: <integer> = if (d1) field5-align(d1, 21, #t) else 0 end;
  let d2 :: <integer> = if (d2) reg-align(d2, 16, #t) else 0 end;
  let fsf :: <integer> = if (fsf) field-align(lo-part(fsf), 17, #t) else 0 end;
  let condition :: <integer> = if (condition) ash(condition, -16) else 0 end;

  let high-part :: <integer> =
    mw-hi-part(op) + r1 + r2 + d1 + d2 + fsf + condition;

  emit(be, high-part)

end;

define inline method sdi-ppc
    (op :: <opcode>,
     #key r1, r2, r3, r4,
     d1, d2, d3,
     fsf, low-part, condition, displacement)
 => (split-instruction :: <list>)
  let r1 :: <integer> = if (r1) reg-align(r1, 21, #t) else 0 end;
  let r2 :: <integer> = if (r2) reg-align(r2, 16, #t) else 0 end;
  let d1 :: <integer> = if (d1) field5-align(d1, 21, #t) else 0 end;
  let d2 :: <integer> = if (d2) reg-align(d2, 16, #t) else 0 end;
  let fsf :: <integer> = if (fsf) field-align(lo-part(fsf), 17, #t) else 0 end;
  let condition :: <integer> = if (condition) ash(condition, -16) else 0 end;

  let high-part :: <integer> =
    mw-hi-part(op) + r1 + r2 + d1 + d2 + fsf + condition;

  let r3 :: <integer> = if (r3) reg-align(r3, 11, #f) else 0 end;
  let r4 :: <integer> = if (r4) reg-align(r4, 6, #f) else 0 end;
  let d3 :: <integer> = if (d3) field5-align(d3, 11, #f) else 0 end;
  let low-part :: <integer> = if (low-part) lo-part(low-part) else 0 end;
  let displacement :: <integer> = if (displacement) displacement else 0 end;

  let low-part :: <integer> =
    mw-lo-part(op) + r3 + r4 + d3 + low-part + displacement;

  list(high-part, low-part)

end;


/// Format X0 looks as follows ...
/// MSB   0          6        11        16       21       22          31  LSB
///       Primary-Op Reg-1    Reg-2     Reg-3    Overflow Extended-Op Record
/// Format X is similar, but Overflow must be 0.
/// Format A is similar, but overflow must be 0 and Extended-Op is smaller.
/// Since Primary-Op Secondary-Op Overflow and Record are all treated as
/// part of the opcode fragment, we can treat these as the same type.

define method emit-x
    (be :: <powerpc-back-end>, opcode :: <opcode>, reg1, reg2, reg3)
 => ()
  emit-ppc(be, opcode, r1: reg1, r2: reg2, r3: reg3)
end method emit-x;

/// It seems stupid to me but ....
/// The Floating Multiply instruction (fmul-op) has its third register
/// argument in the wrong place compared with other type X or A instructions.
/// We need a special version of emit-x to handle this.

define method emit-x-f4
    (be :: <powerpc-back-end>, opcode :: <opcode>, reg1, reg2, reg3)
 => ()
  emit-ppc(be, opcode, r1: reg1, r2: reg2, r4: reg3)
end method emit-x-f4;


/// Format XL looks as follows ...
/// MSB   0          6        11        16       21           31  LSB
///       Primary-Op Data     Data      Data     Extended-Op  Link-bit
/// We include the link-bit with the opcode for simplicity.

define method emit-xl
    (be :: <powerpc-back-end>, opcode :: <opcode>, d1, d2, d3)
 => ()
  emit-ppc(be, opcode, d1: d1, d2: d2, d3: d3)
end method emit-xl;


/// Format D looks as follows ...
/// MSB   0          6        11        16                    31  LSB
///       Primary-Op Reg-1    Reg-2     16-bit-data-field

define method emit-d
    (be :: <powerpc-back-end>, opcode :: <opcode>, reg1, reg2, data)
 => ()
  emit-ppc(be, opcode, r1: reg1, r2: reg2, low-part: data)
end method emit-d;

define method emit-d-high
    (be :: <powerpc-back-end>, opcode :: <opcode>, reg1, reg2)
 => ()
  emit-high-part(be, opcode, r1: reg1, r2: reg2)
end method emit-d-high;


/// Format B looks as follows ...
/// MSB   0          6        11        16               30  31  LSB
///       Primary-Op Data     Data      Branch-Displace  AA  Link-Bit
/// Note that the branch displacement is already correctly aligned, and
/// its least significant 2 bits are assumed zero.  
/*
define method emit-b
    (be :: <powerpc-back-end>, opcode :: <opcode>, d1, d2, displacement)
 => ()
  emit-ppc(be, opcode, d1: d1, d2: d2, low-part: displacement)
end method emit-b;
*/

/// finally we need one or two extra, as some instructions (eg cmpi)
/// use a register field as if it were data

/// emit-drr can be used by cmp and cmpl

define method emit-drr
    (be :: <powerpc-back-end>, opcode :: <opcode>, d1, reg2, reg3)
 => ()
  emit-ppc(be, opcode, d1: d1, r2: reg2, r3: reg3)
end method emit-drr;


/// emit-fsf can be used by the mtfsf instruction

define method emit-fsf
    (be :: <powerpc-back-end>, opcode :: <opcode>, flm, reg)
 => ()
  emit-ppc(be, opcode, fsf: flm, r3: reg)
end method emit-fsf;


/// emit-dri can be used by cmpi and cmpli

define method emit-dri
    (be :: <powerpc-back-end>, opcode :: <opcode>, d1, reg2, data)
 => ()
  emit-ppc(be, opcode, d1: d1, r2: reg2, low-part: data)
end method emit-dri;


/// emit-rrd can be used by rlinm

define method emit-rrd
    (be :: <powerpc-back-end>, opcode :: <opcode>, reg1, reg2, d3)
 => ()
  emit-ppc(be, opcode, r1: reg1, r2: reg2, d3: d3)
end method emit-rrd;


/// emit-branch is more convenient than emit-b for conditional branch

define method emit-branch
    (be :: <powerpc-back-end>, opcode :: <opcode>, condition :: <integer>,
     displacement)
 => ()
  emit-ppc(be, opcode, condition: condition,
            displacement: logand(displacement, #xfffc))
end method emit-branch;


/// emit-br-reg is more convenient than emit-x for branch conditional register

define method emit-br-reg
    (be :: <powerpc-back-end>, opcode :: <opcode>, condition :: <integer>)
 => ()
  emit-ppc(be, opcode, condition: condition)
end method emit-br-reg;


/// The SDI code routines:


define method sdi-x (opcode :: <opcode>, reg1, reg2, reg3)
 => (split-instruction :: <list>)
  sdi-ppc(opcode, r1: reg1, r2: reg2, r3: reg3)
end method sdi-x;


define method sdi-d (opcode :: <opcode>, reg1, reg2, displacement)
 => (split-instruction :: <list>)
  sdi-ppc(opcode, r1: reg1, r2: reg2, low-part: displacement)
end method sdi-d;


define method sdi-data (data)
 => (split-data :: <list>)
  list(hi-part(data), lo-part(data))
end method sdi-data;


/// Format l (for simple branch)

define method sdi-l (opcode :: <opcode>, li :: <integer>)
 => (split-instruction :: <list>)
  let val :: <opcode> = mw-add(opcode, logand(li, #x3fffffc));
  list(mw-hi-part(val), mw-lo-part(val))
end method sdi-l;


/// Format B for conditional branch 
/// note the merged condition fields which are already aligned

define method sdi-branch (opcode :: <opcode>, condition :: <integer>,
                          displacement)
 => (split-instruction :: <list>)
  let val :: <opcode> = mw-add(opcode, condition + logand(displacement, #xfffc));
  list(mw-hi-part(val), mw-lo-part(val))
end method sdi-branch;


/// The list of opcodes - in alphabetical order - straight out the book.  


define opcode addc-op       31   10;
// define opcode adde-op      31  138;
define opcode addic-op      12;

// define opcode addic!-op     13;
// define opcode addme-op     31  234;
define opcode and-op     31   28;
define opcode andc-op    31   60;

define opcode andi!-op  28;
define opcode andis!-op  29;
// define opcode addze-op     31  202;
define opcode b-op       18;

define opcode bc-op      16;
define opcode bcctr-op     19  528;
define opcode bclr-op     19   16;
define opcode addi-op     14;

define opcode addis-op     15;
// define opcode add-op     31  266;
define opcode cmp-op     31    0;
define opcode cmpi-op    11;

define opcode cmpl-op    31   32;
define opcode cmpli-op   10;
// define opcode cntlzw-op   31   26;
// define opcode crand-op   19  257;

// define opcode crandc-op  19  129;
// define opcode creqv-op   19  289;
// define opcode crnand-op  19  225;
// define opcode crnor-op   19   33;

// define opcode cror-op    19  449;
// define opcode crorc-op   19  417;
// define opcode crxor-op   19  193;

// define opcode eqv-op     31  284;

// define opcode extsh-op    31  922;
define opcode fadd-op      63   21;
define opcode fabs-op    63  264;

define opcode fctiw-op   63   14;

// define opcode fcmpo-op   63   32;
define opcode fcmpu-op   63    0;

define opcode fdiv-op      63   18;
define opcode fmul-op      63   25;
// define opcode fmadd-op     63   29;

define opcode fmr-op     63   72;
// define opcode fmsub-op     63   28;
// define opcode fnabs-op   63  136;
define opcode fneg-op    63   40;

// define opcode fnmadd-op    63   31;
// define opcode fnmsub-op    63   30;
define opcode frsp-op    63   12;
define opcode fsub-op      63   20;

define opcode fsqrt-op      63   22;
define opcode fsqrts-op     59   22;

define opcode lwz-op       32;
// define opcode lwbrx-op    31  534;
define opcode lbz-op     34;
// define opcode lbzu-op    35;

// define opcode lbzux-op   31  119;
define opcode lbzx-op    31   87;
define opcode lfd-op     50;
// define opcode lfdu-op    51;

// define opcode lfdux-op   31  631;
define opcode lfdx-op    31  599;
define opcode lfs-op     48;
// define opcode lfsu-op    49;

// define opcode lfsux-op   31  567;
define opcode lfsx-op    31  535;
define opcode lha-op     42;
// define opcode lhau-op    43;

// define opcode lhaux-op   32  375;
define opcode lhax-op    31  343;
// define opcode lhbrx-op   31  790;
define opcode lhz-op     40;

// define opcode lhzu-op    41;
// define opcode lhzux-op   31  311;
define opcode lhzx-op    31  279;
define opcode lmw-op      46;

// define opcode lswi-op     31  597;
// define opcode lswx-op     31  533;
define opcode lwzu-op      33;

// define opcode lwzux-op     31   55;
define opcode lwzx-op      31   23;

// define opcode mcrf-op    19    0;
// define opcode mcrfs-op   63   64;
define opcode mcrxr-op   31  512;
// define opcode mfcr-op    31   19;

define opcode mffs-op    63  583;
// define opcode mfmsr-op   31   83;
define opcode mfspr-op   31  339;
// define opcode mtcrf-op   31  144;

define opcode mtfsb0-op  63   70;
define opcode mtfsb1-op  63   38;
define opcode mtfsf-op   63  711;
// define opcode mtfsfi-op  63  134;

define opcode mtspr-op   31  467;
define opcode mulli-op    07;
define opcode mullw-op    31  235;

// define opcode nand-op    31  476;
define opcode neg-op     31  104;
define opcode nor-op     31  124;

define opcode or-op      31  444;
// define opcode orc-op     31  412;
define opcode ori-op    24;
define opcode oris-op    25;

define opcode rlwimi-op   20;
define opcode rlwinm-op   21;
define opcode rlwnm-op    23;

define opcode subfc-op      31    8;
// define opcode subfe-op     31  136;
define opcode subfic-op     08;

// define opcode subfme-op    31  232;
// define opcode subfze-op    31  200;
define opcode slw-op      31   24;

define opcode srw-op      31  536;
define opcode sraw-op     31  792;
define opcode srawi-op    31  824;

define opcode stw-op      36;
define opcode stb-op     38;
// define opcode std-op      62;
// define opcode stwbrx-op   31  662;

// define opcode stbu-op    39;
// define opcode stbux-op   31  247;
define opcode stbx-op    31  215;
define opcode stfd-op    54;

define opcode stfdu-op   55;
// define opcode stfdux-op  31  759;
define opcode stfdx-op   31  727;
define opcode stfs-op    52;

// define opcode stfsu-op   53;
// define opcode stfsux-op  31  695;
define opcode stfsx-op   31  663;
define opcode sth-op     44;

// define opcode sthbrx-op  31  918;
// define opcode sthu-op    45;
// define opcode sthux-op   31  439;
define opcode sthx-op    31  407;

define opcode stmw-op     47;
// define opcode stswi-op    31  725;
// define opcode stswx-op    31  661;
define opcode stwu-op     37;

// define opcode stwux-op    31  183;
define opcode stwx-op     31  151;
// define opcode svc-op     17;
define opcode tw-op       31    4;

// define opcode twi-op      03;
define opcode xor-op     31  316;
define opcode xori-op   26;
define opcode xoris-op   27;

define opcode lwarx-op    31  20;
define opcode stwcx-op    31  150;


/// the next three are not in the manual - but IBM say they exist

// define opcode dcs-op     31  598;  // Data Cache Synchronise
// define opcode ics-op     19  150;  // Instruction Cache Synchronise

define opcode divw-op    31  491;
define opcode divwu-op   31  459;
define opcode mulhw-op   31   75;
define opcode mulhwu-op   31   11;

/// some modifiers for the above

define constant oe-mod =
  coerce-integer-to-machine-word(ash(1, 10)); // op-code modifier to set Overflow Exception
define constant rc-mod =
  coerce-integer-to-machine-word(1);          // op-code modifier to record conditions
// define constant aa-mod =
//   coerce-integer-to-machine-word(2);          // op-code modifier for absolute address
define constant lk-mod =
  coerce-integer-to-machine-word(1);          // op-code modifier add link bit

// define constant icls-ra = #b01100;       // clcs Instruction Cache Line Size
// define constant dcls-ra = #b01101;       // clcs Data Cache Line Size
// define constant mincls-ra = #b01110;     // clcs Mininum Cache Line Size
// define constant maxcls-ra = #b01111;     // clcs Maximum Cache Line Size


/// and some useful modified instructions to set condition register

define constant addc!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, addc-op);
// define constant addco-op :: <opcode> =
//  machine-word-add-signal-overflow(oe-mod, addc-op);
define constant addco!-op :: <opcode> =
  machine-word-add-signal-overflow(oe-mod, addc!-op);

define constant subfc!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, subfc-op);
// define constant subfco-op :: <opcode> =
//  machine-word-add-signal-overflow(oe-mod, subfc-op);
define constant subfco!-op :: <opcode> =
  machine-word-add-signal-overflow(oe-mod, subfc!-op);

define constant mullw!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, mullw-op);
// define constant mullwo-op :: <opcode> =
//   machine-word-add-signal-overflow(oe-mod, mullw-op);
define constant mullwo!-op :: <opcode> =
  machine-word-add-signal-overflow(oe-mod, mullw!-op);
define constant mulhw!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, mulhw-op);
define constant mulhwu!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, mulhwu-op);

define constant divw!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, divw-op);
// define constant divwo-op :: <opcode> =
//   machine-word-add-signal-overflow(oe-mod, divw-op);
define constant divwo!-op :: <opcode> =
  machine-word-add-signal-overflow(oe-mod, divw!-op);

define constant and!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, and-op);
define constant andc!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, andc-op);
define constant or!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, or-op);
define constant xor!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, xor-op);

/*
define constant slw!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, slw-op);
*/

define constant rlwnm!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, rlwnm-op);
define constant rlwinm!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, rlwinm-op);

define constant stwcx!-op :: <opcode> =
  machine-word-add-signal-overflow(rc-mod, stwcx-op);

// define constant bcl-op :: <opcode> =
//  machine-word-add-signal-overflow(lk-mod, bc-op);
define constant bclrl-op :: <opcode> =
  machine-word-add-signal-overflow(lk-mod, bclr-op);
define constant bcctrl-op :: <opcode> =
  machine-word-add-signal-overflow(lk-mod, bcctr-op);
define constant bl-op :: <opcode> =
  machine-word-add-signal-overflow(lk-mod, b-op);

// define constant bla-op :: <opcode> =
//  machine-word-add-signal-overflow(aa-mod, bl-op);


/// finally, some condition codes - align the fields from the beginning

define constant brit-cc = ash(#b01100, 21);  // branch if true
define constant brif-cc = ash(#b00100, 21);  // branch if false
define constant bra-cc = ash(#b10100, 21);   // branch always
define constant dcr-cc = ash(#b10000, 21);   // decrement count register and
                                             // branch if CTR is not 0

/// Fixed point exception register looks as follows:
/// Bit   0    1    2    3
/// Means SO   OV   CA   --

// define constant ov-cc  = ash(#b00001, 16);
define constant ov2-cc = ash(#b01001, 16);

/// FPSCR looks as follows:
/// Bit   .....  30   31
/// Means        RNU  RNL
/// RNU.RNL make up the rounding mode: 0=to nearest, 1=to zero, 2=up, 3=down.

/// Condition Register looks as follows:
/// Bit   0    1    2    3    4    5    6    7  ........
/// Test  LT   GT   EQ   SO   LT   GT   EQ   SO
/// Field 0    0    0    0    1    1    1    1

define constant lt0-cc = ash(#b000, 16);
define constant gt0-cc = ash(#b001, 16);
define constant eq0-cc = ash(#b010, 16);
// define constant so0-cc = ash(#b011, 16);
// define constant lt1-cc = ash(#b100, 16);
// define constant gt1-cc = ash(#b101, 16);
// define constant eq1-cc = ash(#b110, 16);
// define constant so1-cc = ash(#b111, 16);

define constant crf0 = 0;         // BF field for compare opcodes for field 0
// define constant crf1 = ash(1, 2); // BF field for compare opcodes for field 1

/// Can test condition register fields 0 or 1. 

define constant bov-cc = brit-cc + ov2-cc;  // branch if overflow (XER position in CR2 field)
define constant bno-cc = brif-cc + ov2-cc;  // branch if not overflow (XER position in CR2 field)
// define constant bso-cc = brit-cc + so0-cc; // branch if summary overflow
// define constant bns-cc = brif-cc + so0-cc; // branch if not summary overflow 
define constant beq-cc = brit-cc + eq0-cc; // branch if equal
define constant bne-cc = brif-cc + eq0-cc; // branch if not equal
define constant bgt-cc = brit-cc + gt0-cc; // branch if greater
define constant bge-cc = brif-cc + lt0-cc; // branch if greater or equal
define constant blt-cc = brit-cc + lt0-cc; // branch if less
define constant ble-cc = brif-cc + gt0-cc; // branch if less or equal

// define constant beq1-cc = brit-cc + eq1-cc;  // branch if equal (f1)
// define constant bne1-cc = brif-cc + eq1-cc;  // branch if not equal (f1)
// define constant bgt1-cc = brit-cc + gt1-cc;  // branch if greater (f1)
// define constant bge1-cc = brif-cc + lt1-cc;  // branch if greater or equal (f1)
// define constant blt1-cc = brit-cc + lt1-cc;  // branch if less (f1)
// define constant ble1-cc = brif-cc + gt1-cc;  // branch if less or equal (f1)


/// to invert a test, invert the one bit difference between brit-cc and brif-cc

define method opposite-condition (condition-code)
  let truth-bit = ash(#b01000, 21);
  logxor(condition-code, truth-bit)
end method opposite-condition;


