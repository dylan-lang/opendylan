module:    harp-utils
Synopsis:  An implementation of LOGCOUNT in Dylan.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// How to do logcount$fixnum
////

//// well, basically, split the fixnum into 4 bytes,
//// index into bitcount vector for each, add results.


define constant bit-count-for-logcount :: <simple-integer-vector> =

//  0 1 2 3 4 5 6 7 8 9 A B C D E F

  as(<simple-integer-vector>,

     #[0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,      // 0X
       1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,      // 1X
       1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,      // 2X
       2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,      // 3X
       1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,      // 4X
       2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,      // 5X
       2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,      // 6X
       3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,      // 7X
       1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,      // 8X
       2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,      // 9X
       2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,      // AX
       3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,      // BX
       2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,      // CX
       3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,      // DX
       3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,      // EX
       4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8]);    // FX

define inline method count-bits-in-byte (x :: <integer>) => (count :: <integer>)
  let index :: <integer> = logand(x, #xff);
  without-bounds-checks
    bit-count-for-logcount[index];
  end;
end;

define inline method logcount (x :: <integer>) => (count :: <integer>)
  let x :: <integer> =
    if (x < 0) lognot(x) else x end;    // CL compatibility

  count-bits-in-byte(x) +
  count-bits-in-byte(ash(x, -8)) +
  count-bits-in-byte(ash(x, -16)) +
  count-bits-in-byte(ash(x, -24));
end;

define method logcount (vec :: <simple-object-vector>) => (count :: <integer>)
  let res :: <integer> = 0;
  for (i :: <integer> in vec)
    res := res + logcount(i);
  end for;
  res;
end;

