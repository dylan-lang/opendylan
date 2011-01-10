module:    harp-instructions
Synopsis:  Macro definitions for all major HARP instruction types
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Define the instruction operand accessors

define with-xyz-macro  none   0, 0 end;
define with-xyz-macro  t   0, 0 end;

define with-txyz-macro u      0, 1 end;
define with-txyz-macro uu     0, 2 end;
define with-txyz-macro uuu    0, 3 end;
define with-txyz-macro uuuu   0, 4 end;
define with-txyz-macro uuuuu  0, 5 end;
define with-txyz-macro uuuuuu 0, 6 end;
define with-txyz-macro ux     0, #F end;

define with-txyz-macro d      1, 0 end;
define with-txyz-macro du     1, 1 end;
define with-txyz-macro duu    1, 2 end;
define with-txyz-macro duuu   1, 3 end;
define with-txyz-macro duuuu  1, 4 end;
define with-txyz-macro dux    1, #F end;

define with-txyz-macro dd     2, 0 end;
define with-txyz-macro ddu    2, 1 end;
define with-txyz-macro dduu   2, 2 end;
define with-txyz-macro dduuu  2, 3 end;
define with-txyz-macro dduuuu 2, 4 end;
define with-txyz-macro ddux   2, #F end;


//// (Define-the macros which create the ins--name generic functions
//// for each op.


define xyz-definer-macro du (d, u);
define xyz-definer-macro duu (d, u1, u2);
define xyz-definer-macro duuu (d1, u1, u2, u3);
define xyz-definer-macro duuuu (d1, u1, u2, u3, u4);
define xyz-definer-macro uuuu (u1, u2, u3, u4);
define xyz-definer-macro ddu (d1, d2, u1);
define xyz-definer-macro dduu (d1, d2, u1, u2);
define xyz-definer-macro dduuu (d1, d2, u1, u2, u3);
define xyz-definer-macro tu (tt, u1);
define xyz-definer-macro tuu (tt, u1, u2);
define xyz-definer-macro td (tt, d);
define xyz-definer-macro tdu (tt, d, u);
define xyz-definer-macro tduu (tt, d, u1, u2);
define xyz-definer-macro tddu (tt, d1, d2, u1);
define xyz-definer-macro tdduu (tt, d1, d2, u1, u2);
define xyz-definer-macro u (u);
define xyz-definer-macro uu (u1, u2);
define xyz-definer-macro uuu (u1, u2, u3);
define xyz-definer-macro d (d);
define xyz-definer-macro none ();
define xyz-definer-macro tuuu (tt, u1, u2, u3);
define xyz-definer-macro t (tt);


// We now have to define each of the instructions.


