module:    powerpc-harp
Synopsis:  PowerPC floating point branches
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


with-ops-in powerpc-instructions (fbeq, dbeq) info := beq-cc end;
with-ops-in powerpc-instructions (fbne, dbne) info := bne-cc end;

with-ops-in powerpc-instructions (fbge, dbge) info := bge-cc end;
with-ops-in powerpc-instructions (fbgt) info := bgt-cc end;

with-ops-in powerpc-instructions (fblt, dblt) info := blt-cc end;
with-ops-in powerpc-instructions (fble, dble) info := ble-cc end;


define powerpc-template (fbeq, fbne, fbgt, fbge, fblt, fble,
			 dbeq, dbne, dbge, dblt, dble)
  options (self);

  pattern (be, b :: <integer> by op-info, tag, r, s)
    let rr = femit-make-reg(be, r, reg--ftmp1);
    let ss = femit-make-reg(be, s, reg--ftmp2);
    emit-drr(be, fcmpu-op, 0, rr, ss);
    emit-branch-sdi(be, b, tag)

end powerpc-template;

