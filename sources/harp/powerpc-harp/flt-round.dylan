module:    powerpc-harp
Synopsis:  PowerPC floating point status register manipulation templates
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define powerpc-template set-control-register

  pattern (be, s)       // this time spill case easy, reg case hard!
    let src = emit-make-reg(be, s, reg--tmp1);
    emit-d(be, stwu-op, src, reg--stack, -4);
    emit-d(be, addi-op, reg--tmp1, r0, -1);  // #xffffffff is what mffs-op uses
    emit-d(be, stwu-op, reg--tmp1, reg--stack, -4);
    emit-d(be, lfd-op, reg--ftmp1, reg--stack, 0);
    emit-d(be, addic-op, reg--stack, reg--stack, 8);
    emit-fsf(be, mtfsf-op, #xff, reg--ftmp1)

end powerpc-template;


define powerpc-template get-control-register

  pattern (be, d)
    let dst = dst-place(d, reg--tmp1);
    emit-x(be, mffs-op, reg--ftmp1, f0, f0);
    emit-d(be, stfdu-op, reg--ftmp1, reg--stack, -8);
    emit-d(be, lwz-op, dst, reg--stack, 4);
    emit-d(be, addic-op, reg--stack, reg--stack, 8);
    dst-move(be, d, dst)

end powerpc-template;

