module:    powerpc-harp
Synopsis:  PowerPC stack push/pop code
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// The stack pointer always points at the last word pushed on the stack.
/// Hence we can do pushes with the store-with-update instruction, but when we
/// pop, we must first load the word, and then adjust the stack pointer.

define powerpc-template push

  pattern (be, d)
    let dd = emit-make-reg(be, d, reg--tmp1);
    emit-d(be, stwu-op, dd, reg--stack, -4)

end powerpc-template;


define powerpc-template t-push

  pattern (be, d, stack :: <real-register> by colour)
    let dd = emit-make-reg(be, d, reg--tmp1);
    emit-d(be, stwu-op, dd, stack, -4)

end powerpc-template;


define powerpc-template push-mem

  pattern (be, d, o)
    harp-out (be)
      ld(be, reg--tmp1, d, o);
      push(be, reg--tmp1);
    end;
end powerpc-template;


define powerpc-template pop

  pattern (be, d)
    emit-d-via-tmp1-dest1(be, lwz-op, d, reg--stack, 0);
    emit-d(be, addic-op, reg--stack, reg--stack, 4);

end powerpc-template;


define powerpc-template t-pop

  pattern (be, d, stack :: <real-register> by colour)
    emit-d-via-tmp1-dest1(be, lwz-op, d, stack, 0);
    emit-d(be, addic-op, stack, stack, 4);

end powerpc-template;


define powerpc-template pop-mem

  // pop a real register ...
  pattern (be, d :: <real-register> by colour, o)
    harp-out (be)
      pop(be, reg--tmp1);
      st(be, reg--tmp1, d, o);
    end;

end powerpc-template;


/// PEA - as with many processors, we must obtain the PC by subterfuge, add
/// in the offset, then push the result.  Since the add-immediate instruction 
/// only takes a 16 bit signed offset, we have 2 alternative schemes.


define powerpc-template pea

  pattern (be, tag, offset :: <integer>)
    emit-pea-sdi(be, tag, offset: offset);

end powerpc-template;


define method short-pea-sdi
    (self :: <new-sdi>, span, code?)
  let rel = span - 4;   // since link is address of NEXT instruction
  if (-#x8000 <= rel & rel <= #x8000)
    if (code?)
      concatenate
	(sdi-l(bl-op, 4),  // branch to next inst & record its address
	 sdi-x(mfspr-op, reg--link, lr, r0),
	 sdi-d(addic-op, reg--link, reg--link, rel),
	 sdi-d(stwu-op, reg--link, reg--stack, -4));
    else
      16
    end
  end
end method short-pea-sdi;


define method long-pea-sdi
    (self :: <new-sdi>, span, code?)
  let rel = span - 4;
  if (code?)
    concatenate
      (sdi-l(bl-op, 8),    // skip over immediate data
       sdi-data(rel),      // link register contains address of this
       sdi-x(mfspr-op, reg--link, lr, r0),
       sdi-d(lwz-op, reg--tmp1, reg--link, 0),  // load rel
       sdi-x(addc-op, reg--link, reg--link, reg--tmp1),
       sdi-d(stwu-op, reg--link, reg--stack, -4));
  else
    24
  end
end method long-pea-sdi;


define constant pea-methods = vector(short-pea-sdi, long-pea-sdi);

define method emit-pea-sdi
    (be :: <powerpc-back-end>, tag :: <tag>,
     #key offset = 0)
  let sdi =
    make(<new-sdi>,
	 dest-tag: tag,
	 dest-offset: offset,
	 cached-size: 16,
	 method-index: 0,
	 method-vector: pea-methods,
	 code-fragment: #"error");
  emit-sdi(be, sdi)
end method emit-pea-sdi;


