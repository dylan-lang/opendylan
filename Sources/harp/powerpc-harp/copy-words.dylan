module:    powerpc-harp
Synopsis:  PowerPC copy/fill instructions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// The general idea for all the copy and fill instructions is to make the
/// loops as fast as possible by using the branch-and-decrement-count-register
/// instruction to count the number of times around the loop, and by using
/// the load/store with update instructions to update the addresses each time
/// round the loop.

/// On entry to each loop, the count (which describes the number of bytes)
/// must be divided by 4 (to get the number of words), and 1 must be added,
/// as we can only test for zero, not for negative.

/// Similarly, the address must be the address of the word either above or
/// below the real start point - as the load/store instruction must include
/// an offset from the first instruction.






/// setup-count-register moves (+1 (/ num 4)) into the count register,
/// via temp. Num may be either a register or a constant on entry.

define method setup-count-register
    (be :: <powerpc-back-end>, num, temp,
     words? :: <boolean>) => ()
  if (unsigned-32bit-const-ref(num))
    harp-out(be)
      move(be, temp,
	   if (words?) num + 1
	   else floor/(num, 4) + 1 end)
    end;
  else
    if (words?)
      harp-out(be) add(be, temp, num, 1) end
    else
      harp-out(be)
	lsr(be, temp, num, 2);
	add(be, temp, temp, 1)
      end
    end
  end;
  emit-x(be, mtspr-op, temp, ctr, r0)
end method setup-count-register;



/// Fill-words we do by finding the address of the word above the
/// highest word we will fill, and using store-with-update to adjust
/// this address (in tmp2) by -4 each time. We count how many times to
/// fill by loading the counter register with (/ (+ how-many 4) 4),
/// and decrementing the counter register each time round the loop.


with-ops-in powerpc-instructions (fill-words)   info := #f end;
with-ops-in powerpc-instructions (fill-words-w)   info := #t end;

with-ops-in powerpc-instructions (fill-words-w, fill-bytes)
  disallow-fn := tmp3-fn
end;


define powerpc-template (fill-words, fill-words-w)
  options (self);

  pattern (be, words? :: <boolean> by op-info, at, how-many, with)

    let num = unsigned-32bit-const-ref(how-many)
              | emit-make-reg(be, how-many, reg--tmp2);

    // First get the number of words + 1 into the count register
    setup-count-register(be, num, reg--tmp1, words?);

    // Now put address of one word too far into tmp2
    if (words?)
      if (unsigned-32bit-const-ref(num))
	harp-out(be) add(be, reg--tmp2, at, 4 * num) end
      else
        harp-out(be)
	  asl(be, reg--tmp3, num, 2);
          add(be, reg--tmp2, at, reg--tmp3)
        end
      end
    else
      harp-out(be)
        add(be, reg--tmp2, at, num)
      end
    end;

    let wiv = emit-make-reg(be, with, reg--tmp1);
    emit-branch(be, bc-op, bra-cc, 8);            // branch to TEST
    // LOOP
    emit-d(be, stwu-op, wiv, reg--tmp2, -4);
    // TEST
    emit-branch(be, bc-op, dcr-cc, -4)            // decrement count register and
                                              // branch to LOOP if CTR not 0

end powerpc-template;


// Fill-bytes fills with bytes. Implement in terms of fill-words
// for efficiency

define powerpc-template (fill-bytes)
  pattern (be, at, how-many, with)

    // fill all full words

    harp-out (be) move(be, reg--tmp3, with) end;

    // strip all but the low byte
    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp3, reg--tmp3, #xff);

    // get the second byte
    harp-out (be) asl(be, reg--tmp2, reg--tmp3, 8) end;
    emit-x-via-tmp1-dest2(be, or-op, reg--tmp3, reg--tmp3, reg--tmp2);

    // get bytes 3 & 4
    harp-out (be) asl(be, reg--tmp2, reg--tmp3, 16) end;
    emit-x-via-tmp1-dest2(be, or-op, reg--tmp3, reg--tmp3, reg--tmp2);

    harp-out (be) fill-words(be, at, how-many, reg--tmp3) end;

    // fill any left over bytes

    let num = emit-make-reg(be, how-many, reg--tmp1);
    let wiv = emit-make-reg(be, with, reg--tmp3);

    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp1, num, #b11);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 0);
    emit-branch(be, bc-op, beq-cc, 16 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 1);
    emit-branch(be, bc-op, beq-cc, 6 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 2);
    emit-branch(be, bc-op, beq-cc, 6 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 3);
    emit-branch(be, bc-op, beq-cc, 7 * 4);

    trap-always(be);

    // fill 1 byte
    emit-d(be, stb-op, wiv, reg--tmp2, -4);
    emit-branch(be, bc-op, bra-cc, 7 * 4);

    // fill 2 bytes
    for (i :: <integer> from 0 to 1)
      emit-d(be, stb-op, wiv, reg--tmp2, -(4 + i));
    end;
    emit-branch(be, bc-op, bra-cc, 4 * 4);

    // fill 3 bytes
    for (i :: <integer> from 0 to 2)
      emit-d(be, stb-op, wiv, reg--tmp2, -(4 + i));
    end;

end powerpc-template;



/// COPY-WORDS-DOWN-W COPY-WORDS-UP-W are like COPY-WORDS-DOWN
/// COPY-WORDS-UP respectively, but take a size argument in words
/// rather than bytes.


with-ops-in powerpc-instructions (copy-words-down)   info := #f end;
with-ops-in powerpc-instructions (copy-words-up)     info := #f end;
with-ops-in powerpc-instructions (copy-words-down-w) info := #t end;
with-ops-in powerpc-instructions (copy-words-up-w)   info := #t end;



/// Copy-words-up is similar to fill-words. We find the addresses one word
/// above the from and to regions, and load the count register with
/// (/ (+ how-many 4) 4).  Again the addresses are updated with the load
/// and store instructions, and the count is decremented by the branch.

with-ops-in powerpc-instructions (copy-words-up, copy-bytes-up)
  disallow-fn := tmp3-fn;
end;

with-ops-in powerpc-instructions (copy-words-up-w)
  disallow-fn := tmp34-fn;
end;

define powerpc-template (copy-words-up, copy-words-up-w)
  options (self);

  pattern (be, words? :: <boolean> by op-info, to, from, how-many)

    let num = unsigned-32bit-const-ref(how-many)
              | emit-make-reg(be, how-many, reg--tmp2);
    let fm = reg--tmp3;
    let too = reg--tmp2;

    // First get the number of words + 1 into the count register
    setup-count-register(be, num, reg--tmp1, words?);

    // Now find addresses just past from & to areas
    if (words?)
      if (unsigned-32bit-const-ref(num))
	harp-out(be)
	  add(be, fm, from, 4 * num);
	  add(be, too, to, 4 * num)
        end
      else
	harp-out(be)
	  asl(be, reg--tmp4, num, 2);
	  add(be, fm, from, reg--tmp4);
	  add(be, too, to, reg--tmp4)
	end
      end
    else
      harp-out(be)
        add(be, fm, from, num);
        add(be, too, to, num)
      end
    end;

    emit-branch(be, bc-op, bra-cc, 12);       // branch to TEST
    // LOOP
    emit-d(be, lwzu-op, reg--tmp1, fm, -4);
    emit-d(be, stwu-op, reg--tmp1, too, -4);
    // TEST
    emit-branch(be, bc-op, dcr-cc, -8)        // decrement count register and
                                          // branch to LOOP if CTR not 0

end powerpc-template;



/// For Copy-words-down, we find the addresses one word below the from
/// and to regions - which actually needs a subtraction.  This time we
/// use load and store multiple with a positive offset. We use the count
/// register as for the previous instructions. 

with-ops-in powerpc-instructions
  (copy-words-down, copy-words-down-w, copy-bytes-down)
  disallow-fn := tmp3-fn;
end;

define powerpc-template (copy-words-down, copy-words-down-w)
  options (self);

  pattern (be, words? :: <boolean> by op-info, to, from, how-many)

    // first get the number of words + 1 into the count register
    setup-count-register(be, how-many, reg--tmp1, words?);

    let fm = reg--tmp3;
    let too = reg--tmp2;

    // Now find the addresses one word before the from & to regions
    harp-out(be)
      sub(be, too, to, 4);
      sub(be, fm, from, 4)
    end;

    emit-branch(be, bc-op, bra-cc, 12);       // branch to TEST
    // LOOP
    emit-d(be, lwzu-op, reg--tmp1, fm, 4);
    emit-d(be, stwu-op, reg--tmp1, too, 4);
    // TEST
    emit-branch(be, bc-op, dcr-cc, -8)        // decrement count register and
                                          // branch to LOOP if CTR not 0

end powerpc-template;



define powerpc-template (copy-bytes-up)
  pattern (be, to, from, how-many)

    // copy all full words
    harp-out (be) copy-words-up(be, to, from, how-many) end;

    // copy any left over bytes

    let num = emit-make-reg(be, how-many, reg--tmp1);
    let too = reg--tmp2;
    let fm  = reg--tmp3;

    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp1, num, #b11);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 0);
    emit-branch(be, bc-op, beq-cc, 22 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 1);
    emit-branch(be, bc-op, beq-cc, 6 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 2);
    emit-branch(be, bc-op, beq-cc, 7 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 3);
    emit-branch(be, bc-op, beq-cc, 10 * 4);

    trap-always(be);

    // copy 1 byte
    emit-d(be, lbz-op, reg--tmp1, fm, -4);
    emit-d(be, stb-op, reg--tmp1, too, -4);
    emit-branch(be, bc-op, bra-cc, 12 * 4);

    // copy 2 bytes
    for (i :: <integer> from 0 to 1)
      emit-d(be, lbz-op, reg--tmp1, fm, -(4 + i));
      emit-d(be, stb-op, reg--tmp1, too, -(4 + i));
    end;
    emit-branch(be, bc-op, bra-cc, 7 * 4);

    // copy 3 bytes
    for (i :: <integer> from 0 to 2)
      emit-d(be, lbz-op, reg--tmp1, fm, -(4 + i));
      emit-d(be, stb-op, reg--tmp1, too, -(4 + i));
    end;

end powerpc-template;


define powerpc-template (copy-bytes-down)
  pattern (be, to, from, how-many)

    // copy all full words
    harp-out (be) copy-words-down(be, to, from, how-many) end;

    // copy any left over bytes

    let num = emit-make-reg(be, how-many, reg--tmp1);
    let too = reg--tmp2;
    let fm  = reg--tmp3;

    emit-d-via-tmp1-dest2(be, andi!-op, reg--tmp1, num, #b11);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 0);
    emit-branch(be, bc-op, beq-cc, 22 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 1);
    emit-branch(be, bc-op, beq-cc, 6 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 2);
    emit-branch(be, bc-op, beq-cc, 7 * 4);

    emit-dri(be, cmpi-op, 0, reg--tmp1, 3);
    emit-branch(be, bc-op, beq-cc, 10 * 4);

    trap-always(be);

    // copy 1 byte
    emit-d(be, lbz-op, reg--tmp1, fm, 4);
    emit-d(be, stb-op, reg--tmp1, too, 4);
    emit-branch(be, bc-op, bra-cc, 12 * 4);

    // copy 2 bytes
    for (i :: <integer> from 0 to 1)
      emit-d(be, lbz-op, reg--tmp1, fm, 4 + i);
      emit-d(be, stb-op, reg--tmp1, too, 4 + i);
    end;
    emit-branch(be, bc-op, bra-cc, 7 * 4);

    // copy 3 bytes
    for (i :: <integer> from 0 to 2)
      emit-d(be, lbz-op, reg--tmp1, fm, 4 + i);
      emit-d(be, stb-op, reg--tmp1, too, 4 + i);
    end;

end powerpc-template;

