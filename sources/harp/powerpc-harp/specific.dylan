module:    powerpc-harp
Synopsis:  PowerPC only bits
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/*

/// Template MACHINE-SPECIFIC is of type DUUU.
/// We support the following patterns ...

///    reg 1 dummy dummy         - move link-register to reg
///    reg 2 dummy dummy         - move reg to link register
///    dummy 3 start length      - flush data cache for data at location start


define constant *save-link-register* = 1;
define constant *restore-link-register* = 2;
define constant *flush-data-cache* = 3;


define method save-link-reg (x)
  x == *save-link-register*
end method save-link-reg;

define method restore-link-reg (x)
  x == *restore-link-register*
end method restore-link-reg;

define method flush-data-cache (x)
  x == *flush-data-cache*
end method flush-data-cache;



define powerpc-template machine-specific

  pattern (be, d :: <real-register> by colour,
	   arg by save-link-reg, x, y)
    emit-x(be, mfspr-op, d, lr, r0);  // move link-register to d


  pattern (be, d :: <real-register> by colour,
	   arg by restore-link-reg, x, y)
    emit-x(be, mtspr-op, d, lr, r0);  // move d to link-register

  pattern (be, d, arg by flush-data-cache, start, length)
    /// FLUSH-DATA-CACHE uses the cache-line-flush instruction (clf)
    /// to remove instructions from the pipeline. This is done within a
    /// loop, incrementing the instruction address by the instruction
    /// cache size each time around. The instruction cache size is 64
    /// bytes for RS6000 model 300 machines, or 128 bytes for 500 series.
    /// The data-cache-synchronise instruction (dcs) is then used -
    /// this waits for the store-back to main memory from data cache
    /// to finish.  Finally, the instruction-cache-synchronise
    /// instruction is used - this invalidates any prefetched 
    /// instructions.

    /// MJS 28Nov95: added clcs instruction to compute the line size
    /// instead of assuming it is 64 bytes.

    let base = reg--tmp1;
    let end_ = reg--tmp2;
    let diff = reg--tmp2;
    let clen = reg--tmp3;
    let tempclen = reg--tmp1;
    let cshift = reg--tmp4;

    emit-ppc(be, clcs-op, r1: clen, d2: mincls-ra, low-part: 0);

    harp-out(be)
      move(be, cshift, -1);
      move(be, tempclen, clen)
    end;

    // SHIFTLOOP

    harp-out(be)
      lsr(be, tempclen, tempclen, 1);
      add(be, cshift, cshift, 1)
    end;
    emit-dri(be, cmpi-op, 0, tempclen, 0);
    emit-branch(be, bc-op, bne-cc, -12);    // Loop to compute ceil(log2(clen))

    harp-out(be)
      add(be, end_, start, length);
      add(be, end_, end_, clen);
      sub(be, end_, end_, 1);
      lsr(be, end_, end_, cshift);        // After last cache line number
      lsr(be, base, start, cshift);     // First cache line number
      sub(be, diff, end_, base);         // Number of lines to flush
      asl(be, base, base, cshift);      // Start of first cache line
    end;
    emit-x(be, mtspr-op, diff, ctr, r0);    // Setup count register

    // LOOP
    emit-x(be, clf-op, r0, r0, base);       // Flush this line
    emit-x(be, addc-op, base, base, clen);     // Move to next line
    emit-branch(be, bc-op, dcr-cc, -8);     // Loop using count register

    emit-ppc(be, dcs-op);              // Data Cache Synchronise
    emit-ppc(be, ics-op)               // Instruction Cache Synchronize



    // MJS 09/03/91: Scrapped this version as it is too complicated to prove
    // why it might fail.

    let st = reg--tmp1;
    let end_ = reg--tmp1;
    let len = reg--tmp2;
    let cnt = reg--tmp1;
    let sh = 6;                         // shift size to align to cache line
    let size = ash(1, sh);              // length of an instruction cache line (64)
    harp-out(be)
      sub(be, len, length, 4);          // Subtract length by 1 instruction
      lsr(be, cnt, len, sh);            // length/size is cache line number
      add(be, cnt, cnt, 1);             // allow for loop pre-decrement
    end;
    emit-x(be, mtspr-op, cnt, ctr, r0);     // use count register for loop

    harp-out(be)
      lsr(be, st, start, sh);           // effective address of changed data
      asl(be, st, st, sh);              //   aligned to cache boundary
    end;

    // LOOP
    emit-x(be, clf-op, r0, r0, st);         // Flush cache line
    emit-d(be, addic-op, st, st, size);        // increase EA by cache line length
    emit-branch(be, bc-op, dcr-cc, -8);     // decrement count register and
                                        // branch to LOOP if CTR not 0

    harp-out(be) add(be, end_, start, len) end;
    emit-x(be, clf-op, r0, r0, end_);       // Ensure end cache line is flushed

    emit-ppc(be, dcs-op);               // Data Cache Synchronise
    emit-ppc(be, ics-op)                // Instruction Cache Synchronize



end powerpc-template;


*/

define method teb-mv-count-set-offset
      (be :: <powerpc-back-end>) => (i :: <integer>)
 36
end method;

define method teb-mv-area-offset
      (be :: <powerpc-back-end>) => (i :: <integer>)
 40
end method;


define powerpc-template reset-values

  pattern (be)
    harp-out(be) st-teb(be, 0, be.teb-mv-count-set-offset) end;

end powerpc-template;


define powerpc-template set-values

  pattern (be)
    harp-out(be) st-teb(be, 1, be.teb-mv-count-set-offset) end;

end powerpc-template;

/// BMVSET and BMVUNSET
/// These branch when the multiple value count is set / unset
/// respectively. 

with-ops-in powerpc-instructions (bmvset)   info := beq-cc end;
with-ops-in powerpc-instructions (bmvunset) info := bne-cc end;


define powerpc-template (bmvset, bmvunset)
  options (self);

  pattern (be, b :: <integer> by op-info, tag)
    harp-out(be) ld-teb(be, reg--tmp1, be.teb-mv-count-set-offset) end;
    branch-immediate(be, cmpi-op, b, tag, reg--tmp1, 1);

end powerpc-template;


define powerpc-template halt
  pattern (be)
    trap-always(be);
end powerpc-template;

