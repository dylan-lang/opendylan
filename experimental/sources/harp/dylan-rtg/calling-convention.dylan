module:    dylan-rtg
Synopsis:  Support for generic calling conventions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// A processor specific DDUUU op

define method op--copy-registers-with-update
    (be :: <harp-back-end>, new-mem, new-count, mem, count, reg :: <integer>,
     #key down?, to?, C?, base-count = 0,
          done-tag, continue-tag)
 => ()
  let max-num-arg-regs =
   if (C?)
     be.registers.c-arguments-passed-in-registers;
   else
     be.registers.arguments-passed-in-registers;
   end;
  let num-cases = max(max-num-arg-regs - reg, 0);
  let cases? = num-cases > 0;
  let tags :: <simple-object-vector> = make-tags(be, num-cases);
  let done :: <tag> = done-tag | make-tag(be);

  for (tag :: <tag> in tags,
       i :: <integer> from num-cases - 1 by -1)
    ins--bgt(be, tag, count, i + base-count);
  end for;

  // Zero case
  copy-registers-case-generator
    (be, 0, new-mem, new-count, mem, count, reg,
     down?: down?, to?: to?, C?: C?);
  cases? & ins--bra(be, done);

  for (tag :: <tag> in tags,
       i :: <integer> from num-cases by -1)
    ins--tag(be, tag);
    copy-registers-case-generator
      (be, i, new-mem, new-count, mem, count, reg,
       down?: down?, to?: to?, C?: C?);
    if (continue-tag)
      if (i == num-cases)
	ins--beq(be, done, count, num-cases + base-count);
	ins--bra(be, continue-tag);
      else
	ins--bra(be, done);
      end;
    else
      ins--bra(be, done);
    end;
  end for;

  cases? & ~done-tag & ins--tag(be, done);

end method;

define method op--copy-registers-with-update
    (be :: <harp-back-end>,
     new-mem, new-count,
     mem, count :: <integer>,
     reg :: <integer>,
     #key down?, to?, C?,
     #all-keys)
 => ()
  copy-registers-case-generator
    (be, count, new-mem, new-count, mem, count, reg,
     down?: down?, to?: to?, C?: C?);
end method;

define method copy-registers-case-generator
    (be :: <harp-back-end>, i :: <integer>,
     new-mem, new-count, mem, count, reg :: <integer>,
     #key down?, to?, C?)
 => ()
  if (i == 0)
    // Zero case.
    // Just move mem and count
    new-mem & (mem ~== new-mem) & ins--move(be, new-mem, mem);
    new-count & (count ~== new-count) & ins--move(be, new-count, count);
  else
    for (j :: <integer> from reg to reg + i - 1,
	 k :: <integer> from reg + i - 1 to reg by -1)
      let index :: <integer> =
	if (down?) k else j end;
      let offset :: <integer> =
	if (down?) -4 * (j - reg + 1) else 4 * (j - reg) end;
      let op :: <function> =
	if (to?) ins--ld else ins--st end;
      let arg-reg =
	if (C?) be.registers.reg-c-machine-arguments[index]
	else be.registers.reg-machine-arguments[index]
	end;
      op(be, arg-reg, mem, offset);
    end;
    if (down?)
      new-mem & ins--sub(be, new-mem, mem, 4 * i);
      new-count & ins--add(be, new-count, count, i);
    else
      new-mem & ins--add(be, new-mem, mem, 4 * i);
      new-count & ins--sub(be, new-count, count, i);
    end;
  end if;
end method;

define method op--move-to-registers
    (be :: <harp-back-end>, num-args :: <integer>,
     #rest args)
 => (args-on-stack :: <sequence>)
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let num-cases = min(max(max-num-arg-regs - num-args, 0), args.size);

  for (i :: <integer> from 0 below num-cases)
    ins--move(be, be.registers.reg-machine-arguments[i + num-args], args[i]);
  end for;
  copy-sequence(args, start: num-cases);
end method;

/*
define method op--allocate-registers
    (be :: <harp-back-end>, num-args :: <integer>)
 => (registers :: <simple-object-vector>)
end method;
*/

define macro with-preserved-argument-registers
  { with-preserved-argument-registers (?start:expression) ?:body end }
    => { with-harp (?=be)

           let regs =
	     copy-sequence(?=be.registers.reg-machine-arguments,
			   start: ?start);

           for (reg in regs) ins--push(?=be, reg) end;
           ?body;
           for (reg in regs.reverse) ins--pop(?=be, reg) end;

         end with-harp }
end macro;


define method op--map-calling-convention
    (be :: <harp-back-end>, count, reg :: <integer>)
 => ()
  with-harp (be)
    nreg stack-count, stack-size, arg-start, src;
    stack stack;
    tag registers-to-stack, stack-to-registers, stack-to-stack, done;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let c-max-num-arg-regs = be.registers.c-arguments-passed-in-registers;
  let num-R2R-cases = min(max(c-max-num-arg-regs - reg, 0), max-num-arg-regs);
  let tags :: <simple-object-vector> = make-tags(be, num-R2R-cases);

  for (tag :: <tag> in tags,
       i :: <integer> from num-R2R-cases - 1 by -1)
    ins--bgt(be, tag, count, i);
  end for;

  ins--beq(be, done, count, 0);

  for (tag :: <tag> in tags,
       i :: <integer> from num-R2R-cases - 1 by -1)
    ins--tag(be, tag);
    map-registers-case-generator
      (be, i, reg, direction: #"C-to-Dylan");
    if (i == num-R2R-cases - 1)
      ins--beq(be, done, count, num-R2R-cases);
      if (max(c-max-num-arg-regs - reg, 0) > max-num-arg-regs)
	ins--bra(be, registers-to-stack);
      else
	ins--bra(be, stack-to-registers);
      end;
    else
      ins--bra(be, done);
    end;
  end for;

  let old-args-on-stack = c-arguments-on-stack(be, reg);

  let num-R2S-cases = max(c-max-num-arg-regs - num-R2R-cases - reg, 0);
  let tags :: <simple-object-vector> = make-tags(be, num-R2S-cases);

  unless (num-R2S-cases == 0)
    ins--tag(be, registers-to-stack);
    op--copy-registers-with-update
      (be, stack, #f, stack, count, reg + num-R2R-cases,
       down?: #t, C?: #t,
       base-count: max-num-arg-regs,
       done-tag: done,
       continue-tag: stack-to-stack);

    ins--tag(be, stack-to-stack);
    // Prepare source stack.
    // start of args on stack/First arg to copy
    ins--load-address-of-stack-arg-n(be, src, old-args-on-stack + num-R2S-cases);
    // Prepare target stack.
    // (max-num-arg-regs in registers) + (num-R2S-cases on stack) args
    // have already been passed
    ins--sub(be, stack-count, count, max-num-arg-regs + num-R2S-cases);
  end;

  let num-S2R-cases = max(max-num-arg-regs - num-R2R-cases, 0);
  let tags :: <simple-object-vector> = make-tags(be, num-S2R-cases);

  unless (num-S2R-cases == 0)
    ins--tag(be, stack-to-registers);
    // Prepare source stack.
    // start of args on stack
    ins--load-address-of-stack-arg-n(be, arg-start, old-args-on-stack);
    op--copy-registers-with-update
      (be, src, #f, arg-start, count, num-R2R-cases,
       to?: #t,
       base-count: max(c-max-num-arg-regs - reg, 0),
       done-tag: done,
       continue-tag: stack-to-stack);

    ins--tag(be, stack-to-stack);
    // Prepare target stack.
    // max-num-arg-regs args are passed in registers
    ins--sub(be, stack-count, count, max-num-arg-regs);
  end;

  // Copy any (additional) args which are passed on the stack to the stack.
  ins--asl(be, stack-size, stack-count, 2); // size in bytes of args on stack
  ins--sub(be, stack, stack, stack-size);
  op--copy-words-with-update(be, #f, stack, src, stack-count);

  ins--tag(be, done);

  end with-harp;

end method;

define method map-registers-case-generator
    (be :: <harp-back-end>, i :: <integer>, reg :: <integer>,
     #key direction = #"C-to-Dylan")
 => ()
  for (j :: <integer> from reg to reg + i)
    select (direction)
      #"C-to-Dylan" =>
	ins--move(be, argument-register(j - reg), c-argument-register(j));
      #"Dylan-to-C" =>
	ins--move(be, c-argument-register(j - reg), argument-register(j));
    end;
  end;
end method;


define macro if-return-address
  { if-return-address () ?:body end }
    => { 
	 if (?=be.return-address-on-stack?)
           ?body;
	 end;
       }

    { if-return-address () ?body-1:body else ?body-2:body end }
    => { 
	 if (?=be.return-address-on-stack?)
           ?body-1
	 else
           ?body-2
	 end;
       }
end macro;

define open generic return-address-on-stack?
    (be :: <harp-back-end>) => (on-stack? :: <boolean>);

define method return-address-on-stack?
    (be :: <harp-back-end>) => (on-stack? :: <boolean>)
  #f
end method;

define method return-address-size
    (be :: <harp-back-end>) => (size :: <integer>)
  if (be.return-address-on-stack?) 1
  else 0
  end;
end method;

define method return-address-size-in-bytes
    (be :: <harp-back-end>) => (size :: <integer>)
  4 * be.return-address-size
end method;

