module:    dylan-rtg
Synopsis:  Complex Primitives for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define leaf runtime-primitive stack-allocate-vector
  // On entry:
  //   size  - the size in words of the vector
  // On exit:
  //   A pointer to freshly stack-allocated memory
  op--stack-allocate-vector-internal(be, 0, method (be, vec, size) #f end);
end runtime-primitive;


define leaf runtime-primitive stack-allocate-vector-from-buffer
  // On entry:
  //   size  - the size in words of the vector
  //   buffer - buffer of fill data
  // On exit:
  //   A pointer to freshly stack-allocated memory

  stack stack;
  tmp 1, buffer;

  op--load-arguments-leafcase(be, #f, buffer);
  let manipulate-vector =
    method (be :: <harp-back-end>, vec :: <register>, size :: <register>)
      op--fill-vector-from-buffer(be, vec, buffer, size);
    end method;
  op--stack-allocate-vector-internal(be, 4, manipulate-vector);
end runtime-primitive;


define leaf runtime-primitive stack-allocate-vector-from-buffer-with-offset
  // On entry:
  //   size  - the size in words of the vector
  //   buffer - buffer of fill data
  //   offset - offset into buffer for start of copy
  // On exit:
  //   A pointer to freshly stack-allocated memory

  stack stack;
  tmp 1, buffer;
  nreg offset;

  op--load-arguments-leafcase(be, #f, buffer, offset);
  ins--asl(be, offset, offset, 2);        // get the offset in bytes
  ins--add(be, buffer, buffer, offset);   // and add to buffer
  let manipulate-vector =
    method (be :: <harp-back-end>, vec :: <register>, size :: <register>)
      op--fill-vector-from-buffer(be, vec, buffer, size);
    end method;
  op--stack-allocate-vector-internal(be, 8, manipulate-vector);
end runtime-primitive;


/// heap-vector-remaining-values is not actually a leaf primitive
/// but it is included here because of its similarity with its
/// stack allocating counterpart
///
define runtime-primitive heap-vector-remaining-values
  // On entry:
  //   offset  - the index of the first MV to stack allocate
  //           - THIS USES AN UNUSUAL CALLING CONVENTION
  //           - the argument is passed in MList so we don't clobber
  //           - any initial values.
  // On exit:
  //   A pointer to freshly allocated memory of all included values

  mlist offset;
  result result;
  tmp 1, buffer;
  arg0 size;
  nreg nsize, nbuffer;

  op--buffer-up-remaining-values(be, buffer, size, offset);
  ins--move(be, nbuffer, buffer);
  ins--move(be, nsize, size);
  ins--move(be, result, op--make-vector-from-data(be, nbuffer, nsize));
  ins--rts-and-drop(be, 0);

end runtime-primitive;


define leaf runtime-primitive stack-vector-remaining-values
  // On entry:
  //   offset  - the index of the first MV to stack allocate
  //           - THIS USES AN UNUSUAL CALLING CONVENTION
  //           - the argument is passed in MList so we don't clobber
  //           - any initial values.
  // On exit:
  //   A pointer to freshly stack-allocated memory of all included values

  arg0 size;
  mlist offset;
  tmp 1, buffer;

  op--buffer-up-remaining-values(be, buffer, size, offset);

  local method manipulate-vector 
            (be :: <harp-back-end>, vec :: <register>, size :: <register>)
          op--fill-vector-from-buffer(be, vec, buffer, size);
        end method;
  op--stack-allocate-vector-internal(be, 0, manipulate-vector);
end runtime-primitive;


/// op--buffer-up-remaining-values is a DDU op which ignores MVs up to
/// offset, sets the buffer register to point to an appropriately
/// fillled part of the MV area containing the remaining values, and
/// sets the size register to point to the size of valid data.
/// This is carefully implemented so that buffer may be a temporary
/// register.
///
define method op--buffer-up-remaining-values
    (be :: <harp-back-end>, buffer :: <register>, 
     size :: <register>, offset :: <register>)
  with-harp (be)
    tag have-mvs, mvs-done, skip-it, continue;
    arg0 single-value;
    nreg count;
    
    op--ld-mv-area-address(be, buffer);
    ins--bmvset(be, have-mvs);
    
    // Case where there is a single value
    ins--bne(be, skip-it, offset, 0);
    
    // Case where we want the single value
    ins--st(be, single-value, buffer, 0); // put only value in MV area
    ins--move(be, size, 1);
    ins--bra(be, mvs-done);
    
    // Case where we don't want the single value
    ins--tag(be, skip-it);
    ins--move(be, size, 0);
    ins--bra(be, mvs-done);
    
    // Case where there are multiple values
    ins--tag(be, have-mvs);
    op--ld-mv-count(be, count);
    ins--bge(be, skip-it, offset, count);   // Check for values all used up
  
    // Case where we need to copy values from the MV area
    // First, ensure first value in MV area
    ins--bne(be, continue, offset, 0);
    ins--st(be, single-value, buffer, 0);
    ins--tag(be, continue);
    ins--sub(be, count, count, offset);     // Number of interesting values
    ins--asl(be, offset, offset, 2);        // the MV offset in bytes
    ins--add(be, buffer, buffer, offset);   // address of first interesting value
    ins--move(be, size, count);
    ins--bra(be, mvs-done);
    
    ins--tag(be, mvs-done);
  end with-harp;
end method;


define method op--fill-vector-from-buffer
    (be :: <harp-back-end>, 
     vec :: <register>, buffer :: <register>, size :: <register>)
  with-harp (be)
    nreg vec-data;
    arg-count nsize;  // good register for the pentium
    ins--add(be, vec-data, vec, 8);
    ins--move(be, nsize, size);
    op--copy-words-with-update(be, #f, vec-data, buffer, nsize);
  end with-harp;
end method;


define method op--stack-allocate-vector-internal
     (be :: <harp-back-end>, 
      args-to-drop :: <integer>, 
      manipulate-vector :: <function>)
  // On entry:
  //   size  - the size in words of the vector
  // On exit:
  //   A pointer to freshly stack-allocated memory, after any manipulations
  with-harp (be)
    arg0 arg0;
    result result;
    stack stack;
    nreg ret-addr, size, vec, vec-size-in-bytes;

    if-return-address() ins--pop(be, ret-addr) end;
    unless (args-to-drop == 0)
      ins--add(be, stack, stack, args-to-drop);
    end unless;
    ins--move(be, size, arg0);
    ins--asl(be, vec-size-in-bytes, size, 2);  // get the size in bytes
    ins--add(be, vec-size-in-bytes, vec-size-in-bytes, 8); // allow for header
    ins--sub(be, stack, stack, vec-size-in-bytes); // do the allocation
    ins--move(be, vec, stack);
    op--set-vector-size(be, size, vec);
    op--set-vector-class(be, vec);
    manipulate-vector(be, vec, size);
    ins--move(be, result, vec);
    if-return-address()
      ins--jmp(be, ret-addr, 0);
    else
      ins--rts(be);
    end;
  end with-harp;
end method;


define leaf runtime-primitive mep-apply
  // On entry:
  //    function, next-methods, argument-vector
  //    (the argument-vector does not have any optionals vectored up)
  // On exit:
  //    tail call the mep with the appropriate arguments
  // Strategy:
  //    We are called with 3 arguments. Look to see if we 
  //    want to end up with more or less on the stack or
  //    in registers depending on the active calling convention.
  //    Set up space on the stack for the arguments as required.
  //    Block copy from the buffer into the stack space,
  //    and into required argument registers.
  //    Set up the mlist and argument registers.
  //    Tail call the MEP
  //    We have a different version for optionals because 
  //    optionals require a count 
  //
  op--mep-apply-select(be, 
                       op--simple-mep-apply-internal, 
                       op--optionals-mep-apply-unvectored-internal);
end runtime-primitive;


define leaf runtime-primitive mep-apply-with-optionals
  // On entry:
  //    function, next-methods, argument-vector
  //    (the argument-vector has any optionals already vectored up)
  // On exit:
  //    tail call the mep with the appropriate arguments
  // Strategy:
  //    We are called with 3 arguments. Look to see if we 
  //    want to end up with more or less on the stack or
  //    in registers depending on the active calling convention.
  //    Set up space on the stack for the arguments as required.
  //    Block copy from the buffer into the stack space,
  //    and into required argument registers.
  //    Set up the mlist and argument registers.
  //    Tail call the MEP
  //    We have a different version for optionals because 
  //    optionals require a count 
  //
  op--mep-apply-select(be, 
                       op--simple-mep-apply-internal, 
                       op--optionals-mep-apply-vectored-internal);
end runtime-primitive;


define method op--mep-apply-select
     (be :: <harp-back-end>, no-opts :: <function>, with-opts :: <function>)
  // Select between functions with and without optionals, and dispatch to
  // the appropriate code.

  with-harp (be)
    function function;
  
    nreg mlist-reg, vector-reg, vec, vec-data;
    arg-count argnum;
  
    tag tag-have-optionals;
  
    // prolog
    op--load-arguments(be, function, mlist-reg, vector-reg);
    ins--move(be, vec, vector-reg);
    op--vector-size(be, argnum, vec);
    ins--add(be, vec-data, vec, 8);
  
    // do the hard work
    op--branch-if-function-with-optionals(be, tag-have-optionals);
    no-opts(be, mlist-reg, vec-data, argnum);
    ins--tag(be, tag-have-optionals);
    with-opts(be, mlist-reg, vec-data, argnum);
  end with-harp;
end method;


define method op--simple-mep-apply-internal
     (be :: <harp-back-end>, 
      mlist :: <register>, vec-data :: <register>, argnum :: <register>)
  with-harp (be)
    tmp 1, tmp;
    stack stack;
    tag tag-many-stack-args;
    nreg arg-dest;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    let old-args = 3;

    let num-cases = 1 + max(max-num-arg-regs, old-args);
    let tags :: <simple-object-vector> = make-tags(be, num-cases);

    // test for the special cases
    for (tag :: <tag> in tags,
	 i :: <integer> from 0)
      ins--beq(be, tag, argnum, i);
    end for;

    // fall through
    ins--bra(be, tag-many-stack-args);

    // generate all the special cases
    for (tag :: <tag> in tags,
	 i :: <integer> from 0)
      ins--tag(be, tag);
      mep-apply-case-generator(be, i, mlist, vec-data, tmp);
    end for;
  
    // case where we need to extend the stack
    ins--tag(be, tag-many-stack-args);
    let retaddr = tmp;
    // the number of args which go on the stack
    ins--sub(be, argnum, argnum, max-num-arg-regs);
    // pop the return address
    if-return-address() ins--pop(be, retaddr) end;
    let old-args-on-stack = arguments-on-stack(be, old-args);
    unless (old-args-on-stack == 0)
      // maybe remove mlist and arg-vec
      ins--add(be, stack, stack, 4 * old-args-on-stack);
    end;
    ins--sub(be, stack, stack, op--multiply-by-4(be, argnum));
    ins--move(be, arg-dest, stack);
    // restore ret addr after making space
    if-return-address() ins--push(be, retaddr) end;
    // don't copy arguments designated for registers
    ins--add(be, vec-data, vec-data, 4 * max-num-arg-regs);
    op--copy-words-with-update(be, #f, arg-dest, vec-data, argnum);
    op--tail-call-mep-loading-arg-regs
      (be, max-num-arg-regs, mlist, vec-data, -4 * max-num-arg-regs);
  end with-harp;
end method;


define method op--optionals-mep-apply-vectored-internal
     (be :: <harp-back-end>, 
      mlist :: <register>, vec-data :: <register>, argnum :: <register>)
  // For methods with optionals, we must allow for the extra count word
  // on the stack, and we must set it correctly.
  // NB the vectored optionals are not in our frame [because we were not
  // called with optional arguments]. This will prohibit tail-calling
  // of this primitive by the function which vectored up the optionals.

  with-harp (be)
    tmp 1, tmp;
    stack stack;
    tag tag-many-stack-args;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    let old-args = 3;
    let old-args-on-stack = arguments-on-stack(be, old-args);

    let num-cases =
      // A special case for each fillable register
      max-num-arg-regs
      // Also, a special case for all filled registers plus
      // a replaceable stack location
      + (if (old-args-on-stack > 0) 1 else 0 end); 
    let tags :: <simple-object-vector> = make-tags(be, num-cases);
  
    // test for the special cases
    // (NB argnum must be at least 1 because of #rest)
    for (tag :: <tag> in tags,
	 i :: <integer> from 1)
      ins--beq(be, tag, argnum, i);
    end for;

    // fall through
    ins--bra(be, tag-many-stack-args);

    // generate all the special cases
    for (tag :: <tag> in tags,
	 i :: <integer> from 1)
      ins--tag(be, tag);
      mep-apply-case-generator(be, i, mlist, vec-data, tmp,
			       optionals?: #t);
    end for;

    // case where we need to extend the stack
    ins--tag(be, tag-many-stack-args);
    let arg-dest = make-n-register(be);
    let retaddr = tmp;

    // First get the count right. 
    let adjust = 4 * (1 - max-num-arg-regs);
    let argnum-in-bytes = op--multiply-by-4(be, argnum);
    unless (adjust == 0)
      ins--add(be, argnum-in-bytes, argnum-in-bytes, adjust);
    end;
    if (old-args-on-stack == 0)
      // No room on the stack for the count
      // pop the return address
      if-return-address() ins--pop(be, tmp) end;
      ins--push(be, argnum-in-bytes);     // push count
    else
      let last-old-arg-on-stack = max(old-args-on-stack - 1, 0);
      ins--store-stack-arg-n(be, argnum-in-bytes, last-old-arg-on-stack);
      // pop the return address
      if-return-address() ins--pop(be, retaddr) end;
    end;

    // Now arrange for the stack extension and fill.
    // the number of args which go on the stack
    ins--sub(be, argnum, argnum, max-num-arg-regs);
    // one old-arg-on-stack may be replaced by the count on the stack
    let old-args-to-pop = max(old-args-on-stack - 1, 0);
    unless (old-args-to-pop == 0)
      // maybe remove mlist leaving count on stack
      ins--add(be, stack, stack, 4 * old-args-to-pop);
    end;
    ins--sub(be, stack, stack, op--multiply-by-4(be, argnum));
    ins--move(be, arg-dest, stack);
    // restore ret addr after making space
    if-return-address() ins--push(be, retaddr) end;
    // don't copy arguments designated for registers
    ins--add(be, vec-data, vec-data, 4 * max-num-arg-regs);
    op--copy-words-with-update(be, #f, arg-dest, vec-data, argnum);
    op--tail-call-mep-loading-arg-regs
      (be, max-num-arg-regs, mlist, vec-data, -4 * max-num-arg-regs);
  end with-harp;
end method;

define method mep-apply-case-generator
    (be :: <harp-back-end>, num-args :: <integer>,
     mlist :: <register>, vec-data :: <register>, tmp :: <register>,
     #key optionals?)
 => ()
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let old-args = 3;
  let old-args-on-stack = arguments-on-stack(be, old-args);
  let args-on-stack :: <integer> = arguments-on-stack(be, num-args);
  let total-args-on-stack =
    if (optionals?) args-on-stack + 1 else args-on-stack end;
  let args-in-regs :: <integer> = arguments-in-registers(be, num-args);
  let adjust = max(old-args-on-stack - total-args-on-stack, 0);
  for (i :: <integer> from 0 below args-on-stack,
       j :: <integer> from adjust)
    // don't copy arguments designated for registers
    ins--ld(be, tmp, vec-data, 4 * (max-num-arg-regs + i));
    ins--store-stack-arg-n(be, tmp, j);
  end;
  let stack = be.registers.reg-stack;
  if (optionals?)
    // count is total-args-on-stack bytes
    if (old-args-on-stack == 0)
      // No room on the stack for the count
      // pop the return address
      if-return-address() ins--pop(be, tmp) end;
      ins--push(be, 4 * total-args-on-stack); // push count
      // restore ret addr after making space
      if-return-address() ins--push(be, tmp) end;
    else
      let last-old-arg-on-stack = max(old-args-on-stack - 1, 0);
      ins--store-stack-arg-n(be, 4 * total-args-on-stack,
			     last-old-arg-on-stack);
    end;
  end;
  unless (adjust == 0)
    // get rid of the extra args on the stack;
    // (<= old-args depending on case & active calling convention)
    ins--move-return-address(be, #f, 4 * adjust, #f);
  end;
  op--tail-call-mep-loading-arg-regs
    (be, args-in-regs, mlist, vec-data, 0);
end method;


define method op--optionals-mep-apply-unvectored-internal
     (be :: <harp-back-end>, 
      mlist :: <register>, vec-data :: <register>, argnum :: <register>)
  // Check whether there are any required arguments. If not, then we can
  // use the argument vector as the #rest vector. Otherwise we must
  // do something more complicated.
  with-harp (be)
    nreg required;
    tag rest-in-arg0;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    op--number-required(be, required);
    ins--beq(be, rest-in-arg0, required, 0);

    op--optionals-mep-apply-unvectored-with-required
      (be, mlist, vec-data, argnum);

    ins--tag(be, rest-in-arg0);
    op--optionals-mep-apply-unvectored-no-required(be, mlist, vec-data, argnum);
  end with-harp;
end method;


define method op--optionals-mep-apply-unvectored-no-required
     (be :: <harp-back-end>, 
      mlist :: <register>, vec-data :: <register>, argnum :: <register>)
  // If there are no required arguments, then the vector of supplied
  // arguments is appropriate for use as the #rest vector.
  with-harp (be)
    stack stack;
    arg0 arg0;
    mlist regs-mlist;
    nreg tmp;

    let old-args = 3;
    let old-args-on-stack = arguments-on-stack(be, old-args);

    ins--sub(be, arg0, vec-data, 8);  // get back the vector as a 1st class obj
    // count of args to be removed (the count itself)
    if (old-args-on-stack == 0)
      // No room on the stack for the count
      // pop the return address
      if-return-address() ins--pop(be, tmp) end;
      // push count
      ins--push(be, 4);
      // restore ret addr after making space
      if-return-address() ins--push(be, tmp) end;
    else
      let last-old-arg-on-stack = max(old-args-on-stack - 1, 0);
      ins--store-stack-arg-n(be, 4, last-old-arg-on-stack);
      unless (last-old-arg-on-stack == 0)
	ins--move-return-address(be, #f, 4 * last-old-arg-on-stack, #f);
      end;
    end;
    ins--move(be, regs-mlist, mlist);
    op--tail-call-mep(be);
  end with-harp;
end method;


define method op--optionals-mep-apply-unvectored-with-required
     (be :: <harp-back-end>, 
      mlist :: <register>, vec-data :: <register>, argnum :: <register>)
  // For methods with optionals, we first copy all of the arguments onto the
  // stack. We then vector up the rest args, as for an XEP call with optionals.
  // Finally, we jump to the MEP.

  with-harp (be)
    stack stack;
    arg-count argc;
    mlist regs-mlist;
    tmp 1, arg-bytes;
    nreg ret-addr, ret-addr2, to, argn, required;


    // Here we copy the argument vector onto the stack. 
    // First make room on the stack for all the arguments

    // pop the return address
    if-return-address() ins--pop(be, ret-addr) end;

    op--preserve-mlist-for-mep-apply(be, mlist);

    ins--asl(be, arg-bytes, argnum, 2);
    ins--sub(be, stack, stack, arg-bytes);  // make room for all the data
    if-return-address()
      // restore the return address
      ins--push(be, ret-addr);
      // space on stack, above return addr
      ins--add(be, to, stack, 4);
    else
      ins--move(be, to, stack);
    end;
    ins--push(be, argnum);       // remember the arg count for later
    // now copy the data onto the stack.
    op--copy-words-with-update(be, #f, to, vec-data, argnum);  // Do the copy

    ins--pop(be, argc);        // setup the arg-count as for an XEP call

    if-return-address()
      ins--add(be, to, stack, 4);
    else
      ins--move(be, to, stack);
    end;
    // Set up our argument registers
    op--copy-registers-with-update
      (be, to, #f, to, argc, 0, to?: #t);
  
    if-return-address()
      ins--pop(be, ret-addr2);
      ins--move(be, stack, to);
      ins--push(be, ret-addr2);
    else
      ins--move(be, stack, to);
    end;

    // Now vector up the #rest arguments
    vector-up-rest-args-case-generator(be, required, 1);

    op--restore-mlist-for-mep-apply(be, mlist);

    ins--move(be, regs-mlist, mlist);
    op--tail-call-mep(be);
  end with-harp;
end method;

define method vector-up-rest-args-case-generator
    (be :: <harp-back-end>, required :: <register>,
     case-1 :: <integer>) => ()
  with-harp (be)
    tag dynamic-case, done;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    let num-cases = max(max-num-arg-regs - case-1, 0);
    let cases? = num-cases > 0;
    let tags :: <simple-object-vector> = make-tags(be, num-cases);

    if (cases?)
      op--number-required(be, required);
    end;
  
    // test for the special cases (#rest in an argument register)
    for (tag :: <tag> in tags,
	 i :: <integer> from case-1)
      ins--beq(be, tag, required, i);
    end for;
    
    // fall through
    cases? & ins--bra(be, dynamic-case);

    // generate all the special cases (#rest in an argument register)
    for (tag :: <tag> in tags,
	 i :: <integer> from case-1)
      ins--tag(be, tag);
      op--vector-up-rest-args(be, i, 0);
      ins--bra(be, done);
    end for;

    // case where #rest goes unto the stack
    cases? & ins--tag(be, dynamic-case);
    op--vector-up-rest-args(be, #"dynamic", 0);
    cases? & ins--tag(be, done);
    
  end with-harp;
end method;

define open generic op--preserve-mlist-for-mep-apply
    (be :: <harp-back-end>, mlist :: <register>) => ();

define open generic op--restore-mlist-for-mep-apply
    (be :: <harp-back-end>, mlist :: <register>) => ();

define method op--preserve-mlist-for-mep-apply
    (be :: <harp-back-end>, mlist :: <register>) => ()
end method;

define method op--restore-mlist-for-mep-apply
    (be :: <harp-back-end>, mlist :: <register>) => ()
end method;



define method op--tail-call-mep-loading-arg-regs
      (be :: <harp-back-end>, args-in-regs :: <integer>,
       mlist-reg, base,
       offset :: <integer>) => ()
  with-harp (be)
    mlist mlist;
    ins--move(be, mlist, mlist-reg);
    for (i :: <integer> from 0 below args-in-regs)
      ins--ld(be, argument-register(i), base, offset + 4 * i);
    end;
    op--tail-call-mep(be);
  end with-harp;
end method;


define method op--tail-call-mep (be :: <harp-back-end>)
  with-harp (be)
    function function;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    ins--jmp-indirect(be, function, be.function-mep-offset, max-num-arg-regs);
  end with-harp;
end method;



/*  // No longer used ...

define leaf runtime-primitive xep-apply
  // On entry:
  //    function, arg-count, argument-buffer
  // On exit:
  //    tail call the xep with the appropriate arguments
  // Strategy:
  //    Set up space on the stack for the arguments.
  //    Block copy from the buffer into the stack space.
  //    Set up the arg-count and argument registers.
  //    Tail call the XEP
  //

  arg0 arg0;
  tmp1 tmp;
  function function;
  stack stack;
  arg-count argc;
  nreg arg-dest, argnum;
  arg-count args-minus-one;
  nreg buffer-arg;
  arg0 buffer;
  tmp1 retaddr;
  nreg dummy;
  tag tag1, tag2;
  
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;

  // prolog
  op--load-arguments(be, function, argnum, buffer-arg);
  ins--move(be, buffer, buffer-arg);
  
  // calculate the number of arguments which live on the stack
  ins--beq(be, tag1, argnum, 0);    // test if the buffer is empty
  ins--sub(be, args-minus-one, argnum, 1);  // one arg goes in a register
  
  // get rid of the 2 stack arguments and make space for the new args
  ins--pop(be, retaddr);            // pop the return address
  ins--add(be, stack, stack, 8);    // remove argnum and buffer
  ins--sub(be, stack, stack, op--multiply-by-4(be, args-minus-one));
  ins--move(be, arg-dest, stack);
  ins--push(be, retaddr);           // restore ret addr after making space
  
  // copy the arguments from the buffer onto the stack
  let buffer-next = tmp;
  ins--add(be, buffer-next, buffer, 4);  // location of second arg
  op--copy-words-with-update(be, dummy, arg-dest, buffer-next, args-minus-one);
  
  // fill the argument register with the first arg
  ins--ld(be, arg0, buffer, 0);
  
  // tail call the XEP
  ins--tag(be, tag2);
  ins--move(be, argc, argnum);
  ins--jmp-indirect(be, function, be.function-xep-offset, max-num-arg-regs);
  
  // case where there are no args
  ins--tag(be, tag1);
  ins--move-return-address(be, #f, 8, #f); // pop the args on the stack
  ins--bra(be, tag2);                      // tail call the XEP
end runtime-primitive;

*/




define leaf runtime-primitive apply
  // On entry:
  //    function, vector-of-apply-arguments
  // On exit:
  //    tail call the apply-xep

  tmp 1, tmp;
  stack stack;
  function function;
  arg-count argc;
  nreg count, copy-count, arg-bytes, ret-addr, src, dst;
  reg vec;
  tag general-case;

  local method apply-ref (requireds)
          ins--constant-ref(be, entry-point-name(be, "apply-xep", requireds));
        end method;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  op--load-arguments-leafcase(be, function, vec);
  op--vector-size(be, count, vec);

  // Copy the vector of arguments into the appropriate position on the stack.
  // But, as an optimization, check for "required" argument registers first, 
  // to avoid a block copy.

  let old-args = 2;
  let old-args-on-stack = arguments-on-stack(be, old-args);

  let num-cases =
    // A special case for each fillable register
    max-num-arg-regs
    // Also, a special case for all filled registers plus
    // a replaceable stack location
    + (if (old-args-on-stack > 0) 1 else 0 end);
  let tags :: <simple-object-vector> = make-tags(be, num-cases);

  // test for the special cases
  // (NB count must be at least 1 because of the inner vector)
  for (tag :: <tag> in tags,
       i :: <integer> from 1)
    ins--beq(be, tag, count, i);
  end for;

  // fall through
  ins--bra(be, general-case);

  // generate all the special cases
  for (tag :: <tag> in tags,
       i :: <integer> from 1)
    ins--tag(be, tag);
    apply-case-generator(be, i, vec, tmp);
  end for;

  // General case where we have to copy the arg vector onto the stack.
  // Argument registers will be filled entirely with values in the vector.
  // We also know the vector is bigger than max-num-arg-regs + 1 (inner vector).
  // For special circumstances(1-argument-register calling convention),
  // there may already be the (uwanted) vector parameter on the stack.
  ins--tag(be, general-case);
  // the return address
  if-return-address() ins--pop(be, ret-addr) end;
  ins--asl(be, arg-bytes, count, 2);   // size of all args in bytes
  ins--sub(be, stack, stack, arg-bytes);
  // args to copy onto the stack 
  ins--sub(be, copy-count, count, max-num-arg-regs);
  // remove unwanted required arg regs & possible vector arg
  let adjust = 4 * (old-args-on-stack + max-num-arg-regs);
  ins--add(be, stack, stack, adjust);
  // restore the return address
  if-return-address() ins--push(be, ret-addr) end;
  ins--add(be, dst, stack, 4);         // Destination for copy
  // First argument to copy onto stack
  ins--add(be, src, vec, 8 + 4 * max-num-arg-regs);
  op--copy-words-with-update(be, #f, dst, src, copy-count);  // Do the copy
  // NB For GC purposes, vec must still be live here. So it's not particularly
  // expensive to reuse it to re-calculate the count
  op--vector-size(be, count, vec);
  ins--move(be, argc, count);
  // load "required" arguments into registers
  ins--add(be, vec, vec, 8);
  op--copy-registers-with-update
    (be, #f, #f, vec, max-num-arg-regs, 0, to?: #t);
  ins--jmp(be, apply-ref(#"dynamic"), max-num-arg-regs,
	   function: #t, arg-count: #t);

end runtime-primitive;


define method apply-case-generator
    (be :: <harp-back-end>, num-args :: <integer>,
     vec :: <register>, tmp :: <register>)
 => ()
  with-harp (be)
    stack stack;

  local method apply-ref (requireds)
          ins--constant-ref(be, entry-point-name(be, "apply-xep", requireds));
        end method;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let old-args = 2;
  let old-args-on-stack = arguments-on-stack(be, old-args);
  let args-on-stack :: <integer> = arguments-on-stack(be, num-args);
  let args-in-regs :: <integer> = arguments-in-registers(be, num-args);
  let adjust = max(old-args-on-stack - args-on-stack, 0);

  for (i :: <integer> from 0 below args-in-regs)
    // this is a "required" argument (or the vector itself)
    ins--ld(be, argument-register(i), vec, 8 + 4 * i);
  end;

  // some very special cases
  // (1-argument-register calling convention only)
  if (adjust == 1)
    // 1. Case where there is only an inner vector.
    if-return-address()
      // pop the return address
      ins--pop(be, tmp);
      // overwrite the unwanted vector argument with the ret addr
      ins--st(be, tmp, stack, 0);
    end;
  end;
  if (old-args-on-stack == 1)
    if (old-args-on-stack == args-on-stack)
      // 2. Case where there is a single required argument + an inner vector.
      // this is the vector to apply
      ins--ld(be, tmp, vec, 12);
      // overwrite outer vector with inner vector
      ins--st(be, tmp, stack, 4);
    end;
  end;
  ins--jmp(be, apply-ref(num-args - 1), max-num-arg-regs, function: #t);

  end with-harp;
end method;


/*
// PRIMITIVE-FAST-APPLY could be used as the IEP for the APPLY function.
// It mangles all the arguments in-place on the stack. It's currently not
// used, however.

define leaf runtime-primitive fast-apply
  // On entry:
  //    the args are as for a Dylan call to apply
  // On exit:
  //    tail call the apply-xep

  arg0 arg0;
  tmp1 tmp;
  function function;
  arg-count argc;
  tag no-args-on-stack;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let apply-xep-ref = ins--constant-ref(be, entry-point-name(be, "apply-xep", #"dynamic"));
  ins--move(be, function, arg0); // the function object is the first arg
  ins--ble(be, no-args-on-stack, argc, 1);
  ins--pop(be, tmp);  // the return address
  ins--pop(be, arg0); // first real argument
  ins--push(be, tmp);
  ins--tag(be, no-args-on-stack);
  ins--sub(be, argc, argc, 1);   // the real call doesn't get the function arg
  ins--jmp(be, apply-xep-ref, max-num-arg-regs);
end runtime-primitive;

*/


define leaf runtime-primitive remove-optionals
  // On entry: reg--tmp1  is the caller's index onto the stack which
  //           contains the count (in bytes) of the optionals to drop.
  //    
  // On exit:  The stack has had the optionals removed.
  // Preserved registers:
  //           reg--function, reg--arg-count, reg--arg0, reg--mlist
  //    

  arg0 arg0;
  tmp 1, tmp;
  function function;
  arg-count argc;
  mlist mlist;
  stack stack;
  nreg offset, drop-size, bytes-of-required;
  tmp1 callers-offset;
  
  // preserve the registers we care about 
  let bytes-pushed = op--push-registers-for-remove-optionals(be);
  
  // calculate the new offset of the optionals count allowing
  // for our return address and the registers pushed on the stack:
  ins--add(be, offset, callers-offset,
	   bytes-pushed + be.return-address-size-in-bytes);
  
  // calculate the drop size from the count. It's not the same as the
  // count because there may be required arguments. We first calculate 
  // bytes-of-required which is the bytes of required args for the caller.
  ins--ld(be, drop-size, stack, offset);  // the count from the stack
  if-return-address()
    // don't include ret addr
    ins--sub(be, bytes-of-required, callers-offset, 4);
  else
    ins--move(be, bytes-of-required, callers-offset);
  end;
  ins--sub(be, drop-size, drop-size, bytes-of-required);
  
  // now copy everything below the offset upwards to remove the optionals
  // Use copy-words-up to get the right direction of move to prevent clobbering.
  // The last required argument is copied first. The parameters to copy-words-up
  // are the lowest addresses of the move. (It actually starts copying from the
  // highest addresses)
  // We include in the copy the caller's required args and return address (i.e. 
  // callers-offset bytes). 
  let from-reg = tmp;
  let to-reg = make-n-register(be);
  let how-many = make-n-register(be);

  // move requireds + 0/1 ret addr
  ins--move(be, how-many, callers-offset);
  // start above pushed regs
  ins--add(be, from-reg, stack,
	   bytes-pushed + be.return-address-size-in-bytes);
  ins--add(be, to-reg, from-reg, drop-size);   // move up by drop-size
  ins--copy-words-up(be, to-reg, from-reg, how-many);
  
  // restore the registers we care about
  op--pop-registers-for-remove-optionals(be);
  
  // and return, getting rid of the drop space in the process
  ins--rts-and-drop(be, drop-size);
end runtime-primitive;


define open generic op--push-registers-for-remove-optionals
    (be :: <harp-back-end>) => (bytes-pushed :: <integer>);

define method op--push-registers-for-remove-optionals
    (be :: <harp-back-end>) => (bytes-pushed :: <integer>)
  with-harp (be)
    arg0 arg0;
    arg-count argc;
    mlist mlist;
    function function;
  
    ins--push(be, arg0);
    ins--push(be, argc);
    ins--push(be, mlist);
    ins--push(be, function);

    4 * 4
  end with-harp;
end method;

define open generic op--pop-registers-for-remove-optionals
    (be :: <harp-back-end>) => ();

define method op--pop-registers-for-remove-optionals
    (be :: <harp-back-end>) => ()
  with-harp (be)
    arg0 arg0;
    arg-count argc;
    mlist mlist;
    function function;

    ins--pop(be, function);
    ins--pop(be, mlist);
    ins--pop(be, argc);
    ins--pop(be, arg0);
  end with-harp;
end method;
