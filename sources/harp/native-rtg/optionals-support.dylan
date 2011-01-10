module:    native-rtg
Synopsis:  Support for entry points with optionals 
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// General support for XEPs with optional arguments


// OP--VECTOR-UP-REST-ARGS
// Shuffles the stack and builds the #rest vector, making
// space for the keys in the process. 
// On exit
//  Value 1
//   A register is returned containing the location of the first
//   optional argument (just after the size field of the stack vector)
//  Value 2
//   A register is returned containing a pointer to the rest vector

define method op--vector-up-rest-args-special
    (be :: <harp-back-end>, required :: <integer>, keys-size) 
    => (first-opt :: <register>, rest-vec :: <register>)
  // Less than (max-num-arg-regs) required args is a special case
  // because the rest args pointer goes into an arg register; 
  // we may or may not have to push the register args onto the stack.
  // Make space for the count, the vector header & the size and any keyword 
  // parameters. The rest arg goes into an arg register.

  let gap = 3;  // space for the 3 words above.

  with-harp (be)
    tmp 1, ret-addr;
    nreg save-argc, opts;
    arg-count argc;
    stack stack;
    tag tag1;

    // Get hold of the return address
    if-return-address() ins--pop(be, ret-addr) end;
  
    // Sort out the pathological case where optional args are in registers
    op--copy-registers-with-update
      (be, stack, #f, stack, argc, required, down?: #t, base-count: required);

    // Calculate the size of the space:
    let tot-space = needed-space-in-bytes(be, gap, keys-size);

    // Now shuffle along the stack
    ins--move(be, opts, stack);    // address of the start of the optionals
    ins--sub(be, stack, stack, tot-space); // Make space for the gap

    // Put back the return address
    if-return-address() ins--push(be, ret-addr) end;
  
    // Now initialize the vector
    let rest-vec = 
      op--initialize-vector-header(be, opts, argc, save-argc, 
                                   gap, required, keys-size);
  
    // return the location of the first optional
    values(opts, rest-vec);
  end with-harp;
end method;



define method op--vector-up-rest-args 
    (be :: <harp-back-end>, required, keys-size)
    => (first-opt :: <register>, rest-vec :: <register>)
  if (special-case?(be, required))
    op--vector-up-rest-args-special(be, required, keys-size);
  else
    // General case for shuffling the stack and stack allocating #rest.
    // Make space for the rest arg, the count, the vector header & the size
    // and any keyword parameters
    let gap = 4; // space for the 4 words above;

    with-harp (be)
      arg-count argc;
      nreg opts, save-argc;

      let top-size = op--shuffle-size-for-requireds(be, required);
      
      // Shuffle stack to make space for the headers etc.
      op--shuffle-stack(be, opts, save-argc, top-size, gap, keys-size);
      
      // Now initialize the vector
      let rest-vec =
	op--initialize-vector-header(be, opts, save-argc, argc, 
				     gap, required, keys-size);
      
      // return the location of the first optional
      values(opts, rest-vec);
    end with-harp;
  end if;
end method;



/// OP--SHUFFLE-SIZE-FOR-REQUIREDS
/// This returns either an integer or a register containing the number
/// of words of data at the top of stack which must be shuffled to make
/// room for the keywords and vector header. The result is based on the
/// the number of required arguments.

define method op--shuffle-size-for-requireds
    (be :: <harp-back-end>, requireds :: <integer>) 
    => (r :: <integer>);
  let args-in-regs = required-arguments-in-registers(be, requireds);
  // allow for the return address
  let extras-on-stack = be.return-address-size;
  let shuffle-size = requireds + extras-on-stack - args-in-regs;
  shuffle-size;
end method;


define method op--shuffle-size-for-requireds
    (be :: <harp-back-end>, requireds == #"dynamic") 
    => (r :: <register>);
  let copy-count = make-n-register(be);
  let args-in-regs = required-arguments-in-registers(be, requireds);
  // allow for the return address;
  let extras-on-stack = be.return-address-size;
  let shuffle-size = extras-on-stack - args-in-regs;
  // now calculate the amount to copy. Its number-required + shuffle-size
  op--number-required(be, copy-count);
  unless (shuffle-size == 0)
     ins--add(be, copy-count, copy-count, shuffle-size);  // amount to copy
  end unless;
  copy-count;
end method;



// OP--INITIALIZE-VECTOR-HEADER 
// Makes a valid stack allocated vector header.
// On entry:
//   DATA is the address of the start of the vector data
//   ARGC-1 is a register containing the arg-count
//   ARGC-2 is a register to be loaded with a value derived from ARGC-1
// On exit:
//   Return the rest vector

define method op--initialize-vector-header
    (be :: <harp-back-end>, 
     data :: <register>, 
     argc-1 :: <register>, argc-2 :: <register>,
     gap :: <integer>,
     required, keys-size) => (rest-val :: <register>)

  // process the arg count
  ins--asl(be, argc-1, argc-1, 2);  // multiply arg count by 4
  ins--move(be, argc-2, argc-1);    // remember the arg count (times 4)

  // store the vector size
  op--tagged-size-of-optionals(be, argc-1, argc-1, required);
  ins--st(be, argc-1, data, -4);  // size of the vector as tagged int

  // store the class of the stack allocated vector
  ins--st(be, dylan-sov-class, data, -8);  // <simple-object-vector>

  // Calculate the size of the space:
  let safe-keys-size = ensure-safe-key-space(be, keys-size);
  let gap-space = needed-space-in-bytes(be, gap, safe-keys-size);

  // store the count on the stack
  op--calc-count-of-bytes-to-drop(be, argc-2, argc-2, required, gap-space);
  ins--st(be, argc-2, data, -12);  // the count of bytes to drop

  // store the #rest value in the appropriate place

  if (special-case?(be, required))
    // An arg register has value of #rest var
    with-harp (be)
      nreg rest-val;
      ins--add(be, rest-val, data, -8);  // the start of the vector
      ins--move(be, argument-register(required), rest-val);
      rest-val;
    end with-harp;
  else
    // store the the #rest pointer itself
    let rest-val = op--put-rest-variable-on-stack(be, data, gap-space);
    rest-val;
  end if;
end method;


define method op--put-rest-variable-on-stack
    (be :: <harp-back-end>, 
     data :: <register>,
     gap-space :: <register>)
     => (rest-val :: <register>)
  with-harp (be)
    tmp 1, dest;
    nreg rest-val;
    ins--sub(be, dest, data, gap-space);
    ins--add(be, rest-val, data, -8);  // the start of the vector
    ins--st(be, rest-val, dest, 0);
    rest-val;
  end with-harp;
end method;

define method op--put-rest-variable-on-stack
    (be :: <harp-back-end>, 
     data :: <register>,
     gap-space :: <integer>)
     => (rest-val :: <register>)
  let rest-val = make-n-register(be);
  ins--add(be, rest-val, data, -8);  // the start of the vector
  ins--st(be, rest-val, data, - gap-space);
  rest-val;
end method;


define method op--calc-count-of-bytes-to-drop
    (be :: <harp-back-end>, 
     dest :: <register>, argc-times-4 :: <register>,
     required, gap-bytes :: <integer>)
  let args-in-regs = required-arguments-in-registers(be, required);
  let reg-bytes = 4 * args-in-regs; // bytes of required args in registers
  ins--add(be, dest, argc-times-4, gap-bytes - reg-bytes);
end method;


define method op--calc-count-of-bytes-to-drop
    (be :: <harp-back-end>, 
     dest :: <register>, argc-times-4 :: <register>,
     required, gap-bytes :: <register>)
  let args-in-regs = required-arguments-in-registers(be, required);
  let reg-bytes = 4 * args-in-regs; // bytes of required args in registers
  ins--add(be, dest, argc-times-4, gap-bytes);
  unless (reg-bytes == 0)
    ins--sub(be, dest, dest, reg-bytes);
  end unless;
end method;



// ENSURE-SAFE-KEY-SPACE
// To relieve register pressure for some back-ends.

define open generic ensure-safe-key-space 
    (be :: <harp-back-end>, key-space);

define method ensure-safe-key-space 
    (be :: <harp-back-end>, key-space)
  key-space;
end method;



// OP-TAGGED-SIZE-OF-OPTIONALS 
// Loads dest with the size of the optionals vector as a tagged integer. 
// It should be ((4 * opt-num) + 1), but argc-times-4 is already 
// ((4 * opt-num) + (4 * required)), we can actually use
// (argc-times-4 + 1 - (4 * required))

define method op--tagged-size-of-optionals 
    (be :: <harp-back-end>, 
     dest :: <register>,
     argc-times-4 :: <register>, 
     required :: <integer>)
  let opt-fix = 1 - (4 * required);
  ins--add(be, dest, argc-times-4, opt-fix); // optional argc as tagged int
end method;


define method op--tagged-size-of-optionals 
    (be :: <harp-back-end>, 
     dest :: <register>,
     argc-times-4 :: <register>, 
     required == #"dynamic")
  let req-times-4 = be.registers.reg-tmp1;
  op--number-required-times-4(be, req-times-4);
  ins--sub(be, dest, argc-times-4, req-times-4);
  ins--add(be, dest, dest, 1);
end method;



// OP--SHUFFLE-STACK
// This shuffles TOP-SIZE objects at top of stack (including the return 
// address) to leave space for keys and the #rest vector header.
// On entry:
//   OPTS        Dest register (for storing location of optionals)
//   SAVE-ARGC   If a register is given, then the arg-count is preserved 
//               in this register across the shuffle.
//   TOP-SIZE    is the total number of words to move
//   OPT-SPACE   is the size in words needed for the #rest header
//   KEY-SPACE   is the size in words needed for the keys (either in
//               a register or as an integer)
// Results:
//   OPTS: register points to the lowest free address of extra  space.


define open generic op--shuffle-stack
    (be :: <harp-back-end>, 
     opts :: <register>, 
     save-argc,
     top-size, 
     opt-space :: <integer>, 
     key-space);

define method op--shuffle-stack
    (be :: <harp-back-end>, 
     opts :: <register>, 
     save-argc,
     top-size :: <integer>, 
     opt-space :: <integer>, 
     key-space)

  with-harp (be)
    arg-count argc;
    stack stack;
    tmp 1, tmp1;
    tmp 2, tmp2;
    nreg from;

    let tot-space = needed-space-in-bytes(be, opt-space, key-space);
  
    ins--sub(be, stack, stack, tot-space);    // make room for the new space
    ins--add(be, from, stack, tot-space);     // start copying from old SP
    let to = stack;                           // start copying to new SP
    if (save-argc)
      ins--move(be, save-argc, argc);         // save the arg count
    end if;
  
    // Do the copying in-line.
    for (i from 0 below top-size - 1 by 2)
      let byte-index = i * 4;
      ins--ld(be, tmp1, from, byte-index);
      ins--ld(be, tmp2, from, byte-index + 4);
      ins--st(be, tmp1, to, byte-index);
      ins--st(be, tmp2, to, byte-index + 4);
      finally
        if (odd?(top-size))
          let byte-index = (top-size - 1) * 4;
          ins--ld(be, tmp1, from, byte-index);
          ins--st(be, tmp1, to, byte-index);
        end if;
    end for;
  
    ins--add(be, opts, from, 4 * top-size);
  end with-harp;
end method;


define method op--shuffle-stack
    (be :: <harp-back-end>, 
     opts :: <register>, 
     save-argc,
     top-size,
     opt-space :: <integer>, 
     key-space) 

  with-harp (be)
    arg-count argc;
    stack stack;
    tmp 1, from;
    nreg to;
    arg-count copy-count;

    let tot-space = needed-space-in-bytes(be, opt-space, key-space);
  
    ins--move(be, from, stack);               // start copying from ret addr
    ins--sub(be, stack, stack, tot-space);    // make room for the new space
    if (save-argc)
      ins--move(be, save-argc, argc);         // save the arg count
    end if;
    ins--move(be, copy-count, top-size);      // and use the reg for the copy
    ins--move(be, to, stack);                 // copy ret-addr to new SP
    op--copy-words-with-update(be, opts, to, from, copy-count);  // Do the copy
  end with-harp;
end method;



define method needed-space-in-bytes 
    (be :: <harp-back-end>, 
     opt-space :: <integer>, key-space :: <integer>) 
    => (r :: <integer>);
  (opt-space + key-space) * 4;
end method;

define method needed-space-in-bytes 
    (be :: <harp-back-end>, 
     opt-space :: <integer>, key-space :: <register>) 
    => (r :: <register>);
  let res = make-n-register(be);
  if (opt-space == 0)
    ins--move(be, res, key-space);
  else
    ins--add(be, res, key-space, opt-space);
  end if;
  ins--asl(be, res, res, 2);
  res;
end method;

