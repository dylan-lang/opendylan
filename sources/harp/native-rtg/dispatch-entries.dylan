module:    harp-native-rtg
Synopsis:  Generation of dispatch & engine node entry points for the RTG
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// dispatch-case-limit - this constant determines how many special cases
// there are for selecting arguments. The default version may be used whenever 
// selected argument is on the stack - but it will still be slower than
// a special case version.
//
// Since the choice of special/default version of the function is made
// entirely locally in this file, the only hard requirement is that the value
// is at least as great as the number of arguments passed in registers
// minus 1.

define inline method dispatch-case-limit
    (be :: <harp-back-end>) => (limit :: <integer>)
  be.registers.arguments-passed-in-registers - 1;
end;


// key-check-case-limit is the limit for the key checking entry-points
// This has the same constraints as dispatch-case-limit, but is set slightly
// higher for efficiency reasons.

define inline method key-check-case-limit
    (be :: <harp-back-end>) => (limit :: <integer>)
  dispatch-case-limit(be) + 2
end;


// $discriminator-case-limit is the limit for the discriminator entry-points
// which have special versions for accessing particular args

define constant $discriminator-case-limit = 6;




/// External references

define extensions-iep runtime-external dylan-unbound-instance-slot  
  = "unbound-instance-slot";

define extensions-iep runtime-external dylan-unbound-repeated-slot
  = "unbound-repeated-slot";

define extensions-iep runtime-external dylan-invalid-keyword
  = "invalid-keyword-trap";

define extensions-iep runtime-external dylan-odd-number-keyword-args
  = "odd-number-of-keyword-args-trap";

define extensions-iep runtime-external dylan-repeated-slot-getter-index-oor  
  = "repeated-slot-getter-index-out-of-range-trap";

define extensions-iep runtime-external dylan-repeated-slot-setter-index-oor  
  = "repeated-slot-setter-index-out-of-range-trap";

define dispatch-engine-indirect runtime-external dylan-inapplicable-engine-node  
  = "$inapplicable-engine-node";

define dispatch-engine-indirect runtime-external dylan-absent-engine-node  
  = "$absent-engine-node";







/// ENGINE-NODE-APPLY-WITH-OPTIONALS
///
/// This is very similar to MEP-APPLY-WITH-OPTIONALS - but the "signature" of the
/// engine node is decoded in a separate way from the "signature" of a normal function.

define leaf runtime-primitive engine-node-apply-with-optionals
  // On entry:
  //    function, next-methods, argument-vector
  //    (the argument-vector has any optionals already vectored up)
  // On exit:
  //    tail call the engine-node with the appropriate arguments
  // Strategy:
  //    We are called with 3 arguments. Look to see if we 
  //    want to end up with more or less on the stack or
  //    in registers depending on the active calling convention.
  //    Set up space on the stack for the arguments as required.
  //    Block copy from the buffer into the stack space,
  //    and into required argument registers.
  //    Set up the mlist and argument registers.
  //    Tail call the ENGINE-NODE
  //    We have a different version for optionals because 
  //    optionals require a count 
  //
  op--engine-node-apply-select(be, 
                               op--simple-mep-apply-internal, 
                               op--optionals-mep-apply-vectored-internal);
end runtime-primitive;


define method op--engine-node-apply-select
     (be :: <harp-back-end>, no-opts :: <function>, with-opts :: <function>)
  // Select between functions with and without optionals, and dispatch to
  // the appropriate code.

  with-harp (be)
    function engine;
  
    nreg vector-reg, vec;
    greg disphdr-reg;
    arg-count argnum;
    arg0 arg0;
  
    tag tag-have-optionals;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    // prolog
    op--load-arguments(be, engine, disphdr-reg, vector-reg);
    ins--move(be, vec, vector-reg);
    op--vector-size(be, argnum, vec);
    let vec-data =
      if (max-num-arg-regs = 1) arg0
      else make-n-register(be) end;
    ins--add(be, vec-data, vec, 8);
  
    // do the hard work
    op--branch-if-dispatch-header-with-optionals(be, tag-have-optionals, disphdr-reg);
    no-opts(be, disphdr-reg, vec-data, argnum);
    ins--tag(be, tag-have-optionals);
    with-opts(be, disphdr-reg, vec-data, argnum);
  end with-harp;
end method;



/// GENERAL-ENGINE-NODE-1, GENERAL-ENGINE-NODE-2, GENERAL-ENGINE-NODE-3


define leaf runtime-function general-engine-node-1-entry
  // On entry:
  //   Args as for engine node convention, with 1 required argument
  // On exit:
  //   tail-call the callback with the GF args, followed by the engine node
  //   followed by the GF/DISPATCH-HEADER
  op--general-engine-node-fixed(be, 1);
end runtime-function;


define leaf runtime-function general-engine-node-2-entry
  // On entry:
  //   Args as for engine node convention, with 2 required arguments
  // On exit:
  //   tail-call the callback with the GF args, followed by the engine node
  //   followed by the GF/DISPATCH-HEADER
  op--general-engine-node-fixed(be, 2);
end runtime-function;


define leaf runtime-function general-engine-node-3-entry
  // On entry:
  //   Args as for engine node convention, with 3 required arguments
  // On exit:
  //   tail-call the callback with the GF args, followed by the engine node
  //   followed by the GF/DISPATCH-HEADER
  op--general-engine-node-fixed(be, 3);
end runtime-function;



define method op--general-engine-node-fixed 
    (be :: <harp-back-end>, n :: <integer>) => ()
  with-harp (be)
    function engine;
    mlist disphdr;
    stack stack;
    tmp 1, tmp1;
    nreg callback;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    let args-on-stack :: <integer> = arguments-on-stack(be, n);

    let extra-args :: <sequence> =
      op--move-to-registers(be, n, engine, disphdr);
    let extra-args-size :: <integer> = extra-args.size;

    unless (extra-args-size == 0)
      // Add any extras to the end of the arguments on the stack
      let arg-offset = 0;              // byte offset of first stack arg
      let shift = 4 * extra-args-size;   // amount by which to shuffle the stack
      ins--sub(be, stack, stack, shift);   // make room for the extra args
      // start at 0 to include the return address if any
      for (i :: <integer> from 0 to args-on-stack - 1 + be.return-address-size)  
	ins--ld(be, tmp1, stack, arg-offset + shift);  // load from old location
	ins--st(be, tmp1, stack, arg-offset);          // store to new
	arg-offset := arg-offset + 4;
      end for;
      for (extra in extra-args)
	ins--st(be, extra, stack, arg-offset);
	arg-offset := arg-offset + 4;
      end;
    end;

    // tail-call the callback function 
    ins--ld(be, callback, engine, be.engine-node-callback-offset);
    ins--jmp(be, callback, max-num-arg-regs);
  end with-harp;
end method;


/// GENERAL-ENGINE-NODE-N, GENERAL-ENGINE-NODE-SPREAD


// Need separate version of this for the optionals and non-optionals
// cases. The spread version is only special when there are optionals.


define entry-point general-engine-node-n
      (be :: <harp-back-end>, required, limit: dispatch-case-limit)
  function function;
  mlist mlist;
  greg arg-vec, disphdr, engine;

  ins--move(be, engine, function);
  ins--move(be, disphdr, mlist);
  op--dispatch-header-gf(be, arg-vec, disphdr);
  let req-args = op--required-args-num(be, arg-vec, required);
  op--vector-up-requireds(be, arg-vec, disphdr, req-args);
  ins--preserve-registers-entry(be);
  op--general-engine-node-callback(be, arg-vec, engine, disphdr);
  ins--preserve-registers-exit(be);
  op--return-after-vectoring-requireds(be, req-args);
end entry-point;


define entry-point general-engine-node-n-optionals
      (be :: <harp-back-end>, required, limit: dispatch-case-limit)
  op--general-node-with-optionals
    (be, 
     required, 
     op--vector-up-requireds-and-optionals, 
     op--return-after-vectoring-requireds-and-optionals);
end entry-point;


define entry-point general-engine-node-spread-optionals
      (be :: <harp-back-end>, required, limit: dispatch-case-limit)
  op--general-node-with-optionals
    (be, 
     required, 
     op--vector-up-requireds-spreading-optionals, 
     op--return-after-vectoring-requireds-spreading-optionals);
end entry-point;



define method op--general-node-with-optionals
    (be :: <harp-back-end>, 
     required, 
     vectoring-fn :: <function>, 
     returning-fn :: <function>)
    => ()
  with-harp (be)
    function function;
    mlist mlist;
    greg gf, engine;
  
    ins--move(be, engine, function);
    ins--move(be, gf, mlist);

    op--perform-engine-node-callback
      (be, required, gf, engine, vectoring-fn, returning-fn, 
       op--random-engine-node-required-args-num,
       op--general-engine-node-callback);
  end with-harp;
end method;



/// BOXED-INSTANCE-SLOT-GETTER


define leaf runtime-function boxed-instance-slot-getter-entry
  // On entry: Engine node convention with args (object)
  //    
  // On exit:  The value of the slot
  // 
  function engine;
  result result;
  greg value, tagged-offset, object;
  nreg offset, word-offset;
  tag unbound, return-value;

  op--load-arguments(be, object);
  op--engine-slot-accessor-offset(be, offset, engine);
  ins--ld(be, value, object, offset);
  ins--beq(be, unbound, value, dylan-unbound);

  ins--move(be, result, value);
  ins--tag(be, return-value);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 1);

  ins--tag(be, unbound);
  ins--asr(be, word-offset, offset, 2);
  ins--sub(be, word-offset, word-offset, 1);  // word-offset without wrapper
  op--taggify(be, tagged-offset, word-offset);
  op--call-iep(be, dylan-unbound-instance-slot, object, tagged-offset);
  ins--bra(be, return-value);
end runtime-function;



/// BOXED-INSTANCE-SLOT-SETTER


define leaf runtime-function boxed-instance-slot-setter-entry
  // On entry: Engine node convention with args (newval, object)
  //    
  // On exit:  The value of the slot
  // 
  function engine;
  result result;
  greg newval, object;
  nreg offset;

  op--load-arguments(be, newval, object);
  op--engine-slot-accessor-offset(be, offset, engine);
  ins--st(be, newval, object, offset);
  ins--move(be, result, newval);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 2);
end runtime-function;



/// BOXED-REPEATED-INSTANCE-SLOT-GETTER


define leaf runtime-function boxed-repeated-instance-slot-getter-entry
  // On entry: Engine node convention with args (object, index)
  //    
  // On exit:  The value of the slot
  // 
  function engine;
  result result;
  greg size, value, object, index;
  nreg offset, raw-byte-index, repeat-base;
  tag out-of-range, unbound, return-value;

  // First load the size of the repeated slot
  op--load-arguments(be, object, index);
  op--engine-slot-accessor-offset(be, offset, engine);
  ins--add(be, repeat-base, object, offset); // the address of element 0
  ins--ld(be, size, repeat-base, -4); // the size field
  // NB both index and size are tagged Dylan integers

  // Check that index is in-bounds
  ins--blt(be, out-of-range, index, 0);
  ins--bge(be, out-of-range, index, size);

  // load the value
  op--untaggify-times-4(be, raw-byte-index, index);
  ins--ld(be, value, repeat-base, raw-byte-index);
  ins--beq(be, unbound, value, dylan-unbound);

  ins--move(be, result, value);
  ins--tag(be, return-value);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 2);

  ins--tag(be, out-of-range);
  op--call-iep(be, dylan-repeated-slot-getter-index-oor, object, index);
  ins--bra(be, return-value);

  ins--tag(be, unbound);
  op--call-iep(be, dylan-unbound-repeated-slot, object, index);
  ins--bra(be, return-value);
end runtime-function;



/// BOXED-REPEATED-INSTANCE-SLOT-SETTER


define leaf runtime-function boxed-repeated-instance-slot-setter-entry
  // On entry: Engine node convention with args (newval, object, index)
  //    
  // On exit:  The value of the slot
  // 
  function engine;
  result result;
  greg size, newval, object, index;
  nreg offset, raw-byte-index, repeat-base;
  tag out-of-range, return-value;

  // First load the size of the repeated slot
  op--load-arguments(be, newval, object, index);
  op--engine-slot-accessor-offset(be, offset, engine);
  ins--add(be, repeat-base, object, offset); // the address of element 0
  ins--ld(be, size, repeat-base, -4); // the size field
  // NB both index and size are tagged Dylan integers

  // Check that index is in-bounds
  ins--blt(be, out-of-range, index, 0);
  ins--bge(be, out-of-range, index, size);

  // store the value
  op--untaggify-times-4(be, raw-byte-index, index);
  ins--st(be, newval, repeat-base, raw-byte-index);
  ins--move(be, result, newval);
  ins--tag(be, return-value);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 2);

  ins--tag(be, out-of-range);
  op--call-iep(be, dylan-repeated-slot-getter-index-oor, object, index);
  ins--bra(be, return-value);
end runtime-function;



/*

/// BYTE-SLOT-GETTER


define leaf runtime-function byte-slot-getter-entry
  // On entry: Engine node convention with args (object)
  //    
  // On exit:  The value of the slot as a tagged integer
  // 
  function engine;
  result result;
  greg object;
  nreg offset, raw-byte;

  op--load-arguments(be, object);
  op--engine-slot-accessor-offset(be, offset, engine);
  ins--ldb(be, raw-byte, object, offset);
  op--taggify-as-character(be, result, raw-byte);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 1);
end runtime-function;



/// BYTE-SLOT-SETTER


define leaf runtime-function byte-slot-setter-entry
  // On entry: Engine node convention with args (newval, object)
  //    
  // On exit:  The value of the slot
  // 
  function engine;
  result result;
  greg newval, object;
  nreg offset, rawval;

  op--load-arguments(be, newval, object);
  op--engine-slot-accessor-offset(be, offset, engine);
  op--untaggify(be, rawval, newval);
  ins--stb(be, rawval, object, offset);
  ins--move(be, result, newval);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 2);
end runtime-function;

*/



/// RAW-BYTE-REPEATED-INSTANCE-SLOT-GETTER


define leaf runtime-function raw-byte-repeated-instance-slot-getter-entry
  // On entry: Engine node convention with args (object, index)
  //    
  // On exit:  The value of the slot as a tagged integer
  // 
  function engine;
  result result;
  greg size, object, index;
  nreg offset, raw-byte, raw-byte-index, repeat-base;
  tag out-of-range, return-value;

  // First load the size of the repeated slot
  op--load-arguments(be, object, index);
  op--engine-slot-accessor-offset(be, offset, engine);
  ins--add(be, repeat-base, object, offset); // the address of element 0
  ins--ld(be, size, repeat-base, -4); // the size field
  // NB both index and size are tagged Dylan integers

  // Check that index is in-bounds
  ins--blt(be, out-of-range, index, 0);
  ins--bge(be, out-of-range, index, size);

  // load the value
  op--untaggify(be, raw-byte-index, index);
  ins--ldb(be, raw-byte, repeat-base, raw-byte-index);
  op--taggify-as-character(be, result, raw-byte);
  ins--tag(be, return-value);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 2);

  ins--tag(be, out-of-range);
  op--call-iep(be, dylan-repeated-slot-getter-index-oor, object, index);
  ins--bra(be, return-value);
end runtime-function;




/// RAW-BYTE-REPEATED-INSTANCE-SLOT-SETTER


define leaf runtime-function raw-byte-repeated-instance-slot-setter-entry
  // On entry: Engine node convention with args (newval, object, index)
  //    
  // On exit:  The value of the slot
  // 
  function engine;
  result result;
  greg size, newval, object, index;
  nreg offset, rawval, raw-byte-index, repeat-base;
  tag out-of-range, return-value;

  // First load the size of the repeated slot
  op--load-arguments(be, newval, object, index);
  op--engine-slot-accessor-offset(be, offset, engine);
  ins--add(be, repeat-base, object, offset); // the address of element 0
  ins--ld(be, size, repeat-base, -4); // the size field
  // NB both index and size are tagged Dylan integers

  // Check that index is in-bounds
  ins--blt(be, out-of-range, index, 0);
  ins--bge(be, out-of-range, index, size);

  // store the value
  op--untaggify(be, rawval, newval);
  op--untaggify(be, raw-byte-index, index);
  ins--stb(be, rawval, repeat-base, raw-byte-index);
  ins--move(be, result, newval);
  ins--tag(be, return-value);
  ins--reset-values(be);
  op--rts-dropping-n-args(be, 2);

  ins--tag(be, out-of-range);
  op--call-iep(be, dylan-repeated-slot-getter-index-oor, object, index);
  ins--bra(be, return-value);
end runtime-function;




/// SINGLE-METHOD


define leaf runtime-function single-method-entry
  // On entry: Args as for dispatch engine convention
  //    
  // On exit:  Tail call the method in the DATA-1 slot of the engine node
  //           with the MList taken from the DATA-2 slot
  // 
  local method no-key-checking (#rest r) end;
  op--single-method-entry-checking-keys(be, #"dummy", no-key-checking);
end runtime-function;



/// UNRESTRICTED-KEYED-SINGLE-METHOD


define entry-point unrestricted-keyed-single-method-entry
      (be :: <harp-back-end>, required, limit: key-check-case-limit)
  op--single-method-entry-checking-keys(be, required, op--unrestricted-key-check);
end entry-point;


define method op--unrestricted-key-check 
    (be :: <harp-back-end>, required, disphdr :: <register>, engine :: <register>)
     => ()
  with-harp (be)
    // nreg restreg;
    // greg restsize;
    nreg restreg, restsize;
    tag evennum;

    op--load-rest-parameter(be, restreg, engine, required);
    op--vector-size-as-tagged-int(be, restsize, restreg);
    op--branch-unless-odd-tagged-integer(be, evennum, restsize);

    op--perform-engine-node-callback
      (be, required, disphdr, engine,
       op--vector-up-requireds-and-optionals,
       op--return-after-vectoring-requireds-and-optionals,
       op--single-method-engine-node-required-args-num,
       op--odd-keys-callback);
       
    ins--tag(be, evennum);
  end with-harp;
end method;




/// EXPLICIT-KEYED-SINGLE-METHOD


define entry-point explicit-keyed-single-method-entry
      (be :: <harp-back-end>, required, limit: key-check-case-limit)
  op--single-method-entry-checking-keys(be, required, op--explicit-key-check);
end entry-point;


define method op--explicit-key-check 
    (be :: <harp-back-end>, required, disphdr :: <register>,
     engine :: <register>)
     => ()
  op--engine-node-key-check(be, required, disphdr, engine, #f);
end method;



/// IMPLICIT-KEYED-SINGLE-METHOD


define entry-point implicit-keyed-single-method-entry
      (be :: <harp-back-end>, required, limit: key-check-case-limit)
  op--single-method-entry-checking-keys(be, required, op--implicit-key-check);
end entry-point;


define method op--implicit-key-check 
    (be :: <harp-back-end>, required, disphdr :: <register>, engine :: <register>)
     => ()
  op--engine-node-key-check(be, required, disphdr, engine, #t);
end method;




/// DISCRIMINATE-ON-ARGUMENT


define entry-point discriminate-on-argument-entry
      (be :: <harp-back-end>, arg-num, limit: $discriminator-case-limit)
  function engine;
  mlist gf;
  result result;
  reg argument;
  nreg callback, new-entry;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  // Load the argument to discriminate against
  op--load-discriminator-argument(be, argument, engine, arg-num);

  // call the callback function
  with-preserved-arguments (mlist)
    ins--ld(be, callback, engine, be.engine-node-callback-offset);
    op--call-iep(be, callback, argument, gf, engine);
    ins--move(be, engine, result);
  end with-preserved-arguments;

  // Jump to the resulting engine node
  ins--ld(be, new-entry, engine, be.engine-node-entry-point-offset);
  ins--jmp(be, new-entry, max-num-arg-regs, mlist: #t, function: #t);
end entry-point;




/// IF-TYPE-DISCRIMINATOR-ENGINE


define entry-point if-type-discriminator-engine
      (be :: <harp-back-end>, arg-num, limit: $discriminator-case-limit)
  function engine;
  mlist gf;
  result result;
  reg argument;
  nreg true-type, new-entry;
  tag true-tag, done-tag;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  // Load the argument to discriminate against
  op--load-discriminator-argument(be, argument, engine, arg-num);

  // Perform an instance? test
  with-preserved-arguments (mlist)
    ins--ld(be, true-type, engine, be.engine-node-data-1-offset);
    ins--push(be, engine);
    op--primitive-instance?(be, true-tag, argument, true-type);
    ins--pop(be, engine);
    ins--ld(be, engine, engine, be.engine-node-data-3-offset);
    ins--bra(be, done-tag);
    ins--tag(be, true-tag);
    ins--pop(be, engine);
    ins--ld(be, engine, engine, be.engine-node-data-2-offset);
    ins--tag(be, done-tag);
  end with-preserved-arguments;

  // Jump to the resulting engine node
  ins--ld(be, new-entry, engine, be.engine-node-entry-point-offset);
  ins--jmp(be, new-entry, max-num-arg-regs, mlist: #t, function: #t);
end entry-point;


/// TYPECHECK-DISCRIMINATOR-ENGINE


define entry-point typecheck-discriminator-engine
      (be :: <harp-back-end>, arg-num, limit: $discriminator-case-limit)
  function engine;
  mlist gf;
  result result;
  reg argument;
  nreg true-type, new-entry;
  tag true-tag, done-tag;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  // Load the argument to discriminate against
  op--load-discriminator-argument(be, argument, engine, arg-num);

  // Perform an instance? test
  with-preserved-arguments (mlist)
    ins--ld(be, true-type, engine, be.engine-node-data-1-offset);
    ins--push(be, engine);
    op--primitive-instance?(be, true-tag, argument, true-type);
    ins--pop(be, engine);
    ins--move(be, engine, dylan-inapplicable-engine-node);
    ins--bra(be, done-tag);
    ins--tag(be, true-tag);
    ins--pop(be, engine);
    ins--ld(be, engine, engine, be.engine-node-data-2-offset);
    ins--tag(be, done-tag);
  end with-preserved-arguments;

  // Jump to the resulting engine node
  ins--ld(be, new-entry, engine, be.engine-node-entry-point-offset);
  ins--jmp(be, new-entry, max-num-arg-regs, mlist: #t, function: #t);
end entry-point;

define entry-point monomorphic-by-class-discriminator-engine
      (be :: <harp-back-end>, arg-num, limit: $discriminator-case-limit)
  function engine;
  mlist gf;
  reg  arg-key;
  nreg key;
  tag non-pointer-tag, miss-tag, continue;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  // Load the argument to discriminate against
  op--load-discriminator-argument(be, arg-key, engine, arg-num);
  ins--move(be, key, arg-key);
  ins--and(be,  key, key, 3);             // check tag
  ins--bne(be,  non-pointer-tag, key, 0); // filter out non-pointers

  // pointers
  ins--ld(be, arg-key, arg-key, 0);      // fetch wrapper

  ins--tag(be, continue);
  // unique-key in iclass case:
  // ins--ld(be, arg-key, arg-key, 4);  // fetch implementation class
  // ins--ld(be, arg-key, arg-key, 24); // fetch dispatch key

  // unique-key is wrapper address as integer case:
  ins--or(be, arg-key, arg-key, 1);     // insert integer tag

  ins--ld(be, key, engine, be.engine-node-data-1-offset);
  ins--bne(be, miss-tag, key, arg-key);

  // hit
  ins--ld(be, engine, engine, be.engine-node-data-2-offset);
  ins--ld(be, key, engine, be.engine-node-entry-point-offset);
  ins--jmp(be, key, max-num-arg-regs, mlist: #t, function: #t);

  ins--tag(be, miss-tag);
  ins--move(be, engine, dylan-absent-engine-node);
  // Jump to the resulting engine node
  ins--ld(be, key, engine, be.engine-node-entry-point-offset);
  ins--jmp(be, key, max-num-arg-regs, mlist: #t, function: #t);

  ins--tag(be, non-pointer-tag);
  // unique-key in iclass case:
  // ins--ld-index-scaled(be, arg-key, dylan-direct-object-classes, key, 0);
  // unique-key is wrapper address as integer case:
  op--load-index-scaled(be, arg-key, dylan-direct-object-mm-wrappers, key, 0);
  ins--bra(be, continue);
end entry-point;





/// TEMPORARY !"$%
///
/// A couple of entry points for dynamically switching between specialized 
/// entry points


define leaf runtime-function general-engine-node-spread-entry
  // On entry: Args as for dispatch engine convention
  //    
  // On exit:  Tail call the more specific entry point, given the GF
  // 
  op--jump-to-specific-entry(be, 
                             general-engine-node-n-refs,
                             general-engine-node-spread-optionals-refs);
end runtime-function;


define leaf runtime-function general-engine-node-n-entry
  // On entry: Args as for dispatch engine convention
  //    
  // On exit:  Tail call the more specific entry point, given the GF
  // 
  op--jump-to-specific-entry(be, 
                             general-engine-node-n-refs,
                             general-engine-node-n-optionals-refs);
end runtime-function;

/// END TEMPORARY CODE




/// CACHE-HEADER-ENTRY

define leaf runtime-function cache-header-entry
  // On entry: Args as for dispatch engine convention
  //    
  // On exit:  Tail call the entry point slot of the "next" slot, 
  //               setting function to next slot contents, and
  //               setting mlist to the cache-header-engine-node itself.
  // 
  // Jump to the resulting engine node
  function engine;
  mlist disphdr;
  nreg new-entry;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  ins--move(be, disphdr, engine);
  ins--ld(be, engine, disphdr, be.cache-header-engine-node-next-offset);
  ins--ld(be, new-entry, engine, be.engine-node-entry-point-offset);
  ins--jmp(be, new-entry, max-num-arg-regs, mlist: #t, function: #t);
end runtime-function;

define leaf runtime-function profiling-cache-header-entry
  // On entry: Args as for dispatch engine convention
  //    
  // On exit:  Tail call the entry point slot of the "next" slot, setting function to next
  // 
  // Jump to the resulting engine node
  function engine;
  mlist disphdr;
  nreg new-entry;
  tag overflow, continue;

  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  ins--move(be, disphdr, engine);
  ins--beq(be, continue, dispatch-profiling?, dylan-false);
  ins--xadd-mem-locked(be, new-entry, engine, be.profiling-cache-header-engine-node-count-1-offset, 1);
  ins--beq(be, overflow, new-entry, 0);
  ins--tag(be, continue);
  ins--ld(be, engine, engine, be.cache-header-engine-node-next-offset);
  ins--ld(be, new-entry, engine, be.engine-node-entry-point-offset);
  ins--jmp(be, new-entry, max-num-arg-regs, mlist: #t, function: #t);

  ins--tag(be, overflow);
  ins--add2-mem-locked(be, engine, engine, be.profiling-cache-header-engine-node-count-2-offset, 1);
  ins--bra(be, continue);
end runtime-function;



/// PRIMITIVE-ENABLE-CACHE-HEADER-ENGINE-NODE


define runtime-primitive enable-cache-header-engine-node
/*
  On entry:
    engine  - a cache-header engine node
    gf      - a GF to which it applies (ignored for native back end)
  On exit:
    the engine node
*/

  arg0 arg0;
  result result;
  greg engine;
  nreg entry;

  ins--move(be, engine, arg0);
  ins--move(be, entry, cache-header-entry-ref);
  ins--st(be, entry, engine, be.engine-node-entry-point-offset);
  ins--move(be, result, engine);
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;




/// PRIMITIVE-INVALIDATE-CACHE-HEADER-ENGINE-NODE


define runtime-primitive invalidate-cache-header-engine-node
/*
  On entry:
    engine  - a cache-header engine node
    gf      - a GF to which it applies (ignored for native back end)
  On exit:
    the engine node
*/

  arg0 arg0;
  result result;
  greg engine;
  nreg entry;

  ins--move(be, engine, arg0);
  ins--move(be, entry, general-engine-node-n-entry-ref);
  ins--st(be, entry, engine, be.engine-node-entry-point-offset);
  ins--move(be, result, engine);
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;







/// PRIMITIVE-INITIALIZE-ENGINE-NODE


// Implement this via a switch table.

define runtime-switch-table initialize-engine-node-table 
  size 32;

  prolog
    (arg0 arg0;
     result result;
     greg engine;
     nreg required, entry;
     ignore(required);
     ins--move(be, engine, arg0)
    );

  epilog
    (ins--st(be, entry, engine, be.engine-node-entry-point-offset);
     ins--move(be, result, engine);
     ins--rts-and-drop(be, 0)
    );

  1, 15 =>                    // GENERAL-ENGINE-NODE-SPREAD
    ins--move(be, entry, general-engine-node-spread-entry-ref);


  2 =>                        // SINGLE-METHOD
    ins--move(be, entry, single-method-entry-ref);

  3 =>                        // IMPLICIT-KEYED-SINGLE-METHOD
    op--select-single-method-engine-node-entry-point-by-required
      (be, engine, entry, required, implicit-keyed-single-method-entry-refs);

  4 =>                        // EXPLICIT-KEYED-SINGLE-METHOD
    op--select-single-method-engine-node-entry-point-by-required
      (be, engine, entry, required, explicit-keyed-single-method-entry-refs);

  5 =>                        // UNRESTRICTED-KEYED-SINGLE-METHOD
    op--select-single-method-engine-node-entry-point-by-required
      (be, engine, entry, required, unrestricted-keyed-single-method-entry-refs);

  13 =>			      // PROFILING-CACHE-HEADER
    ins--move(be, entry, profiling-cache-header-entry-ref);

  14 =>			      // CACHE-HEADER
    ins--move(be, entry, cache-header-entry-ref);

  16 =>                       // BOXED-INSTANCE-SLOT-GETTER
    ins--move(be, entry, boxed-instance-slot-getter-entry-ref);

  17 =>                       // BOXED-INSTANCE-SLOT-SETTER
    ins--move(be, entry, boxed-instance-slot-setter-entry-ref);

  18 =>                       // BOXED-REPEATED-INSTANCE-SLOT-GETTER
    ins--move(be, entry, boxed-repeated-instance-slot-getter-entry-ref);

  19 =>                       // BOXED-REPEATED-INSTANCE-SLOT-SETTER
    ins--move(be, entry, boxed-repeated-instance-slot-setter-entry-ref);

  22 =>                       // RAW-BYTE-REPEATED-INSTANCE-SLOT-GETTER
    ins--move(be, entry, raw-byte-repeated-instance-slot-getter-entry-ref);

  23 =>                       // RAW-BYTE-REPEATED-INSTANCE-SLOT-SETTER
    ins--move(be, entry, raw-byte-repeated-instance-slot-setter-entry-ref);

  20, 24, 26 =>               // GENERAL-ENGINE-NODE-1
    ins--move(be, entry, general-engine-node-1-entry-ref);

  21, 25, 27, 28, 30 =>       // GENERAL-ENGINE-NODE-2
    ins--move(be, entry, general-engine-node-2-entry-ref);

  29, 31 =>                   // GENERAL-ENGINE-NODE-3
    ins--move(be, entry, general-engine-node-3-entry-ref);

  otherwise =>                // Default case is GENERAL-ENGINE-NODE-N
    ins--move(be, entry, general-engine-node-n-entry-ref);

end runtime-switch-table;


define runtime-primitive initialize-engine-node
/*
  On entry:
    engine  - an engine node
  On exit:
    the engine node
*/

  arg0 engine;
  nreg type;

  op--engine-entry-type-in-bytes(be, type, engine);
  op--jump-into-switch-table(be, initialize-engine-node-table, type);
end runtime-primitive;




/// PRIMITIVE-INITIALIZE-DISCRIMINATOR


define runtime-primitive initialize-discriminator
/*
  On entry:
    engine  - a discriminator  engine node
  On exit:
    the engine node
*/

  arg0 arg0;
  result result;
  greg engine;
  nreg argnum, entry, type;
  tag if-type-tag, typecheck-tag, monomorphic-by-class-tag, profiling-tag, done-type-tag;

  let engine-typecheck = 32;
  let engine-monomorphic-by-class = 42;
  let engine-if-type   = 33;
  let engine-profiling = 13;

  ins--move(be, engine, arg0);
  op--discriminator-argnum(be, argnum, engine);

  // Choose the discriminator by type. Only if-type & typecheck nodes are special
  op--engine-entry-type(be, type, engine);
  ins--beq(be, if-type-tag,   type, engine-if-type);
  ins--beq(be, typecheck-tag, type, engine-typecheck);
  ins--beq(be, monomorphic-by-class-tag, type, engine-monomorphic-by-class);
  ins--beq(be, profiling-tag, type, engine-profiling);

  // default case
  op--select-entry-point(be, entry, argnum, discriminate-on-argument-entry-refs);
  ins--bra(be, done-type-tag);

  // if-type case
  ins--tag(be, if-type-tag);
  op--select-entry-point(be, entry, argnum, if-type-discriminator-engine-refs);
  ins--bra(be, done-type-tag);

  // typecheck case
  ins--tag(be, typecheck-tag);
  op--select-entry-point(be, entry, argnum, typecheck-discriminator-engine-refs);
  ins--bra(be, done-type-tag);

  // monomorphic-by-class case
  ins--tag(be, monomorphic-by-class-tag);
  op--select-entry-point(be, entry, argnum, monomorphic-by-class-discriminator-engine-refs);
  ins--bra(be, done-type-tag);

  // profiling case
  ins--tag(be, profiling-tag);
  ins--move(be, entry, profiling-cache-header-entry-ref);
  ins--bra(be, done-type-tag);

  ins--tag(be, done-type-tag);
  ins--st(be, entry, engine, be.engine-node-entry-point-offset);
  ins--move(be, result, engine);
  ins--rts-and-drop(be, 0);
end runtime-primitive;





/// PRIMITIVE-SET-GENERIC-FUNCTION-ENTRYPOINTS


define runtime-primitive set-generic-function-entrypoints
/*
  On entry:
    gf  - a generic function
  On exit:
    the GF, with a newly set XEP
*/

  arg0 arg0;
  result result;
  greg gf;
  nreg argnum, xep, required;
  tag opts, done-xep;

  ins--move(be, gf, arg0);
  op--number-required(be, required, function: gf);
  op--branch-if-function-with-optionals(be, opts, function: gf);

  // XEP case with no optionals
  op--select-entry-point(be, xep, required, new-gf-xep-refs);
  ins--st(be, xep, gf, be.function-xep-offset);
  ins--bra(be, done-xep);

  // XEP case with optionals
  ins--tag(be, opts);
  op--select-entry-point(be, xep, required, new-gf-optional-xep-refs);
  ins--st(be, xep, gf, be.function-xep-offset);

  // And return
  ins--tag(be, done-xep);
  ins--move(be, result, gf);
  ins--rts-and-drop(be, 0);
end runtime-primitive;
