module:    native-rtg
Synopsis:  Entry point generation for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



//// Generation of entry points


/// Accessor method XEPs

define internal-iep runtime-external single-instance-getter
  = "%slotacc-single-q-instance-getter";

define internal-iep runtime-external single-instance-setter
  = "%slotacc-single-q-instance-setter";

define internal-iep runtime-external single-class-getter
  = "%slotacc-single-q-class-getter";

define internal-iep runtime-external single-class-setter
  = "%slotacc-single-q-class-setter";

define internal-iep runtime-external repeated-instance-getter
  = "%slotacc-repeated-instance-getter";

define internal-iep runtime-external repeated-instance-setter
  = "%slotacc-repeated-instance-setter";


define leaf runtime-function slotacc-single-q-instance-getter-xep
  // On entry: XEP convention with args (accmeth, inst)
  //    
  // On exit:  Tail call the appropriate IEP
  // 
  op--slotacc-xep(be, 2, single-instance-getter);
end runtime-function;

define leaf runtime-function slotacc-single-q-instance-setter-xep
  // On entry: XEP convention with args (value, accmeth, inst)
  //    
  // On exit:  Tail call the appropriate IEP
  // 
  op--slotacc-xep(be, 3, single-instance-setter);
end runtime-function;

define leaf runtime-function slotacc-single-q-class-getter-xep
  // On entry: XEP convention with args (accmeth, inst)
  //    
  // On exit:  Tail call the appropriate IEP
  // 
  op--slotacc-xep(be, 2, single-class-getter);
end runtime-function;

define leaf runtime-function slotacc-single-q-class-setter-xep
  // On entry: XEP convention with args (value, accmeth, inst)
  //    
  // On exit:  Tail call the appropriate IEP
  // 
  op--slotacc-xep(be, 3, single-class-setter);
end runtime-function;

define leaf runtime-function slotacc-repeated-instance-getter-xep
  // On entry: XEP convention with args (accmeth, inst, idx)
  //    
  // On exit:  Tail call the appropriate IEP
  // 
  op--slotacc-xep(be, 3, repeated-instance-getter);
end runtime-function;

define leaf runtime-function slotacc-repeated-instance-setter-xep
  // On entry: XEP convention with args (value, accmeth, inst, idx)
  //    
  // On exit:  Tail call the appropriate IEP
  // 
  op--slotacc-xep(be, 4, repeated-instance-setter);
end runtime-function;


define method op--slotacc-xep 
    (be :: <harp-back-end>, required, iep-ref)
  let optionals? = #f;
  let keys? = #f;
  let xep-continuation =
    method (be :: <harp-back-end>, required)
      let max-num-arg-regs = be.registers.arguments-passed-in-registers;
      ignore(required);
      let mlist = be.registers.reg-mlist;
      ins--move(be, mlist, dylan-false);
      ins--jmp(be, iep-ref, max-num-arg-regs, mlist: #t, function: #t);
    end method;
  op--general-xep-support(be, required, optionals?, keys?, xep-continuation, 
                          check-specializers?: #f);
end method;


define runtime-primitive set-accessor-method-xep
/*
  On entry:
    am       - an accessor method
    what     - an integer indicating the type of XEP required
  On exit:
    the accessor method, with a newly set XEP
*/

  result result;
  greg am;
  nreg xep, what;
  tag c0, c1, c2, c3, c4, c5, have-xep;

  op--load-arguments(be, am, what);
  op--untaggify(be, what, what);

  // Check what XEP we want
  ins--beq(be, c0, what, 0);
  ins--beq(be, c1, what, 1);
  ins--beq(be, c2, what, 2);
  ins--beq(be, c3, what, 3);
  ins--beq(be, c4, what, 4);
  ins--beq(be, c5, what, 5);

  local method op--do-case (be, tag, ref)
	  ins--tag(be, tag);
	  ins--move(be, xep, ref);
	  ins--bra(be, have-xep);
	end method;
 
  op--do-case(be, c0, slotacc-single-q-instance-getter-xep-ref);
  op--do-case(be, c1, slotacc-single-q-instance-setter-xep-ref);
  op--do-case(be, c2, slotacc-single-q-class-getter-xep-ref);
  op--do-case(be, c3, slotacc-single-q-class-setter-xep-ref);
  op--do-case(be, c4, slotacc-repeated-instance-getter-xep-ref);
  op--do-case(be, c5, slotacc-repeated-instance-setter-xep-ref);

  // Stash the XEP in place
  ins--tag(be, have-xep);
  ins--st(be, xep, am, be.function-xep-offset);

  // And return
  ins--move(be, result, am);
  op--rts-dropping-n-args(be, 2);
end runtime-primitive;



/// GF IEPs


/*  Now unused
define leaf runtime-function gf-iep
  // On entry: Args as IEP convention for entry to a GF
  //    
  // On exit:  Tail call the first engine node
  // 
  op--gf-iep-jump-to-engine-node(be);
end runtime-function;
*/


define method op--gf-iep-jump-to-engine-node (be :: <harp-back-end>)
  with-harp (be)
    function function;
    mlist mlist;
    greg engine, gf;
    nreg entry;
  
    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    ins--move(be, gf, function);
    ins--ld(be, engine, gf, be.generic-function-engine-offset);
    ins--ld(be, entry, engine, be.engine-node-entry-point-offset);
    ins--move(be, mlist, gf);
    ins--move(be, function, engine);
    ins--jmp(be, entry, max-num-arg-regs, mlist: #t, function: #t);
  end with-harp;
end method;


/// Simple XEPs


define entry-point xep (be :: <harp-back-end>, required)
  op--outer-xep(be, required, #f, #f, method (be, req) #f end);
end entry-point;



/// Rest XEPs


define entry-point rest-xep (be :: <harp-back-end>, required)
  op--outer-xep(be, required, #t, #f, op--rest-xep-internal);
end entry-point;


define method op--rest-xep-internal (be :: <harp-back-end>, required)
  op--vector-up-rest-args(be, required, 0);
end method;




/// Keyword XEPs


define entry-point rest-key-xep (be :: <harp-back-end>, required)
  op--outer-xep(be, required, #t, #t, op--rest-key-xep-internal);
end entry-point;


define method op--rest-key-xep-internal (be :: <harp-back-end>, required)
  with-harp (be)
    function function;
    nreg keys-size, last-key;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    op--keywords-size(be, keys-size);
    if (special-case?(be, required))
      let args-in-regs = arguments-in-registers(be, required + 1);
      let keys-in-regs = max-num-arg-regs - args-in-regs;
      unless (keys-in-regs == 0)
	let continue = make-tag(be);
	ins--sub(be, keys-size, keys-size, keys-in-regs);
	ins--bge(be, continue, keys-size, 0);
	ins--move(be, keys-size, 0);
	ins--tag(be, continue);
      end;
    end;
    let (first-opt, rest-vec) = op--vector-up-rest-args(be, required, keys-size);
    ins--sub(be, last-key, rest-vec, 8);  // get last key skipping count
    op--tail-call-process-keys-for-xep
      (be, rest-vec, last-key, required,
       set-mlist: dylan-false, 
       set-function: function);
  end with-harp;
end method;



/// Keyword MEPs


define entry-point rest-key-mep (be :: <harp-back-end>, required)
  op--rest-key-mep-preserving(be, required);
end entry-point;


define method op--rest-key-mep-preserving
    (be :: <harp-back-end>, required :: <integer>)
  with-harp (be)
    function function;
    mlist mlist;
    nreg mlist-reg;
    ins--move(be, mlist-reg, mlist);
    let regs-pushed = 0;
    let (rest-vec, last-key) 
      = op--rest-key-mep-internal(be, required, regs-pushed);
    op--tail-call-process-keys(be, rest-vec, last-key, required,
                               set-mlist: mlist-reg,
                               set-function: function);
  end with-harp;
end method;


define method op--rest-key-mep-preserving 
    (be :: <harp-back-end>, required == #"dynamic")
  with-harp (be)
    function function;
    mlist mlist;
    ins--push(be, mlist);
    ins--push(be, function);
    let regs-pushed = 2;
    let (rest-vec, last-key) = 
      op--rest-key-mep-internal(be, required, regs-pushed);
    ins--pop(be, function);
    ins--pop(be, mlist);
    op--tail-call-process-keys(be, rest-vec, last-key, required);
  end with-harp;
end method;


define method op--rest-key-mep-internal 
    (be :: <harp-back-end>, required, pushes :: <integer>)
    => (rest-vec :: <register>, last-key :: <register>)
  with-harp (be)
    stack stack;
    arg-count keys-size;
    arg0 arg0;
    nreg count-loc, rest-vec, last-key;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    let top-size = op--shuffle-size-for-mep(be, required, regs-pushed: pushes);
    op--keywords-size(be, keys-size);
    if (special-case?(be, required))
      let args-in-regs = arguments-in-registers(be, required + 1);
      let keys-in-regs = max-num-arg-regs - args-in-regs;
      unless (keys-in-regs == 0)
	let continue = make-tag(be);
	ins--sub(be, keys-size, keys-size, keys-in-regs);
	ins--bge(be, continue, keys-size, 0);
	ins--move(be, keys-size, 0);
	ins--tag(be, continue);
      end;
    end;
    let keys-size-in-bytes = op--multiply-by-4(be, keys-size, may-be-temp?: #t);
    let top-size-in-bytes = op--multiply-by-4(be, top-size);
    ins--add2-mem(be, stack, top-size-in-bytes, 0, keys-size-in-bytes);
    op--shuffle-stack(be, count-loc, #f, top-size, 0, keys-size);
    // count-loc points to the count on the stack
    ins--sub(be, last-key, count-loc, 4);
    if (special-case?(be, required))
      ins--move(be, rest-vec, argument-register(required));
    else
      let rest-index = op--subtract-4(be, top-size-in-bytes);
      ins--ld(be, rest-vec, stack, rest-index);
    end if;
    values(rest-vec, last-key);
  end with-harp;
end method;



/// OP--SHUFFLE-SIZE-FOR-MEP
/// This returns either an integer or a register containing the number
/// of words of data at the top of stack which must be shuffled to make
/// room for the keywords for an MEP. The result is based on the
/// the number of (required arguments plus #rest value) on the stack.
/// For an MEP, the rest value must be shuffled along too.

define method op--shuffle-size-for-mep
    (be :: <harp-back-end>, requireds :: <integer>, #key regs-pushed = 0)
     => (r :: <integer>);
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  // allow for the return address;
  let extras-on-stack = be.return-address-size + regs-pushed;
  let args-on-stack :: <integer> =
    // Allow for #rest in register or on stack
    arguments-on-stack(be, 1 + requireds);
  let shuffle-size = args-on-stack + extras-on-stack;
  shuffle-size;
end method;


define method op--shuffle-size-for-mep
    (be :: <harp-back-end>, requireds == #"dynamic", #key regs-pushed = 0)
     => (r :: <register>);
  let copy-count = make-n-register(be);
  let args-in-regs = required-arguments-in-registers(be, requireds);
  // Allow for the return address
  let extras-on-stack = be.return-address-size + regs-pushed;
  // Allow for #rest on stack
  let shuffle-size = 1 + extras-on-stack - args-in-regs;
  // now calculate the amount to copy. Its number-required + shuffle-size
  op--number-required(be, copy-count);
  unless (shuffle-size == 0)
     ins--add(be, copy-count, copy-count, shuffle-size);  // amount to copy
  end unless;
  copy-count;
end method;






/// Simple GF XEPs


define entry-point new-gf-xep (be :: <harp-back-end>, required, limit: 6)
  op--outer-gf-xep(be, required, #f, #f, method (be, req) #f end);
end entry-point;



/*   OLD CODE REMOVED

// Obsolete: old-style GF XEPs before the "engine-node" convention
//
// Under the "old" discriminator implementation, the dynamic case GF XEP 
// has an inconsistent convention, and always vectors up all of the arguments
// into a single sequence. However, with the engine-node implementation supported
// here, the dynamic case GF XEP has the normal consistent behaviour. 
// For testing purposes, we currently re-define a new GF XEP here.
// In the future, the "real" entry point code should be replaced with the
// one defined here.


define entry-point gf-xep (be :: <harp-back-end>, required, limit: 6)
  op--outer-gf-xep(be, required, #f, #f, op--simple-gf-xep-internal);
end entry-point;

define method op--simple-gf-xep-internal 
    (be :: <harp-back-end>, required :: <integer>)
  #f;
end method;

define method op--simple-gf-xep-internal 
    (be :: <harp-back-end>, required == #"dynamic")
  // The dynamic case is not like that for simple XEPs.
  // We must work with the dynamic version of the GF discriminator
  // function, which takes a #rest arg. This case is therefore
  // like op--rest-xep with 0 requireds. We actually implement it 
  // in this way - although it's slightly sub-optimal since we
  // know all the argument registers are full.
  op--vector-up-rest-args(be, 0, 0);
end method;

*/


/// Optional GF XEPs


define entry-point new-gf-optional-xep (be :: <harp-back-end>, required, limit: 6)
  op--outer-gf-xep(be, required, #t, #f, op--rest-xep-internal);
end entry-point;


/*   OLD CODE REMOVED

define entry-point gf-optional-xep (be :: <harp-back-end>, required, limit: 6)
  op--outer-gf-xep(be, required, #t, #f, op--gf-optional-xep-internal);
end entry-point;

define method op--gf-optional-xep-internal 
    (be :: <harp-back-end>, required :: <integer>)
  // The normal case is just like rest-xep
  op--vector-up-rest-args(be, required, 0);
end method;



// This implementation vectors up the #rest before vectoring up everything else

define method op--gf-optional-xep-internal 
    (be :: <harp-back-end>, required == #"dynamic")
  // The optionals should be vectored up first - and then the resultant 
  // args vectored up - taking care to keep the count correct
  //

/*  // Kludge disabled 
  // TEMPORARY KLUDGE : .......................
  // We heap allocate the optionals vector, because the default 
  // GF discriminator for optionals (the only function which uses this XEP)
  // attempts to do an optimized tail call to apply. The compiler knows how
  // to evacuate one level of stack-vector to the heap - but it doesn't 
  // consider that the vector might itself refer to stack allocated data.
  //
  // Eventually, the compiler will optimize tail calls to apply where the 
  // optionals are passed straight on to the applied function. We will
  // then be able to avoid heap allocating the optionals and this kludge 
  // can be removed.
*/
  with-harp (be)
    stack stack;
    tmp1 tmp;
    arg0 arg0;
    arg-count argc;
    nreg rest-var;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    // first vector up the optionals
    let (opt1, rest) = op--vector-up-rest-args(be, required, 0);
/*
    // Now the kludge. Heap allocate this vector ...
    ins--push(be, arg0);  // preserve the argument register when allocating
    ins--push(be, rest);  // preserve the stack allocated vector
    let heap-vec = op--copy-vector-internal(be, rest);
    ins--pop(be, rest-var);   // restore stack-allocated vector
    ins--sub(be, rest-var, rest-var, 8); // rest variable is 8 below vector
    ins--pop(be, arg0);       // restore argument register
    ins--st(be, heap-vec, rest-var, 0); // put heap vector in this variable
    // ... end of kludge
*/
    // now make it look as though we were just passed the required
    // along with the already vectored #rest
    op--number-required(be, argc);
    ins--add(be, argc, argc, 1);
  
    // Now vector up these pesudo arguments (using the doctored arg count);
    // All argument registers are pushed unto the stack, and #rest becomes
    // the only argument register
    op--vector-up-rest-args(be, 0, 0);
    
    // Now adjust the count.
    // The real count is the count of the first alloc
    // plus 12 + bytes%(max-num-arg-regs)
    // (to allow for the second alloc: 8 for the vector header,
    //  max-num-arg-regs, and 4 for the second count itself).

    // the count of the second alloc
    // (allowing for preceding args, #rest in register, zero keys)
    ins--ld(be, argc, stack, 4 * max-num-arg-regs);
    // the count of the first alloc (allowing for its preceding #rest, zero keys)
    op--load-index(be, argc, stack, argc, 4);
    // add first and second alloc counts
    ins--add(be, argc, argc, 12 + 4 * max-num-arg-regs);
    // and store back as second alloc count
    ins--st(be, argc, stack, 4 * max-num-arg-regs);

  end with-harp;
end method;

*/




/// General support for XEPs 


define method op--outer-xep 
    (be :: <harp-back-end>, required, optionals?, keywords?, 
     continuation :: <function>)
  let xep-continuation =
    method (be :: <harp-back-end>, required)
      let mlist = be.registers.reg-mlist;
      continuation(be, required);
      ins--move(be, mlist, dylan-false);
    end method;
  op--general-xep-support(be, required, optionals?, keywords?, xep-continuation);
end method;


define method op--outer-gf-xep 
    (be :: <harp-back-end>, required, optionals?, keywords?,
     continuation :: <function>)
  let xep-continuation =
    method (be :: <harp-back-end>, required)
      continuation(be, required);
      op--gf-iep-jump-to-engine-node(be);
    end method;
  op--general-xep-support(be, required, optionals?, keywords?, xep-continuation, 
                          check-specializers?: #f);
end method;


define method op--general-xep-support
    (be :: <harp-back-end>, required, opts?, keys?,
     continuation :: <function>,
     #key 
     live-mlist? = #t,
     check-args? = #t, 
     check-stack? = #t, 
     check-specializers? = #t)
  with-harp (be)
    arg-count args;
    tag tag-a, tag-o, tag-s;
    if (check-args?) op--arg-count-check(be, tag-a, args, required, opts?) end;
    if (check-stack?) op--stack-check(be, tag-o) end;
    continuation(be, required);
    if (check-specializers?) op--specializer-checks(be, tag-s, required) end;
    op--tail-call-iep(be, live-mlist?: live-mlist?, keyword-method?: keys?);
    ins--tag(be, tag-a);
    if (check-args?) op--args-error-call(be, args, required) end;
    ins--tag(be, tag-o);
    if (check-stack?) op--stack-overflow-error-call(be) end;
    ins--tag(be, tag-s);
    if (check-specializers?) op--specializer-error-call(be) end;
  end with-harp;
end method;


define method op--tail-call-iep 
    (be :: <harp-back-end>, #key keyword-method? = #f, live-mlist? = #t)
  with-harp (be)
    function function;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    if (keyword-method? == #"unknown")
      // Don't know whether it's a keyword method, so must test to find IEP offset
      with-harp (be)
        nreg props;
        tag have-keys;
        op--function-properties(be, props);
        ins--and(be, props, props, tagged-key-p-mask);
        ins--bne(be, have-keys, props, 0);
        op--tail-call-iep(be, keyword-method?: #f, live-mlist?: live-mlist?);
        ins--tag(be, have-keys);
        op--tail-call-iep(be, keyword-method?: #t, live-mlist?: live-mlist?);
      end with-harp;
    else
      let offset = if (keyword-method?)
                     be.function-iep-offset
                   else be.function-mep-offset;
                   end if;
      ins--jmp-indirect(be, function, offset, max-num-arg-regs, 
                        mlist: live-mlist?, arg-count: #f);
    end if;
  end with-harp;
end method;






