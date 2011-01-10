Module:    native-rtg
Synopsis:  Support for dispatch & engine node entry points for the RTG
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// GENERAL-ENGINE-NODE-N, GENERAL-ENGINE-NODE-SPREAD

define method op--general-engine-node-callback
    (be :: <harp-back-end>, arg-vec :: <register>, 
     engine :: <register>, disphdr :: <register>)
    => ()
  with-harp (be)
    nreg callback;
    ins--ld(be, callback, engine, be.engine-node-callback-offset);
    op--call-iep(be, callback, arg-vec, engine, disphdr);
  end with-harp;
end method;


define method op--vector-up-requireds
    (be :: <harp-back-end>, arg-vec :: <register>, 
     disphdr :: <register>, req-args)
    => ()
  with-harp (be)
    // The easiest strategy is to manipulate the args in-place on the 
    // stack to make a vector, and then to build a frame afterwards.
    stack stack;
    nreg ret-addr, words;
    arg0 arg0;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    if-return-address() ins--pop(be, ret-addr) end;

    // Ensure all arguments are on the stack
    if (max-num-arg-regs = 1)
      unless (req-args == 0) ins--push(be, arg0) end;
    else
      let words =
	// if the number is passed as a register, then its been multiplied by 4.
	if (instance?(req-args, <register>))
	  op--untaggify(be, words, req-args); words
	else req-args
	end;
      op--copy-registers-with-update
	(be, stack, #f, stack, words, 0, down?: #t);
    end;

    ins--push(be, op--taggify-arg-num(be, req-args));
    ins--push(be, dylan-sov-class);
    ins--move(be, arg-vec, stack);

    if-return-address() ins--push(be, ret-addr) end;
  end with-harp;
end method;


define method op--vector-up-requireds-and-optionals
    (be :: <harp-back-end>, arg-vec :: <register>, 
     disphdr :: <register>, req-args)
    => ()
  with-harp (be)
    // Strategy: Build a frame first, and then stack-allocate
    // a new vector within the frame.
    stack stack;
    op--stack-alloc-vector-of-requireds-and-optionals(be, req-args);
    op--fill-vector-with-requireds(be, req-args);
    op--fill-vector-with-optionals(be, req-args);
    ins--move(be, arg-vec, stack);
  end with-harp;
end method;


define method op--vector-up-requireds-spreading-optionals
    (be :: <harp-back-end>, arg-vec :: <register>, 
     disphdr :: <register>, req-args)
    => ()
  with-harp (be)
    // Strategy: Build a frame first, and then stack-allocate
    // a new vector within the frame.
    stack stack;

    if (special-case?(be, req-args))
      // Strategy: If there are argument registers available, then the rest 
      // arg is already an appropriate vector
      ins--move(be, arg-vec, argument-register(req-args));
    else
      op--stack-alloc-vector-of-requireds-and-optionals-spread(be, req-args);
      op--fill-vector-with-requireds(be, req-args);
      op--fill-vector-with-optionals-spread(be, req-args);
      ins--move(be, arg-vec, stack);
    end;
  end with-harp;
end method;


define method op--stack-alloc-vector-of-requireds-and-optionals
    (be :: <harp-back-end>, req-args :: <integer>)
    => ()
  with-harp (be)
    stack stack;
    let vec-size = req-args + 1;  // requireds + rest
    ins--sub(be, stack, stack, vec-size * 4);
    ins--push(be, op--taggify-arg-num(be, vec-size));
    ins--push(be, dylan-sov-class);
  end with-harp;
end method;

define method op--stack-alloc-vector-of-requireds-and-optionals
    (be :: <harp-back-end>, req-args :: <register>)
    => ()
  with-harp (be)
    stack stack;
    nreg vec-size;
    ins--add(be, vec-size, req-args, 4); // size in bytes of requireds + rest
    ins--sub(be, stack, stack, vec-size);
    ins--push(be, op--taggify-arg-num(be, vec-size));
    ins--push(be, dylan-sov-class);
  end with-harp;
end method;


define method op--stack-alloc-vector-of-requireds-and-optionals-spread
    (be :: <harp-back-end>, req-args)
    => ()
  with-harp (be)
    stack stack;
    greg rest-vec;
    nreg opt-size, tot-size;
    
    // First make room for the optionals
    op--load-argument-n-via-frame(be, rest-vec, req-args);
    op--vector-size-times-4(be, opt-size, rest-vec);
    ins--sub(be, stack, stack, opt-size);

    // Now do the rest
    let req-size = op--ensure-arg-num-times-4(be, req-args);
    ins--add(be, tot-size, opt-size, req-size);
    ins--sub(be, stack, stack, req-size);
    ins--push(be, op--taggify-arg-num(be, tot-size));
    ins--push(be, dylan-sov-class);
  end with-harp;
end method;


define method op--fill-vector-with-requireds
    (be :: <harp-back-end>, req-args :: <integer>)
    => ()
  for (i from 0 below req-args)
    with-harp (be)
      stack stack;
      greg arg; 
      op--load-argument-n-via-frame(be, arg, i); 
      ins--st(be, arg, stack, 8 + (i * 4));
    end with-harp;
  end for;
end method;


define method op--fill-vector-with-requireds
    (be :: <harp-back-end>, req-args :: <register>)
    => ()
  let in-regs = be.registers.arguments-passed-in-registers;
  // First fill in those args passed in regs
  op--fill-vector-with-requireds(be, in-regs);
  // Now blat in those args passed in the stack
  with-harp (be)
    stack stack;
    frame frame;
    tmp 1, src; // use a temporary to ease register pressure.
    nreg dest, count;
    
    ins--add(be, src, frame, 8); // first stack arg is past saved frame & ret addr
    ins--add(be, dest, stack, 8 + (4 * in-regs)); // allow for 8 bytes of vec header
    ins--asr(be, count, req-args, 2);    // raw count of number of requireds
    ins--sub(be, count, count, in-regs); // don't copy those in regs
    op--copy-words-with-update(be, #f, dest, src, count);
  end with-harp;
end method;


define method op--fill-vector-with-optionals
    (be :: <harp-back-end>, req-args)
    => ()
  with-harp (be)
    stack stack;
    greg optionals;

    let byte-offset = op--ensure-arg-num-times-4(be, req-args);
    op--load-argument-n-via-frame(be, optionals, req-args);
    op--store-index(be, optionals, stack, byte-offset, 8);
  end with-harp;
end method;


define method op--fill-vector-with-optionals-spread
    (be :: <harp-back-end>, req-args)
    => ()
  with-harp (be)
    stack stack;
    greg optionals;
    nreg dest, count, src;

    let byte-offset = op--ensure-arg-num-times-4(be, req-args);
    op--load-argument-n-via-frame(be, optionals, req-args);
    op--vector-size(be, count, optionals);
    ins--add(be, src, optionals, 8);
    ins--add(be, dest, stack, 8);
    ins--add(be, dest, dest, byte-offset);
    op--copy-words-with-update(be, #f, dest, src, count);
  end with-harp;
end method;



define method op--return-after-vectoring-requireds
    (be :: <harp-back-end>, req-args :: <integer>)
    => ()
  // drop args + 2 words of vector header
  ins--rts-and-drop(be, (req-args + 2) * 4);
end method;


define method op--return-after-vectoring-requireds
    (be :: <harp-back-end>, req-args)
    => ()
  with-harp (be)
    stack stack;
    greg vector-size;
    nreg bytes-to-drop;

    // The stack now has the return address followed by the 
    // stack-allocated vector;
    // after ret-addr & wrapper
    ins--ld(be, vector-size, stack,
	    4 + be.return-address-size-in-bytes); 
    // Amount to drop is + vector size - integer tag + vector header
    ins--add(be, bytes-to-drop, vector-size, 8 - 1); 
    ins--rts-and-drop(be, bytes-to-drop);
  end with-harp;
end method;


define method op--return-after-vectoring-requireds-and-optionals
    (be :: <harp-back-end>, req-args)
    => ()
  op--rts-dropping-n-requireds-and-optionals(be, req-args);
end method;


define method op--return-after-vectoring-requireds-spreading-optionals
    (be :: <harp-back-end>, req-args)
    => ()
  op--rts-dropping-n-requireds-and-optionals(be, req-args);
end method;



/// op--required-args-num 
// returns the number of required args taken by a function, for an
// entry-point. In the dynamic case, the value is returned as a
// register containing the number of arguments times 4 (the arg-size
// in bytes).

define method op--required-args-num 
    (be :: <harp-back-end>, function :: <register>, required :: <integer>)
    => (num-args :: <integer>)
  required;
end method;


define method op--required-args-num 
    (be :: <harp-back-end>, function :: <register>, required == #"dynamic")
    => (num-args :: <register>)
  with-harp (be)
    nreg num-args;
    op--number-required-times-4(be, num-args, function: function);
    num-args;
  end with-harp;
end method;


define method op--single-method-engine-node-required-args-num
    (be :: <harp-back-end>, disphdr :: <register>, 
     engine :: <register>, required :: <integer>)
  required
end method;

define method op--single-method-engine-node-required-args-num
  // We only use the engine.
    (be :: <harp-back-end>, disphdr :: <register>, 
     engine :: <register>, required == #"dynamic")
  with-harp (be)
    nreg num-args;
    op--engine-field-in-bytes(be, num-args, engine, smen$v-nrequired, smen$s-nrequired);
    num-args;
  end with-harp;
end method;

define method op--random-engine-node-required-args-num
    (be :: <harp-back-end>, disphdr :: <register>, engine :: <register>,
     required :: <integer>)
  required
end method;

define method op--random-engine-node-required-args-num
    (be :: <harp-back-end>, disphdr :: <register>, engine :: <register>,
     required == #"dynamic")
  // We only use the disphdr, the engine is useless to us.
  with-harp (be)
    nreg req-args;
    ins--preserve-registers-entry(be);
    ins--push(be, disphdr);
    op--dispatch-header-gf(be, disphdr, disphdr);
    op--number-required-times-4(be, req-args, function: disphdr);
    ins--pop(be, disphdr);
    ins--preserve-registers-exit(be);
    req-args;
  end with-harp;
end method;


/// op--taggify-arg-num
// returns the number of args as a tagged integer. 
// The format of the number is as for op--required-args-num - i.e.
// if the number is passed as a register, then its been multiplied by 4.


define method op--taggify-arg-num
    (be :: <harp-back-end>, arg-num :: <integer>)
    => (size :: <integer>)
  tag-as-integer(be, arg-num);
end method;


define method op--taggify-arg-num
    (be :: <harp-back-end>, arg-num :: <register>)
    => (size :: <register>)
  with-harp (be)
    greg tagged-num;
    ins--add(be, tagged-num, arg-num, 1);
    tagged-num;
  end with-harp;
end method;


/// op--ensure-arg-num-times-4
// Returns the arg-num times 4. This is a NOP if the arg-num is represented
// that way anyway in a register.


define method op--ensure-arg-num-times-4
    (be :: <harp-back-end>, arg-num :: <integer>)
    => (size :: <integer>)
  arg-num * 4;
end method;


define method op--ensure-arg-num-times-4
    (be :: <harp-back-end>, arg-num :: <register>)
    => (size :: <register>)
  arg-num;
end method;


/// General support for calling methods from engine nodes


define method op--perform-engine-node-callback
    (be :: <harp-back-end>, 
     required, 
     disphdr :: <register>,
     engine :: <register>,
     vectoring-fn :: <function>, 
     returning-fn :: <function>,
     req-args-fn :: <function>,
     callback-fn  :: <function>,
     #key tmp1?)
    => ()
  // We get invoked in two circumstances.
  // In one, we are implementing one of the general-engine-node-xxx routines.
  // In this case, the engine-node is some kind of terminal or invalidated
  // engine node, and we generally cannot get any information from it.  (e.g.,
  // it could be an engine-node which is not shared per-function, like the
  // $absent-engine-node, or one of the slot accessing engine nodes).  In this
  // case, we may have to do what we would like to avoid, which is backing up
  // from the dispatch-header to the generic function in order to find out about
  // the calling sequence.



  with-harp (be)
    frame frame;
    greg arg-vec;

    // op--dispatch-header-gf(be, arg-vec, disphdr);
    // let req-args = op--required-args-num(be, arg-vec, required);
    let req-args = req-args-fn(be, disphdr, engine, required);

    let full = instance?(req-args, <register>); // check for register pressure
    ins--preserve-registers-entry(be);
    ins--push(be, disphdr);           // Need general ease of register pressure
    if (full)               // ease register pressure
      ins--push(be, engine);
    end if;
    if (tmp1?) ins--push(be, tmp1?) end;
    vectoring-fn(be, arg-vec, disphdr, req-args);
    if (tmp1?)
      ins--ld(be, tmp1?, frame, if (full) -12 else -8 end)
    end;
    if (full)               // ease register pressure
      ins--push(be, req-args);
      ins--ld(be, engine, frame, -8);
    end if;
    ins--ld(be, disphdr, frame, -4);  // Need general ease of register pressure
    callback-fn(be, arg-vec, engine, disphdr);
    if (full) ins--pop(be, req-args) end;         // ease register pressure
    ins--preserve-registers-exit(be);
    returning-fn(be, req-args);
  end with-harp;
end method;


define method op--load-rest-parameter 
    (be :: <harp-back-end>, dest :: <register>, 
     engine :: <register>, required :: <integer>) 
    => ()
  op--load-argument-n-leafcase(be, dest, required);
end method;

  
define method op--load-rest-parameter 
    (be :: <harp-back-end>, dest :: <register>, 
     engine :: <register>, required == #"dynamic") 
    => ()
  with-harp (be)
    nreg num-required;
    op--single-method-engine-node-number-required-in-bytes(be, num-required, engine);
    op--load-argument-n-leafcase(be, dest, num-required);
  end with-harp;
end method;
     


/// Keyword checking


define method op--single-method-entry-checking-keys
    (be :: <harp-back-end>, required, check-keys :: <function>) 
    => ()
  with-harp (be)
    function function;
    mlist mlist;
    greg engine, disphdr;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    ins--move(be, engine, function);
    ins--move(be, disphdr, mlist);
    check-keys(be, required, disphdr, engine);

    // Through with disphdr.  Reuse it for the mlist.
    ins--ld(be, disphdr, engine, be.engine-node-data-2-offset);
    // Through with engine.  Reuse it for the method to call.
    ins--ld(be, engine, engine, be.engine-node-data-1-offset);

    ins--move(be, function, engine);
    ins--move(be, mlist, disphdr);
    ins--jmp-indirect(be, function, be.function-mep-offset, max-num-arg-regs);
  end with-harp;
end method;



define method op--engine-node-key-check 
    (be :: <harp-back-end>, required, disphdr :: <register>,
     engine :: <register>, implicit? :: <boolean>)
     => ()
  with-harp (be)
    greg restreg, restreg2;
    greg restsize;
    tmp 1, tmp1, key-error;
    tag keys-ok, odd-keys, bad-key;

    let full = required == #"dynamic"; // check for register pressure
    if (full) ins--move(be, tmp1, engine) end;
    op--load-rest-parameter(be, restreg, engine, required);
    op--vector-size-as-tagged-int(be, restsize, restreg);
    // Check first for odd keys
    op--branch-if-odd-tagged-integer(be, odd-keys, restsize);
    if (full) ins--move(be, engine, tmp1) end;
    // And then for unwanted keys
    op--load-rest-parameter(be, restreg2, engine, required);
    op--check-for-invalid-keys
      (be, bad-key, disphdr, engine, key-error, restreg2, implicit?);
    ins--bra(be, keys-ok);

    // Case where we've detected odd keys
    ins--tag(be, odd-keys);
    if (full) ins--move(be, engine, tmp1) end;
    // Use zero to indicate odd keys rather than unwanted key
    ins--move(be, key-error, 0);  

    // General case where there's a problem with the keys.
    // This might be odd keys or an unwanted key. The value of
    // key-error says which.
    ins--tag(be, bad-key);

    local method bad-key-callback
              (be :: <harp-back-end>, arg-vec :: <register>, 
	       engine :: <register>, disphdr :: <register>)
	    with-harp(be)
              greg keys;
	      tag done, odd;
	      ins--beq(be, odd, key-error, 0);
              // Recalc the allowed keys set, to ease pressure
              op--load-accepted-keywords(be, keys, engine, implicit?);
	      op--unwanted-key-callback
	        (be, arg-vec, engine, disphdr, key-error, keys, implicit?);
              ins--bra(be, done);
	      ins--tag(be, odd);
	      op--odd-keys-callback(be, arg-vec, engine, disphdr);
	      ins--tag(be, done);
	    end with-harp;
	  end method;

    op--perform-engine-node-callback
      (be, required, disphdr, engine,
       op--vector-up-requireds-and-optionals,
       op--return-after-vectoring-requireds-and-optionals,
       op--single-method-engine-node-required-args-num,
       bad-key-callback,
       tmp1?: key-error);
       
    ins--tag(be, keys-ok);
  end with-harp;
end method;


define method op--check-for-invalid-keys
    (be :: <harp-back-end>, bad-key :: <tag>, 
     disphdr :: <register>, engine :: <register>, curr-key :: <register>,
     rest-vec :: <register>, implicit? :: <boolean>)

  // There is some serious register pressure for the key checks - so
  // take some steps to alleviate it

  with-harp (be)
    tag all-ok, bad-key-internal;
    arg0 arg0;
    stack stack;
    frame frame;
    arg0 new-rest-vec;
    greg key-vec;
    
    local method preserve-regs ()
            ins--push(be, arg0);
            ins--push(be, frame);
            ins--push(be, engine);
            ins--push(be, disphdr);
            ins--move(be, new-rest-vec, rest-vec);
          end method;

    local method restore-regs (#key tail-calling?)
            if (tail-calling?)
              // Don't need disphdr any more
              ins--add(be, stack, stack, 4);
            else ins--pop(be, disphdr);
            end if;
            ins--pop(be, engine);
            ins--pop(be, frame);
            ins--pop(be, arg0);
          end method;

    preserve-regs();
    op--load-accepted-keywords(be, key-vec, engine, implicit?);
    op--check-for-invalid-keys-internal
      (be, bad-key-internal, curr-key, new-rest-vec, key-vec, implicit?);
    ins--bra(be, all-ok);

    ins--tag(be, bad-key-internal);
    restore-regs();
    ins--bra(be, bad-key);


    ins--tag(be, all-ok);
    restore-regs(tail-calling?: #t);
  end with-harp;
end method;


define method op--check-for-invalid-keys-internal
    (be :: <harp-back-end>, bad-key :: <tag>, curr-key :: <register>,
     rest-vec :: <register>, key-vec :: <register>, implicit? :: <boolean>)

  // Strategy: Scan down the rest parameters from last to first.
  // For each key, check whether it's in the key-vec list

  let rest-vec-inc = 8; // args are always key/value pairs
  with-harp (be)
    tag outer-loop, all-ok;
    nreg rest-cursor;

    // Set up the cursor into the rest vector
    op--vector-size-times-4(be, rest-cursor, rest-vec);
    ins--add(be, rest-cursor, rest-cursor, rest-vec);     // Address of last key

    // Start the outer loop
    ins--tag(be, outer-loop);
    ins--ble(be, all-ok, rest-cursor, rest-vec);          // check for start of the vec
    // Main body of loop
    ins--ld(be, curr-key, rest-cursor, 0);                // Load the key
    op--check-if-key-is-valid(be, bad-key, curr-key, key-vec, implicit?);
    ins--sub(be, rest-cursor, rest-cursor, rest-vec-inc); // move to prev key
    ins--bra(be, outer-loop);   // and loop again

    ins--tag(be, all-ok); // Fall through if there were no problems
  end with-harp;
end method;


define method op--check-if-key-is-valid
    (be :: <harp-back-end>, bad-key :: <tag>, curr-key :: <register>,
     key-vec :: <register>, implicit? :: <boolean>)

  // Strategy: Scan down the permitted keys vector from last to first.
  // For each key, check whether it's in the key-vec list

  // When iterating over the keys, remember that implicit keys are
  // supplied as key/default-value pairs
  let key-vec-inc = if (implicit?) 8 else 4 end;
  with-harp (be)
    tag inner-loop, key-ok;
    frame key-cursor;
    nreg test-key;

    // Set up the cursor into the key vector, and the limit
    op--vector-size-times-4(be, key-cursor, key-vec);
    ins--add(be, key-cursor, key-cursor, key-vec);     // Address of last key
    // Explicit keys are contiguous, so we want to start indexing
    // from the very last index of the vector. Since we don't
    // want to look at the size slot, we just add an offset of 4
    let cursor-offset = if (implicit?) 0 else 4 end;

    // Start the inner loop
    ins--tag(be, inner-loop);
    // check for limit of the vec. There was an illegal key if we find it
    ins--ble(be, bad-key, key-cursor, key-vec);       

    // Main body of loop. Look for a match
    ins--ld(be, test-key, key-cursor, cursor-offset);
    ins--beq(be, key-ok, test-key, curr-key);

    ins--sub(be, key-cursor, key-cursor, key-vec-inc); // move to prev key
    ins--bra(be, inner-loop);   // and loop again

    ins--tag(be, key-ok); // Fall through if there were no problems
  end with-harp;
end method;


define method op--load-accepted-keywords
    (be :: <harp-back-end>, key-vec :: <register>, engine :: <register>,
     implicit? == #t)
  ignore(implicit?);
  with-harp (be)
    greg first-method;
    op--engine-data-1(be, first-method, engine);
    op--method-keywords(be, key-vec, function: first-method);
  end with-harp;
end method;

define method op--load-accepted-keywords
    (be :: <harp-back-end>, key-vec :: <register>, engine :: <register>,
     implicit? == #f)
  ignore(implicit?);
  op--engine-data-3(be, key-vec, engine);
end method;


define method op--odd-keys-callback
    (be :: <harp-back-end>, arg-vec :: <register>, 
     engine :: <register>, disphdr :: <register>)
  with-harp (be)
    op--call-iep(be, dylan-odd-number-keyword-args, arg-vec, disphdr, engine);
  end with-harp;
end method;


define method op--unwanted-key-callback
    (be :: <harp-back-end>, arg-vec :: <register>, 
     engine :: <register>, disphdr :: <register>, 
     bad-key :: <register>, key-vec :: <register>,
     implicit? :: <boolean>)
  with-harp (be)
    op--call-iep(be, dylan-invalid-keyword, 
		 arg-vec, disphdr, engine, bad-key, key-vec,
		 if (implicit?) dylan-true else dylan-false end);
  end with-harp;
end method;





/// DISCRIMINATE-ON-ARGUMENT


define method op--load-discriminator-argument 
     (be :: <harp-back-end>, argument :: <register>, 
      engine :: <register>, arg-num :: <integer>) => ()
  op--load-argument-n-leafcase(be, argument, arg-num);
end method;


define method op--load-discriminator-argument 
     (be :: <harp-back-end>, argument :: <register>, 
      engine :: <register>, arg-num == #"dynamic") => ()
  with-harp (be)
    nreg argnum;
    op--discriminator-argnum-in-bytes(be, argnum, engine);
    op--load-argument-n-leafcase(be, argument, argnum);
  end with-harp;
end method;




// This is *only* for single-method-engine-nodes, which store the
// argument information we need.
define method op--select-single-method-engine-node-entry-point-by-required
    (be :: <harp-back-end>, engine, entry, required, refs)
  op--single-method-engine-node-number-required(be, required, engine); 
  op--select-entry-point(be, entry, required, refs);
end method;


/*
// TEMPORARY METHOD UNTIL THE PROPERTY FIELDS OF ENGINE NODES INCLUDE
// DETAILS OF REQUIRED PARAMETERS
// !"$%

define method op--select-entry-point-by-required-from-method
    (be :: <harp-back-end>, engine, entry, required, refs)
  op--engine-data-1(be, required, engine);   // The method
  op--number-required(be, required, function: required); 
  op--select-entry-point(be, entry, required, refs);
end method;
*/

/*
define method op--select-discriminator-entry-point-by-optionals
    (be :: <harp-back-end>, engine, entry, required, normal-refs, optional-refs)
  with-harp (be)
    tag opts, done;
    op--discriminator-number-required(be, required, engine); 
    op--branch-if-discriminator-with-optionals(be, opts, engine);
    op--select-entry-point(be, entry, required, normal-refs);
    ins--bra(be, done);
    ins--tag(be, opts);
    op--select-entry-point(be, entry, required, optional-refs);
    ins--tag(be, done);
  end with-harp;
end method;
*/


//// This is *only* for single-method-engine-nodes, which store the
//// argument information we need.
//define method op--select-single-method-engine-node-entry-point-by-optionals
//    (be :: <harp-back-end>, engine, entry, required, normal-refs, optional-refs)
//  with-harp (be)
//    tag opts, done;
//    op--single-method-engine-node-number-required(be, required, engine); 
//    op--branch-if-single-method-engine-node-with-optionals(be, opts, engine);
//    op--select-entry-point(be, entry, required, normal-refs);
//    ins--bra(be, done);
//    ins--tag(be, opts);
//    op--select-entry-point(be, entry, required, optional-refs);
//    ins--tag(be, done);
//  end with-harp;
//end method;



define method op--jump-to-specific-entry
    (be :: <harp-back-end>, normal-refs, optional-refs)
  with-harp (be)
    function engine;
    mlist disphdr;
    nreg entry, required;
    greg gf;
    tag opts, done;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    op--dispatch-header-gf(be, gf, disphdr);
    op--number-required(be, required, function: gf); 
    op--branch-if-function-with-optionals(be, opts, function: gf);
    op--select-entry-point(be, entry, required, normal-refs);
    ins--bra(be, done);
    ins--tag(be, opts);
    op--select-entry-point(be, entry, required, optional-refs);
    ins--tag(be, done);

    ins--jmp(be, entry, max-num-arg-regs, function: #t, mlist: #t);
  end with-harp;
end method;





/// Support for unpicking PROPERTIES
///
/// All the ops which follow are DU accessors



define method op--engine-data-1 
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  ins--ld(be, dest, engine, be.engine-node-data-1-offset);
end method;


/*
define method op--engine-data-2
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  ins--ld(be, dest, engine, be.engine-node-data-2-offset);
end method;
*/


define method op--engine-data-3
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  ins--ld(be, dest, engine, be.engine-node-data-3-offset);
end method;


/*
define method op--engine-entry-point
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  ins--ld(be, dest, engine, be.engine-node-entry-point-offset);
end method;
*/


define method op--engine-callback 
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  ins--ld(be, dest, engine, be.engine-node-callback-offset);
end method;


define method op--engine-slot-accessor-offset
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-callback(be, dest, engine);
  // TEMPORARY !"$%
  // The slots are currently stored as tagged indexes, not raw byte-offsets
  // so we have to add 3
  ins--add(be, dest, dest, 3);
end method;


// @@@@ These should go somewhere else.


// It is by design that dest may be the same as disphdr.
define method op--dispatch-header-gf
    (be :: <harp-back-end>, dest :: <register>, disphdr :: <object>)
  (with-harp (be)
     nreg tmp;
   tag done, check-again;
   
   ins--move(be, dest, disphdr);

   ins--tag(be, check-again);

   ins--ld(be, tmp, dest, 0);	// Load wrapper
   ins--ld(be, tmp, tmp, be.mm-wrapper-subtype-mask-offset);
   ins--and(be, tmp, tmp, ash($mm-wrapper-subtype-cache-header-engine-node-mask, 2));
   ins--beq(be, done, tmp, 0);		// If not a cache-header-engine-node we're done
   ins--ld(be, dest, dest, be.engine-node-data-2-offset);
   ins--bra(be, check-again);

   ins--tag(be, done);
  end with-harp);
end method;



define method op--branch-if-dispatch-header-with-optionals
    (be :: <harp-back-end>, tag-with-optionals, disphdr :: <register>)
  (with-harp (be)
     nreg tmp;
   tag done, check-again;

   ins--push(be, disphdr);
   ins--tag(be, check-again);
   ins--ld(be, tmp, disphdr, 0);	// Load wrapper
   ins--ld(be, tmp, tmp, be.mm-wrapper-subtype-mask-offset);
   ins--and(be, tmp, tmp, ash($mm-wrapper-subtype-cache-header-engine-node-mask, 2));
   ins--beq(be, done, tmp, 0);		// If not a cache-header-engine-node we're done
   ins--ld(be, disphdr, disphdr, be.engine-node-data-2-offset);
   ins--bra(be, check-again);
   ins--tag(be, done);
   op--function-properties(be, tmp, function: disphdr);
   ins--pop(be, disphdr);
   ins--and(be, tmp, tmp, ash(optionals-p-mask, 2));
   ins--bne(be, tag-with-optionals, tmp, 0);
  end with-harp);
end method;


// Accessors for the properties

// define constant $type-shift = 2;
// define constant $type-mask  = #x3f;


// Discriminator specific properties


define method op--engine-properties 
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  ins--ld(be, dest, engine, be.engine-node-properties-offset);
end method;


define method op--engine-field
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>,
     shift :: <integer>, field-size :: <integer>)
  let mask :: <integer> = ash(1, field-size) - 1;
  op--engine-properties(be, dest, engine);
  ins--asr(be, dest, dest, shift);
  ins--and(be, dest, dest, mask);
end method;

define method op--engine-field-in-bytes
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>,
     shift :: <integer>, field-size :: <integer>)
  let mask :: <integer> = ash(1, field-size) - 1;
  let byte-shift = (shift - 2);
  let byte-mask = ash(mask, 2);
  op--engine-properties(be, dest, engine);
  unless (byte-shift = 0)
    ins--asr(be, dest, dest, byte-shift) 
  end;
  ins--and(be, dest, dest, byte-mask);
end method;


define method op--engine-entry-type
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field(be, dest, engine, properties$v-entry-type, properties$s-entry-type);
end method;

define method op--engine-entry-type-in-bytes
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field-in-bytes(be, dest, engine, properties$v-entry-type, properties$s-entry-type);
end method;


define method op--single-method-engine-node-number-required
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field(be, dest, engine, smen$v-nrequired, smen$s-nrequired);
end method;

define method op--single-method-engine-node-number-required-in-bytes
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field-in-bytes(be, dest, engine, smen$v-nrequired, smen$s-nrequired);
end method;


/*
define method op--discriminator-number-required
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field(be, dest, engine, discriminator$v-nrequired, discriminator$s-nrequired);
end method;

define method op--discriminator-number-required-in-bytes
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field-in-bytes(be, dest, engine, discriminator$v-nrequired, discriminator$s-nrequired);
end method;
*/


define method op--discriminator-argnum
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field(be, dest, engine, discriminator$v-argnum, discriminator$s-argnum);
end method;

define method op--discriminator-argnum-in-bytes
    (be :: <harp-back-end>, dest :: <register>, engine :: <object>)
  op--engine-field-in-bytes(be, dest, engine, discriminator$v-argnum, discriminator$s-argnum);
end method;


// OP--BRANCH-IF-SINGLE-METHOD-ENGINE-NODE-WITH-OPTIONALS
// branches to tag if the engine node has optionals

/*
define method op--branch-if-single-method-engine-node-with-optionals
    (be :: <harp-back-end>, tag :: <tag>, engine :: <object>)
  let properties = make-n-register(be);
  op--engine-properties(be, properties, engine);
  // mask out other bits
  ins--and(be, properties, properties, ash(1, smen$v-restp));
  ins--bne(be, tag, properties, 0);
end method;
*/


// OP--BRANCH-IF-DISCRIMINATOR-WITH-OPTIONALS
// branches to tag if the discriminator has optionals

/*
define method op--branch-if-discriminator-with-optionals
    (be :: <harp-back-end>, tag :: <tag>, engine :: <object>)
  let properties = make-n-register(be);
  op--engine-properties(be, properties, engine);
  // mask out other bits
  ins--and(be, properties, properties, ash(1, discriminator$v-restp));
  ins--bne(be, tag, properties, 0);
end method;
*/
