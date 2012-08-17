module:    harp-native-rtg
Synopsis:  Support for entry points with keywords
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Keyword processing


// Variable to control whether to perform XEP key checking.
// If set to false, then invalid keys will not flag errors

define variable *perform-key-checking* = #f;



// PRIMITIVE-PROCESS-KEYS assumes an appropriate IEP stack layout,
// with space already on the stack for keyword variables. It
// looks at the optional parameters supplied (from first-opt upwards)
// and compares them with the expected keywords of the function
// object.
// On entry:
//   tmp1:      contains the #rest vector
//   arg-count: contains the address of the hole for the last keyword param
//   function:  contains function object as normal
// On exit  
//   The function IEP is tail-called
//   MList, Function, Arg0 are preserved.


define entry-point primitive-process-keys
  (be :: <harp-back-end>, entry-point,
   limit: method(be :: <harp-back-end>)
	      be.registers.arguments-passed-in-registers - 2
	  end)
  let keys-in-registers =
    if (instance?(entry-point, <integer>)) entry-point + 1
    else 0 end;
  op--safe-process-keys(be, keys-in-registers);
  op--tail-call-iep(be, keyword-method?: #t);
end entry-point;



// PRIMITIVE-PROCESS-KEYS-FOR-XEP is similar to PRIMITIVE-PROCESS-KEYS
// It additionally performs keyword checks (not yet implemented)


define entry-point primitive-process-keys-for-xep
  (be :: <harp-back-end>, entry-point,
   limit: method(be :: <harp-back-end>)
	      be.registers.arguments-passed-in-registers - 2
	  end)
  let keys-in-registers =
    if (instance?(entry-point, <integer>)) entry-point + 1
    else 0 end;
  op--safe-process-keys(be, keys-in-registers,
			check-keys?: *perform-key-checking*);
  op--tail-call-iep(be, keyword-method?: #t);
end entry-point;



// PRIMITIVE-PROCESS-KEYS-CHECKING-ARGS-FOR-XEP is similar to 
// PRIMITIVE-PROCESS-KEYS-FOR-XEP
// It additionally checks required arguments against the specializer types


define entry-point primitive-process-keys-checking-args-for-xep
  (be :: <harp-back-end>, entry-point,
   limit: method(be :: <harp-back-end>)
	      be.registers.arguments-passed-in-registers - 2
	  end)
  nreg req-num;
  tag err-tag;

  let keys-in-registers =
    if (instance?(entry-point, <integer>)) entry-point + 1
    else 0 end;
  op--safe-process-keys(be, keys-in-registers,
			check-keys?: *perform-key-checking*);
  // Build a stack frame around the call 
  ins--preserve-registers-entry(be);
  op--number-required-times-4(be, req-num);
  op--specializer-checks-via-loop(be, err-tag, req-num);
  op--tail-call-iep(be, keyword-method?: #t);
  ins--tag(be, err-tag);
  op--specializer-error-call(be);
end entry-point;



define method op--safe-process-keys 
    (be :: <harp-back-end>, keys-in-registers :: <integer>,
     #key check-keys? = #f)
  with-harp (be)
    // We need all the spare registers we can get for this bit
    // so for the Pentium, we push all live registers including frame.
  
    greg the-keys;
    tmp 1, rest-vec;
    arg-count last-key;
    frame frame;
    arg0 arg0;
    function function;
    mlist mlist;
    stack stack;
  
    if (argument-on-stack?(rest-vec))
      ins--pop(be, rest-vec);
    end;

    // We also need to push the key vector itself - because later code 
    // may only keep derived pointers in the register set & we can't let it move
    op--method-keywords(be, the-keys);

    local method preserve-args (be)
            ins--push(be, arg0);
            ins--push(be, function);
            ins--push(be, mlist);
            ins--push(be, the-keys);
          end method,

          method restore-args (be)
            ins--add(be, stack, stack, 4); // "pop" off the-keys
            ins--pop(be, mlist);
            ins--pop(be, function);
            ins--pop(be, arg0);
          end method;

    op--process-keys-aux(be, rest-vec, last-key, the-keys,
			 keys-in-registers,
                         preserve-fn: preserve-args,
                         restore-fn:  restore-args,
                         check-keys?: check-keys?);


  end with-harp;
end method;

define inline method argument-on-stack? (reg :: <register>)
 => (on-stack? :: <boolean>)
  instance?(reg, <virtual-register>)
end method;


define method op--process-keys-aux
    (be :: <harp-back-end>, rest-vec :: <register>, 
     last-param :: <register>, copy-of-the-keys :: <register>,
     keys-in-registers :: <integer>,
     #key preserve-fn = method (be) end, 
          restore-fn = method (be) end, 
          check-keys? = #f)

  // Strategy: 
  // First fill in the parameters with their default values.
  // Next scan down the supplied optional arguments, last to first,
  // looking for keywords. For each such keyword, check for it in the
  // vector of accepted keys, storing it into the corresponding 
  // keyword parameter if found. If it wasn't found, then possibly error.

  with-harp (be)
    greg the-keys, properties;
    nreg first-opt;           // address of the first optional arg
    tmp 1, curr-opt;          // address of optional arg being processed
    nreg all-keys;            // zero if the function doesn't take all-keys
    nreg opt-size;            // size of optional args
    nreg key-size;            // size of key/default vector
    nreg first-key;           // address of first key in vec of defaults
    nreg end-of-keys;         // address past the last key in vec of defaults
    stack stack;
    tag not-odd, loop-test, start-loop;
    let first-param = last-param;  // address of first parameter in arguments

    preserve-fn(be);               // save any arguments etc.

    op--function-properties(be, properties);
    ins--and(be, all-keys, properties, ash(all-keys-p-mask, 2));
    ins--push(be, all-keys);     // Save the all-keys on the stack
    local method restore-fn-1 (be)
            ins--add(be, stack, stack, 4); // drop all-keys 
            restore-fn(be);  // invoke the caller's restore fn
          end method;

    // Set the keyword parameters to their defaults
    op--method-keywords(be, the-keys);
    op--default-all-key-parameters(be, last-param, the-keys, keys-in-registers);

    // set up the first and last opts
    op--vector-size-times-4(be, opt-size, rest-vec);
    ins--nbit(be, not-odd, opt-size, #b100, 0);
    op--report-odd-args(be, restore-fn-1);
    ins--tag(be, not-odd);
    ins--add(be, first-opt, rest-vec, 8);  // the first optional
    ins--add(be, curr-opt, opt-size, first-opt); // just past live data

    op--vector-size-times-4(be, key-size, the-keys);
    ins--add(be, first-key, the-keys, 8); // address of first key
    ins--add(be, end-of-keys, first-key, key-size); // just past end of keys
    ins--sub(be, end-of-keys, end-of-keys, 8);  // address of last key

    ins--asr(be, key-size, key-size, 1);   // size of keyword parameter area
    unless (keys-in-registers == 0)
      let continue = make-tag(be);
      ins--sub(be, key-size, key-size, 4 * keys-in-registers);
      ins--bge(be, continue, key-size, 0);
      ins--move(be, key-size, 0);
      ins--tag(be, continue);
    end;

    ins--sub(be, first-param, last-param, key-size);
    ins--add(be, first-param, first-param, 4);  // address of first keyword param

    ins--push(be, first-opt); // lots of register pressure
    let all-keys-offset = 4; // stack offset of all-keys word
    local method new-restore-fn (be)
            ins--add(be, stack, stack, 8); // drop first-opt & all-keys 
            restore-fn(be);  // invoke the caller's restore fn
          end method;

    // Now scan down the supplied optionals
    ins--bra(be, loop-test);               // start by going to the end test
    
    // Start the main loop
    ins--tag(be, start-loop);
    op--process-one-keyval-argument
      (be, curr-opt, first-param, first-key, end-of-keys,
       all-keys-offset, new-restore-fn, check-keys?, keys-in-registers);

    // Update loop variables & test whether to branch back to the start
    ins--tag(be, loop-test);
    ins--sub(be, curr-opt, curr-opt, 8);   // move back to previous key/val pair
    ins--ld(be, first-opt, stack, 0);      // Spilled to relieve register pressure
    ins--bhs(be, start-loop, curr-opt, first-opt);

    new-restore-fn(be);                // restore any arguments etc.
  end with-harp;
end method;


define method op--process-one-keyval-argument
    (be :: <harp-back-end>, 
     curr-opt :: <register>,
     first-param :: <register>,
     first-key :: <register>,
     end-of-keys :: <register>,
     all-keys-offset :: <integer>, 
     restore-fn :: <function>,
     check-keys? :: <boolean>,
     keys-in-registers :: <integer>)
  with-harp (be)
    greg supplied-value; // the value in the optionals vector
    greg supplied-key;   // the key in the optionals vector
    nreg key-offset;
    nreg cursor;
    nreg all-keys;
    stack stack;
    tag start-loop, loop-test, found, done;

    // Initialize the loop, and jump to the test
    ins--ld(be, supplied-key, curr-opt, 0);
    ins--move(be, cursor, first-key);
    ins--bra(be, loop-test);

    // Main body of loop.
    ins--tag(be, start-loop);
    ins--beq-mem(be, found, cursor, 0, supplied-key);
    ins--add(be, cursor, cursor, 8);  // move onto next key/value pair

    ins--tag(be, loop-test);
    ins--bls(be, start-loop, cursor, end-of-keys);

    // If we get here, then the key wasn't expected
    if (check-keys?)
      ins--ld(be, all-keys, stack, all-keys-offset);
      ins--bne(be, done, all-keys, 0); 
      op--report-unknown-key(be, restore-fn, supplied-key);
    end if;
    ins--bra(be, done);

    // If we get here then a key was found, so store the value into the params
    ins--tag(be, found);
    ins--sub(be, key-offset, cursor, first-key); // index into key/value data
    ins--asr(be, key-offset, key-offset, 1); // corresponding index into params
    ins--ld(be, supplied-value, curr-opt, 4);

    keyval-argument-case-generator
      (be, first-param, supplied-value, key-offset, keys-in-registers);

    ins--tag(be, done);
  end with-harp;
end method;

define method keyval-argument-case-generator
    (be :: <harp-back-end>,
     first-param :: <register>,
     supplied-value :: <register>,
     key-offset :: <register>,
     num-cases :: <integer>) => ()
  with-harp (be)
    tag key-on-stack, done;
    nreg key-offset-on-stack;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    let case-1 = max(max-num-arg-regs - num-cases, 0);
    let cases? = num-cases > 0;
    let tags :: <simple-object-vector> = make-tags(be, num-cases);
  
    // test for the special cases (key in an argument register)
    for (tag :: <tag> in tags,
	 i :: <integer> from 0)
      // key-offset is in bytes
      ins--beq(be, tag, key-offset, 4 * i);
    end for;
    
    // fall through
    cases? & ins--bra(be, key-on-stack);

    // generate all the special cases (key in an argument register)
    for (tag :: <tag> in tags,
	 i :: <integer> from case-1)
      ins--tag(be, tag);
      ins--move(be, argument-register(i), supplied-value);
      ins--bra(be, done);
    end for;

    // case where key goes unto the stack
    if (cases?)
      ins--tag(be, key-on-stack);
      ins--sub(be, key-offset-on-stack, key-offset, 4 * num-cases);
      ins--st(be, supplied-value, first-param, key-offset-on-stack);
      ins--tag(be, done);
    else
      ins--st(be, supplied-value, first-param, key-offset);
    end;
    
  end with-harp;
end method;


// OP--DEFAULT-ALL-KEY-PARAMETERS
// initializes the key parameters with their default values

define method op--default-all-key-parameters
    (be :: <harp-back-end>, last-param :: <register>,
     defaults :: <register>, keys-in-registers :: <integer>)
  with-harp (be)
    nreg defaults-size, key-size, param-ptr, default-ptr;
    greg val;
    tag loop, done;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    op--vector-size-times-4(be, defaults-size, defaults); // size of key/default pairs
    ins--lsr(be, key-size, defaults-size, 1);   // size in bytes of keys alone
    ins--add(be, default-ptr, defaults, 8 + 4); // First default value

    for (i :: <integer> from max-num-arg-regs - keys-in-registers + 1
	                to max-num-arg-regs)
      ins--ble(be, done, key-size, 0);
      ins--ld(be, val, default-ptr, 0);
      ins--move(be, argument-register(i - 1), val);
      ins--add(be, default-ptr, default-ptr, 8);
      ins--sub(be, key-size, key-size, 4);
    end;

    ins--sub(be, param-ptr, last-param, key-size);
    ins--add(be, param-ptr, param-ptr, 4);      // First key
    ins--tag(be, loop);
    ins--bhi(be, done, param-ptr, last-param);

    ins--ld(be, val, default-ptr, 0);
    ins--st(be, val, param-ptr, 0);
    ins--add(be, default-ptr, default-ptr, 8);
    ins--add(be, param-ptr, param-ptr, 4);

    ins--bra(be, loop);
    ins--tag(be, done);
  end with-harp;
end method;


// OP--REPORT-ODD-ARGS
// generates a call to the odd keyword arguments error handler

define method op--report-odd-args (be :: <harp-back-end>, restore-fn :: <function>)
  // Ideally, this would be smart about leaving the world in a recoverable position
  // but for now, we assume that the error function cannot return.
  with-harp (be)
    function function;

    restore-fn(be);
    ins--preserve-registers-entry(be);
    op--call-iep(be, dylan-odd-keys-error, function);
    ins--rts(be);
  end with-harp;
end method;


// OP--REPORT-UNKNOWN-KEY
// generates a call to the unknown keyword argument error handler

define method op--report-unknown-key
    (be :: <harp-back-end>, restore-fn :: <function>, key :: <register>)
  // Ideally, this would be smart about leaving the world in a recoverable position
  // but for now, we assume that the error function cannot return.
  with-harp (be)
    function function;
    greg copy-key;

    ins--move(be, copy-key, key);
    restore-fn(be);
    ins--preserve-registers-entry(be);
    op--call-iep(be, dylan-unknown-key-error, function, copy-key);
    ins--rts(be);
  end with-harp;
end method;



// OP--TAIL-CALL-PROCESS-KEYS
// generates a tail jump to the keyword processing entry point.

define method op--tail-call-process-keys
    (be :: <harp-back-end>, 
     rest-vec :: <register>, last-key :: <register>, required,
     #key set-mlist = #f, set-function = #f)
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let keys-in-regs =
    if (special-case?(be, required))
      let args-in-regs = arguments-in-registers(be, required + 1);
      max-num-arg-regs - args-in-regs;
    else 0
    end;
  let entry-point =
    if (keys-in-regs = 0) max-num-arg-regs - 1
    else keys-in-regs - 1
    end;
  op--tail-call-xep-key-processor
    (be, primitive-process-keys-refs[entry-point],
     rest-vec, last-key, set-mlist, set-function);
end method;


// OP--TAIL-CALL-PROCESS-KEYS-FOR-XEP
// generates a tail jump to the XEP keyword processing entry point.
// Unless there are no required values, the entry point which checks
// args against specializers will be used

define method op--tail-call-process-keys-for-xep
    (be :: <harp-back-end>, 
     rest-vec :: <register>, last-key :: <register>, required, 
     #key set-mlist = #f, set-function = #f)
  let max-num-arg-regs = be.registers.arguments-passed-in-registers;
  let keys-in-regs =
    if (special-case?(be, required))
      let args-in-regs = arguments-in-registers(be, required + 1);
      max-num-arg-regs - args-in-regs;
    else 0
    end;
  let entry-point =
    if (keys-in-regs = 0) max-num-arg-regs - 1
    else keys-in-regs - 1
    end;
  let key-processor =
    if (required == 0)
      primitive-process-keys-for-xep-refs[entry-point]
    else
      primitive-process-keys-checking-args-for-xep-refs[entry-point]
    end if;
  op--tail-call-xep-key-processor
    (be, key-processor, rest-vec, last-key, set-mlist, set-function);
end method;


define method op--tail-call-xep-key-processor
    (be :: <harp-back-end>, key-processor :: <constant-reference>,
     rest-vec :: <register>, last-key :: <register>, 
     set-mlist, set-function)
  with-harp (be)
    arg-count argc;
    tmp 1, tmp1;
    mlist mlist;
    function function;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    ins--move(be, argc, last-key);

    if (argument-on-stack?(tmp1))
      ins--push(be, rest-vec);
    else
      ins--move(be, tmp1, rest-vec);
    end;

    if (set-mlist)
      ins--move(be, mlist, set-mlist);
    end if;
    if (set-function)
      ins--move(be, function, set-function);
    end if;
    ins--jmp(be, key-processor, max-num-arg-regs, 
             mlist: #t, arg-count: #t);
  end with-harp;
end method;





/************************ OLD CODE ************************

// This was well used and tested, and possibly more efficient - but doesn't check
// for invalid keys.


define method op--process-keys-internal
    (be :: <harp-back-end>, rest-vec :: <register>, 
     last-key :: <register>, copy-of-the-keys :: <register>, #key
     preserve-fn = method (be) end, 
     restore-fn = method (be) end, 
     check-keys? = #f)

  // !@#$
  // We currently don't check for unexpected keys.
  // We also don't currently check for unpaired keys.

  // Strategy: Scan down the function keywords vector from last
  // to first. For each key, we scan along the optionals to
  // see if a value was supplied. If so, we stick it in the 
  // reserved space on the stack. If not, we get the default instead. 
  //
  // We keep a count of how many more times to go around the outer loop.
  // To make it easier to index using the count, we multiply by 4, so 
  // that we get the count in bytes.

  with-harp (be)
    nreg tmp;
    frame frame;               // available as another temporary
    arg-count key-count;       // times round outer loop
    nreg curr-key;             // key to check against
    frame curr-dest;           // variable to initialize
    nreg opt-limit;            // the last opt
    let first-opt = rest-vec;  // the first opt
    let n-last-key = op--duplicate(be, last-key);
  
    preserve-fn(be);
    ins--push(be, frame);

    // Set up outer loop to look at each keyword parameter in turn 
    // (backwards)
  
    // set up the count (raw number of keys times 4)
    op--keywords-size-times-4(be, key-count);
  
    // set up the current key
    op--method-keywords(be, curr-key);    // the key vector itself
    ins--asl(be, tmp, key-count, 1);      // key vector size in bytes
    ins--add(be, curr-key, curr-key, tmp);// holds address of last key
    // set up the current stack variable destination
    ins--move(be, curr-dest, n-last-key);
    // set up the first and last opts
    op--vector-size-times-4(be, opt-limit, rest-vec);
    ins--add(be, first-opt, rest-vec, 8);  // the first optional
    ins--add(be, opt-limit, opt-limit, first-opt); // just past live data
  
    // Now start the loop
    let outer-loop-test = make-tag(be);
    let start-outer-loop = make-tag(be);
  
    ins--bra(be, outer-loop-test);         // start by going to the end test
  
    ins--tag(be, start-outer-loop);
    // do the body
    op--process-one-keyword(be, curr-key, curr-dest, first-opt, opt-limit);
    // and step the control variables
    ins--sub(be, key-count, key-count, 4);
    ins--sub(be, curr-key, curr-key, 8);
    ins--sub(be, curr-dest, curr-dest, 4);
    // and branch back to the start
    ins--tag(be, outer-loop-test);
    ins--bne(be, start-outer-loop, key-count, 0);

    ins--pop(be, frame);
    restore-fn(be);
  end with-harp;
end method;


define method op--process-one-keyword
    (be :: <harp-back-end>, 
     curr-key :: <register>,
     curr-dest :: <register>,
     first-opt :: <register>,
     opt-limit :: <register>)
  let regs = be.registers;
  let curr-opt = make-n-register(be);     // argument to check against
  let search-key = make-n-register(be);   // key we are looking for
  let the-value = search-key;             // register to hold the result

  // initialize the loop to scan the supplied optionals from the 
  // first to the limit
  ins--move(be, curr-opt, first-opt);
  ins--ld(be, search-key, curr-key, 0);

  // Now start the loop
  let inner-loop-test = make-tag(be);
  let start-inner-loop = make-tag(be);
  let end-inner-loop = make-tag(be);
  let key-found = make-tag(be);

  ins--bra(be, inner-loop-test);         // start by going to the end test
  ins--tag(be, start-inner-loop);        // the real start of loop

  // The main body of the loop. Test if we have a keyword match.
  ins--beq-mem(be, key-found, curr-opt, 0, search-key);

  // step the control variable by 8 (past a key/value pair)
  ins--add(be, curr-opt, curr-opt, 8);

  // test whether to branch back to the start
  ins--tag(be, inner-loop-test);
  ins--blt(be, start-inner-loop, curr-opt, opt-limit);

  // Do the value defaulting if a key was not found
  ins--ld(be, the-value, curr-key, 4);    // get the default value
  ins--bra(be, end-inner-loop);

  // Accept the value if a key was found
  ins--tag(be, key-found);
  ins--ld(be, the-value, curr-opt, 4);    // load the value from the pair

  // The real finish
  ins--tag(be, end-inner-loop);
  ins--st(be, the-value, curr-dest, 0);   // Put the key value into place
end method;

/// end of old code /////////////////////////////////////////// 

*/
