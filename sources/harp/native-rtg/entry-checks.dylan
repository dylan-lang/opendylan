module:    native-rtg
Synopsis:  Entry point checking for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



//// Basic functionality for checking in entry points



/// STACK CHECK

define method op--stack-check 
    (be :: <harp-back-end>, tag :: <tag>)
  // Do nothing for now;
end method;


define method op--stack-overflow-error-call 
    (be :: <harp-back-end>)
end method;




/// ARG COUNT CHECK


define method op--arg-count-check 
    (be :: <harp-back-end>, tag :: <tag>, 
     received-args, required-args == 0, optionals? == #t)
  // no test needed for a method which has no requireds & takes opts
end method;

define method op--arg-count-check 
    (be :: <harp-back-end>, tag :: <tag>, 
     received-args, required-args, optionals?)
  // generate an arg check against a constant
  let required = op--required-args-for-check(be, required-args);
  let ins--test = invalid-args-branch-fn(be, optionals?);
  ins--test(be, tag, received-args, required);
end method;


define method invalid-args-branch-fn
    (be :: <harp-back-end>, optionals?) => (r :: <function>);
  if (optionals?)
    ins--blo;
  else
    ins--bne;
  end if;
end method;


define method op--required-args-for-check 
      (be :: <harp-back-end>, required-args :: <integer>) 
      => (r :: <integer>);
  required-args;
end;

define method op--required-args-for-check 
      (be :: <harp-back-end>, required-args == #"dynamic") 
      => (r :: <register>);
  let reg = make-n-register(be);
  op--number-required(be, reg);
  reg;
end;


define method op--args-error-call 
    (be :: <harp-back-end>, args, required)
  // Report inappropriate argument count in an XEP call.
  // This should be smart about about cleaning up the stack and 
  // returning, to allow the error handler function to return (if it 
  // has a restart handler, for example). But it's not really worth 
  // doing that until the error handler gets passed all the arguments.
  let regs = be.registers;
  let function = regs.reg-function;
  // Build a stack frame around the call to the handler
  ins--preserve-registers-entry(be);
  op--call-iep(be, dylan-arg-count-error, function, tag-as-integer(be, args));
  ins--rts(be);
end method;




/// SPECIALIZER CHECK


define method op--specializer-checks
    (be :: <harp-back-end>, tag :: <tag>, required :: <integer>)
  let in-line-limit = max(3, be.registers.arguments-passed-in-registers);
  unless (required == 0)
    if (required <= in-line-limit)
      op--specializer-checks-in-line(be, tag, required);
    else
      // Don't perform the checks in-line
      // op--specializer-checks-via-loop(be, tag, required);
      op--tail-call-check-specializers(be, required);
    end if;
  end unless;
end method;


define method op--specializer-checks
    (be :: <harp-back-end>, tag :: <tag>, required == #"dynamic")
  with-harp (be)
    nreg req-num;
    op--number-required-times-4(be, req-num);
    // Don't perform the checks in-line
    // op--specializer-checks-via-loop(be, tag, req-num);
    op--tail-call-check-specializers(be, req-num);
  end with-harp;
end method;


define method op--preserve-args-for-specializer-check
    (be :: <harp-back-end>, 
     value, type, specs,
     #key spare1, spare2, frame?)
  // Build a stack frame around the call
  if (frame?)
    ins--preserve-registers-entry(be);
  end;

  ins--push(be, specs);  // FP -4
  ins--push(be, value);  // FP -8
  ins--push(be, type);   // FP -12
  if (spare1) ins--push(be, spare1) end; // FP -16
  if (spare2) ins--push(be, spare2) end; // FP -20
end method;


define method op--load-specializer-saved-registers
    (be :: <harp-back-end>, #key specs, value, type, spare1, spare2)
  if (spare2) ins--load-frame-offset(be, spare2, -5) end;
  if (spare1) ins--load-frame-offset(be, spare1, -4) end;
  if (type)   ins--load-frame-offset(be, type,   -3) end;
  if (value)  ins--load-frame-offset(be, value,  -2)  end;
  if (specs)  ins--load-frame-offset(be, specs,  -1)  end;
end method;


define method op--store-specializer-saved-registers
    (be :: <harp-back-end>, #key specs, value, type, spare1, spare2)
  if (spare2) ins--store-frame-offset(be, spare2, -5) end;
  if (spare1) ins--store-frame-offset(be, spare1, -4) end;
  if (type)   ins--store-frame-offset(be, type,   -3) end;
  if (value)  ins--store-frame-offset(be, value,  -2)  end;
  if (specs)  ins--store-frame-offset(be, specs,  -1)  end;
end method;


define method op--load-specializer-saved-arg-register
    (be :: <harp-back-end>, i :: <integer>)
  ins--load-frame-offset(be, be.registers.reg-machine-arguments[i], -5 - i);
end method;


define method op--restore-args-for-specializer-check
    (be :: <harp-back-end>, specs :: <register>)
  op--load-specializer-saved-registers(be, specs: specs);
  ins--preserve-registers-exit(be);
end method;


define method op--store-args-for-specializer-error-call 
    (be :: <harp-back-end>, value :: <register>, type :: <register>)
  op--store-specializer-saved-registers(be, value: value, type: type);
end method;

define method op--retrieve-args-for-specializer-error-call 
    (be :: <harp-back-end>, value :: <register>, type :: <register>)
  op--load-specializer-saved-registers(be, value: value, type: type);
end method;


define method op--specializer-error-call 
    (be :: <harp-back-end>)
  with-harp (be)
    greg value, type;
    // NB: register convention at this point is that
    // there is already a function frame, and the value and type
    // parameters were saved with op--push-args-for-specializer-error-call 
    op--retrieve-args-for-specializer-error-call(be, value, type);
    op--call-iep(be, dylan-type-check-error, value, type);
    ins--rts(be);
  end with-harp;
end method;



// OP--SPECIALIZER-CHECKS-VIA-LOOP
// Checks all arguments against the expected specializer types, using a loop
// for the arguments passed on the stack. The function assumes that all argument
// registers are full. 
//
// req-num is a register containing the total number of required arguments times 4,
// or is an integer count of the number of required arguments

define method op--specializer-checks-via-loop
    (be :: <harp-back-end>, err-tag :: <tag>, req-num :: <integer>)
  with-harp (be)
    nreg req-reg;
    ins--move(be, req-reg, req-num * 4);
    op--specializer-checks-via-loop(be, err-tag, req-reg);
  end with-harp;
end method;


define method op--specializer-checks-via-loop
    (be :: <harp-back-end>, err-tag :: <tag>, req-num :: <register>)
  with-harp (be)
    greg value, specs;
    nreg arg-num;
    tmp 1, type;
    function function;
    result result;
    tag loop-start, loop-finished;
      
    ins--move(be, arg-num, req-num);
    ins--ld(be, specs, function, be.function-signature-offset);
    ins--ld(be, specs, specs, be.signature-required-offset);
    op--preserve-args-for-specializer-check(be, 0, 0, specs, spare1: arg-num, frame?: #f);
    with-preserved-arguments (function, mlist)

      let arg-regs = be.registers.reg-machine-arguments;
      let reg-limit = arg-regs.size * 4;

      // First test all those args which were passed in registers
      for (reg in arg-regs,
	   spec-index :: <integer> from 8 by 4,
	   i :: <integer> from 0)
        with-harp (be)
          tag reg-done;
          ins--ld(be, type, specs, spec-index);
          // Don't check if the type is <object>
          ins--beq(be, reg-done, type, object-class-class);
          // Otherwise check by calling into Dylan 
	  unless (i == 0)
	    // load arg register from preserved set
	    op--load-specializer-saved-arg-register(be, i);
	  end;
          op--store-args-for-specializer-error-call(be, reg, type);
          op--primitive-instance?-setting-result(be, reg, type);
          op--load-specializer-saved-registers(be, specs: specs);
          ins--beq(be, err-tag, result, dylan-false);
          ins--tag(be, reg-done);
	  unless (reg-limit == 4)
	    // argument registers may not be full of requireds otherwise
            ins--beq(be, loop-finished, (i + 1) * 4, req-num);
	  end;
        end with-harp;
      end for;

      // Next loop over all the args passed on the stack, in reverse order
      op--load-specializer-saved-registers(be, spare1: arg-num);
      // decrement the argnum before the first test 'cos arg indexing is from 0
      ins--tag(be, loop-start);
      ins--ble(be, loop-finished, arg-num, reg-limit);
      ins--sub(be, arg-num, arg-num, 4);
      op--load-index(be, type, specs, arg-num, 8);
      // Don't check if the type is <object>
      ins--beq(be, loop-start, type, object-class-class);
      // Otherwise check by calling into Dylan
      op--load-argument-n-via-frame(be, value, arg-num);
      op--store-args-for-specializer-error-call(be, value, type);
      op--store-specializer-saved-registers(be, spare1: arg-num);
      op--primitive-instance?-setting-result(be, value, type);
      op--load-specializer-saved-registers(be, specs: specs, spare1: arg-num);
      ins--beq(be, err-tag, result, dylan-false);
      ins--bra(be, loop-start);
      ins--tag(be, loop-finished);

    end;
    op--restore-args-for-specializer-check(be, specs);
  end with-harp;
end method;



// OP--SPECIALIZER-CHECKS-IN-LINE
// Checks all arguments against the expected specializer types, using an
// in-line test for each argument. 
//
// required is an integer count of the number of required arguments

define method op--specializer-checks-in-line
    (be :: <harp-back-end>, err-tag :: <tag>, required :: <integer>)
  for (arg-num from 0 below required)
    with-harp (be)
      greg value, specs;
      tmp 1, type;
      function function;
      result result;
      tag done;
      
      ins--ld(be, specs, function, be.function-signature-offset);
      ins--ld(be, specs, specs, be.signature-required-offset);
      op--load-index-scaled(be, type, specs, arg-num, 8);
      // Don't check if the type is <object>
      ins--beq(be, done, type, object-class-class);
      // Otherwise check by calling into Dylan
      op--load-argument-n-leafcase(be, value, arg-num);
      op--preserve-args-for-specializer-check(be, value, type, specs, frame?: #t);
      with-preserved-arguments (function, mlist)
        op--primitive-instance?-setting-result(be, value, type);
        ins--beq(be, err-tag, result, dylan-false);
      end;
      op--restore-args-for-specializer-check(be, specs);
      ins--tag(be, done);
    end with-harp;
  end for;
end method;



//// Specializer checks out-of-line


// PRIMITIVE-CHECK-SPECIALIZERS assumes an appropriate IEP stack layout.
// It checks the required arguments against the specializers, and signals
// error for any type misses. Finally, it calls the IEP of the function.
//
// NB THIS FUNCTION ASSUMES THAT ALL THE ARGUMENT REGISTERS ARE FULL
//
// On entry:
//   arg-count: contains the number of required parameters times 4
//   function:  contains function object as normal
// On exit  
//   The function IEP is tail-called
//   MList, Function, Arg0 are preserved.

define leaf runtime-primitive check-specializers
  arg-count arg-count;
  tag err-tag;

  // Build a stack frame around the call 
  ins--preserve-registers-entry(be);
  op--specializer-checks-via-loop(be, err-tag, arg-count);
  op--tail-call-iep(be, keyword-method?: #"unknown");
  ins--tag(be, err-tag);
  op--specializer-error-call(be);
end runtime-primitive;



// OP--TAIL-CALL-CHECK-SPECIALIZERS
// generates a tail jump to the specializer check entry point.

define method op--tail-call-check-specializers
    (be :: <harp-back-end>, arg-num :: <register>)
  with-harp (be)
    arg-count argc;

    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    ins--move(be, argc, arg-num);
    ins--jmp(be, primitive-check-specializers-ref, max-num-arg-regs, 
             mlist: #t, arg-count: #t, function: #t);
  end with-harp;
end method;

define method op--tail-call-check-specializers
    (be :: <harp-back-end>, arg-num :: <integer>)
  with-harp (be)
    arg-count argc;
    ins--move(be, argc, arg-num * 4);
    op--tail-call-check-specializers(be, argc);
  end with-harp;
end method;
