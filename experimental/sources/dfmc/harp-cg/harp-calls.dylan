Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open generic stack-arguments-set-up(back-end :: <harp-back-end>, arguments :: <sequence>) => ();

define open generic register-arguments-set-up(back-end :: <harp-back-end>, arguments :: <sequence>) => (arguments :: <sequence>);

define open generic arguments-set-up(back-end :: <harp-back-end>, c :: <function-call>, arguments :: <sequence>) => (arguments :: <sequence>);


define method emit-call-prolog(back-end :: <harp-back-end>, c :: <method-call>, f) => ()

  ins--move(back-end, back-end.registers.reg-function, emit-reference(back-end, #f, c.function));
  ins--move(back-end, back-end.registers.reg-mlist, emit-reference(back-end, #f, c.next-methods));

end method emit-call-prolog;

define method emit-call-prolog(back-end :: <harp-back-end>, c :: <method-call>, f :: <&iep>) => ()

  if (^next?(function(f)))
    // We only get <method-call>'s to top level methods that don't have
    // closures, so we don't have to worry about setting the function
    // register.
    ins--move(back-end, back-end.registers.reg-mlist, emit-reference(back-end, #f, c.next-methods));
  end if;

end method emit-call-prolog;

define method emit-call-prolog(back-end :: <harp-back-end>, c :: <simple-call>, f :: <&iep>) => ()

  let env = f.environment;
  unless (~env | empty?(env.closure))
    ins--move(back-end, back-end.registers.reg-function, emit-reference(back-end, #f, c.function));
  end unless;

end method emit-call-prolog;

define method call-xep(back-end :: <harp-back-end>, c :: <simple-call>, result, function, #rest arguments) => ()
 let arguments-ct = push-arguments(back-end, arguments);
  ins--move(back-end, back-end.registers.reg-arg-count, arguments.size);
  ins--move(back-end, back-end.registers.reg-function, function);
  ins--call-indirect(back-end, back-end.registers.reg-function,
                     back-end.function-xep-offset, arguments-ct,
		     nlx-tags: *live-nlx-tags*);
  emit-result-assignment(back-end, result);
end method call-xep;

define method call-iep(back-end :: <harp-back-end>, c :: <simple-call>, f, result, func, #rest arguments) => ()
  let arguments = arguments-set-up(back-end, c, arguments);
  let arguments-ct = push-arguments(back-end, arguments);

  emit-call-prolog(back-end, c, f);

  ins--call(back-end, func, arguments-ct,
	    nlx-tags: *live-nlx-tags*);
  emit-result-assignment(back-end, result);
end method call-iep;

define method call-engine-node(back-end :: <harp-back-end>, c :: <simple-call>, result, engine, #rest arguments) => ()
  let arguments = arguments-set-up(back-end, c, arguments);
  let arguments-ct = push-arguments(back-end, arguments);
  let c-function = emit-reference(back-end, #f, c.function);

  ins--move(back-end, back-end.registers.reg-mlist, c-function);
  if (engine)
    ins--move(back-end, back-end.registers.reg-function, engine)
  else
    ins--load(back-end, back-end.registers.reg-function, c-function,
	      back-end.generic-function-engine-offset)
  end if;

  ins--call-indirect(back-end, back-end.registers.reg-function,
                     back-end.engine-node-entry-point-offset, arguments-ct,
		     nlx-tags: *live-nlx-tags*);
  emit-result-assignment(back-end, result);
end method call-engine-node;

define method call-mep
    (back-end :: <harp-back-end>, c :: <method-call>,
     f, result, function, next-methods,
     #rest the-arguments) => ()
  if (c.optional-arguments?)
    let args-size = the-arguments.size;
    ins--push
      (back-end,
       bytes%(back-end,
	      args-size
		+ 1
		+ argument-register-padding(back-end, args-size)
		- back-end.registers.arguments-passed-in-registers));
  end if;
  let arguments-ct = push-arguments(back-end, the-arguments);

  emit-call-prolog(back-end, c, f);
  ins--call-indirect(back-end, back-end.registers.reg-function,
		     back-end.function-mep-offset, arguments-ct,
		     nlx-tags: *live-nlx-tags*);
  emit-result-assignment(back-end, result);
end method call-mep;

define method call-primitive(back-end :: <harp-back-end>, result, function, #rest arguments) => ()
  let arguments-ct = push-arguments(back-end, arguments);
  let function-ref = function.runtime-reference;
  ins--call(back-end, function-ref, arguments-ct,
	    nlx-tags: *live-nlx-tags*);
  emit-result-assignment(back-end, result);
end method call-primitive;

define method call-c-primitive(back-end :: <harp-back-end>, result, function, #rest arguments) => ()
  call-foreign-function(back-end, result, function.runtime-reference, arguments, #f, #f, #"CDECL", preserve-teb?: #f);
  emit-ffi-result(back-end, result, #f, #f);
end method call-c-primitive;

define method call-apply(back-end :: <harp-back-end>, result, function, #rest arguments) => ()
  let arguments-ct = push-arguments(back-end, arguments);
  let entry-point = arguments.size - 1;
  let dynamic? = entry-point > 9;
  let apply-xep =
    if (dynamic?)
      $apply-xeps[$number-xeps]
    else
      $apply-xeps[entry-point];
    end if;

  if (dynamic?)
    ins--move(back-end, back-end.registers.reg-arg-count, arguments.size);
  end if;
  ins--move(back-end, back-end.registers.reg-function, function);
  ins--call(back-end, apply-xep.runtime-reference, arguments-ct,
	    arg-count: dynamic?,
	    nlx-tags: *live-nlx-tags*);

  emit-result-assignment(back-end, result);
end method call-apply;

define function op--mep-apply-n(back-end :: <harp-back-end>, number-req, function, next-methods, arguments) => ()

    let required-arguments = copy-sequence(arguments, end: number-req);
    let extra-optional-arguments = copy-sequence(arguments, start: number-req, end: arguments.size - 1);
    let extra-optionals-size = extra-optional-arguments.size;
    let original-optionals = arguments.last;
    let optionals = #f;
    let stack = back-end.registers.reg-stack;
    let stack-vector-size = 0;

    ins--rem(back-end, "Set up stack for mep-call");
    if (extra-optionals-size = 0)
      optionals := original-optionals;
    else
      let original-optionals-size = op--raw(back-end, #f, op--vector-size(back-end, make-register(back-end), original-optionals));
      stack-vector-size := make-n-register(back-end);
      let optionals-size = make-n-register(back-end);
      ins--add(back-end, optionals-size, original-optionals-size, extra-optionals-size);
      ins--add(back-end, stack-vector-size, optionals-size, 2);
      stack-vector-size := bytes%(back-end, stack-vector-size);
      let class-wrapper = ^class-mm-wrapper(&object-class(#[]));

      ins--rem(back-end, "stack vector");
      ins--sub(back-end, stack, stack, stack-vector-size);
      ins--store(back-end, emit-reference(back-end, #f, class-wrapper), stack, 0);
      ins--store(back-end, op--integer(back-end, #f, optionals-size), stack, bytes%(back-end, 1));
      for (argument in extra-optional-arguments, i from 2)
        ins--store(back-end, argument, stack, bytes%(back-end, i));
      end for;

      let raw-original-optionals = op--vector-as-raw(back-end, make-n-register(back-end), original-optionals);
      let start = make-n-register(back-end);

      ins--add(back-end, start, stack, bytes%(back-end, extra-optionals-size + 2));
      ins--copy-words-down-w(back-end,
			     start,
			     raw-original-optionals,
			     original-optionals-size);
      optionals := make-register(back-end);
      ins--move(back-end, optionals, stack);

    end if;

    let arguments-size = required-arguments.size + 2 - back-end.registers.arguments-passed-in-registers;
    ins--push(back-end, bytes%(back-end, arguments-size));
    let arguments-ct = push-arguments(back-end, concatenate(required-arguments, list(optionals)));

    ins--move(back-end, back-end.registers.reg-function, function);
    ins--move(back-end, back-end.registers.reg-mlist, next-methods);

    ins--call-indirect(back-end, back-end.registers.reg-function,
		       back-end.function-mep-offset, arguments-ct,
		       nlx-tags: *live-nlx-tags*);
    unless(extra-optionals-size = 0)
      ins--add(back-end, stack, stack, stack-vector-size);
    end unless;

end function op--mep-apply-n;


define method call-mep-apply(back-end :: <harp-back-end>, c, result, function, next-methods, #rest arguments) => ()

  let meth = c.call-effective-function;

  let number-required 
    = meth & spec-argument-number-required(signature-spec(meth));

  if (instance?(meth, <&method>) & size(arguments) > number-required)
    op--mep-apply-n(back-end,
		    number-required,
		    function,
		    next-methods,
		    arguments);
    emit-result-assignment(back-end, result);
  else
    apply(call-primitive,
          back-end,
          result,
          $primitive-mep-apply,
          function,
          next-methods,
          arguments);
  end if;

end method call-mep-apply;

define inline method arguments-in-registers 
    (back-end :: <harp-back-end>, arguments :: <sequence>)
 => (r :: <integer>);
  min(arguments.size, back-end.registers.arguments-passed-in-registers);
end method;

define inline method arguments-in-registers 
    (back-end :: <harp-back-end>, arguments :: <integer>)
 => (r :: <integer>);
  min(arguments, back-end.registers.arguments-passed-in-registers);
end method;

define inline method arguments-on-stack
    (back-end :: <harp-back-end>, arguments :: <sequence>)
 => (r :: <integer>);
  max(arguments.size - back-end.registers.arguments-passed-in-registers, 0);
end method;

define method c-arguments-in-registers 
    (back-end :: <harp-back-end>, arguments :: <sequence>)
 => (r :: <integer>);
  min(arguments.size,
      back-end.registers.c-arguments-passed-in-registers);
end method;

define method c-arguments-in-registers 
    (back-end :: <harp-back-end>, arguments :: <integer>)
 => (r :: <integer>);
  min(arguments,
      back-end.registers.c-arguments-passed-in-registers);
end method;

define inline method c-arguments-on-stack
    (back-end :: <harp-back-end>, arguments :: <sequence>)
 => (r :: <integer>);
  max(arguments.size - back-end.registers.c-arguments-passed-in-registers, 0);
end method;

define inline method c-arguments-on-stack
    (back-end :: <harp-back-end>, arguments :: <integer>)
 => (r :: <integer>);
  max(arguments - back-end.registers.c-arguments-passed-in-registers, 0);
end method;

define inline method argument-register-padding
    (back-end :: <harp-back-end>, arguments :: <integer>)
 => (r :: <integer>)
  max(back-end.registers.arguments-passed-in-registers - arguments,
      0);
end method;

define method push-arguments
    (back-end :: <harp-back-end>, arguments :: <sequence>)
 => (arguments-passed-in-registers :: <integer>)
  if (arguments.empty?)
    0;
  else
    stack-arguments-set-up(back-end, arguments);
    register-arguments-set-up(back-end, arguments);
    arguments-in-registers(back-end, arguments);
  end if;
end method push-arguments;

/*
define method stack-allocate-vector(back-end :: <harp-back-end>, result, number-values, arguments,
				    #key tail-call?) => ()

  if (tail-call?)
    stack-allocate-vector-for-tail-call(back-end, result, number-values, arguments);
  else
    stack-allocate-vector-for-normal-call(back-end, result, number-values, arguments);
  end if;    

end method stack-allocate-vector;
*/

define method stack-allocate-vector-for-normal-call(back-end :: <harp-back-end>, result, number-values, arguments) => ()
  let class-wrapper = ^class-mm-wrapper(&object-class(#[]));
  let stack = back-end.registers.reg-stack;
  let stack-vector-size = bytes%(back-end, number-values + 2);

  ins--rem(back-end, "stack vector");
  ins--sub(back-end, stack, stack, stack-vector-size);
  ins--store(back-end, emit-reference(back-end, #f, class-wrapper), stack, 0);
  ins--store(back-end, emit-object(back-end, #f, number-values), stack, bytes%(back-end, 1));

  for (argument in arguments, i from 2)
    ins--store(back-end, argument, stack, bytes%(back-end, i));
  end for;

  ins--move(back-end, result, stack);

  result;

end method stack-allocate-vector-for-normal-call;

define method stack-allocate-vector-for-tail-call
    (back-end :: <harp-back-end>, result, number-values, arguments)
 => ()
  let class-wrapper = ^class-mm-wrapper(&object-class(#[]));
  let stack = back-end.registers.reg-stack;
  let stack-vector-size = bytes%(back-end, number-values + 2);
  let stack-vector-start = tail-call-stack-vector-start(back-end, number-values);

  ins--rem(back-end, "stack vector for tail call");
  let arguments = concatenate(vector(emit-reference(back-end, #f, class-wrapper),
				     emit-object(back-end, #f, number-values)),
			      arguments);

  move-virtuals-to-stack-arguments(back-end, arguments, stack-vector-start);

  result;

end method stack-allocate-vector-for-tail-call;

define method tail-call-stack-vector-start
    (back-end :: <harp-back-end>, number-values)
 => (stack-vector-start :: <integer>)
  let stack-vector-end =
    back-end.cg-variables.stack-shift
    + number-of-stack-allocated-arguments
      (back-end, back-end.cg-variables.current-lambda) - 1;
  let stack-vector-start = stack-vector-end - number-values - 1;

  stack-vector-start

end method tail-call-stack-vector-start;


/// TAIL-CALL SUPPORT


define variable *tail-call-optimize?* = #t;


define method tail-call-prolog
    (back-end :: <harp-back-end>, c :: <simple-call>,
     function, next-methods, the-arguments :: <sequence>)
 => (function, next-methods, ret-addr-shft :: <integer>)
  // For a tail call, we move all potential stack args into new virtuals
  // then handle the register argument(s) and finally move the stack arg
  // virtuals back into the correct place.
  //copy-stack-vector?(back-end, c);
  let ret-addr-shft = return-address-shift(back-end, c);
  if (the-arguments.empty?)
    values(function, next-methods, ret-addr-shft);
  else
    let arguments-rest = register-arguments-set-up(back-end, the-arguments);
    let stack-arguments =
      move-stack-arguments-to-virtuals(back-end, arguments-rest, ret-addr-shft, ret-addr-shft);
    let stack-offset = ret-addr-shft + arguments-rest.size;
    let stack-vector-tmp = any?(new-stack-allocated-data?, c.arguments);
    let stack-vector = stack-vector-tmp & stack-vector-tmp.generator;
    let stack-vector-arguments =
      if (stack-vector)
	move-stack-arguments-to-virtuals(back-end,
					 emit-references(back-end, stack-vector.arguments),
					 ret-addr-shft, stack-offset + 2);
      else
	#[]
      end if;
    let stack-vector-size =
      if (stack-vector) stack-vector-arguments.size + 2
      else 0
      end if;
    let stack-offset = stack-offset + stack-vector-size;
    let function =
      function &
      safe-register(back-end, function, ret-addr-shft, stack-offset);
    let next-methods =
      next-methods &
      safe-register(back-end, next-methods, ret-addr-shft, stack-offset);

    move-virtuals-to-stack-arguments(back-end, stack-arguments, ret-addr-shft);

    // stack-allocate vector for tail-call last

    if (stack-vector)
      stack-allocate-vector-for-tail-call(back-end,
                                          #f,
                                          stack-vector-tmp.number-values,
                                          stack-vector-arguments);
    end if;

    values(function, next-methods, ret-addr-shft);
  end if;
end method tail-call-prolog;

define method tail-call-xep(back-end :: <harp-back-end>, c :: <simple-call>, result, function, #rest arguments) => ()
  let (function, next-methods, ret-addr-shft) =
    tail-call-prolog(back-end, c, function, #f, arguments);

  ins--move(back-end, back-end.registers.reg-arg-count, arguments.size);
  ins--move(back-end, back-end.registers.reg-function, function);
  ins--jmp-indirect(back-end,
		    back-end.registers.reg-function,
		    back-end.function-xep-offset,
		    arguments-in-registers(back-end, arguments),
		    return-address-shift: bytes%(back-end, ret-addr-shft),
		    optionals-marker:
                     if (same-optionals-as-caller?(back-end, c))
                       back-end.cg-variables.count-vreg
                     else
		      arg-spill-location(back-end.cg-variables.args-to-be-dropped)
                     end if);
  emit-result-assignment(back-end, result);
end method tail-call-xep;

define method tail-call-iep(back-end :: <harp-back-end>, c :: <simple-call>, f, result, func, #rest arguments) => ()
  let same-optionals? = same-optionals-as-caller?(back-end, c);
  let arguments =
    if (same-optionals?)
      arguments
    else
      arguments-set-up(back-end, c, arguments);
    end if;
  let (func, next-methods, ret-addr-shft) =
    tail-call-prolog(back-end, c, func, #f, arguments);

  emit-call-prolog(back-end, c, f);

  ins--jmp(back-end,
	   func,
	   arguments-in-registers(back-end, arguments),
	   return-address-shift: bytes%(back-end, ret-addr-shft),
	   optionals-marker:
             if (same-optionals?)
               back-end.cg-variables.count-vreg
             else
               back-end.cg-variables.args-to-be-dropped.arg-spill-location
             end if,
	   pointer-into-stack: stacked-vector-arg-spill(back-end, c, ret-addr-shft));
end method tail-call-iep;

define method tail-call-engine-node(back-end :: <harp-back-end>, c :: <simple-call>, result, engine, #rest arguments) => ()
  let same-optionals? = same-optionals-as-caller?(back-end, c);
  let arguments =
    if (same-optionals?)
      arguments
    else
      arguments-set-up(back-end, c, arguments);
    end if;
  let function = emit-reference(back-end, #f, c.function);
  let (engine, function, ret-addr-shft) =
    tail-call-prolog(back-end, c, engine, function, arguments);

  if (engine)
    ins--move(back-end, back-end.registers.reg-function, engine);
  else
    ins--load(back-end, back-end.registers.reg-function, function,
	      back-end.generic-function-engine-offset)
  end if;
  ins--move(back-end, back-end.registers.reg-mlist, function);

  ins--jmp-indirect(back-end,
		    back-end.registers.reg-function,
		    back-end.engine-node-entry-point-offset,
		    arguments-in-registers(back-end, arguments),
		    return-address-shift: bytes%(back-end, ret-addr-shft),
		    optionals-marker:
                     if (same-optionals?)
                       back-end.cg-variables.count-vreg
                     else
		      arg-spill-location(back-end.cg-variables.args-to-be-dropped)
                     end if,
		    pointer-into-stack: stacked-vector-arg-spill(back-end, c, ret-addr-shft));
end method tail-call-engine-node;

define method tail-call-mep(back-end :: <harp-back-end>, c :: <method-call>, f, result, function, next-methods, #rest arguments) => ()
  let same-optionals? = same-optionals-as-caller?(back-end, c);
  let arguments =
    if (same-optionals?)
      arguments
    else
      arguments-set-up(back-end, c, arguments);
    end if;
  let (function, next-methods, ret-addr-shft) =
    tail-call-prolog(back-end, c, function, next-methods, arguments);

  emit-call-prolog(back-end, c, f);
  ins--jmp-indirect(back-end,
		    back-end.registers.reg-function,
		    back-end.function-mep-offset,
		    arguments-in-registers(back-end, arguments),
		    return-address-shift: bytes%(back-end, ret-addr-shft),
		    optionals-marker:
                      if (same-optionals?)
                        back-end.cg-variables.count-vreg
                      else
                        back-end.cg-variables.args-to-be-dropped.arg-spill-location
                      end if,
                    pointer-into-stack:
                      stacked-vector-arg-spill(back-end, c, ret-addr-shft));
  emit-result-assignment(back-end, result);
end method tail-call-mep;

define method move-stack-arguments-to-virtuals
(back-end :: <harp-back-end>, arguments :: <sequence>,
 stack-spill0 :: <number>, stack-offset0 :: <number>) => (arguments :: <sequence>)
  for (offset from stack-offset0,
       argument in arguments,
       stack-args = #() then 
	 pair(safe-register(back-end, argument, stack-spill0, offset),
	      stack-args))
  finally stack-args.reverse;
  end for;
end method move-stack-arguments-to-virtuals;

define method move-virtuals-to-stack-arguments
    (back-end :: <harp-back-end>, stack-arguments :: <sequence>, stack-spill0 :: <number>) => ()
  for (offset from stack-spill0,
       argument in stack-arguments)
    ins--store-stack-arg-n(back-end, argument, offset);
  end for;
end method move-virtuals-to-stack-arguments;

define method safe-register
    (back-end :: <harp-back-end>, register, stack-spill0 :: <number>,
     offset :: <number>) => (safe-register)
  let colour = arg-spill-location(register);
  if (colour & offset & (stack-spill0 <= colour) & (colour < offset))
    let safe-register = make-register(back-end);
    ins--move(back-end, safe-register, register);
    safe-register;
  else
    register;
  end if;
end method safe-register;

define method return-address-shift(back-end :: <harp-back-end>, c :: <simple-call>) => (return-address-shift :: <integer>)
  let same-optionals? = same-optionals-as-caller?(back-end, c);
  let ret-addr-shft =
    max(number-of-stack-allocated-arguments(back-end, back-end.cg-variables.current-lambda)
        + back-end.cg-variables.stack-shift
        - number-of-stack-allocated-slots(back-end, c,
					  same-optionals?: same-optionals?),
        0);
  ret-addr-shft
end method return-address-shift;

define method same-optionals-as-caller?(back-end :: <harp-back-end>, c :: <simple-call>) => (same-optionals?)
  
  let sad = find-key(c.arguments,
                     method(arg)
                       instance?(arg, <lexical-rest-variable>)
                     end method);
  if (sad)
    let f = c.call-effective-function%;

    select(f by instance?)
      <&iep>, <&method>, <&generic-function> =>
        sad = spec-argument-number-required(signature-spec(function(f)));
      otherwise => #f;
    end
  end
end method same-optionals-as-caller?;

define method stacked-vector-arg-spill(back-end :: <harp-back-end>, c :: <simple-call>, ret-addr-shft :: <number>) => (arg-spill)
  let arg-spill = find-key(c.arguments, new-stack-allocated-data?);

  select(arg-spill)
   0 => back-end.registers.reg-arg0;
   #f => #f;
   otherwise => ret-addr-shft + arg-spill - back-end.registers.arguments-passed-in-registers;
  end select;
end method stacked-vector-arg-spill;

define method tail-call?(back-end :: <harp-back-end>, c :: <simple-call>)
  *tail-call-optimize?* & ~ *emitting-init-code?*
  & c.tail-position?
  & ~ stack-allocated-closures?(c)
  & ~ illegal-stack-allocated-data?(back-end, c)
  & ~ adjust-multiple-values?(back-end, c);
end method tail-call?;

define method tail-call?(back-end :: <harp-back-end>, c :: <call>)
  #f;
end method tail-call?;

define method tail-call?(back-end :: <harp-back-end>, c)
  #f;
end method tail-call?;

define method adjust-multiple-values?(back-end :: <harp-back-end>, c :: <simple-call>) => (adjust? :: <boolean>)
  let mv-req = back-end.cg-variables.required-multiple-values;

  if (mv-req)
    let c-req = multiple-values-required(c);
    c-req ~== mv-req
  end if;
end method;

define method stack-allocated-closure?(fn) => (closure? :: <boolean>)
  instance?(fn, <temporary>)
  & instance?(fn.generator, <make-closure>)
  & fn.generator.closure-has-dynamic-extent?;
end method;

define method stack-allocated-closures?(c :: <simple-call>) => (closure? :: <boolean>)
  c.function.stack-allocated-closure?
  | any?(stack-allocated-closure?,
	 c.arguments);
end method;

define function tail-call-optimizable?(back-end :: <harp-back-end>, c)
  instance?(c, <simple-call>) & member?(c, *tail-calls*)
end function;

define method stack-allocate-vector-for-tail-call?(c :: <stack-vector>)

  any?(method(c :: <computation>)
         tail-call-optimizable?(*harp-back-end*, c)
       end method,
       c.temporary.users);

end method stack-allocate-vector-for-tail-call?;

define function stack-allocated-data?(arg)
    (instance?(arg, <lexical-rest-variable>)
     | instance?(arg, <stack-vector-temporary>))
    & arg
end function;

define function new-stack-allocated-data?(arg)
    instance?(arg, <stack-vector-temporary>)
    & arg
end function;

define method illegal-stack-allocated-data?
 (back-end :: <harp-back-end>, c :: <simple-call>)
  let f = c.call-effective-function%;

  select(f by instance?)
    <&iep>, <&method>, <&generic-function> =>
      let sad = find-key(c.arguments, stack-allocated-data?);
      let sig-spec = signature-spec(function(f));

      (sad & sig-spec & (sad ~= spec-argument-number-required(sig-spec)))
      | nested-stack-allocated-data?(back-end, c);
    otherwise => #f;
  end select;
end method illegal-stack-allocated-data?;

define method nested-stack-allocated-data?
 (back-end :: <harp-back-end>, c :: <simple-call>)
 let sad = any?(new-stack-allocated-data?, c.arguments);

 sad & nested-stack-allocated-data?(back-end, sad.generator);

end method nested-stack-allocated-data?;

define method nested-stack-allocated-data?
 (back-end :: <harp-back-end>, c :: <stack-vector>)
 any?(stack-allocated-data?, c.arguments);
end method nested-stack-allocated-data?;

define method call-effective-function%(c :: <call>) => (function)
  c.call-effective-function;
end method;

define method call-effective-function%(c :: <method-call>) => (function :: type-union(<&function>, <&iep>))
  let f = c.call-effective-function;

  f | dynamic-next-method(c.function);

end method;

// This is a bit of a hack -- assumes existence of a DFM <slot-value> computation,
// and retrieves the next-method temporary from that

define method dynamic-next-method(t :: <temporary>) => (meth :: <&method>)
  let gen :: <slot-value> = t.generator;
  let instance :: <temporary> = gen.computation-instance;
  instance.environment.lambda
end method;

/// FFI SUPPORT

define constant $inside-dylan = -1;       // value of the TEB runtime state in Dylan

define constant $outside-dylan = 0;       // value of the TEB runtime state outside Dylan

define method call-foreign-function
    (back-end :: <harp-back-end>, result, function, the-arguments :: <sequence>,
     typespecs, result-typespec, calling-convention, #key preserve-teb? = #t) => ()
  let arguments-size = c-ffi-arguments-size(the-arguments, typespecs, result-typespec);

  // First push any caller-reserved-space
  op--push-space-for-callee(back-end);

  c-register-arguments-set-up(back-end, the-arguments, typespecs, result-typespec);
  c-stack-arguments-set-up(back-end, the-arguments, typespecs, result-typespec);
  if (preserve-teb?)
   let teb = make-n-register(back-end);
   ins--get-teb(back-end, teb);  // Record the TEB, & mark it to show we're out of Dylan
   ins--st(back-end, $outside-dylan, teb, back-end.teb-runtime-state-offset);
   ins--call-alien(back-end,
                   function,
                   c-arguments-in-registers(back-end, arguments-size));
   ins--set-teb(back-end, teb);  // Restore the TEB, & mark it as back inside Dylan
   ins--st(back-end, $inside-dylan, teb, back-end.teb-runtime-state-offset);
  elseif (#t)
   ins--call-alien(back-end,
                   function,
                   c-arguments-in-registers(back-end, arguments-size));
  else
   ins--jmp-alien(back-end,
                  function,
                  c-arguments-in-registers(back-end, arguments-size));
  end if;
  pop-stack(back-end,
	    c-arguments-on-stack(back-end, arguments-size),
	    calling-convention);

  // And pop the caller-reserved-space
  op--pop-space-for-callee(back-end);

end method call-foreign-function;

define method c-register-arguments-set-up
    (back-end :: <harp-back-end>, arguments :: <sequence>,
     typespecs, result-typespec) => ()
  let arguments-passed-in-registers :: <integer> =
    c-arguments-in-registers(back-end, arguments);
  for (index :: <integer>
	 from 0
	 below arguments-passed-in-registers)
    let argument = element(arguments, index);
    let type = c-ffi-argument-type(typespecs, index);
    if (instance?(type, <&raw-aggregate-type>))
      error("Passing structs by value calling convention "
	    "Not Yet Implemented for c machine registers");
    end if;
    let op--move-into-c-argument = op--move-into-c-argument%(argument);
    op--move-into-c-argument(back-end, argument, index);
  end for;
end method c-register-arguments-set-up;

define method c-stack-arguments-set-up
    (back-end :: <harp-back-end>, arguments :: <sequence>,
     typespecs, result-typespec) => ()
  let arguments-passed-in-registers :: <integer> =
    c-arguments-in-registers(back-end, arguments);
  for (index :: <integer>
	 from arguments.size - arguments-passed-in-registers - 1
	 above -1
	 by -1)
    let argument = element(arguments, index);
    let (type, storage-size) = c-ffi-argument-type(typespecs, index);

    if (instance?(type, <&raw-aggregate-type>))
      // For passing structs by value, Harp must dereference raw-pointer
      // and copy all the structure in place on the stack for C-function
      let stack = back-end.registers.reg-stack;
      ins--sub(back-end, stack, stack, bytes%(back-end, storage-size));
      ins--copy-words-down-w(back-end,
                             stack,
                             argument,
                             storage-size);
    else
      ins--push(back-end, argument);
    end if;
  end for;
  // if structure size is less than 2 words, it is returned in {eax, edx}
  if (result-typespec & (result-typespec.tail > 2))
    ins--push(back-end, result-typespec.head);
  end if;
end method c-stack-arguments-set-up;

define open generic op--push-space-for-callee
    (be :: <harp-back-end>);

define method op--push-space-for-callee
    (be :: <harp-back-end>)
end;

define open generic op--pop-space-for-callee
    (be :: <harp-back-end>);

define method op--pop-space-for-callee
    (be :: <harp-back-end>)
end;

define method c-ffi-arguments-size
    (arguments :: <sequence>, typespecs, result-typespec) => (size :: <integer>)
  let args-size =
    if (typespecs)
      reduce(method(sum, type)
		 sum + type.tail
	     end method,
	     0,
	     typespecs);
    else
      arguments.size
    end if;
  args-size +
    // if structure size is less than 2 words, it is returned in {eax, edx}
    if (result-typespec & (result-typespec.tail > 2)) 1 else 0 end;
end method c-ffi-arguments-size;

define method c-ffi-argument-type(typespecs, index) => (raw-type, storage-size)
  if (typespecs)
    let type = typespecs[index];

    values(type.head, type.tail)
  end if;
end method c-ffi-argument-type;

define method pop-stack(back-end :: <harp-back-end>, number-to-pop :: <integer>, calling-convention == #"CDECL") => ()
  unless (number-to-pop == 0)
    let stack = back-end.registers.reg-stack;
    ins--add(back-end, stack, stack, bytes%(back-end, number-to-pop));
  end;
end method pop-stack;

define method pop-stack(back-end :: <harp-back-end>, number-to-pop :: <integer>, calling-convention == #"STDCALL") => ()
 #f;
end method pop-stack;
