Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method emit-prolog(back-end :: <harp-back-end>, o :: <&iep>) => ()
  emit-ffi-pre-prolog(back-end, o.function);

  ins--rem(back-end, "function prolog");
  move-arguments-to-virtuals(back-end, o);
  environment-prolog(back-end, o);
end method emit-prolog;


define method emit-ffi-pre-prolog(back-end :: <harp-back-end>, fun) => ()
  // Do nothing by default
end method emit-ffi-pre-prolog;



define method emit-ffi-pre-prolog
    (back-end :: <harp-back-end>, fun :: <&c-callable-function>) => ()
  let callin-handler = if (fun.c-modifiers = "__stdcall")
                         $dylan-call-in
                       else $dylan-call-in-syscall
                       end if;
  let real-entry-tag = make-tag(back-end);
  let regs = back-end.registers;
  let function = regs.reg-arg0;
  let arg-count = regs.reg-arg-count;

  ins--rem(back-end, "call-in FFI entry pre-prolog");
  ins--lea0(back-end, function, real-entry-tag);
  ins--move(back-end, arg-count, arg-size-on-stack-in-words(back-end, fun));
  ins--jmp(back-end, callin-handler.runtime-reference, 1, 
           arg-count: #t);

  ins--tag(back-end, real-entry-tag);
end method emit-ffi-pre-prolog;


define method arg-size-on-stack-in-words 
    (back-end :: <harp-back-end>, function :: <&c-callable-function>) => (words :: <integer>)
  let signature = function.^function-signature;
  let total = 0;

  for (parameter-type in signature.^signature-required,
       i from 0 below signature.^signature-number-required,
       registers from back-end.registers.c-arguments-passed-in-registers by -1)
    if (registers <= 0)
       total := total + parameter-type-size(back-end, parameter-type);
    end if;
  end for;

  total;
end method;


define method environment-prolog(back-end :: <harp-back-end>, o :: <&iep>) => ()
  back-end.cg-variables.cg-temporaries[0] := #f;
  unless (empty?(o.environment.closure))
    if (closure-size(o.environment) > 0)
      let indirections = closure-indirections(back-end, o);
      ins--move(back-end,
		back-end.registers.reg-environment,
		back-end.registers.reg-function);
      op--environment-parameter(back-end, indirections: indirections);
    end if;
  end unless;
end method environment-prolog;

define method move-arguments-to-virtuals
(back-end :: <harp-back-end>, lambda :: <&iep>) => ()
 let function = lambda.function;
 let stack-shft = adjust-stack-for-lambda(back-end, lambda);
 ins--rem(back-end, "move arguments to virtuals");

 if (instance?(function, <&method>) & function.^function-next?)
   let temp = make-g-register(back-end);
   ins--move(back-end, temp, back-end.registers.reg-mlist);
   back-end.cg-variables.next-methods-vreg := temp;
 end if;
 let stack-offset = stack-shft;
 let parameters = emit-objects(back-end, lambda.parameters);
 back-end.cg-variables.current-parameters := parameters;
 let c-callable? = instance?(function, <&c-callable-function>);
 let signature = c-callable? & function.^function-signature.^signature-required;

 local method parameter-type(parameter-index)
	 if (signature)
	   signature[parameter-index]
	 end if;	 
       end method;

 let arguments-passed-in-registers :: <integer> =
    back-end.arguments-passed-in-registers%;
 for (parameter in parameters,
      registers :: <integer> from 0,
      parameter-index :: <integer> from 0)

   if (registers < arguments-passed-in-registers)
       let op--move-outof-argument = op--move-outof-argument%(parameter);
       op--move-outof-argument(back-end, parameter, registers);
   else
     if (c-callable?)
       let type = parameter-type(parameter-index);
       if (instance?(type, <&raw-aggregate-type>))
         ins--load-address-of-stack-arg-n(back-end, parameter, stack-offset);
       else
	 select (parameter by instance?)
	   <sfreg> =>
	     let temp = make-n-register(back-end);
	     ins--load-stack-arg-n(back-end, temp, stack-offset);
	     ins--move-to-sfreg(back-end, parameter, temp);
	   <dfreg> =>
	     let temp = make-n-register(back-end);
	     ins--load-address-of-stack-arg-n(back-end, temp, stack-offset);
	     ins--dld(back-end, parameter, temp, 0);
	   otherwise =>
	     ins--load-stack-arg-n(back-end, parameter, stack-offset);
	 end select;
       end if;
       stack-offset := stack-offset + parameter-type-size(back-end, type);
     else
       ins--load-stack-arg-n(back-end, parameter, stack-offset);
       stack-offset := stack-offset + 1;
     end if;
   end if;
 end for;
 back-end.cg-variables.args-to-be-dropped :=
    back-end.cg-variables.count-vreg | bytes%(back-end, stack-offset);

 initialize-temporaries(back-end, lambda);

end method move-arguments-to-virtuals;

/*
define method move-arguments-to-home
(back-end :: <harp-back-end>, lambda :: <&iep>) => ()
 unless (empty? (lambda.environment.closure))
   ins--rem(back-end, "move arguments to home");
 end unless;
end method move-arguments-to-home;
*/
      
define method initialize-temporaries
    (back-end :: <harp-back-end>, lambda :: <&iep>) => ()

end method initialize-temporaries;

define method adjust-stack-for-lambda
    (back-end :: <harp-back-end>, lambda :: <&iep>)
 => (stack-adjustment :: <integer>)
 let no-of-stack-alloc-args = number-of-stack-allocated-arguments(back-end, lambda);
 let stack-shft =
   max(maximum-tail-call-arg-ct(back-end, lambda) - no-of-stack-alloc-args, 0);
 back-end.cg-variables.stack-shift := stack-shft;
 if (lambda.optional-arguments?)
   back-end.cg-variables.count-vreg := make-g-register(back-end);
   ins--load-count-adjusting-stack(back-end, back-end.cg-variables.count-vreg,
                                   bytes%(back-end, stack-shft),
                                   stack-shft + no-of-stack-alloc-args);
 else
   back-end.cg-variables.count-vreg := #F;
   ins--adjust-stack(back-end, bytes%(back-end, stack-shft));
 end if;
 stack-shft;
end method adjust-stack-for-lambda;

// find maximum stack adjustment required in lambda prologs by
// inspecting all calls in tail-call positions in lambda tree

define method maximum-tail-call-arg-ct
(back-end :: <harp-back-end>, lambda :: <&iep>) => (count :: <integer>)
  if (*tail-call-optimize?*)
    tail-call-walk(lambda.body, #f, 0);
  else
    0;
  end if;
end method maximum-tail-call-arg-ct;

define method tail-call-walk(c :: <computation>, last, ct) => (count :: <integer>)

  case
    c == last => ct;
    ~next-computation(c) => ct;
    otherwise => tail-call-walk(next-computation(c), last, ct);
  end case;

end method tail-call-walk;

define method tail-call-walk(c, last, ct) => (count :: <integer>)
  ct;
end method tail-call-walk;

define method tail-call-walk(c :: <end>, last, ct) => (count :: <integer>)
  ct;
end method tail-call-walk;

define method tail-call-walk(c :: <exit>, last, ct) => (count :: <integer>)
  ct;
end method tail-call-walk;

define method tail-call-walk(c :: <return>, last, ct) => (count :: <integer>)
  ct;
end method tail-call-walk;

define method tail-call-walk(c :: <end-block>, last, ct) => (count :: <integer>)
  ct;
end method tail-call-walk;

define method tail-call-walk(c :: <if>, last, ct) => (count :: <integer>)
    tail-call-walk(c.next-computation, last,
		   max(ct,
		       tail-call-walk(c.alternative, c.next-computation, ct),
		       tail-call-walk(c.consequent, c.next-computation, ct)));
end method tail-call-walk;

define method tail-call-walk(c :: <unwind-protect>, last, ct) => (count :: <integer>)
    tail-call-walk(c.next-computation, last,
		   max(ct,
		       tail-call-walk(c.cleanups, c.next-computation, ct)));
end method tail-call-walk;

define method tail-call-walk(c :: <bind-exit>, last, ct) => (count :: <integer>)
    tail-call-walk(c.next-computation, last, ct);
end method tail-call-walk;

define method tail-call-walk(c :: <function-call>, last, ct) => (count :: <integer>)
  let ct =
    if (tail-call?(*harp-back-end*, c))
      *tail-calls* := pair(c, *tail-calls*);
      max(number-of-stack-allocated-slots(*harp-back-end*, c),
  	  ct);
    else
      ct;
    end if;
  case
    c == last => ct;
    ~next-computation(c) => ct;
    otherwise => tail-call-walk(next-computation(c), last, ct);
  end case;

end method tail-call-walk;

define method number-of-stack-allocated-arguments
    (back-end :: <harp-back-end>, lambda :: <&iep>)
 => (count :: <integer>)
    max(lambda.parameters.size
	- back-end.registers.arguments-passed-in-registers, 0);
end method number-of-stack-allocated-arguments;

define method number-of-stack-allocated-arguments
    (back-end :: <harp-back-end>, call :: <function-call>)
 => (count :: <integer>)
  let args-size = call.arguments.size;
  let total-args =
    if (call.optional-arguments?)
      args-size + stack-vector-size(back-end, call)
	+ argument-register-padding(back-end, args-size);
    else
      args-size
    end if;

  max(total-args - back-end.registers.arguments-passed-in-registers, 0);
end method number-of-stack-allocated-arguments;

define method number-of-stack-allocated-slots
    (back-end :: <harp-back-end>, object,
     #key same-optionals?) => (count :: <integer>)
  let ct = number-of-stack-allocated-arguments(back-end, object);
  if (same-optionals?)
    // do not include allocation for count
    ct
  else
    let optionals? = object.optional-arguments?;
    if (optionals?)
      // for xep jumps do not include allocation for the count
      select(object by instance?)
	<method-call>, <engine-node-call> => 1 + ct;
	<simple-call> =>
	  select(optionals? by instance?)
	    <&iep> => 1 + ct;
	    <&generic-function> =>
	      if (call-congruent?(object)) 1 + ct;
	      else ct end;
	    otherwise => ct;
	  end select;
	otherwise => 1 + ct;
      end select;
    else
      ct
    end if;
  end if;

end method number-of-stack-allocated-slots;

define method stack-vector-size(back-end :: <harp-back-end>, c :: <simple-call>) => (count :: <integer>)
 let stack-vector? =
   any?(new-stack-allocated-data?,
        c.arguments);

 (stack-vector?
    & stack-vector?.generator.arguments.size + 2)
 | 0;
end method stack-vector-size;

define method optional-arguments?(lambda) => (optionals? :: <boolean>)
  #f
end method optional-arguments?;

define inline method optional-arguments?(lambda :: <&iep>) => (optionals? :: <boolean>)
  spec-argument-optionals?(signature-spec(function(lambda)))
end method optional-arguments?;

define inline method optional-arguments?(lambda :: <&generic-function>) => (optionals? :: <boolean>)
  spec-argument-optionals?(signature-spec(lambda))
end method optional-arguments?;

define method optional-arguments?(lambda :: <&method>) => (optionals? :: <boolean>)
  spec-argument-optionals?(signature-spec(lambda))
end method optional-arguments?;

define method optional-arguments?(call :: <call>) => (optionals?)
  let f = call.call-effective-function%;

  f.optional-arguments? & f

end method optional-arguments?;

define method reg-machine-arguments%(back-end :: <harp-back-end>) => (machine-arguments :: <simple-object-vector>)
 select(back-end.cg-variables.current-lambda.function by instance?)
   <&c-callable-function> => back-end.registers.reg-c-machine-arguments;
   <&lambda> => back-end.registers.reg-machine-arguments;
 end select;
end method reg-machine-arguments%;

define method reg-float-machine-arguments%(back-end :: <harp-back-end>) => (machine-arguments :: <simple-object-vector>)
 select(back-end.cg-variables.current-lambda.function by instance?)
   <&c-callable-function> => back-end.registers.reg-c-float-machine-arguments;
   <&lambda> => back-end.registers.reg-float-machine-arguments;
 end select;
end method reg-float-machine-arguments%;

define method arguments-passed-in-registers%(back-end :: <harp-back-end>) => (arguments-passed-in-registers :: <integer>)
 select(back-end.cg-variables.current-lambda.function by instance?)
   <&c-callable-function> => back-end.registers.c-arguments-passed-in-registers;
   <&lambda> => back-end.registers.arguments-passed-in-registers;
 end select;
end method arguments-passed-in-registers%;


define constant *value-cell-value-offset* = 4;

define method make-closure-indirection 
    (back-end :: <harp-back-end>, name :: <byte-string>, 
     offset :: <integer>, value-cell? :: <boolean>)
 => (indirection)
  if (value-cell?)
    let sub = make-indirection-variable(back-end, *value-cell-value-offset*,
                                        name: name);
    make-indirection-variable(back-end, offset, sub-indirections: vector(sub));
  else
    make-indirection-variable(back-end, offset, name: name);
  end if;
end method;

define method closure-indirections 
    (back-end :: <harp-back-end>, o :: <&iep>)
 => (indirections :: <simple-object-vector>)
  let env = o.environment;
  let closure = env.closure;
  let len = closure-size(env);
  let inds = make(<vector>, size: len);
  let *names* = make(<vector>, size: len, fill: #f);
  local method unique-name(name :: <byte-string>, i :: <integer>)
         => (unique-name :: <byte-string>)
	  let ambiguous? = member?(name, *names*, test: \=);
	  let unique-name =
	    if (ambiguous?) format-to-string("%s%d", name, i)
	    else name end;
	  *names*[i] := unique-name;
	end method;
  let environment-offset = closure-environment-offset%(back-end);
  for (var in closure,
       i from 0 below len)
    // if (~ var.name) format-out("### closure-indirections: anonymous %= %=\n", var, o) end;
    let name =
      concatenate("closed_",
		  unique-name(if (var.name) local-mangle(back-end, var.name)
			      else "anonymous" end, i));
    let value-cell? = var.cell?;
    let offset = environment-offset + bytes%(back-end, i);
    inds[i] := make-closure-indirection(back-end, name, offset, value-cell?);
  end for;
  inds;
end method;

define method op--c-load-arguments
    (be :: <harp-back-end>, #rest regs) => ()
  let args-in-regs =
    min(regs.size,
	be.registers.c-arguments-passed-in-registers);
  for (i :: <integer> from 0 below args-in-regs)
    ins--move(be, regs[i], be.registers.reg-c-machine-arguments[i]);
  end;
  for (i :: <integer> from args-in-regs below regs.size)
    ins--load-stack-arg-n(be, regs[i], i - args-in-regs);
  end for;
end method;
