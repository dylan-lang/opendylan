Module: dfmc-harp-cg
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method emit-lambda (back-end :: <harp-back-end>, stream, o :: <&lambda>, #rest flags, #key, #all-keys) => (compiled-lambdas :: <list>)
  #()
end method;

define method emit-lambda(back-end :: <harp-back-end>, stream, o :: <&iep>,
			  #rest flags, #key harp-output? = unsupplied(), #all-keys) => (compiled-lambdas :: <list>)
  let fun = o.function;
  let c-callable? = instance?(fun, <&c-callable-function>);
  let lambda-name =
    if (c-callable?)
      emit-name(back-end, stream, o)
    else o
    end if;
  let (static?, export?) =
    select(fun by instance?)
      <&c-callable-function> => 
	let export? = if (fun.dll-export?) #"code-stub" else #f end;
	values(~ fun.binding-name, export?);
      <&lambda> =>
	let public? = fun.model-has-definition?;
	values(~ public?,
	       if (public?)
		 if (fun.lambda-runtime-function?) #f
		 else
		   model-externally-visible?(o)
		 end
	       end);
    end select;
  let source-locator = find-a-source-location(o);
  format-out?("\n COMPILING LAMBDA %=...\n", o);
  let compiled-lambda =
    with-harp-emitter(back-end,
		      stream,
		      lambda-name,
		      call-in: c-callable?,
		      export: export?,
                      static: static?,
		      source-locator: source-locator,
		      harp-debug: harp-output?)
          back-end.cg-variables.current-lambda := o;
          back-end.cg-variables.current-scl := source-locator;
          back-end.cg-variables.exit-tag := make-tag(back-end);
          back-end.cg-variables.required-multiple-values := multiple-values-required(o);
          emit-prolog(back-end, o);

          with-labeling-from-dynamic
            emit-lambda-body(back-end, stream, o);
          end with-labeling-from-dynamic;

          emit-epilog(back-end, o);
    end with-harp-emitter;

  o.code := list(compiled-lambda);
end method emit-lambda;

define method emit-lambda-body
    (back-end :: <harp-back-end>, stream, o :: <&iep>) => ()
  allocate-registers(o.function);
  dynamic-bind (*current-environment* = o.environment)

    emit-computations(back-end, o.body, #f);

  end dynamic-bind;

end method emit-lambda-body;

/// COMPUTATIONS

define method emit-label
    (back-end :: <harp-back-end>, c :: <computation>) => ()
  maybe-label!(c);
  ins--tag(back-end, op--tag(back-end, c.label));
end method emit-label;

define method emit-goto
    (back-end :: <harp-back-end>, c :: <computation>) => ()
  maybe-label!(c);
  ins--bra(back-end, op--tag(back-end, c.label));
end method emit-goto;

define variable *trace-harp?* = #f;

define function trace-harp?(back-end :: <harp-back-end>, c) => ()

  format-out("\n Harp Output at %=\n", c);
  print-instructions(back-end, linearise?: #f);

end function;

define method emit-computations
    (back-end :: <harp-back-end>, c :: <computation>, last) => ()
  iterate loop (c = c)
    if (c & c ~== last)
      *trace-harp?* & trace-harp?(back-end, c);
      op--scl(back-end, c, #f);
      emit-computation(back-end, c);
      loop(next-computation(c));
    end if;
  end iterate;
end method emit-computations;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <computation>) => ()
end method;

/// SOURCE CODE LOCATIONS

define variable *debug-level* = 2;

define method op--scl
    (back-end :: <harp-back-end>,
     c :: false-or(<computation>),
     scl) => ()

    unless (*debug-level* = 0)
      let scl = scl | c.computation-source-location;

      if (valid-source-code-locator?(back-end, scl))
	if (*debug-level* = 1)
	  ins--scl(back-end, scl, #[]);
	else
	  ins--scl(back-end, scl, back-end.cg-variables.current-parameters);
	end if;
      end if;
    end unless;

end method op--scl;

define function valid-source-code-locator?(back-end :: <harp-back-end>, scl)

  let cg-variables = back-end.cg-variables;
  let current-scl = cg-variables.current-scl;
  let current-lambda :: <&iep> = cg-variables.current-lambda;
  let valid-scl? =
    scl
    & current-scl
    & (current-scl.source-location-record == scl.source-location-record);

  if (valid-scl? & (~ current-lambda.function.model-has-definition?))
    if (dummy-harp-source-locator?(current-scl))
      current-scl.source-location-start-offset := scl.source-location-start-offset;
      current-scl.source-location-end-offset := scl.source-location-end-offset;
    else
      if (range-source-offset-greater-than?(current-scl.source-location-start-offset, scl.source-location-start-offset))
	current-scl.source-location-start-offset := scl.source-location-start-offset;
      end if;
      if (range-source-offset-greater-than?(scl.source-location-end-offset, current-scl.source-location-end-offset))
	current-scl.source-location-end-offset := scl.source-location-end-offset;
      end if;
    end if;
  end if;

  valid-scl?;

end function;

define constant $dummy-start-offset :: <integer> = 0;

define inline function make-dummy-source-locator(cr)
  make(<compiler-range-source-location>,
       source-record: cr,
       start-offset: $dummy-start-offset,
       end-offset: $dummy-start-offset);
end function;

define sideways method dummy-harp-source-locator?(locator :: <compiler-range-source-location>)
 => (dummy? :: <boolean>)
  (locator.source-location-start-offset = $dummy-start-offset)
  & (locator.source-location-end-offset = $dummy-start-offset)
end method;

define function find-a-source-location(o :: <&iep>)

  if (o.function.model-has-definition?)
    o.function.model-source-location
  else
    make-dummy-source-locator(model-compilation-record(o));
  end if;

end function;

/// EMIT


define function emit-objects(back-end :: <harp-back-end>, objects :: <sequence>)
 => (objects  :: <simple-object-vector>)

  let new-objects :: <simple-object-vector> = make(<vector>, size: objects.size);

  for (object in objects,
       index :: <integer> from 0)
    new-objects[index] := emit-object(back-end, #f, object);
  end for;

  new-objects;

end function;

define function emit-references
    (back-end :: <harp-back-end>, objects :: <sequence>) => (references  :: <sequence>)

  let references = make(<vector>, size: objects.size);

  for (object in objects,
       index :: <integer> from 0)
    references[index] := emit-reference(back-end, #f, object);
  end for;

  references;

end function;

// define inline function emit-references-on-stack
//     (back-end :: <harp-back-end>, #rest objects) => (#rest references)
// 
//   for (object in objects,
//        index :: <integer> from 0)
//     objects[index] := emit-reference(back-end, #f, object);
//   end for;
// 
//   apply(values, objects);
// 
// end function;

define method closure? (o) => (closure? :: <boolean>)
  #f
end method;

define method closure? (o :: <&lambda-or-code>) => (closure? :: <boolean>)
  ~lambda-top-level?(o) & (closure-size(o.environment) ~= 0)
end method;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <make-closure>) => ()
  let o = function(c.computation-closure-method);
  let sigtmp = c.computation-signature-value;
  let key?   = instance?(o, <&keyword-method>);
  if (closure?(o))
    let init? = computation-init-closure?(c);
    let top-level? = computation-top-level-closure?(c);
    let env = o.environment;
    let closure =
      if (init? & ~top-level?)
	env.closure
      else
	list()
      end if;

    if (c.closure-has-dynamic-extent?) 
      let fn-size = if (key?) back-end.keyword-closure-size
		    else back-end.simple-closure-size
		    end if;
      let fn-size-in-words = truncate/(fn-size, 4);
      let closure-size = closure-size(env);
      let closure-size-in-bytes = bytes%(back-end, closure-size);
      let total-size = fn-size + closure-size-in-bytes;
      let fn = emit-reference(back-end, #f, c.temporary) | make-g-register(back-end);
      let template = emit-reference(back-end, #f, o);
      let env = make-g-register(back-end);
      let stack = back-end.registers.reg-stack;

      ins--sub(back-end, stack, stack, total-size);
      ins--move(back-end, fn, stack);
      ins--copy-words-down-w(back-end, fn, template, fn-size-in-words);
      let env-offset = 
	if (key?) back-end.keyword-closure-environment-offset
	else back-end.closure-environment-offset
	end if;
      ins--store(back-end,
		 op--integer(back-end, #f, closure-size),
		 fn, env-offset);
      let word-size = bytes%(back-end, 1);
      for (reference in emit-references(back-end, closure),
	   offset :: <integer> from env-offset + word-size by word-size)
	ins--store(back-end, reference, fn, offset)
      end for;

      if (sigtmp)
	ins--st(back-end, emit-reference(back-end, #f, sigtmp), fn, back-end.function-signature-offset);
      end if;
    else
      if (sigtmp)
	if (top-level?)
	  let o-ref = emit-reference(back-end, #f, o);
	  ins--st(back-end,
		  emit-reference(back-end, #f, sigtmp),
		  o-ref,
		  back-end.function-signature-offset);
	  ins--move(back-end,
		    emit-reference(back-end, #f, c.temporary),
		    o-ref);
	else
	  call-protocol
	    (call-primitive, back-end)
	    (reference, identity, reference, reference, identity)
	    (c.temporary,
	     if (init?)
	       if (key?)
		 $primitive-make-keyword-closure-with-environment-signature
	       else
		 $primitive-make-closure-with-environment-signature
	       end if
	     else
	       if (key?)
		 $primitive-make-keyword-closure-with-signature
	       else
		 $primitive-make-closure-with-signature
	       end if
	     end if,
	     o,
	     sigtmp,
	     closure-size(env),
	     rest closure)
	   end;
	end if;
      else
        call-protocol
	  (call-primitive, back-end)
	  (reference, identity, reference, identity)
	  (c.temporary,
	   if (init?)
	     if (key?)
	       $primitive-make-keyword-closure-with-environment
	     else
	       $primitive-make-closure-with-environment
	     end if
	   else
	     if (key?)
	       $primitive-make-keyword-closure
	     else
	       $primitive-make-closure
	     end if
	   end if,
	   o,
	   closure-size(env),
	   rest closure)
         end;
      end if;
    end if;
  else
    if (sigtmp)
      call-protocol
	(call-primitive, back-end)
	(reference, identity, reference, reference)
	(c.temporary,
	 if (key?)
	   $primitive-make-keyword-method-with-signature
	 else
	   $primitive-make-method-with-signature
	 end if,
	 o,
	 sigtmp)
       end;
    else
      emit-assignment(back-end,
		      c.temporary,
		      emit-reference(back-end, #f, o));
    end if
  end if;
end method;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <initialize-closure>) => ()
  let o = function(c.computation-closure-method);
  if (closure?(o))
    let env = o.environment;

    call-protocol
      (call-primitive, back-end)
      (identity, identity, reference, identity)
      (#f,
       if (instance?(o, <&keyword-method>)) $primitive-initialize-keyword-closure
       else $primitive-initialize-closure
       end if,
       c.computation-closure,
       closure-size(env),
       rest env.closure)
     end;

  end if;
end method;


/// LOOP MERGE


define method emit-computation
    (back-end :: <harp-back-end>, c :: <loop-merge>) => ()
    let tmp = c.temporary;
    if (loop-merge-initial?(c))
      if (tmp)
        let lhs-ref = emit-reference(back-end, #f, tmp);
        let op--move = op--move%(lhs-ref);
	op--move(back-end,
                 lhs-ref,
                 loop-temporary(back-end, tmp));
      end if;
    else
      emit-assignment(back-end,
		      tmp,
		      emit-reference(back-end, #f, loop-merge-parameter(c)));
    end if;
end method;


/// MULTIPLE VALUES

define class <cg-multiple-values>(<object>)
  constant slot mv-register :: false-or(<register>), required-init-keyword: register:;
  constant slot mv-elements :: <vector> = #[], init-keyword: elements:;
end class;

define method make-multiple-values(back-end :: <harp-back-end>)
  make(<cg-multiple-values>, register: make-register(back-end));
end method;

define method emit-multiple-value
    (back-end :: <harp-back-end>, dest, mv-ref :: <cg-multiple-values>, index :: <integer>)
 => (mv-element :: <register>)
  let mvalues = mv-ref.mv-elements;
  let number-values = mvalues.size;
  case
    (index = 0) => mv-ref.mv-register;
    (index > number-values - 1) =>
      let dest = dest | make-register(back-end);
      ins--load(back-end, dest,
		multiple-values-area(back-end),
		bytes%(back-end, index));
      dest;
    otherwise =>
      let mv-elt = mvalues[index];
      case
	mv-elt => mv-elt;
	dest => 
	  ins--load(back-end, dest,
		    multiple-values-area(back-end),
		    bytes%(back-end, index));
	  dest;
	otherwise =>
	  mvalues[index] := make-register(back-end);
      end case;
  end case;
end method;

define method emit-mv-reference
    (back-end :: <harp-back-end>, stream, o :: <multiple-value-temporary>) => (mval :: false-or(<cg-multiple-values>))
  if (o.used?)
    emit-reference(back-end, stream, o);
    mv-temporary(back-end, o);
  end if;
end method;

define method op--move-mv
    (back-end :: <harp-back-end>,
     destination,
     source,
     index :: <integer>) => ()
  let destination = destination & emit-multiple-value(back-end, #f, destination, index);
  let source = emit-multiple-value(back-end, destination, source, index);
  unless(source == destination)
    ins--move(back-end, destination, source);
  end unless;
end method;

define method multiple-values-area(back-end :: <harp-back-end>) => (mv-area :: <register>)
  back-end.cg-variables.cg-multiple-values :=
    back-end.cg-variables.cg-multiple-values
    | op--ld-mv-area-address(back-end);
end method;

define method clear-multiple-values-area(back-end :: <harp-back-end>) => ()
  back-end.cg-variables.cg-multiple-values := #f;
end method;

define method local-multiple-value-temporary?(temp) => (multiple-values? :: <boolean>)
  instance?(temp, <multiple-value-temporary>)
  & (required-values(temp) > 0)
end method;

define method emit-computation (back-end :: <harp-back-end>, c :: <values>) => ()
  if (c.temporary & c.temporary.used?)
    let fixed-values = c.fixed-values;
    let number-fixed-values = size(fixed-values);
    let number-local-values = required-values(c.temporary);
    let mv-ref = emit-mv-reference(back-end, #f, c.temporary);

    clear-multiple-values-area(back-end);

    ins--move(back-end,
	      mv-ref.mv-register,
	      emit-reference(back-end,
			     #f,
			     if (number-fixed-values > 0)
			       fixed-values[0]
			     else
			       #f
			     end if));

    for (i :: <integer> from 0 below number-fixed-values)
      let t = fixed-values[i];
      if (i < number-local-values)
	unless (i = 0)
	  ins--move(back-end,
		    emit-multiple-value(back-end, #f, mv-ref, i),
		    emit-reference(back-end, #f, t));
	end unless;
      else
  	ins--store(back-end,
		   emit-reference(back-end, #f, t),
		   multiple-values-area(back-end),
		   bytes%(back-end, i));
      end if;
    end for;

    for (i :: <integer> from number-fixed-values below number-local-values)
      ins--move(back-end,
		emit-multiple-value(back-end, #f, mv-ref, i),
		$false);
    end for;

    let rest-temp = c.rest-value;
    if (rest-temp)
      // If a dynamic values #rest, must set up as many locals as possible
      // element-wise from the vector
      let empty-vector? =
	instance?(rest-temp, <object-reference>)
	& (rest-temp.reference-value = #[]);
      unless (empty-vector?)

      let rest-ref = rest-temp & emit-reference(back-end, #f, rest-temp);
      let rest-size = make-n-register(back-end);
      let raw-rest-ref = make-n-register(back-end);
      let continue-tag = make-tag(back-end);
      op--raw(back-end, rest-size, op--vector-size(back-end, rest-size, rest-ref));
      op--vector-as-raw(back-end, raw-rest-ref, rest-ref);
      for (i :: <integer> from number-fixed-values below number-local-values)
	let rest-i = i - number-fixed-values;
	ins--beq(back-end, continue-tag, rest-size, rest-i);
	ins--load(back-end,
		  emit-multiple-value(back-end, #f, mv-ref, i),
		  raw-rest-ref,
		  bytes%(back-end, rest-i));
      end for;
      ins--tag(back-end, continue-tag);

      end unless;
    end if;

    let lhs-imprecise? = rest-values?(c.temporary);
    let rhs-imprecise? = rest-temp;

    if (lhs-imprecise?)
      if (rhs-imprecise?)
	op--mv-set-rest-at(back-end,
			   mv-ref.mv-register,
			   emit-reference(back-end, #f, c.rest-value),
			   number-fixed-values);
      else
	// if we are in an imprecise context, must set the count
	op--store-multiple-values-count(back-end, number-fixed-values);
      end if;
    end if;
  end if;
end method emit-computation;

define method emit-computation
(back-end :: <harp-back-end>, c :: <extract-single-value>) => ()
  if (c.index == 0)
    ins--move(back-end,
	      emit-reference(back-end, #f, c.temporary),
	      emit-reference(back-end, #f, c.computation-value));
  else
    let values-generator = c.computation-value.generator;
    unless(instance?(values-generator, <primitive-call>))
      ins--move(back-end,
		emit-reference(back-end, #f, c.temporary),
		emit-multiple-value(back-end, #f, 
				    emit-mv-reference(back-end, #f, c.computation-value),
				    c.index));
    end unless;
  end if;
end method emit-computation;

define method emit-computation
(back-end :: <harp-back-end>, c :: <extract-rest-value>) => ()

  let comp = c.computation-value;
  if (instance?(comp, <multiple-value-temporary>) & ~ comp.rest-values?)
    // if precise number of multiple values, handle this locally
    let number-values = required-values(comp);
    let mv-ref = emit-mv-reference(back-end, #f, comp);
    let start-index = c.index;
    let vector-size = max(number-values - start-index, 0);
    let mvalues = make(<vector>, size: vector-size);

    for (i :: <integer> from start-index below number-values)
      mvalues[i - start-index] := emit-multiple-value(back-end, #f, mv-ref, i);
    end for;

    apply(call-primitive,
          back-end,
          emit-reference(back-end, #f, c.temporary),
          $primitive-vector,
          vector-size,
          mvalues);
  else
    op--mv-get-rest-at(back-end,
		       emit-reference(back-end, #f, c.temporary),
		       emit-reference(back-end, #f, comp),
		       c.index);
  end if;

end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <multiple-value-spill>) => ()
  let comp = c.computation-value;
  if (instance?(comp, <multiple-value-temporary>) & comp.rest-values?)
    // "unspill" multiple values from global area
    op--mv-get-rest-at(back-end,
                       emit-reference(back-end, #f, c.temporary),
                       emit-reference(back-end, #f, comp),
                       0);
  end if;

end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <multiple-value-unspill>) => ()

  let temp = c.temporary;
  let comp = c.computation-value;
  let previous-comp =
    if (instance?(comp.generator, <multiple-value-spill>))
      comp.generator.computation-value;
    else
      error("emit-computation <multiple-value-spill>");
    end if;

  if (instance?(previous-comp, <multiple-value-temporary>) & previous-comp.rest-values?)
    // "spill" multiple values into global area
    op--mv-set-rest-at(back-end,
                       emit-reference(back-end, #f, temp),
                       emit-reference(back-end, #f, comp),
                       0);
  end if;
  emit-transfer(back-end, temp, previous-comp);
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <adjust-multiple-values>) => ()
  next-method();
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <adjust-multiple-values-rest>) => ()
  next-method();
end method emit-computation;

define method emit-computation (back-end :: <harp-back-end>, c :: <return>) => ()
  let code-generate? =
    if (*emitting-init-code?*) *interactive-mode?*
    else #t;
    end if;

  if (code-generate?)
    if (instance?(c.computation-value, <multiple-value-temporary>))
      // spill local required values into global MV area
      spill-multiple-values(back-end, c.computation-value);
    end if;
    let result = emit-reference(back-end, #f, c.computation-value);
    let op--move-into-result = op--move-into-result%(result);
    op--move-into-result(back-end, result);
  end if;

  ins--bra(back-end, back-end.cg-variables.exit-tag);
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <primitive-call>) => ()

  emit-call(back-end,
	    emit-destination-reference(back-end, #f, c.temporary),
	    c,
	    c.primitive);
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <assignment>) => ()
  let the-value = emit-reference(back-end, #f, c.computation-value);

  if (c.assigned-binding.binding-thread?)
    named-emitter(#"primitive-write-thread-variable")
      (back-end,
       emit-reference(back-end, #f, c.temporary),
       emit-reference(back-end, #f, c.assigned-binding),
       the-value);
  else
    emit-assignment(back-end, c.assigned-binding, the-value);
    emit-assignment(back-end, c.temporary, the-value);
  end if;
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <definition>) => ()
  let the-value = emit-reference(back-end, #f, c.computation-value);

  if (c.assigned-binding.binding-thread?)
    named-emitter(#"primitive-allocate-thread-variable")
      (back-end,
       emit-reference(back-end, #f, c.assigned-binding),
       the-value);
    emit-assignment(back-end, c.temporary, the-value);
  else
    emit-assignment(back-end, c.assigned-binding, the-value);
    emit-assignment(back-end, c.temporary, the-value);
  end if;
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <type-definition>) => ()
  let binding = c.typed-binding;
  unless (constant?(binding))
    let import? = binding-imported-into-library?(binding);
    let name = emit-module-name(back-end, #f, binding);
    let the-value = emit-reference(back-end, #f, c.computation-value);
    let temp = emit-destination-reference(back-end, #f, c.temporary);
    ins--move(back-end,
	      ins--indirect-constant-ref(back-end,
					 concatenate(name, $runtime-module-binding-type-marker),
					 import?: import?),
	      the-value);
    if (temp)
      ins--move(back-end, temp, the-value);
    end if;
  end unless;
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <conditional-update!>) => ()
  if (c.assigned-binding.binding-locked?)
    let fail-tag = make-tag(back-end);
    let done-tag = make-tag(back-end);
    let result = emit-reference(back-end, #f, c.temporary);

    ins--conditional-move(back-end,
			  fail-tag,
			  emit-reference-to-locked-lval(back-end, c.assigned-binding),
			  emit-reference(back-end, #f, c.computation-value),
			  emit-reference(back-end, #f, c.computation-test-value));
    ins--move(back-end, result, $true);
    ins--bra(back-end, done-tag);
    ins--tag(back-end, fail-tag);
    ins--move(back-end, result, $false);
    ins--tag(back-end, done-tag);
  else
    error("<conditional-update!>");
  end if;
end method;


define method emit-reference-to-locked-lval
(back-end :: <harp-back-end>, o :: <module-binding>) => (reference :: <object>)
  let cref = emit-module-reference(back-end, #f, o);

  if (emit-import-adjustment?(back-end) & imported-object?(back-end, o))
    let new-register = make-g-register(back-end);
    ins--move(back-end, new-register, cref); 
    new-register;  // New register contains address of locked variable
  else
    cref           // constant ref actually describes the data but HARP is
                   // designed to handle this to avoid allocating an address ref
  end if;
end method;


// Support for Interactive


define method emit-computation
    (back-end :: <harp-back-end>, c :: <redefinition>) => ()
  let the-value = emit-reference(back-end, #f, c.computation-value);

  if (c.assigned-binding.binding-thread?)
    named-emitter(#"primitive-allocate-thread-variable")
      (back-end,
       emit-reference(back-end, #f, c.assigned-binding),
       the-value);
    emit-assignment(back-end, c.temporary, the-value);
  else
    emit-assignment(back-end, c.assigned-binding, the-value);
    emit-assignment(back-end, c.temporary, the-value);
  end if;
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <type-redefinition>) => ()
end method emit-computation;


define method emit-computation
    (back-end :: <harp-back-end>, c :: <function-call>) => ()
  let effective-function = call-effective-function(c);

  emit-call(back-end,
	    emit-destination-reference(back-end, #f, c.temporary),
	    c,
	    effective-function);
  // transfer any required mv-temp values out of global MV area
  if (instance?(c.temporary, <multiple-value-temporary>))
    unspill-multiple-values(back-end, c.temporary, c);
  end if;  
end method emit-computation;

define function maybe-emit-merge-transfer
    (back-end :: <harp-back-end>, merge :: <computation>, refn :: <function>)
  if (instance?(merge, <merge>))
    let ref         = refn(merge);
    let merge-tmp   = temporary(merge);
    let merge-used? = merge-tmp & used?(merge-tmp);
    if (merge-used? & ref)
      emit-transfer(back-end, merge-tmp, ref);
    end if;
  end if;
end function;

define method emit-transfer
    (back-end :: <harp-back-end>, lhs, rhs) => ()
  let lhs-ref = emit-reference(back-end, #f, lhs);
  let rhs-ref = emit-reference(back-end, #f, rhs);
  let op--move = op--move%(lhs-ref);
  op--move(back-end, lhs-ref, rhs-ref);
end method;

define method emit-transfer
    (back-end :: <harp-back-end>,
     lhs :: <multiple-value-temporary>, rhs :: <multiple-value-temporary>) => ()
  let lhs-req = lhs.required-values;
  let rhs-req = rhs.required-values;
  let min-required = min(lhs-req, rhs-req);
  let max-required = max(lhs-req, rhs-req);
  let lhs-reg = emit-mv-reference(back-end, #f, lhs);
  let rhs-reg = emit-mv-reference(back-end, #f, rhs);
  let lhs-imprecise? = lhs.rest-values?;
  let rhs-imprecise? = rhs.rest-values?;

  clear-multiple-values-area(back-end);

  for (i :: <integer> from 0 below min-required)
    op--move-mv(back-end, lhs-reg, rhs-reg, i);
  end for;

  case
    lhs-req < rhs-req =>
      if (lhs-imprecise?)
	for (i :: <integer> from lhs-req below rhs-req)
	  if (i == 0)
	    op--move-mv(back-end, lhs-reg, rhs-reg, i);
	  else
	    ins--store(back-end,
		       emit-multiple-value(back-end, #f, rhs-reg, i),
		       multiple-values-area(back-end),
		       bytes%(back-end, i));
	  end if;
	end for;
      end if;
    rhs-req < lhs-req =>
      if (rhs-imprecise?)
        ins--move(back-end, back-end.registers.reg-mlist, lhs-req);
        ins--call(back-end,
                  $primitive-pad-mv.runtime-reference,
                  back-end.registers.arguments-passed-in-registers);
	for (i :: <integer> from rhs-req below lhs-req)
          op--move-mv(back-end, lhs-reg, rhs-reg, i);
	end for;
      else
	for (i :: <integer> from rhs-req below lhs-req)
	  ins--move(back-end,
		    emit-multiple-value(back-end, #f, lhs-reg, i),
		    $false);
	end for;
      end if;

    otherwise =>
      // This looks weird. What about 'values()'?
      if (lhs-req = 0 & rhs-req = 0)
	op--move-mv(back-end, lhs-reg, rhs-reg, 0);
      end if;
  end case;

  if (lhs-imprecise? & ~ rhs-imprecise?)
    op--store-multiple-values-count(back-end, max-required);
  end if;

end method;

define method spill-multiple-values
    (back-end :: <harp-back-end>,
     mv :: <multiple-value-temporary>) => ()

  let number-of-values = mv.required-values;
  let mv-reg = emit-mv-reference(back-end, #f, mv);

  if (number-of-values > 1)
    clear-multiple-values-area(back-end);
    ins--store(back-end,
	       mv-reg.mv-register,
	       multiple-values-area(back-end),
	       0);
  end if;
  for (i :: <integer> from 1 below number-of-values)
    ins--store(back-end,
	       emit-multiple-value(back-end, #f, mv-reg, i),
	       multiple-values-area(back-end),
	       bytes%(back-end, i));
  end for;

  unless(mv.rest-values?)
    op--store-multiple-values-count(back-end, number-of-values);
  end unless;

end method;

define method unspill-multiple-values
    (back-end :: <harp-back-end>,
     lhs :: <multiple-value-temporary>,
     rhs) => ()

  let lhs-req = lhs.required-values;
  let lhs-ref = emit-mv-reference(back-end, #f, lhs);

  if (lhs-ref)
    clear-multiple-values-area(back-end);
    if (lhs-req > 1)
    let rhs-req = multiple-values-required(rhs);

    if (~ rhs-req | (rhs-req < lhs-req))
      ins--move(back-end, back-end.registers.reg-mlist, lhs-req);
      ins--call(back-end,
		$primitive-pad-mv.runtime-reference,
		back-end.registers.arguments-passed-in-registers);
    end if;

    for (i :: <integer> from 1 below lhs-req)
      ins--load(back-end,
		emit-multiple-value(back-end, #f, lhs-ref, i),
		multiple-values-area(back-end),
		bytes%(back-end, i));
    end for;
    end if;
  end if;

end method;

define method multiple-values-required(lambda)
end method multiple-values-required;

define method multiple-values-required(lambda :: <&iep>)
  multiple-values-required-internal(lambda)
end method multiple-values-required;

define method multiple-values-required(lambda :: <&generic-function>)
  multiple-values-required-internal(lambda)
end method multiple-values-required;

define method multiple-values-required(lambda :: <&method>)
  multiple-values-required-internal(lambda)
end method multiple-values-required;

define method multiple-values-required(call :: <call>)
  multiple-values-required(call.call-effective-function%)
end method multiple-values-required;

define method multiple-values-required-internal(lambda)
  let sig-spec = signature-spec(function(lambda));

  unless (spec-value-rest-variable-spec(sig-spec))
    spec-value-required-variable-specs(sig-spec).size
  end unless
end method multiple-values-required-internal;

define class <test-result>(<object>)
  constant slot branch :: false-or(<function>), required-init-keyword: branch:;
  constant slot operands :: <sequence>, init-keyword: operands:;
  constant slot result :: false-or(<register>), required-init-keyword: result:;
  constant slot continue-tag :: <tag>, required-init-keyword: continue:;
           slot true-tag :: <tag>;
  constant slot true-tag-internal :: false-or(<tag>) = #f, init-keyword: true-internal:;
           slot false-tag :: <tag>;
           slot false-tag-internal :: false-or(<tag>) = #f, init-keyword: false-internal:;
end class <test-result>;

define method emit-computation (back-end :: <harp-back-end>, c :: <if>) => ()
  let merge = c.next-computation;
  let the-test = emit-reference(back-end, #f, c.test);
  let test-result? = instance?(the-test, <test-result>);
  let done-tag :: <tag> = (test-result? & the-test.continue-tag) | make-tag(back-end);

  if (test-result?)
    cg-temporary(back-end, c.test) := the-test.result;
  end if;

  local method dead-branch? (branch :: <computation>, ref :: false-or(<value-reference>))
	  branch == merge & ~ref
	end method;

  case
    dead-branch?(c.consequent, merge-left-value(merge)) => // branch on true
      if (test-result?)
	ins--tag(back-end, the-test.false-tag);
      else
	ins--bne(back-end, done-tag, the-test, $false);
      end if;
      emit-computations(back-end, c.alternative, merge);
      maybe-emit-merge-transfer(back-end, merge, merge-left-value);
      if (test-result?)
	ins--tag(back-end, the-test.true-tag);
      end if;

    dead-branch?(c.alternative, merge-right-value(merge)) => // branch on false
      if (test-result?)
	ins--tag(back-end, the-test.true-tag);
      else
	ins--beq(back-end, done-tag, the-test, $false);
      end if;
      emit-computations(back-end, c.consequent, merge);
      maybe-emit-merge-transfer(back-end, merge, merge-right-value);
      if (test-result?)
	ins--tag(back-end, the-test.false-tag);
      end if;

    otherwise =>
      let false-tag = (test-result? & the-test.false-tag) | make-tag(back-end);
      if (test-result?)
	ins--tag(back-end, the-test.true-tag);
      else
	ins--beq(back-end, false-tag, the-test, $false);
      end if;
      emit-computations(back-end, c.consequent, merge);
      maybe-emit-merge-transfer(back-end, merge, merge-left-value);
      ins--bra(back-end, done-tag);

      // branch on false
      ins--tag(back-end, false-tag);
      emit-computations(back-end, c.alternative, merge);
      maybe-emit-merge-transfer(back-end, merge, merge-right-value);
  end case;
  ins--tag(back-end, done-tag);

end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <temporary-transfer-computation>) => ()
  let target = c.temporary;
  let source = c.computation-value;

  emit-transfer(back-end, target, source);

end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <variable-reference>)  => ()
  if (c.referenced-binding.binding-thread?)
    named-emitter(#"primitive-read-thread-variable")
      (back-end,
       emit-reference(back-end, #f, c.temporary),
       emit-reference(back-end, #f, c.referenced-binding));
  else
    emit-assignment(back-end,
		    c.temporary,
		    emit-reference(back-end, #f, c.referenced-binding));
  end if;
end method emit-computation;

define method emit-computation (back-end :: <harp-back-end>, c :: <exit>) => ()
  if (c.entry-state.local-entry-state?)
    let me-block :: <bind-exit> = c.entry-state.me-block;
    let nc = me-block.next-computation;
    let temp = if (instance?(nc, <bind-exit-merge>)) temporary(nc) end;

    if (temp & used?(temp))
      emit-transfer(back-end, temp, c.computation-value);
    end if;
    emit-goto(back-end, me-block);
  else
    // spill to MV area before doing non-local exit (which saves
    // the MV area values)
    if (instance?(c.computation-value, <multiple-value-temporary>)
        & required-values(c.computation-value) > 1)
      spill-multiple-values(back-end, c.computation-value);
    end if;
    ins--call(back-end, $primitive-nlx.runtime-reference,
	      push-arguments(back-end, list(emit-reference(back-end, #f, c.entry-state),
					    emit-reference(back-end, #f, c.computation-value))));

    // redundant move to get around Harp Live Registers
    let result = emit-destination-reference(back-end, #f, c.temporary);
    emit-result-assignment(back-end, result);
  end if;
end method emit-computation;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <unwind-protect>) => ()
  let protect-temp =  c.protected-temporary;
  let protected-result = emit-reference(back-end, #f, protect-temp);

  let cleanup-tag = make-tag(back-end);
  let continue-tag = make-tag(back-end);

  op--start-unwind-protect(back-end, c, cleanup-tag);

  dynamic-bind (*live-nlx-tags* = pair(cleanup-tag, *live-nlx-tags*))
    emit-computations(back-end, c.body, c.next-computation);
  end dynamic-bind;

  let multiple-values =
    case
      false-reference?(protected-result) => #();
      local-multiple-value-temporary?(protect-temp) =>
        mv-temporary(back-end, protect-temp).mv-elements;
      otherwise => list(protected-result);
    end case;

  op--unwind-protect-cleanup(back-end, c, cleanup-tag, continue-tag);

  ins--tag(back-end, cleanup-tag);
  for (value in multiple-values)
    ins--force-d(back-end, value);
  end for;

  preserving-cleanup-state(back-end)
    emit-computations(back-end, c.cleanups, c.next-computation);
  end;

  ins--end-cleanup(back-end, continue-tag);

  ins--tag(back-end, continue-tag);
end method emit-computation;



define method false-reference?(ref) => (false? :: <boolean>)
  if (ref)
    ref == $false
  else
    #t
  end if;
end method;

/// NON-LOCAL CONTROL FLOW

// define constant merge-exit-value = merge-left-value;
define constant merge-body-value = merge-right-value;

// Should use a runtime constant for this
define constant $bind-exit-frame-size = 56;

define method emit-computation (back-end :: <harp-back-end>, c :: <bind-exit>) => ()
  let merge = c.next-computation;
  if (c.entry-state.local-entry-state?)
    emit-computations(back-end, c.body, merge);
    maybe-emit-merge-transfer(back-end, merge, merge-body-value);
    emit-label(back-end, c);
  else
    let merge-tmp = merge.temporary;
    let result = c.temporary.used? & emit-object(back-end, #f, merge-tmp);
    let end-tag = make-tag(back-end);

    let bind-exit-frame = op--start-bind-exit(back-end, c, end-tag);

    dynamic-bind (*live-nlx-tags* = pair(end-tag, *live-nlx-tags*))
      emit-computations(back-end, c.body, merge);
      maybe-emit-merge-transfer(back-end, merge, merge-body-value);
    end dynamic-bind;

    // oblidged to set up reg-result for Harp
    if (result)
      ins--move(back-end, back-end.registers.reg-result, result);
    end if;

    ins--tag(back-end, end-tag);
    // Deallocate bind-exit frame by restoring frame pointer
    ins--add(back-end, back-end.registers.reg-stack, bind-exit-frame, $bind-exit-frame-size);
    // in the event of a NLX, must retrieve result from reg-result
    ins--move(back-end, result, back-end.registers.reg-result);
    // note that blocks are converted in all-rest context
    if (result & (required-values(merge-tmp) > 0))
      unspill-multiple-values(back-end, merge-tmp, #f);
    end if;
  end if;
end method emit-computation;

// cells for assignment

define method emit-computation (back-end :: <harp-back-end>, c :: <make-cell>) => ()
  let computation = emit-reference(back-end, #f, c.computation-value);

  if (closed-over?(c.temporary))
    call-primitive(back-end, emit-reference(back-end, #f, c.temporary),
		   op--make-box%(computation), computation);
  else
    let the-temporary = emit-reference(back-end, #f, c.temporary);
    op--move%(computation)(back-end, the-temporary, computation);
  end if;
end method emit-computation;

define method emit-computation
(back-end :: <harp-back-end>, c :: <get-cell-value>) => ()
  let the-temporary = emit-reference(back-end, #f, c.temporary);
  let the-cell = emit-reference(back-end, #f, c.computation-cell);

  if (closed-over?(c.computation-cell))
    op--load-index%(the-temporary)(back-end, the-temporary, the-cell, 0, bytes%(back-end, 1));
  else
    op--move%(the-cell)(back-end, the-temporary, the-cell);
  end if;

end method emit-computation;

define method emit-computation
(back-end :: <harp-back-end>, c :: <set-cell-value!>) => ()
  let the-temporary = emit-reference(back-end, #f, c.temporary);
  let computation = emit-reference(back-end, #f, c.computation-value);

  if (closed-over?(c.computation-cell))
    op--store-index%(computation)
      (back-end, the-temporary, computation,
       emit-reference(back-end, #f, c.computation-cell), 0, bytes%(back-end, 1));
  else
    let op--move = op--move%(computation);
    op--move(back-end, the-temporary, computation);
    op--move(back-end, emit-reference(back-end, #f, c.computation-cell), computation);
  end if;

end method emit-computation;

define method emit-computation (back-end :: <harp-back-end>, c :: <nop-computation>) => ()
end method;

// loop

define method emit-computation (back-end :: <harp-back-end>, c :: <loop>) => ()
  for (merge     in loop-merges(c))
    let tmp = temporary(merge);
    if (tmp & used?(tmp))
      emit-reference(back-end, #f, tmp);
      let lhs-ref = loop-temporary(back-end, tmp);
      let op--move = op--move%(lhs-ref);
      op--move(back-end,
               lhs-ref,
               emit-reference(back-end, #f, loop-merge-parameter(merge)));
    end if;
  end for;
  emit-label(back-end, c);
  emit-computations(back-end, loop-body(c), c.next-computation);

end method;

define method emit-computation (back-end :: <harp-back-end>, c :: <loop-call>) => ()
  let loop = c.loop-call-loop;

  for (initial-merge in loop-merges(loop),
       call-merge    in loop-call-merges(c))
    let tmp = temporary(initial-merge);
    if (tmp & used?(tmp))
      emit-reference(back-end, #f, tmp);
      ins--move(back-end,
		loop-temporary(back-end, tmp),
		emit-reference(back-end, #f, loop-merge-argument(call-merge)));
    end if;
  end for;

  emit-goto(back-end, loop);
end method;

define method emit-computation (back-end :: <harp-back-end>, c :: <end-loop>) => ()
end method;

// types

define method emit-check-type?
    (back-end :: <harp-back-end>, c :: <check-type>)
 => (well? :: <boolean>)
  // Don't emit type checks for the Dylan library
  ~compiling-dylan-library?()
end method;

define method emit-check-type?
    (back-end :: <harp-back-end>, c :: <keyword-check-type>)
 => (well? :: <boolean>)
  // We must emit these cause they check for things like size :: <integer>
  // for vector()
  #t
end method;

define method emit-computation (back-end :: <harp-back-end>, c :: <check-type>) => ()
  if (emit-check-type?(back-end, c))
    emit-type-check(back-end,
		    emit-reference(back-end, #f, c.computation-value),
		    c.type);
  end if;
  next-method();
end method emit-computation;

define variable *inline-type-checks?* = #t;

// some hard-wired slot-offsets -- find a more robust way of doing this!

define constant $union-type-1-slot-offset = 2;
define constant $union-type-2-slot-offset = 3;

define constant $wrapper-subtype-mask-slot-offset = 2;
define constant $class-subtype-bit-slot-offset = 4;

define constant $singleton-object-slot-offset = 2;

define method emit-instance-check
    (back-end :: <harp-back-end>, result, object, type :: <constant-reference>)
 => (test-result :: <test-result>)
  let test-result =
    make(<test-result>,
	 branch: #f,
	 result: result,
	 continue: make-tag(back-end),
	 false-internal: make-tag(back-end),
	 true-internal: make-tag(back-end));

  do-emit-type-check(back-end, object, type.cr-refers-to-object, type,
		     test-result: test-result);
  test-result
end method emit-instance-check;

define method emit-instance-check
    (back-end :: <harp-back-end>, result, object, type) => (result)
  op--instance?(back-end, result, object, type);
end method emit-instance-check;

define inline method dylan-type-check
    (back-end :: <harp-back-end>,
     done :: <tag>, error :: <tag>,
     object, type) => ()
  op--instance?(back-end, #f, object, type);
  ins--bne(back-end, done, back-end.registers.reg-result, $false);
  ins--bra(back-end, error);
end method;


define inline method primitive-type-check
    (back-end :: <harp-back-end>, object, type) => ()

  call-primitive(back-end,
		 #f,
		 $primitive-type-check,
		 object,
		 type);
end method;

define method emit-type-check
    (back-end :: <harp-back-end>, object, type :: <value-reference>) => ()
  // dynamically computed type -- don't attempt to inline
  let type-ref = emit-reference(back-end, #f, type);
  primitive-type-check(back-end, object, type-ref);
end method;

define method emit-type-check(back-end :: <harp-back-end>, object, type :: <object-reference>) => ()
  // type is known at compile-time
  let type = type.reference-value;
  let type-ref = emit-reference(back-end, #f, type);
  do-emit-type-check(back-end, object, type, type-ref);
end method;

define method do-emit-type-check
    (back-end :: <harp-back-end>, object, type :: <&type>, type-ref,
     #key test-result) => ()
  // type is known at compile-time
  if (*inline-type-checks?*)
    let done = (test-result & test-result.true-tag-internal) | make-tag(back-end);
    let continue = #f;
    let error = (test-result & test-result.false-tag-internal) | make-tag(back-end);
    let temp-type-ref = make-register(back-end);

    ins--move(back-end, temp-type-ref, type-ref);
    emit-type-check-internal(back-end, done, error, object, type, temp-type-ref);
    unless (test-result)
      ins--tag(back-end, error);
      call-primitive(back-end,
		     #f,
		     op--dylan-constant-ref(back-end, $dylan-type-check-error),
		     object,
		     type-ref);

      ins--tag(back-end, done);
    end unless;
  else
    primitive-type-check(back-end, object, type-ref);
  end if;
end method;

define method emit-type-check-internal
    (back-end :: <harp-back-end>,
     done :: <tag>, error :: <tag>,
     object, type :: <&type>, type-ref) => ()
  dylan-type-check(back-end,
		   done, error,
		   object, type-ref);
end method;

define method emit-type-check-internal
    (back-end :: <harp-back-end>,
     done :: <tag>, error :: <tag>,
     object, type :: <&class>, type-ref) => ()
  let type-name = type.^debug-name;

  select(type-name by \=)
    "<integer>", "<byte-character>", "<unicode-character>" =>
      let tag-bits =
	select(type-name by \=)
	  "<integer>" => 1;
	  "<byte-character>" => 2;
	  "<unicode-character>" => 3;
	end;
      let temp = make-n-register(back-end);

      ins--and(back-end, temp, object, 3);
      ins--beq(back-end, done, temp, tag-bits);
      ins--bra(back-end, error);
    "<boolean>" =>
      ins--beq(back-end, done, object, $true);
      ins--beq(back-end, done, object, $false);
      ins--bra(back-end, error);
    otherwise =>
      case
	^sealed-with-no-subclasses?(type) =>
	  let temp = make-n-register(back-end);
	  let continue = make-tag(back-end);

	  ins--and(back-end, temp, object, 3);
	  ins--beq(back-end, continue, temp, 0);
	  ins--bra(back-end, error);

	  ins--tag(back-end, continue);
	  ins--beq(back-end,
		   done,
		   op--object-class(back-end, #f, object),
		   type-ref);
	  ins--bra(back-end, error);
	^class-subtype-bit(type) ~== 0 =>
	  let temp = make-n-register(back-end);
	  let integer = make-tag(back-end);
	  let byte-character = make-tag(back-end);
	  let unicode-character = make-tag(back-end);
	  let integer-supertype? = ^subtype?($dylan-integer, type);
	  let byte-char-supertype? = ^subtype?($dylan-byte-character, type);
	  let unicode-char-supertype? = ^subtype?($dylan-unicode-character, type);
	  let wrapper = make-g-register(back-end);
	  let mask = make-n-register(back-end);
	  let bit = make-n-register(back-end);
	  let temp2 = make-n-register(back-end);
	  
	  ins--and(back-end, temp, object, 3);
	  ins--beq(back-end, integer, temp, 1);
	  ins--beq(back-end, byte-character, temp, 2);
	  ins--beq(back-end, unicode-character, temp, 3);

	  ins--load(back-end, wrapper, object, 0);
	  ins--load(back-end, mask, wrapper,
		    bytes%(back-end, $wrapper-subtype-mask-slot-offset));
	  ins--load(back-end, bit, type-ref,
		    bytes%(back-end, $class-subtype-bit-slot-offset));
	  ins--and(back-end, temp2, mask, bit);
	  ins--bne(back-end, done, temp2, 1);
	  ins--bra(back-end, error);

	  ins--tag(back-end, integer);
	  if (integer-supertype?)
	    ins--bra(back-end, done);
	  else
	    ins--bra(back-end, error);
	  end if;

	  ins--tag(back-end, byte-character);
	  if (byte-char-supertype?)
	    ins--bra(back-end, done);
	  else
	    ins--bra(back-end, error);
	  end if;

	  ins--tag(back-end, unicode-character);
	  if (unicode-char-supertype?)
	    ins--bra(back-end, done);
	  else
	    ins--bra(back-end, error);
	  end if;

	otherwise =>
	  dylan-type-check(back-end,
			   done, error,
			   object, type-ref);
      end case;
  end select;
end method;

define method emit-type-check-internal
    (back-end :: <harp-back-end>,
     done :: <tag>, error :: <tag>,
     object, type :: <&union>, type-ref) => ()
  let continue = make-tag(back-end);
  let type1-ref = make-register(back-end);
  let type2-ref = make-register(back-end);

  ins--load(back-end, type1-ref, type-ref,
	    bytes%(back-end, $union-type-1-slot-offset));
  emit-type-check-internal(back-end, done, continue,
			   object, type.^union-type1, type1-ref);
  ins--tag(back-end, continue);
  ins--load(back-end, type2-ref, type-ref,
	    bytes%(back-end, $union-type-2-slot-offset));
  emit-type-check-internal(back-end, done, error,
			   object, type.^union-type2, type2-ref);
end method;

define method emit-type-check-internal
    (back-end :: <harp-back-end>,
     done :: <tag>, error :: <tag>,
     object, type :: <&singleton>, type-ref) => ()
  let singleton-object = $false;

  if (type.^singleton-object)
    singleton-object := make-register(back-end);
    ins--load(back-end, singleton-object, type-ref,
	      bytes%(back-end, $singleton-object-slot-offset));
  end if;
  ins--beq(back-end, done, object, singleton-object);
  ins--bra(back-end, error);
end method;

define method emit-type-check-internal
    (back-end :: <harp-back-end>,
     done :: <tag>, error :: <tag>,
     object, type :: <&limited-integer>, type-ref) => ()
  let integer-min :: false-or(<integer>) = type.^limited-integer-min;
  let integer-max :: false-or(<integer>) = type.^limited-integer-max;

  if (integer-min)
    ins--blt(back-end, error, object, op--integer(back-end, #f, integer-min));
  end;
  if (integer-max)
    ins--bgt(back-end, error, object, op--integer(back-end, #f, integer-max));
  end;
  ins--bra(back-end, done);
end method;

define method emit-computation (back-end :: <harp-back-end>, c :: <multiple-value-check-type>) => ()
  if (compiling-dylan-library?())
    next-method();
  else
    emit-type-checks(back-end, c);
  end if;
end method;

define method emit-computation (back-end :: <harp-back-end>, c :: <multiple-value-check-type-rest>) => ()
  if (compiling-dylan-library?())
    next-method();
  else
    let rest-type = emit-reference(back-end, #f, c.rest-type);
    emit-type-checks(back-end, c,
		     rest-type: (rest-type ~== $false) & rest-type);
  end if;
end method;

define open generic op--bmvset(back-end :: <harp-back-end>, tag :: <tag>) => ();

define method push-multiple-values(back-end :: <harp-back-end>) => ()
  let done = make-tag(back-end);
  let multiple-values = make-tag(back-end);
  let mv-area = multiple-values-area(back-end);

  op--bmvset(back-end, multiple-values);
  ins--push(back-end, 1);
  ins--bra(back-end, done);

  ins--tag(back-end, multiple-values);
  let mv-count = op--ld-mv-count(back-end);
  let mv-count-in-bytes = bytes%(back-end, mv-count);
  let stack = back-end.registers.reg-stack;
  ins--sub(back-end, stack, stack, mv-count-in-bytes);
  ins--copy-words-down-w(back-end, stack, mv-area, mv-count);
  ins--push(back-end, mv-count-in-bytes);

  ins--tag(back-end, done);
end method;

define method pop-multiple-values(back-end :: <harp-back-end>) => ()
  let done = make-tag(back-end);
  let multiple-values = make-tag(back-end);
  let mv-area = multiple-values-area(back-end);
  let mv-count-in-bytes = make-n-register(back-end);

  ins--pop(back-end, mv-count-in-bytes);
  ins--bne(back-end, multiple-values, mv-count-in-bytes, 1);
  op--store-multiple-values-count(back-end, 1);
  ins--bra(back-end, done);

  ins--tag(back-end, multiple-values);
  let mv-count = op--raw(back-end, #f, mv-count-in-bytes);
  let stack = back-end.registers.reg-stack;
  ins--copy-words-down-w(back-end, mv-area, stack, mv-count);
  ins--add(back-end, stack, stack, mv-count-in-bytes);
  op--store-multiple-values-count(back-end, mv-count);

  ins--tag(back-end, done);
end method;


define method emit-type-checks
    (back-end :: <harp-back-end>, c :: <check-type-computation>,
     #key rest-type) => ()
  clear-multiple-values-area(back-end);
  let temp = c.temporary;
  let mv-ref = emit-mv-reference(back-end, #f, temp) | make-multiple-values(back-end);
  let comp-ref = emit-mv-reference(back-end, #f, c.computation-value);
  let imprecise? = temp.rest-values?;
  let number-values = required-values(temp);

  for (i :: <integer> from 0 below max(1, number-values))
    op--move-mv(back-end, mv-ref, comp-ref, i);
  end for;

  ins--rem(back-end, "Return Value Type Checks");

  if (imprecise?)
    push-multiple-values(back-end);
  end if;

  for (i :: <integer> from 0 below number-values,
       type in c.types)
    if (type)
      emit-type-check(back-end,
		      emit-multiple-value(back-end, #f, mv-ref, i),
		      type);
    end if;
  end for;
  if (rest-type)
    call-primitive(back-end,
		   #f,
		   $primitive-type-check-rest-values,
		   bytes%(back-end, number-values),
		   rest-type);
  end if;

  if (imprecise?)
    pop-multiple-values(back-end);
  else
    op--store-multiple-values-count(back-end, number-values);
  end if;

end method;

define method emit-computation (back-end :: <harp-back-end>, c :: <stack-vector>) => ()

  if (c.temporary & c.temporary.used?)
    let tmp = c.temporary;
    let tmp-reg = emit-object(back-end, #f, tmp);

    // Defer tail-call stack-allocation to avoid overwriting stack spills
    if (c.stack-allocate-vector-for-tail-call?)
      ins--load-address-of-stack-arg-n(back-end,
                                       tmp-reg,
                                       tail-call-stack-vector-start(back-end,
                                                                    tmp.number-values));
    else
      stack-allocate-vector-for-normal-call(back-end,
                                            tmp-reg,
                                            tmp.number-values,
					    emit-references(back-end, c.arguments));
    end if;
  end if;
end method;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <slot-value>) => ()
  let result = emit-reference(back-end, #f, c.temporary);
  let instance = emit-reference(back-end, #f, computation-instance(c));
  let offset = computation-slot-offset(c);

  if (computation-guaranteed-initialized?(c))
    op--init-slot-element(back-end, result, instance, offset);
  else
    op--slot-element(back-end, result, instance, offset);
  end if;

end method;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <slot-value-setter>) => ()
  let result = emit-reference(back-end, #f, c.temporary);
  let new-value = emit-reference(back-end, #f, computation-new-value(c));
  let instance = emit-reference(back-end, #f, computation-instance(c));
  let offset = computation-slot-offset(c);

  op--slot-element-setter(back-end, result, new-value, instance, offset);

end method;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <repeated-slot-value>) => ()
  let result = emit-reference(back-end, #f, c.temporary);
  let instance = emit-reference(back-end, #f, computation-instance(c));
  let offset = computation-slot-offset(c);
  let index = emit-reference(back-end, #f, computation-index(c));
  let type = ^slot-type(computation-slot-descriptor(c));
  let op--slot-element =
    select(repeated-representation-size(type))
      1 => op--byte-element;
      2 => op--double-byte-element;
      otherwise => op--repeated-slot-element;
    end select;

  op--slot-element(back-end, result, instance, offset, index,
		   tagged?: computation-index-tagged?(c))
end method;

define method emit-computation
    (back-end :: <harp-back-end>, c :: <repeated-slot-value-setter>) => ()
  let result = emit-reference(back-end, #f, c.temporary);
  let new-value = emit-reference(back-end, #f, computation-new-value(c));
  let instance = emit-reference(back-end, #f, computation-instance(c));
  let offset = computation-slot-offset(c);
  let index = emit-reference(back-end, #f, computation-index(c));
  let type = ^slot-type(computation-slot-descriptor(c));
  let op--slot-element-setter =
    select(repeated-representation-size(type))
      1 => op--byte-element-setter;
      2 => op--double-byte-element-setter;
      otherwise => op--repeated-slot-element-setter;
    end select;

  op--slot-element-setter(back-end, result, new-value, instance, offset, index,
			  tagged?: computation-index-tagged?(c))
end method;

/// EMIT-CALL

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <primitive-call>, f :: <&primitive>) => ()

  let destinations-size = 
    max(1, f.primitive-signature.^signature-number-values);
  let args :: <simple-object-vector> =
    make(<vector>,
	 size: c.arguments.size + destinations-size);

  args[0] := result;

  if (destinations-size > 1)
    primitive-destinations(back-end, c, f, args);
  end;
  primitive-arguments(back-end, c, args, destinations-size);

  let result = apply(f.emitter, back-end, args);

  emit-test(back-end, result, c.next-computation, c.temporary);

end method emit-call;

define method emit-test 
    (back-end :: <harp-back-end>, test, c, t) => ()
end method emit-test;

define method emit-test 
    (back-end :: <harp-back-end>, test :: <test-result>, c, t) => ()

  unless (test.false-tag-internal)
    test.false-tag-internal := make-tag(back-end);
  end unless;
  test.false-tag := test.continue-tag;
  test.true-tag := test.continue-tag;

  emit-test-result(back-end, test);
end method emit-test;

define method emit-test
    (back-end :: <harp-back-end>, test-result :: <test-result>, c :: <if>, t) => ()
  let test = c.test;
  if (t == test)
    cg-temporary(back-end, test) := test-result;

    unless (test-result.false-tag-internal)
      test-result.false-tag-internal := make-tag(back-end);
    end unless;
    test-result.false-tag := make-tag(back-end);
    test-result.true-tag := make-tag(back-end);

    emit-test-result(back-end, test-result);

  else
    next-method();
  end if;
end method emit-test;

define method emit-test-result
    (back-end :: <harp-back-end>, test :: <test-result>) => ()

  let continue-tag :: <tag> = test.continue-tag;
  let true-tag :: <tag> = test.true-tag;
  let true-tag-internal :: false-or(<tag>) = test.true-tag-internal;
  let false-tag :: <tag> = test.false-tag;
  let false-tag-internal :: <tag> = test.false-tag-internal;
  let result = test.result;
  let fall-through? = false-tag == continue-tag;

  if (test.branch)
    apply(test.branch, back-end, false-tag-internal, test.operands);
  end if;

  if (true-tag-internal)
    ins--tag(back-end, true-tag-internal);
  end;
  ins--move(back-end, result, $true);
  ins--bra(back-end, true-tag);

  ins--tag(back-end, false-tag-internal);
  ins--move(back-end, result, $false);

  if (fall-through?)
    ins--tag(back-end, continue-tag);
  else
    ins--bra(back-end, false-tag);
  end;

end method emit-test-result;

define function primitive-destinations
    (back-end :: <harp-back-end>, c :: <primitive-call>, f :: <&primitive>,
     destinations :: <simple-object-vector>) => ()
  let multiple-values = c.temporary.users;
  let values-size = multiple-values.size;
  let multiple-types = f.primitive-signature.^signature-values;

  for (i :: <integer> from 1 to values-size)
    let c = multiple-values[i - 1];
    
    if (instance?(c, <extract-single-value>))
      let index = c.index;
      unless (index = 0)
	destinations[index] := 
	  emit-temporary(back-end, #f, c.temporary,
			 type: multiple-types[index]);
      end unless;
    end if;
  end for;
end function primitive-destinations;

define function primitive-arguments
    (back-end :: <harp-back-end>, c :: <primitive-call>,
     args :: <simple-object-vector>, start :: <integer>) => ()
  for (object in c.arguments,
       index :: <integer> from start)
    args[index] := emit-reference(back-end, #f, object);
  end for;
end function primitive-arguments;

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <simple-call>, f) => ()

  if (tail-call-optimizable?(back-end, c))
    call-protocol
      (tail-call-xep, back-end)
      (identity, identity, reference)
      (c, result, c.function, rest c.arguments)
    end;
  else
    call-protocol
      (call-xep, back-end)
      (identity, identity, reference)
      (c, result, c.function, rest c.arguments)
    end;
  end if;
   
end method emit-call;

define method emit-iep-call 
    (back-end :: <harp-back-end>, result, c :: <simple-call>, f :: <&iep>) => ()

  if (tail-call-optimizable?(back-end, c))
    call-protocol
      (tail-call-iep, back-end)
      (identity, identity, identity, cg-name)
      (c, f, result, f,	rest c.arguments)
    end;
  else
    call-protocol
      (call-iep, back-end)
      (identity, identity, identity, cg-name)
      (c, f, result, f,	rest c.arguments)
    end;
  end if;

end method emit-iep-call;

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <simple-call>, f :: <&iep>) => ()

    emit-iep-call(back-end, result, c, f);

end method emit-call;

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <simple-call>, f :: <&generic-function>) => ()

  if (call-congruent?(c))
    if (tail-call-optimizable?(back-end, c))
      call-protocol
	(tail-call-engine-node, back-end)
	(identity, identity, identity)
	(c, result, #f, rest c.arguments)
      end;
    else
      call-protocol
	(call-engine-node, back-end)
	(identity, identity, identity)
	(c, result, #f, rest c.arguments)
      end;
    end if;
  else
    next-method()
  end if

end method emit-call;

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <engine-node-call>, f :: <&generic-function>) => ()

  if (tail-call-optimizable?(back-end, c))
    call-protocol
      (tail-call-engine-node, back-end)
      (identity, identity, reference)
      (c, result, c.engine-node, rest c.arguments)
    end;
  else
    call-protocol
      (call-engine-node, back-end)
      (identity, identity, reference)
      (c, result, c.engine-node, rest c.arguments)
    end;
  end if;

end method emit-call;

/*
define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <engine-node-apply>, f :: <&generic-function>) => ()

  call-protocol
    (call-engine-node-apply, back-end)
    (identity, identity, reference)
    (c,	result,	c.engine-node, rest c.arguments)
  end;

end method emit-call;
*/

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <method-call>, f) => ()

  if (tail-call-optimizable?(back-end, c))
    call-protocol
      (tail-call-mep, back-end)
      (identity, identity, identity, reference, reference)
      (c, f, result, c.function, c.next-methods, rest c.arguments)
    end;
  else
    call-protocol
      (call-mep, back-end)
      (identity, identity, identity, reference, reference)
      (c, f, result, c.function, c.next-methods, rest c.arguments)
    end;
  end if;

end method emit-call;

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <method-call>, f :: <&iep>) => ()

    emit-iep-call(back-end, result, c, f);

end method emit-call;

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <apply>, f) => ()

  call-protocol
    (call-apply, back-end)
    (identity, reference)
    (result, c.function, rest c.arguments)
  end;

end method emit-call;

define method emit-call 
(back-end :: <harp-back-end>, result, c :: <method-apply>, f) => ()

  call-protocol
    (call-mep-apply, back-end)
    (identity, identity, reference, reference)
    (c, result, c.function, c.next-methods, rest c.arguments)
  end;

end method emit-call;


/// FFI SUPPORT

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <primitive-call>, f :: <&c-function>) => ()

  let calling-convention =
    select(f.c-modifiers by \=)
      "__stdcall" => #"STDCALL";
      otherwise => #"CDECL";
    end select;
  let (result-type, result-typespec) = ffi-result-typespec(back-end, c);

  call-foreign-function(back-end,
			result,
			emit-cg-name(back-end, #f, f),
			emit-references(back-end, c.arguments),
                        parameter-typespecs(back-end, f.c-signature),
			result-typespec,
			calling-convention);
  if (result)
    emit-ffi-result(back-end, result, result-type, result-typespec);
  end if;
end method;

define inline function ffi-result-typespec
    (back-end :: <harp-back-end>, c :: <primitive-call>) => (result-type, result-typespec)
  let c-temporary = c.temporary;
  let result-type =
    if (c-temporary) as(<&type>, type-estimate(c-temporary)) end;
  let result-typespec =
    if (result-type)
      if (instance?(result-type, <&raw-aggregate-type>))
	let type-size = parameter-type-size(back-end, result-type);
	let structure = make-n-register(back-end);
	call-c-primitive(back-end, structure, $mps--malloc.runtime-reference, bytes%(back-end, type-size));
	pair(structure, type-size);
      end if;
    end if;
  values(result-type, result-typespec);
end function;

/*
define method op--dylan-object-as-boolean(back-end :: <harp-back-end>, result) => ()

  let done-tag = make-tag(back-end);

  ins--beq(back-end, done-tag, result, $false);
  ins--move(back-end, result, $true);

  ins--tag(back-end, done-tag);

end method;
*/

define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <primitive-indirect-call>, f :: <&c-function>) => ()

  let calling-convention =
    select(f.c-modifiers by \=)
      "__stdcall" => #"STDCALL";
      otherwise => #"CDECL";
    end select;

  local method emit-primitive-call(function, #rest arguments)

	  let (result-type, result-typespec) = ffi-result-typespec(back-end, c);
          call-foreign-function(back-end,
				result,
				emit-object(back-end, #f, function),
                                emit-references(back-end, arguments),
                                parameter-typespecs(back-end, f.c-signature),
				result-typespec,
				calling-convention);
	  if (result)
	    emit-ffi-result(back-end, result, result-type, result-typespec);
	  end if;

        end method emit-primitive-call;

  apply(emit-primitive-call, c.arguments);

end method;

define method parameter-typespecs
    (back-end :: <harp-back-end>, signature) => (typedefs :: false-or(<sequence>))
  local method maybe-trim-sig-types 
	    (v :: <simple-object-vector>, n :: <integer>) => (res :: <simple-object-vector>)
	    if (n = size(v)) v else copy-sequence(v, end: n) end if
	end method;
  let required-types = maybe-trim-sig-types(signature.^signature-required,
					    signature.^signature-number-required);

  map(method(type)
	  pair(type, parameter-type-size(back-end, type))
      end method,
      required-types)

end method parameter-typespecs;

define method parameter-type-size
    (back-end :: <harp-back-end>, raw-type) => (storage-size :: <integer>)

  let storage-size :: <integer> = raw-type.raw-type-size;
  let bytes :: <integer> = bytes%(back-end, 1);

  ceiling/(storage-size, bytes)

end method parameter-type-size;

// Ensure that return values from C-functions are of their desired types

define method emit-ffi-result
    (back-end :: <harp-back-end>, result, type :: <type-estimate-raw>, result-typespec) => ()
  emit-ffi-result(back-end, result, as(<&type>, type), result-typespec)
end method emit-ffi-result;

define method emit-ffi-result(back-end :: <harp-back-end>, result, type :: <&raw-type>, result-typespec) => ()

  select(type.^debug-name by \=)
    "<raw-c-unsigned-char>" =>
      ins--and(back-end, result, back-end.registers.reg-c-result, #xff);

    "<raw-c-signed-char>" =>
      let done-tag = make-tag(back-end);

      ins--and(back-end, result, back-end.registers.reg-c-result, #xff);
      ins--blt(back-end, done-tag, result, #x80);
      ins--or(back-end, result, result, #xffffff00);
      ins--tag(back-end, done-tag);

    "<raw-c-unsigned-short>" =>
      ins--and(back-end, result, back-end.registers.reg-c-result, #xffff);

    "<raw-c-signed-short>" =>
      let done-tag = make-tag(back-end);

      ins--and(back-end, result, back-end.registers.reg-c-result, #xffff);
      ins--blt(back-end, done-tag, result, #x8000);
      ins--or(back-end, result, result, #xffff0000);
      ins--tag(back-end, done-tag);

    "<raw-c-float>" =>
      ins--fmove(back-end, result, back-end.registers.reg-c-float-result);

    "<raw-c-double>" =>
      ins--dmove(back-end, result, back-end.registers.reg-c-float-result);

    otherwise =>
      ins--move(back-end, result, back-end.registers.reg-c-result);

  end select;

end method emit-ffi-result;

define method emit-ffi-result(back-end :: <harp-back-end>, result, type :: <&raw-aggregate-type>, result-typespec) => ()

  let (structure, type-size) =
    if (result-typespec)
      values(result-typespec.head, result-typespec.tail)
    end if;

  // if structure size is less than 2 words, it is returned in {eax, edx}
  select (type-size)
    1 =>
      ins--store(back-end, back-end.registers.reg-c-result, structure, 0);
      ins--move(back-end, result, structure);
    2 =>
      ins--store(back-end, back-end.registers.reg-c-result, structure, 0);
      ins--store(back-end, back-end.registers.reg-c-result2, structure, bytes%(back-end, 1));
      ins--move(back-end, result, structure);
    otherwise =>
      ins--move(back-end, result, back-end.registers.reg-c-result);
  end select;

end method emit-ffi-result;

define method emit-ffi-result(back-end :: <harp-back-end>, result, type, result-typespec) => ()
  ins--move(back-end, result, back-end.registers.reg-c-result);
end method emit-ffi-result;

define method emit-ffi-result(back-end :: <harp-back-end>, result == #f, type, result-typespec) => ()
end method emit-ffi-result;



define method emit-call 
    (back-end :: <harp-back-end>, result, c :: <c-variable-pointer-call>, f) => ()
  let c-var = c.c-variable;
  let c-name = c-name(back-end, c-var.name);
  let c-reference =
    if (c-var.dll-import?)
      make-imported-constant-reference(back-end, c-var);
    else
      ins--constant-ref(back-end, c-var);
    end if;

  ins--move(back-end, result, c-reference);
end method;

/// EMIT-REFERENCE


define method cg-temporary(back-end :: <harp-back-end>, t :: <temporary>,
			   #key loop?, values?) => (register :: <object>)
  let register = element(back-end.cg-variables.cg-temporaries, t.frame-offset + 1, default: #f);
  if (instance?(register, <cg-register>))
    case
      loop? => register.loop-register;
      values? => register.values-register;
      otherwise => register.cg-register;
    end case
  else register
  end if
end method cg-temporary;

define method cg-temporary(back-end :: <harp-back-end>, t :: <lexical-variable>,
			   #key loop?, values?) => (register :: <object>)
  let register = element(back-end.cg-variables.cg-temporaries, t, default: #f);
  if (instance?(register, <cg-register>))
    case
      loop? => register.loop-register;
      values? => register.values-register;
      otherwise => register.cg-register;
    end case
  else register
  end if
end method cg-temporary;

define method mv-temporary(back-end :: <harp-back-end>, t :: <temporary>) => (mval :: <cg-multiple-values>)
  cg-temporary(back-end, t, values?: #t);
end method mv-temporary;


define method cg-temporary-setter(register, back-end :: <harp-back-end>, t :: <temporary>,
				  #key loop?) => (register :: <object>)
  let values? = #f;
  if (instance?(t, <multiple-value-temporary>))
    let number-values = required-values(t);
    let mval =
      if (number-values = 0)
	make(<cg-multiple-values>, register: register);
      else
	let elements = make(<vector>, size: number-values, fill: #f);
	elements[0] := register;
	make(<cg-multiple-values>, register: register, elements: elements);
      end if;
    values? := mval;
  end if;
  element(back-end.cg-variables.cg-temporaries, t.frame-offset + 1) :=
    if (loop? | values?)
      make(<cg-register>,
	   register: register, loop: loop?, values: values?)
    else
      register
    end if;
  register
end method cg-temporary-setter;

define method cg-temporary-setter(register, back-end :: <harp-back-end>, t :: <lexical-variable>,
				  #key loop?) => (register :: <object>)
  element(back-end.cg-variables.cg-temporaries, t) :=
    if (loop?)
      make(<cg-register>, register: register, loop: loop?)
    else
      register
    end if;
  register
end method cg-temporary-setter;

define class <cg-register>(<object>)
  constant slot cg-register :: <register>, required-init-keyword: register:;
  constant slot loop-register :: false-or(<register>), required-init-keyword: loop:;
  constant slot values-register :: false-or(<cg-multiple-values>), init-keyword: values:;
end class;

define method loop-temporary?(t :: <temporary>) => (loop? :: <boolean>)
  let gen = t.generator;
  let loop? = instance?(gen, <loop-merge>) & loop-merge-initial?(gen);
  loop?
end method;

define method loop-temporary(back-end :: <harp-back-end>, t :: <temporary>) => (register :: <object>)
  cg-temporary(back-end, t, loop?: #t);
end method loop-temporary;


define method emit-destination-reference
    (back-end :: <harp-back-end>, stream, o) => (reference :: <object>)
 o & o.used? & emit-reference(back-end, stream, o);
end method emit-destination-reference;

/*
define method really-used?(o :: <referenced-object>) => (used? :: <boolean>)
  if (empty?(o.users)) #f
  else
    if (*emitting-init-code?*)
      if (*interactive-mode?*) #t
      else
	~ ((o.users.size = 1)
	     & instance?(o.users.first, <return>));
      end;
    else
      #t
    end;
  end;
end method;
*/

define inline function closure-reference? (o) => (res :: <boolean>)
  o.environment ~== *current-environment*
end function;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <stack-vector-temporary>) => (reference :: <object>)
  if (closure-reference?(o))
    break("closure-reference? <stack-vector-temporary>");
  else
    emit-object(back-end, stream, o);
  end if;
end method;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <object-reference>) => (reference :: <object>)
  let o = o.reference-value;
  emit-indirect-reference(back-end, stream, o);
end method;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <method-reference>) => (reference :: <object>)
  let o = function(o.reference-value);
  emit-reference(back-end, stream, o);
end method;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <defined-constant-reference>) => (reference :: <object>)
  emit-reference(back-end, stream, referenced-binding(o));
end method;

define function dump-closure (o :: <temporary>) => ()
/* // This no longer works if it ever did - 'id' is not defined on env. gz, 27-Feb-00.
  let env = *current-environment*;
  format-out("\n*** WARNING: FAILED TO FIND TEMPORARY %= in CLOSURE ENV-%=[", o, env.id);
  let closure = env.closure;
  for (i :: <integer> from 0 below closure.size)
     format-out("%s ", closure[i]);
  end for;
  format-out("] of %=\n", o.environment.lambda);
*/
end function;


define function closure-environment-offset%(back-end :: <harp-back-end>)
 => (offset :: <integer>)
  if (instance?(back-end.cg-variables.current-lambda.function, <&keyword-closure-method>))
    back-end.keyword-closure-environment-offset + 4
  else
    back-end.closure-environment-offset + 4
  end if
end function;

define function emit-closure-reference(back-end :: <harp-back-end>, stream, o :: <temporary>) => (reference :: <object>)
  if (method-top-level?(*current-environment*.lambda))
    op--environment-parameter(back-end);
  else
    let offset = closure-offset(*current-environment*, o);
    unless (offset)
      dump-closure(o);
      offset := -1;
    end unless;
    let new-register = make-g-register(back-end);
    ins--load(back-end, new-register,
	      op--environment-parameter(back-end),
	      closure-environment-offset%(back-end) + bytes%(back-end, offset));
    new-register;
  end if;
end function;

define sideways method emit-reference (back-end :: <harp-back-end>, stream, o :: <temporary>) => (reference :: <object>)
  if (o.used?)
    if (closure-reference?(o))
      emit-closure-reference(back-end, stream, o);
    else
      emit-object(back-end, stream, o);
    end if;
  end if;
end method;


define function binding-of-*current-handlers*?
    (binding) => (res :: <boolean>)
  instance?(binding, <module-binding>)
  & (binding-canonical-binding(dylan-binding(#"*current-handlers*"))
       == binding-canonical-binding(binding))
end function;


define function constant-for-*current-handlers*?
    (const :: <constant-reference>) => (res :: <boolean>)
  const == $current-handlers
end function;


define function emit-module-name
    (back-end :: <harp-back-end>, stream, o :: <module-binding>) => (reference :: <string>)
  let name = o.emitted-name;
  if (instance?(name, <byte-string>)) name
  else
    o.emitted-name := global-mangle(back-end, o)
  end if
end function;

define function emit-module-reference
(back-end :: <harp-back-end>, stream, o :: <module-binding>) => (reference :: <object>)
  if (*emitting-data?*)
    emit-module-name(back-end, stream, o)
  else
    op--constant-ref(back-end, o)
  end if
end function;


/// variables imported from other libraries must be treated specially, because
/// they can no longer be represented with a simple constant reference

define sideways method emit-reference 
(back-end :: <harp-back-end>, stream, o :: <module-binding>) => (reference :: <object>)
  let cref = emit-module-reference(back-end, stream, o);

  if (*emitting-data?*)
    cref;
  else
    if (cref.constant-for-*current-handlers*?)
      cref // hack for threads
    elseif (emit-import-adjustment?(back-end) & imported-object?(back-end, o))
      let new-register = make-g-register(back-end);
      ins--load(back-end, new-register, cref, 0); /// IS THIS SAFE?? What if it's a float?
      new-register;
    else
      cref
    end if;
  end if;
end method;


// References for interactor variables

define sideways method emit-reference 
(back-end :: <harp-back-end>, stream, o :: <interactor-binding>) => (reference :: <object>)
  if (*emitting-data?*)
    error("Internal error in harp-cg: attempt to reference interactor binding from data");
  else
    op--constant-ref(back-end, o, interactor?: #t)
  end if
end method;


// Make symbol-model-objects for symbols

define class <symbol-object>(<object>)
  constant slot cg-symbol-name :: <string>, required-init-keyword: name:;
  constant slot cg-uninterned-symbol :: <constant-reference>, required-init-keyword: value:;
  constant slot cg-indirect-symbol :: <indirect-constant-reference>, required-init-keyword: indirection:;
end class;

define sideways method emit-indirect-reference
  (back-end :: <harp-back-end>, stream, o) => (reference :: <object>)
    if (load-bound-object?(o))
      let local-symbol = element(heap-symbols(*current-heap*), o);

      select(local-symbol by instance?)
	<symbol-object> =>
	  local-symbol.cg-indirect-symbol;
	otherwise =>
	  emit-cg-symbol(back-end, stream, o);
      end select;
    else
      emit-reference(back-end, stream, o);
    end if;
end method;

// Make a symbol-model; remember model in the heap

define method emit-cg-symbol
  (back-end :: <harp-back-end>, stream, o :: <symbol>)
 => (reference :: <constant-reference>)
  let symbol-name = emit-name(back-end, stream, o);
  let indirection =
    ins--indirect-constant-ref
      (back-end,
       concatenate($indirection-prefix, symbol-name));
  let local-symbol =
      make(<symbol-object>,
	   name: symbol-name,
	   value: ins--constant-ref(back-end, o),
	   indirection: indirection);
  element(heap-symbols(*current-heap*), o) := local-symbol;
  indirection
end method;

// Convert model-objects to strings

define sideways method model-object-as-string(object :: <symbol>) => (name :: <string>)
  if (object.model-has-definition?)
    emit-name(current-back-end(), #f, object)
  else
    let local-symbol = element(heap-symbols(*current-heap*), object, default: #f);
    local-symbol.cg-symbol-name
  end if
end method;

define method model-object-as-string(object :: <runtime-object>) => (name :: <string>)
  object.runtime-object-name
end method;

define method model-object-as-string(object :: <c-runtime-object>) => (name :: <string>)
  c-name(current-back-end(), object.runtime-object-name)
end method;

define sideways method model-object-as-string(object :: <module-binding>) => (name :: <string>)
  emit-module-name(current-back-end(), #f, object)
end method;

define sideways method model-object-as-string(object :: <&kernel-ep>) => (name :: <string>)
  emit-reference(current-back-end(), #f, object)
end method;

define sideways method model-object-as-string(object :: <&mep>) => (name :: <string>)
  emit-reference(current-back-end(), #f, object)
end method;

define sideways method model-object-as-string(object) => (name :: <string>)
  emit-name(current-back-end(), #f, object);
end method;

define sideways method model-object-as-string(object :: <interactor-binding>) => (name :: <string>)
  "{Anonymous interactor binding}"
end method;

define sideways method model-object-as-string(object :: <interactor-binding-reference>) => (name :: <string>)
  "{Anonymous interactor reference}"
end method;


define sideways method canonical-interactor-object(object :: <interactor-binding-reference>) => (handle)
  object.referenced-binding.binding-interactor-id;
end method;

define sideways method canonical-interactor-object(object :: <interactor-binding>) => (handle)
  object.binding-interactor-id;
end method;

// Map these compiler entry-point-models to runtime-models

define method emit-xep-reference 
    (back-end :: <harp-back-end>, stream, ep :: <&lambda-xep>)
 => (xep :: <runtime-object>)
  let size     = ^entry-point-number-required(ep);
  let index    = min(size, $number-xeps);

  if (^entry-point-key?(ep))
    $rest-key-xeps[index]
  elseif (^entry-point-rest?(ep))
    $rest-xeps[index]
  else 
    $xeps[index]
  end if
end method;

define method emit-xep-reference
    (back-end :: <harp-back-end>, stream, ep :: <&slot-accessor-xep>)
 => (xep :: <runtime-object>)
  entry-point-reference(^entry-point-name(ep))
end method;

define method emit-xep-reference 
    (back-end :: <harp-back-end>, stream, ep :: <&generic-function-xep>)
 => (xep :: <runtime-object>)

  let req-size = ^entry-point-number-required(ep);
  let index    = min(req-size, $number-gf-xeps);

  if (^entry-point-optionals?(ep)) 
    $new-gf-optional-xeps[index]
  else 
    $new-gf-xeps[index]
  end if
end method;

define sideways method emit-reference (back-end :: <harp-back-end>, stream, o :: <&xep>)
 => (reference :: <string>)
  let ref :: <runtime-object> = emit-xep-reference(back-end, stream, o);
  o.emitted-name := ref;
  ref.runtime-object-name
end method;


define inline-only function default-kernel-ep-reference (o :: <&kernel-ep>)
 => (reference :: <string>)
  concatenate(harp-raw-mangle(o.^entry-point-name), "_entry")
end function;


define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <&engine-node-ep>)
 => (reference :: <string>);
  let ref :: <runtime-object> =
    entry-point-reference(emit-engine-node-ep-reference(back-end, stream, ^engine-node(o), o));
  o.emitted-name := ref;
  ref.runtime-object-name
end method;


define method emit-engine-node-ep-reference
    (back-end :: <harp-back-end>, stream, e :: <&engine-node>, o :: <&engine-node-ep>)
 => (reference :: <string>)
  default-kernel-ep-reference(o)
end method;

define method emit-engine-node-ep-reference
    (back-end :: <harp-back-end>, stream, e :: <&engine-node>, o :: <&rogue-engine-node-ep>)
 => (reference :: <string>)
  default-kernel-ep-reference(o)
end method;

define method emit-engine-node-ep-reference
    (back-end :: <harp-back-end>, stream, e :: <&discriminator>, o :: <&discriminator-ep>)
 => (reference :: <string>)
  // @@@@ This doesn't take into account multiple calling sequences
  // (i.e., $dispatch-case-limit being greater than 0).
  let epname :: <symbol> = ^entry-point-name(o);
  let mangled-epname = harp-raw-mangle(epname);
  let arg-num = ^discriminator-argnum(e);
  let suff = if (epname == #"discriminate-on-argument") "entry" else "engine" end;

  if (arg-num >= $number-discriminators)
    concatenate(mangled-epname, "_%s", suff);
  else
    format-to-string("%s_%s_%d", mangled-epname, suff, arg-num);
  end if
end method;


define method emit-engine-node-ep-reference
    (back-end :: <harp-back-end>, stream,
     e :: <&keyed-single-method-engine-node>,
     o :: <&function-linked-engine-node-ep>) => (reference :: <string>);

  let epname = ^entry-point-name(o);
  let mangled-epname = harp-raw-mangle(epname);
  let req-size :: <integer> = ^engine-node-ep-number-required(o);
  if (req-size >= $number-keyeds)
    concatenate(mangled-epname, "_entry");
  else
    format-to-string("%s_entry_%d", mangled-epname, req-size);
  end if;

end method;

  
define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <&keyword-method-mep>) => (reference :: <string>)
  let size  = ^entry-point-number-required(o);
  let index = min(size, $number-meps);
  let ref :: <runtime-object> = $rest-key-meps[index];
  o.emitted-name := ref;
  ref.runtime-object-name
end method;


define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <float>) => (reference :: <object>)
  emit-object(back-end, stream, o);
end method;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <boolean>) => (reference :: <object>)
  if (*emitting-data?*)
    emit-name(back-end, stream, o)
  else
    if (o) $true
    else $false
    end if;
  end if;
end method;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o) => (reference :: <object>)
  if (direct-object?(o))   // !@#$ need <direct-object>
    emit-object(back-end, stream, o)
  else
    emit-cg-name(back-end, stream, o)
  end if
end method;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <symbol>) => (reference :: <object>)
  if (*emitting-data?*)
    let local-symbol = element(heap-symbols(*current-heap*), o, default: #f);
    if (symbol-emitted?(local-symbol))
      local-symbol.cg-uninterned-symbol
    else
      emit-name(back-end, stream, o)
    end if;
  else
    next-method()
  end if;
end method;

//// CLASS-SPECIFIC EMISSION

// RAW TYPES

// Raw types have no run-time presence. The heap walker doesn't include
// raw types in the heap, but references may remain in some situations
// (signatures for example), although this problem may go away.

// Raw type references are replaced by references to a "static type"
// type marker. This is currently just <object>.

// TODO: Define a distinguished raw type marker is we still need to
// emit raw type references.

define method raw-type-marker () dylan-value(#"<object>") end;

define sideways method emit-reference
    (back-end :: <harp-back-end>, stream, o :: <&raw-type>) => (reference :: <object>)

  emit-reference(back-end, stream, raw-type-marker())
end method;

// WORK AROUND TO PREVENT WARNINGS

// Previously, this code was specialized on <back-end>, but
// then we'd get ambiguous method warnings.

define method emit-object
    (back-end :: <harp-back-end>, stream :: <stream>, o :: <&engine-node>) => (object);
  ^engine-node-callback(o);
  next-method()
end method;


// RAW VALUES

define sideways method emit-object
    (back-end :: <harp-back-end>, stream, o :: <&raw-object>) => (object :: <object>)
  o.^raw-object-value;
end method;

define function coerce-machine-word-to-an-integer(word) => (object :: <abstract-integer>)
  select (word by instance?)
    <integer> => word;
    <machine-word> =>
      // as(<abstract-integer>, word); // This should work - but doesn't
      make(<double-integer>, high: $minimum-unsigned-machine-word, low: word);
  end;
end function;

define sideways method emit-object
    (back-end :: <harp-back-end>, stream, o :: <&raw-single-float>) => (object :: <object>)
  let sfloat :: <single-float> = o.^raw-object-value;
  let bits = decode-single-float(sfloat);
  let result = coerce-machine-word-to-an-integer(bits);

  if (*emitting-data?*)
    result
  else
    ins--reference-sf-data(back-end, result);
  end if;
end method;

define sideways method emit-object
    (back-end :: <harp-back-end>, stream, o :: <&raw-double-float>) => (object :: <object>)
  let dfloat :: <double-float> = o.^raw-object-value;
  let (low, high) = decode-double-float(dfloat);

  if (*emitting-data?*)
    error("Should call emit-multiple-object in harp-cg-linker");
  else
    ins--reference-df-data(back-end,
			   coerce-machine-word-to-an-integer(low),
			   coerce-machine-word-to-an-integer(high));
  end if;
end method;

define sideways method emit-object
    (back-end :: <harp-back-end>, stream, c :: <&raw-byte-string>) => (object :: <string>)
  format-to-string("%=", c.^raw-object-value);
end method;

define sideways method emit-object
    (back-end :: <harp-back-end>, stream, o :: <&raw-boolean>) => (object :: <object>)
  if (o.^raw-object-value)
    $true
  else
    $false
  end if;
end method;

define sideways method emit-object
    (back-end :: <harp-back-end>, stream, o :: <&raw-byte-character>) => (object :: <object>)
  let raw-value :: <byte-character> = o.^raw-object-value;
  as(<integer>, raw-value)
end method;

// INTEGERS

define constant $max-raw-integer =   #x7FFFFFF;

define constant $min-raw-integer =   -#x8000000;

define sideways method emit-object (back-end :: <harp-back-end>, stream, c :: <integer>) => (object :: <abstract-integer>)
  if (c > $max-raw-integer | c < $min-raw-integer)
    generic-logior(generic-ash(c, 2), 1)
  else
    logior(ash(c, 2), 1)
  end if;
end method;

define sideways method emit-object (back-end :: <harp-back-end>, stream, c :: <character>) => (object :: <integer>)
  4 * as(<integer>, c) + 2;
end method;


define constant <backend-type> =
  one-of(#"dylan-object", #"raw-object", #"raw-single-float", #"raw-double-float");

define generic make-backend-type(type) => (backend-type :: <backend-type>);

define method make-backend-type(type :: <&raw-type>) => (backend-type :: <backend-type>)
  select(type.^debug-name by \=)
    "<raw-single-float>", "<raw-c-float>" => #"raw-single-float";
    "<raw-double-float>", "<raw-extended-float>", "<raw-c-double>" => #"raw-double-float";
    otherwise => #"raw-object";
  end select;
end method;

define method make-backend-type(type) => (backend-type :: <backend-type>)
  #"dylan-object"
end method;


define generic infer-backend-type(c, #key, #all-keys) => (backend-type :: <backend-type>);

define method infer-backend-type(c, #key, #all-keys) => (backend-type :: <backend-type>)
  #"dylan-object"
end method;

define method infer-backend-type(c :: <primitive-call>, #key index = 0) => (backend-type :: <backend-type>)
  let f = c.primitive;
  let multiple-types = f.primitive-signature.^signature-values;
  let type = multiple-types[index];
  make-backend-type(type);
end method;

define method infer-backend-type(c :: <c-variable-pointer-call>, #key, #all-keys) => (backend-type :: <backend-type>)
  #"raw-object"
end method;

define method infer-backend-type(c :: <&raw-object>, #key, #all-keys) => (backend-type :: <backend-type>)
  #"raw-object"
end method;

define method infer-backend-type(c :: <&raw-single-float>, #key, #all-keys) => (backend-type :: <backend-type>)
  #"raw-single-float"
end method;

define method infer-backend-type(c :: <&raw-double-float>, #key, #all-keys) => (backend-type :: <backend-type>)
  #"raw-double-float"
end method;

define method infer-backend-type(c :: <&raw-type>, #key, #all-keys) => (backend-type :: <backend-type>)
  make-backend-type(c)
end method;

define method infer-backend-type(c :: <repeated-slot-value>, #key, #all-keys) => (backend-type :: <backend-type>)
  let type = ^slot-type(computation-slot-descriptor(c));

  select(type)
    dylan-value(#"<byte-character>"),
    dylan-value(#"<unicode-character>"),
    dylan-value(#"<byte>"),
    dylan-value(#"<double-byte>"),
    dylan-value(#"<machine-word>"),
    dylan-value(#"<integer>") =>            #"raw-object";
    dylan-value(#"<single-float>")   =>     #"raw-single-float";
    dylan-value(#"<double-float>")   =>     #"raw-double-float";
    otherwise                        =>     #"dylan-object";
  end select;
end method;

define method infer-backend-type(c :: <lexical-specialized-variable>, #key, #all-keys) => (backend-type :: <backend-type>)
  let gen = c.generator;
  if (gen)
    infer-backend-type(gen)
  else
    make-backend-type(c.specializer)
  end if
end method;

define method infer-backend-type(c :: <check-type>, #key, #all-keys) => (backend-type :: <backend-type>)
  infer-backend-type(c.type)
end method;

define method infer-backend-type(c :: <extract-single-value>, #key, #all-keys) => (backend-type :: <backend-type>)
  infer-backend-type(c.computation-value, index: c.index);
end method;

define method infer-backend-type(c :: <binary-merge>, #key, #all-keys) => (backend-type :: <backend-type>)
  infer-backend-type(c.merge-left-value)
end method;

define method infer-backend-type(c :: <object-reference>, #key, #all-keys) => (backend-type :: <backend-type>)
  infer-backend-type(c.reference-value)
end method;

define method infer-backend-type(c :: <temporary>, #key, #all-keys) => (backend-type :: <backend-type>)
  infer-backend-type(c.generator)
end method;

define method infer-backend-type(c :: <make-cell>, #key, #all-keys) => (backend-type :: <backend-type>)
  if (closed-over?(c.temporary)) #"dylan-object"
  else infer-backend-type(c.computation-value)
  end if
end method;

define method infer-backend-type(c :: <get-cell-value>, #key, #all-keys) => (backend-type :: <backend-type>)
  infer-backend-type(c.computation-cell.generator.computation-value)
end method;

define method infer-backend-type(c :: <values>, #key, #all-keys) => (backend-type :: <backend-type>)
  let mvalues = c.fixed-values;
  if (empty?(mvalues)) #"dylan-object"
  else
    infer-backend-type(mvalues.first);
  end if
end method;


define method make-backend-register
  (back-end :: <harp-back-end>, type :: <backend-type>, name :: false-or(<string>))
 => (register :: <virtual-register>)
  select(type)
    #"dylan-object" => make-g-register(back-end, name: name);
    #"raw-object" => make-n-register(back-end, name: name);
    #"raw-single-float" => make-sf-register(back-end, name: name);
    #"raw-double-float" => make-df-register(back-end, name: name);
  end select;
end method;

define method emit-temporary
    (back-end :: <harp-back-end>, stream, o :: <temporary>,
     #key type) => (register :: <object>)

   cg-temporary(back-end, o)
   | (begin
	  let name = o.name & name-of-temporary(back-end, o);

          let backend-type =
	    if (type)
	      make-backend-type(type);
	    else
	      infer-backend-type(o);
	    end if;
	  let register = make-backend-register(back-end, backend-type, name);
          let loop? = #f;

	  if (o.loop-temporary?)
            let name = name & concatenate(name, "loop");
	    loop? := make-backend-register(back-end, backend-type, name);
	  end if;

          cg-temporary(back-end, o, loop?: loop?) := register;
      end);
end method emit-temporary;

define sideways method emit-object 
    (back-end :: <harp-back-end>, stream, o :: <entry-state>) => (object :: <object>)
  emit-temporary(back-end, stream, o);
end method;

define sideways method emit-object 
    (back-end :: <harp-back-end>, stream, o :: <temporary>) => (object :: <object>)
  emit-temporary(back-end, stream, o);
end method;

define sideways method emit-object 
(back-end :: <harp-back-end>, stream, o :: <lexical-variable>) => (object)

  emit-temporary(back-end, stream, o);

end method emit-object;

define method same-name? (x, y) => (same? :: <boolean>) x == y end;

define method same-name? 
    (x :: <variable-name-fragment>, y :: <variable-name-fragment>) => (same? :: <boolean>)
  x.fragment-identifier = y.fragment-identifier
end method;
  
define method ambiguous-lexical-variable? 
    (var :: <temporary>) 
 => (ambiguous? :: <boolean>)
  block (return)
    for (tmp in var.environment.temporaries)
      if (tmp ~== var 
          & same-name?(var.name, tmp.name))
        return(#t);
      end if;
    end for;
    #f
  end block;
end method;

define method name-of-temporary(back-end :: <harp-back-end>, o :: <temporary>) => (name)
  #f
end method name-of-temporary;

define method name-of-temporary(back-end :: <harp-back-end>, o :: <lexical-local-variable>) => (name :: <string>)
  if (o.frame-offset & ambiguous-lexical-variable?(o)) 
    hygienic-mangle(back-end, o.name, o.frame-offset);
  else
    local-mangle(back-end, o.name);
  end if;
end method name-of-temporary;

define method name-of-temporary(back-end :: <harp-back-end>, o :: <lexical-variable>) => (name :: <string>)
  local-mangle(back-end, o.name);
end method name-of-temporary;

define method name-of-temporary(back-end :: <harp-back-end>, o :: <named-temporary>) => (name :: <string>)
  if (o.frame-offset & ambiguous-lexical-variable?(o)) 
    hygienic-mangle(back-end, o.name, o.frame-offset);
  else
    local-mangle(back-end, o.name);
  end if;
end method name-of-temporary;

define method name-of-temporary(back-end :: <harp-back-end>, o :: <entry-state>) => (name :: <string>)
  local-mangle(back-end, o.name);
end method name-of-temporary;


define method emit-assignment
    (back-end :: <harp-back-end>, destination :: <register>, source, #rest ignored) => ()
  let op--move = op--move%(destination);

  // hack for threads
  if (instance?(source, <constant-reference>) 
        & source.constant-for-*current-handlers*?) 
    ins--ld-teb(back-end, destination, back-end.teb-current-handler-offset);
  else
    op--move(back-end, destination, source);
  end if;
end method emit-assignment;

// On Windows, DLL imports require indirections, and code is compiled
// differently statically and dynamically. This is not the case on Linux,
// where there is no extra indirection for imports.

define open generic emit-import-adjustment?
    (back-end :: <harp-back-end>) => (adjust? :: <boolean>);

define sideways method emit-import-adjustment?
    (back-end :: <harp-back-end>) => (adjust? :: <boolean>)
  #t
end method emit-import-adjustment?;

define method emit-import-assignment
    (back-end :: <harp-back-end>, destination :: <indirect-constant-reference>, source) => ()
  if (emit-import-adjustment?(back-end)
	& instance?(destination, <imported-constant-reference>))
    ins--store(back-end, source, destination, 0);
  else
    ins--move(back-end, destination, source);
  end if;
end method emit-import-assignment;

define method emit-assignment
(back-end :: <harp-back-end>, destination :: <indirect-constant-reference>, source, #rest ignored) => ()
  // hack for threads
  if (destination.constant-for-*current-handlers*?)
    ins--st-teb(back-end, source, back-end.teb-current-handler-offset);
  else
    emit-import-assignment(back-end, destination, source);
  end if;
end method emit-assignment;

define method emit-assignment
(back-end :: <harp-back-end>, destination :: <module-binding>, source, #rest ignored) => ()
  let cref = emit-module-reference(back-end, #f, destination);
  emit-assignment(back-end,
                  cref,
                  source);
end method emit-assignment;

define method emit-assignment
(back-end :: <harp-back-end>, destination :: <constant-reference>, source, #rest ignored) => ()
  error("emit-assignment in Dfmc Harp Cg: attempt to assign to a <constant-reference>");
  ins--move(back-end, destination, source);
end method emit-assignment;

define method emit-assignment
(back-end :: <harp-back-end>, destination == #f, source, #rest ignored) => ()
 source;
end method emit-assignment;

define method emit-assignment
(back-end :: <harp-back-end>, destination :: <temporary>, source, #rest ignored) => ()
  if (destination.used?)
    emit-assignment(back-end,
		    emit-reference(back-end, *harp-outputter*, destination),
		    source);
  end if;
end method emit-assignment;

define method emit-assignment
(back-end :: <harp-back-end>, destination, source, #rest ignored) => ()
 emit-assignment(back-end,
                 emit-reference(back-end, *harp-outputter*, destination),
                 source);
end method emit-assignment;

define method emit-result-assignment(back-end :: <harp-back-end>, result) => ()
  if (result)
    let op--move-outof-result = op--move-outof-result%(result);
    op--move-outof-result(back-end, result);
  end if;
end method emit-result-assignment;


// Register runtime-objects

define function runtime-reference(object) => (reference)
  select(object by instance?)
    <runtime-object> =>
      register-extern(current-back-end(), object);
    <constant-reference> =>
      register-extern(current-back-end(), object);
      object;
    otherwise => object;
  end select;
end function runtime-reference;


// Map entry-points to runtime-objects

define function entry-point-reference(entry-point :: <string>) => (reference :: <runtime-object>)
  $named-entry-points[entry-point];
end function entry-point-reference;


define method reg-float-result%(back-end :: <harp-back-end>) => (register :: <register>)
 select(back-end.cg-variables.current-lambda.function by instance?)
   <&c-callable-function> => back-end.registers.reg-c-float-result;
   <&lambda> => back-end.registers.reg-float-result;
 end select;
end method reg-float-result%;

define method reg-result%(back-end :: <harp-back-end>) => (register :: <register>)
 select(back-end.cg-variables.current-lambda.function by instance?)
   <&c-callable-function> => back-end.registers.reg-c-result;
  <&lambda> => back-end.registers.reg-result;
 end select;
end method reg-result%;

define harp-operation move(<object>, <function>) ins--move;
define harp-operation move(<greg>, <function>) ins--move;
define harp-operation move(<nreg>, <function>) ins--move;
define harp-operation move(<sfreg>, <function>) ins--fmove;
define harp-operation move(<sf-constant-reference>, <function>) ins--fmove;
define harp-operation move(<dfreg>, <function>) ins--dmove;
define harp-operation move(<df-constant-reference>, <function>) ins--dmove;

define constant op--move-into-result =
  method(back-end :: <harp-back-end>, result) => ()
      ins--move(back-end, back-end.reg-result%, result);
  end;

define constant op--fmove-into-result =
  method(back-end :: <harp-back-end>, result) => ()
      ins--fmove(back-end, back-end.reg-float-result%, result);
  end;

define constant op--dmove-into-result =
  method(back-end :: <harp-back-end>, result) => ()
      ins--dmove(back-end, back-end.reg-float-result%, result);
  end;

define harp-operation move-into-result(<object>, <function>) op--move-into-result;
define harp-operation move-into-result(<greg>, <function>) op--move-into-result;
define harp-operation move-into-result(<nreg>, <function>) op--move-into-result;
define harp-operation move-into-result(<sfreg>, <function>) op--fmove-into-result;
define harp-operation move-into-result(<sf-constant-reference>, <function>) op--fmove-into-result;
define harp-operation move-into-result(<dfreg>, <function>) op--dmove-into-result;
define harp-operation move-into-result(<df-constant-reference>, <function>) op--dmove-into-result;

define constant op--move-outof-result =
  method(back-end :: <harp-back-end>, result) => ()
      ins--move(back-end, result, back-end.registers.reg-result);
  end;

define constant op--fmove-outof-result =
  method(back-end :: <harp-back-end>, result) => ()
      ins--fmove(back-end, result, back-end.registers.reg-float-result);
  end;

define constant op--dmove-outof-result =
  method(back-end :: <harp-back-end>, result) => ()
      ins--dmove(back-end, result, back-end.registers.reg-float-result);
  end;

define harp-operation move-outof-result(<object>, <function>) op--move-outof-result;
define harp-operation move-outof-result(<greg>, <function>) op--move-outof-result;
define harp-operation move-outof-result(<nreg>, <function>) op--move-outof-result;
define harp-operation move-outof-result(<sfreg>, <function>) op--fmove-outof-result;
define harp-operation move-outof-result(<dfreg>, <function>) op--dmove-outof-result;

define constant op--move-outof-argument =
  method(back-end :: <harp-back-end>, argument, index :: <integer>) => ()
      ins--move(back-end, argument,
		element(back-end.reg-machine-arguments%, index));
  end;

define constant op--fmove-outof-argument =
  method(back-end :: <harp-back-end>, argument, index :: <integer>) => ()
      ins--fmove(back-end, argument,
		 element(back-end.reg-float-machine-arguments%, index));
  end;

define constant op--dmove-outof-argument =
  method(back-end :: <harp-back-end>, argument, index :: <integer>) => ()
      ins--dmove(back-end, argument,
		 element(back-end.reg-float-machine-arguments%, index));
  end;

define harp-operation move-outof-argument(<object>, <function>) op--move-outof-argument;
define harp-operation move-outof-argument(<greg>, <function>) op--move-outof-argument;
define harp-operation move-outof-argument(<nreg>, <function>) op--move-outof-argument;
define harp-operation move-outof-argument(<sfreg>, <function>) op--fmove-outof-argument;
define harp-operation move-outof-argument(<dfreg>, <function>) op--dmove-outof-argument;

define constant op--move-into-c-argument =
  method(back-end :: <harp-back-end>, argument, index :: <integer>) => ()
      ins--move(back-end,
		element(back-end.registers.reg-c-machine-arguments, index),
		argument);
  end;

define constant op--fmove-into-c-argument =
  method(back-end :: <harp-back-end>, argument, index :: <integer>) => ()
      ins--fmove(back-end,
		 element(back-end.registers.reg-c-float-machine-arguments, index),
		 argument);
  end;

define constant op--dmove-into-c-argument =
  method(back-end :: <harp-back-end>, argument, index :: <integer>) => ()
      ins--dmove(back-end,
		 element(back-end.registers.reg-c-float-machine-arguments, index),
		 argument);
  end;

define harp-operation move-into-c-argument(<object>, <function>) op--move-into-c-argument;
define harp-operation move-into-c-argument(<greg>, <function>) op--move-into-c-argument;
define harp-operation move-into-c-argument(<nreg>, <function>) op--move-into-c-argument;
define harp-operation move-into-c-argument(<sfreg>, <function>) op--fmove-into-c-argument;
define harp-operation move-into-c-argument(<sf-constant-reference>, <function>) op--fmove-into-c-argument;
define harp-operation move-into-c-argument(<dfreg>, <function>) op--dmove-into-c-argument;
define harp-operation move-into-c-argument(<df-constant-reference>, <function>) op--dmove-into-c-argument;

define harp-operation load-index(<object>, <function>) op--load-index;
define harp-operation load-index(<greg>, <function>) op--load-index;
define harp-operation load-index(<nreg>, <function>) op--load-index;
define harp-operation load-index(<sfreg>, <function>) op--load-float-index;
define harp-operation load-index(<dfreg>, <function>) op--load-dfloat-index;

define harp-operation store-index(<object>, <function>) op--store-index;
define harp-operation store-index(<greg>, <function>) op--store-index;
define harp-operation store-index(<nreg>, <function>) op--store-index;
define harp-operation store-index(<sfreg>, <function>) op--store-float-index;
define harp-operation store-index(<sf-constant-reference>, <function>) op--store-float-index;
define harp-operation store-index(<dfreg>, <function>) op--store-dfloat-index;
define harp-operation store-index(<df-constant-reference>, <function>) op--store-dfloat-index;

define harp-operation make-box(<object>, <constant-reference>) $primitive-make-box;
define harp-operation make-box(<greg>, <constant-reference>) $primitive-make-box;
define harp-operation make-box(<integer>, <constant-reference>) $primitive-make-raw-box;
define harp-operation make-box(<nreg>, <constant-reference>) $primitive-make-raw-box;
define harp-operation make-box(<sfreg>, <constant-reference>) $primitive-make-single-float-box;
define harp-operation make-box(<sf-constant-reference>, <constant-reference>) $primitive-make-single-float-box;
define harp-operation make-box(<dfreg>, <constant-reference>) $primitive-make-double-float-box;
define harp-operation make-box(<df-constant-reference>, <constant-reference>) $primitive-make-double-float-box;
