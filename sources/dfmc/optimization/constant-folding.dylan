Module: dfmc-optimization
Author: Bob Cassels 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define compilation-pass constant-fold,
//   visit: computations,
//   optimization: low,
//   after: eliminate-assignments,
//   before: analyze-calls;

define method constant-fold (c :: <computation>)
  #f
end method;

define method fold-if-merge! 
    (c :: <if>, sav-first :: <computation>, sav-last  :: <computation>, 
     sav-value :: false-or(<value-reference>), del-first :: <computation>)
 => ()
  let f = lambda(environment(c));
  let merge-c = next-computation(c);
  // SPLICE SAVED BRANCH BEGINNING IN PLACE OF IF
  let pc = previous-computation(c);
  let nc = next-computation(merge-c);
  re-optimize(pc);
  if (sav-first == merge-c)
    redirect-previous-computations!(c, nc);
  else
    previous-computation(sav-first) := pc;
    // Redirect the previous computations (c in this case) of if, to have
    // sav-first as their next computation
    redirect-previous-computations!(c, sav-first);
  end if;
  // SPLICE SAVED BRANCH ENDING IN PLACE OF MERGE
  re-optimize(nc); 
  if (sav-last == c)
    redirect-next-computations!(merge-c, pc);
  else
    next-computation(sav-last) := nc;
    // Redirect the next computations (nc in this case) of merge-c, to have
    // sav-last as their previous computation
    redirect-next-computations!(merge-c, sav-last);
  end if;
  // REPLACE MERGE'S TEMPORARY WITH TMP FROM TT SRC OF SAVED BRANCH
  replace-temporary-in-users!(merge-c.temporary, sav-value);
  // DELETE MERGE
  remove-computation-references!(merge-c);
  // RIP OUT DEAD BRANCH
  remove-computation-block-references!
    (del-first, merge-c, 
     on-deletion: if (*colorize-dispatch*) color-as-eliminated-if-call else identity end);
  // REMOVE LAST OF IF
  remove-computation-references!(c);
end method;

define method primitive-call-temporary? 
    (ref :: <value-reference>, name :: <symbol>) => (res :: singleton(#f))
  #f
end method;

define method primitive-call-temporary? 
    (ref :: <temporary>, name :: <symbol>)
 => (res :: type-union(<boolean>, <primitive-call>))
  let gen = generator(ref);
  instance?(gen, <primitive-call>) & primitive(gen) == dylan-value(name)
end method;

define method constant-fold-if (c :: <if>, test-value)
  let merge = c.next-computation;
  let (sav-first, sav-last, sav-value, del-first) 
    = if (test-value) 
	 values(consequent(c), merge-left-previous-computation(merge),  
		 merge-left-value(merge), alternative(c))
      else 
	 values(alternative(c), merge-right-previous-computation(merge),  
		 merge-right-value(merge), consequent(c))
      end if;
  fold-if-merge!(c, sav-first, sav-last, sav-value, del-first);
  #t
end method;

define method constant-fold (c :: <if>)
  let tst = test(c);
  let (test-constant?, test-value) = fast-constant-value?(tst);
  if (test-constant?)
    constant-fold-if(c, test-value);
    #t
  elseif (primitive-call-temporary?(tst, #"primitive-not"))
    invert-if-branches!(c);
    replace-temporary-references!(c, tst, first(arguments(generator(tst))));
    #t
  else
    let test-estimate  = type-estimate(tst);
    let false-estimate = as(<type-estimate>, dylan-value(#"<boolean>"));
    // TODO: should use singleton(#f) but this conses
    when (guaranteed-disjoint?(test-estimate, false-estimate))
      constant-fold-if(c, #t);
      #t
    end when;
  end if;
end method;

define function invert-if-branches! (c :: <if>) 
  let merge              = next-computation(c);
  let old-consequent     = consequent(c);
  let old-alternative    = alternative(c);
  let old-previous-left  = merge-left-previous-computation(merge);
  let old-previous-right = merge-right-previous-computation(merge);
  let old-left-value     = merge-left-value(merge);
  let old-right-value    = merge-right-value(merge);
  consequent(c)  := old-alternative;
  alternative(c) := old-consequent;
  merge-left-previous-computation(merge)  := old-previous-right;
  merge-right-previous-computation(merge) := old-previous-left;
  merge-replace-left-value!(merge, old-left-value, old-right-value);
  merge-replace-right-value!(merge, old-right-value, old-left-value);
end function;

define method color-as-eliminated-if-call (c :: <computation>) end;

define method color-as-eliminated-if-call (c :: <function-call>)
  color-dispatch(c, #"eliminated");
end;

define method constant-fold (c :: <binary-merge>)
  let left-value = merge-left-value(c);
  let right-value = merge-right-value(c);
  if (left-value & right-value) 
    let left-g  = generator(left-value);
    let right-g = generator(right-value);
    if (left-value == right-value |
	  guaranteed-joint?(type-estimate(left-value),
			    dylan-value(#"<bottom>")))
      // unless (left-value == right-value)
      //   format-out(" MERGE %= BOTTOM LV %=\n", c, left-value);
      // end unless;
      replace-computation-with-temporary!(c, right-value);
      #t
    elseif (guaranteed-joint?(type-estimate(right-value),
			      dylan-value(#"<bottom>")))
      // unless (left-value == right-value)
      //   format-out(" MERGE %= BOTTOM RV %=\n", c, right-value);
      // end unless;
      replace-computation-with-temporary!(c, left-value);
      #t
    end
  end if;
end method;

define method constant-fold (c :: <if-merge>) 
  #f 
end method;

/// HACK: TEMPORARY UNTIL WE CAN FIX RECURSIVE TYPE-INFERENCE

define method constant-fold (c :: <loop-merge>) 
  // next-method();
  #f
end method;

define method constant-fold (c :: <bind-exit-merge>) 
  // next-method();
  #f
end method;

define method fast-constant-argument-value?
    (ref :: <object-reference>) => (well? :: <boolean>, res)
  values(#t, reference-value(ref))
end method;

define method fast-constant-argument-value?
    (ref :: <method-reference>) => (well? :: <boolean>, res)
  let function = reference-value(ref);
  ensure-optimized-method-model(function);
  values(#t, function)
end method;

define method fast-constant-argument-value?
    (ref) => (well? :: <boolean>, res)
  values(#f, #f)
end method;

define inline function fast-constant-argument-value (ref) => (value)
  let (constant?, value) = fast-constant-argument-value?(ref);
  value
end function;

define method fold-function-call (t == #f,
				  function, arguments) => (call-values)
  apply(function, map(fast-constant-argument-value, arguments));
  // result ignored
  #f
end;

define method replace-call-with-values (call-value,
					call :: <call>,
					t == #f) => ()
  delete-computation!(call);
end;

define method fold-function-call (t :: <multiple-value-temporary>,
				  function, arguments) => (call-values)
  let (#rest call-values) = apply(function, map(fast-constant-argument-value, arguments));
  call-values
end;

define method replace-call-with-values (call-values,
					call :: <call>,
					t :: <multiple-value-temporary>) => ()
  let values-temps = map(make-object-reference, call-values);
  // format-out("XXX doing %=.\n", call);
  let padded-values 
    = apply(pad-multiple-values, 
            call.environment, 
            temporary-value-context(t),
            values-temps);              
  let (values-c, values-t) =
    make-with-temporary(environment(call),
                        <values>,
                        values: padded-values,
                        temporary-class: <multiple-value-temporary>);
  values-t.required-values := size(padded-values);
  values-t.rest-values? := #f;
  // format-out("\tgot %=.\n", values-c);
  replace-computation!(call, values-c, values-c, values-t);
end;

define method fold-function-call (t :: <temporary>,
				  function, arguments) => (call-value)
  apply(function, map(fast-constant-argument-value, arguments))
end;
 
define method replace-call-with-values (call-value,
					call :: <call>,
					t :: <temporary>) => ()
  replace-computation-with-temporary!(call, make-object-reference(call-value));
end;

define function maybe-fold-function-call (call, t, function, arg-t*)
  let (call-values, okay?) =
    block ()
      values(fold-function-call(t, function, arg-t*), #t)
    exception (e :: <error>)
      values(#f, #f)		// silently fail to fold
    end;
  if (okay?)
    re-optimize-generators(call.arguments);
    replace-call-with-values(call-values, call, t);
    #t
  end;
end;

// define function variable-slot-access?
//     (function :: <&function>, folder-function :: <function>, call-arguments)
//  => (well? :: <boolean>)
//   // TODO: This ^iep thing is a hack and a half. Remove it when we can
//   // prove it's not necessary.
//   if (folder-function ~== ^iep & size(call-arguments) == 1)
//     let arg = constant-value(call-arguments.first);
//     let class = ^object-class(arg); // Because we get a raw sometimes! 8(
//     if (instance?(class, <&class>))
//       let slotd = ^slot-descriptor(class, function);
//       if (slotd & ^slot-setter(slotd))
//         format-out("!!!Slot access denied: %=.%=\n", arg, function);
//         // #t
//         #f
//       end;
//     end;
//   end;
// end function;

define method constant-fold (c :: <function-call>)
  let (function-constant?, function) = fast-constant-value?(function(c));
  let call-arguments = arguments(c);
  if (function-constant? &
	every?(fast-constant-argument-value?, call-arguments))
    let compile-stage-function = lookup-compile-stage-function(function);
    if (compile-stage-function 
          /* & ~variable-slot-access?
               (function, compile-stage-function, call-arguments) */ )
      maybe-fold-function-call(c, temporary(c),
			       compile-stage-function,
			       call-arguments)
    end
  end
end method;

/*
define method read-only-slot-access? (ref) => (well? :: <boolean>)
  #f
end method;

/// HACK: DON'T HARD CODE THIS INFORMATION HERE
/// HACK: WANT TO INFER THIS WITH SIMILAR WALK TO DYNAMIC-EXTENT
define method read-only-slot-access? (ref :: <simple-call>) => (well? :: <boolean>)
  let (constant-value?, fun) = fast-constant-value?(function(ref));
  if (constant-value?)
    select (fun)
      dylan-value(#"element-range-error") => #t;
      dylan-value(#"aref-rank-error")     => #t;
      otherwise                           => #f;
    end select;
  else 
    #f
  end if;
end method;

define method read-only-slot-access? (ref :: <slot-value>) => (well? :: <boolean>)
  #t
end method;

define method read-only-slot-access? (ref :: <repeated-slot-value>) => (well? :: <boolean>)
  #t
end method;

define method read-only-slot-access? (ref :: <slot-value-setter>) => (well? :: <boolean>)
  #f
end method;

define method read-only-slot-access? (ref :: <repeated-slot-value-setter>) => (well? :: <boolean>)
  #f
end method;

define method read-only-reference? (ref :: <value-reference>)
  every?(read-only-slot-access?, users(ref))
end method;
*/

// TODO: HIDE TAG DETAILS TO MAKE THIS LESS SPECIFIC

// define constant $number-tag-bits = 2;
// define constant $max-tag-bit     = ash(1, $number-tag-bits);
// define constant $tag-bit-mask    = $max-tag-bit - 1;

define constant $number-tag-bits = 2; // HACK: SHOULDN'T HARD CODE THIS IN!!!!

define method constant-fold (c :: <repeated-slot-value>)
  let index-ref    = computation-index(c);
  let (index-constant?, raw-index) = fast-constant-value?(index-ref);
  if (index-constant?)
    let instance-ref = computation-instance(c);
    let vec = maybe-vector-element-references(instance-ref);
    if (vec)
      let index
	= ash(as(<integer>, ^raw-object-value(raw-index)),
	      if (computation-index-tagged?(c)) 
		-$number-tag-bits
	      else
		0
	      end);
      let ref = element(vec, index);
      replace-computation-with-temporary!(c, ref);
      #t
    else 
      #f
    end if;
  else 
    #f
  end if;
  // REALLY NEED OTHER DIRECTION CAUSE THIS DIRECTION IS NOW THE DEFAULT
  /*
  let gen-index = generator(computation-index(c));
  if (primitive-call-to?(gen-index, #"primitive-machine-word-shift-right"))
    let (constant?, raw-amount) 
      = fast-constant-value?(arguments(gen-index)[1]);
    if (constant?)
      let amount :: <integer> = as(<integer>, ^raw-object-value(raw-amount));
      if (amount = $number-tag-bits)
	let new-index = arguments(gen-index)[0];
	remove-user!(computation-index(c), c);
	add-user!(new-index, c);
	computation-index(c) := new-index;
	computation-index-tagged?(c) := #t;
      end if;
    end if;
  end if;
  */
end method;

define method constant-fold (c :: <repeated-slot-value-setter>)
  #f
end method;

define method constant-fold (c :: <stack-vector>)
  /*
  let args = c.arguments;
  if (every?(fast-constant-argument-value?, args))
    let vec = map-as(<vector>, compose(compile-stage, fast-constant-argument-value), args);
    let ref = make-value-reference(vec, <immutable-object-reference>);
    replace-computation-with-temporary!(c, ref);
    #t
  else 
    #f
  end if;
  */
  #f
end method;

define method constant-fold (c :: <primitive-call>)
  /*
  let function = primitive(c);
  let call-arguments = arguments(c);
  if (every?(fast-constant-value?, call-arguments))
    let compile-stage-function = lookup-compile-stage-function(function);
    if (compile-stage-function)
      maybe-fold-function-call(c, temporary(c),
			       compile-stage-function,
			       call-arguments)
    end
  end
  */
  let arguments = c.arguments;
  if (every?(fast-constant-argument-value?, arguments))
    block ()
      // TODO: DOESN"T HANDLE MULTIPLE-VALUE RETURNING PRIMITIVES
      let prim = c.primitive;
      let function = prim & prim.primitive-value;
      if (function)
        let result
          = apply(function,
                  map(compose(compile-stage, fast-constant-argument-value), arguments));
        unless (instance?(result, <unknown>))
          let result-c-tmp = make-object-reference(result);
          replace-computation-with-temporary!(c, result-c-tmp);
          #t
        end unless;
      end if;
    exception (e :: <error>)
      // format-out("Failed to fold %= on: %=\n", c, e);
    end block;
  end if;
end method;

define method constant-fold (c :: <extract-single-value>)
  single-value-propagation(c)
end;

define method constant-fold (c :: <extract-rest-value>)
  single-value-propagation(c)
end;

/*
define method constant-fold (c :: <adjust-multiple-values-computation>)
  next-method();
end method;
*/

define method constant-fold (c :: <adjust-multiple-values>)
  let env = environment(c);
  let values-t = computation-value(c);
  let values-te = type-estimate(values-t);
  let n = number-of-required-values(c);
  local method right-number-of-values? (te :: <type-estimate>)
	  size(type-estimate-fixed-values(te)) = n &
	    ~type-estimate-rest-values(te)
	end;
  if (select (values-te by instance?)
	<type-estimate-bottom> => 
	  #f;
	<type-estimate-values> =>
	  right-number-of-values?(values-te);
	<type-estimate-union> =>
	  every?(right-number-of-values?,
		 type-estimate-unionees(values-te));
      end)
    replace-computation-with-temporary!(c, values-t);
    #t
  elseif (n == 0)
    let (values-c, values-t) =
      make-with-temporary(env,
			  <values>,
			  values: #[],
			  temporary-class: <multiple-value-temporary>);
    replace-computation!(c, values-c, values-c, values-t);
    #t
  else
    let values-generator = generator(values-t);
    if (instance?(values-generator, <values>))
      let fixed-values = fixed-values(values-generator);
      let count = size(fixed-values);
      let (values-first, values-last, values-t) =
	if (count > n)
	  make-with-temporary*(env,
			       <values>,
			       values: copy-sequence(fixed-values, end: n),
			       temporary-class: <multiple-value-temporary>)
	elseif (~rest-value(values-generator))
	  let false-t =
	    make-object-reference(#f);
	  let adjusted-fixed-values =
	    replace-subsequence!(make(<simple-object-vector>,
				      size: n,
				      fill: false-t),
				 fixed-values,
				 end: count);
	  let (values-c, values-t) =
	    make-with-temporary(env,
				<values>,
				values: adjusted-fixed-values,
				temporary-class: <multiple-value-temporary>);
	  values(values-c, values-c, values-t);
	else
	  #f
	end;
      if (values-t)
        values-t.required-values := count;
        values-t.rest-values? := #f;
	replace-computation!(c, values-first, values-last, values-t);
	#t
      else
	#f
      end
    else
      #f
    end
  end
end;

define function weak-closure? 
    (env :: <lambda-lexical-environment>) => (well? :: <boolean>)
  every?(method (tmp) closed-over?(tmp) ~== $strong-closure-entry end, 
         env.closure)
end function;

define method prune-closure (env :: <lambda-lexical-environment>) => ()
  // format-out("PRUNING CLOSURE %= :: ", lambda(env));
  // for (tmp in env.closure)
  //   format-out("%= %= ", tmp, closed-over?(tmp));
  // end for;
  // format-out("\n");
  unless (empty?(env.closure))
    block (return)
      local method ensure-weak-closure (env) weak-closure?(env) | return() end;
      if (weak-closure?(env))
        do-over-lambda-users(ensure-weak-closure, env);
        // We would've leapt out by now on any failure.
        // format-out("  DELETING CLOSURE %= %=\n", lambda(env), env.closure);
        do (method (tmp) closed-over?(tmp) := $no-closure-entry end, 
            env.closure);
        env.closure := #()
      end if;
    end block;
  end unless;
end method;

define method constant-fold-closure (c)
end method;

define method constant-fold-closure (f :: <&lambda>)
  let c = lambda-make-closure(f);
  if (c)
    let lambda  = computation-closure-method(c);
    let sigtmp  = computation-signature-value(c);
    let env     = environment(lambda);
    let closure = env.closure;
    local method maybe-delete-init-closure (init-closure)
            if (init-closure)
              delete-computation!(init-closure);
              computation-init-closure(c) := #f;
            end if;
          end method;
    // format-out("CONSTANT-FOLDING-CLOSURE %=\n", lambda);
    if (empty?(closure))
      if (~sigtmp & ~closed-over?(temporary(c)))
	// format-out("DELETING MAKE-CLOSURE\n");
	// print-method-out(lambda);
	let ref = make(<method-reference>, value: lambda);
	add-user!(lambda, ref);
	replace-computation-with-temporary!(c, ref);
        maybe-delete-init-closure(computation-init-closure(c));
      end if
    elseif (~any?(method (tmp) instance?(generator(tmp), <make-closure>) end, 
                  closure))
      // format-out("DELETING UNNECESSARY INIT-CLOSURE %=\n", f);
      maybe-delete-init-closure(computation-init-closure(c));
    end if;
  end if;
end method;

define method constant-fold (c :: <initialize-closure>)
  let closure = computation-closure(c);
  if (instance?(closure, <temporary>))
    let make-closure-c = generator(closure);
    if (size(users(closure)) = 1)
      // format-out("DELETE INIT-CLOSURE TMP %= USERS %=\n", 
      //            closure.generator, users(closure));
      delete-computation!(c);
      re-optimize(make-closure-c);
      #t
    end if;
  else
    // format-out("DELETE INIT-CLOSURE REF %=\n", closure);
    delete-computation!(c);
    #t
  end if;
end method;

define method constant-fold (c :: <make-closure>)
  let lambda = computation-closure-method(c);
  let sigtmp = computation-signature-value(c);
  if (sigtmp)
    let (constant-value?, constant-value) = fast-constant-value?(sigtmp);
    if (constant-value?)
      ^function-signature(lambda) := constant-value;
      computation-signature-value(c) := #f;
      re-optimize-users(c.temporary);
      re-optimize-type-estimate(c);
      #t
    end if;
  // elseif (~lambda-has-free-references?(lambda))
  //   let ref = make(<method-reference>, value: lambda);
  //   re-optimize-users(c.temporary);
  //   add-user!(lambda, ref);
  //   replace-computation-with-temporary!(c, ref);
  //   #t
  end if;
end method;

/*
define method constant-fold (c :: <make-closure>)
  let lambda = computation-closure-method(c);
  let sigtmp = computation-signature-value(c);
  local method fold ()
          let ref = make(<method-reference>, value: lambda);
          add-user!(lambda, ref);
          re-optimize-users(c.temporary);
          replace-computation-with-temporary!(c, ref);
        end method,
        method maybe-fold-signature (sigtmp)
          if (sigtmp)
	    let (constant-value?, constant-value) 
              = fast-constant-value?(sigtmp);
	    if (constant-value?)
	      ^function-signature(lambda) := constant-value;
	      computation-signature-value(c) := #f;
	      re-optimize-users(c.temporary);
	      re-optimize-type-estimate(c);
	      #t
	    end if;
          end if;
        end method,
        method maybe-fold-closure (sigtmp)
          if (sigtmp)
            maybe-fold-signature(sigtmp);
          else
            fold();
            #t
          end if;
        end method;
  if (computation-maybe-free-references?(c))
    maybe-fold-signature(sigtmp);
  else
    if (computation-no-free-references?(c))
      maybe-fold-closure(sigtmp)
    elseif (environment(lambda))
      let free-refs = lambda-has-free-lexical-references?(lambda);
      if (free-refs)
        computation-maybe-free-references?(c) := #t;
      else
        computation-no-free-references?(c) := #t;
        maybe-fold-closure(sigtmp);
        #t
      end if
    else
      maybe-fold-signature(sigtmp);
    end if;
  end if
end method;
*/

define method constant-fold (c :: <adjust-multiple-values-rest>)
  let values-t = computation-value(c);
  let values-te = type-estimate(values-t);
  let n = number-of-required-values(c);
  local method right-number-of-values? (te :: <type-estimate>)
	  size(type-estimate-fixed-values(te)) >= n 
	end;
  if (select (values-te by instance?)
	<type-estimate-bottom> => 
	  #f;
	<type-estimate-values> =>
	  right-number-of-values?(values-te);
	<type-estimate-union> =>
	  every?(right-number-of-values?,
		 type-estimate-unionees(values-te));
      end)
    replace-computation-with-temporary!(c, values-t);
    #t
  else
    let values-generator = generator(values-t);
    if (instance?(values-generator, <values>))
      let env = environment(c);
      let fixed-values = fixed-values(values-generator);
      let count = size(fixed-values);
      if (count < n & ~rest-value(values-generator))
	let false-t =
	  make-object-reference(#f);
	let adjusted-fixed-values =
	  replace-subsequence!(make(<simple-object-vector>,
				    size: n,
				    fill: false-t),
			       fixed-values,
			       end: count);
	let (values-c, values-t) =
	  make-with-temporary(env,
			      <values>,
			      values: adjusted-fixed-values,
			      temporary-class: <multiple-value-temporary>);
        values-t.required-values := n;
        values-t.rest-values? := #f;
	replace-computation!(c, values-c, values-c, values-t);
	#t
      end
    else
      #f
    end
  end
end;

define function extract-and-optimize-constant (value :: <value-reference>)
 => (constant)
  let constant = extract-constant(value);
  if (instance?(constant, <&lambda>) & ~lambda-top-level?(constant))
    // since this is an inner lambda which
    // isn't part of the outer lambda anymore,
    // finish optimizing it separately
    run-compilation-passes(constant);
  end;
  constant
end;

define method constant-fold (c :: <definition>)
  // The assignment computation is preserved in the case of a thread
  // variable because the thread variable location always has to be
  // dynamically allocated and initialized.
  let binding = assigned-binding(c);
  (~binding-thread?(binding))
    & begin
        let value = computation-value(c);
        let constant? = extractable-constant-value?(value);
        if (constant?)
          let definition
            = untracked-binding-definition(binding, default: #f);
          if (definition & form-dynamic?(definition))
	    #f
	  else
	    binding-model-object(binding)
	      := extract-and-optimize-constant(value);
	    delete-computation!(c);
	    #t
          end if;
        else
          #f
        end
      end;
end method;

define method constant-fold (c :: <type-definition>)
  // WRT the above comment, I assume all thread variables share a
  // single type value.
  let value = computation-value(c);
  let constant? = extractable-constant-value?(value);
  if (constant?)
    /*
    format-out("Installing constant type for %= - %=\n",
               typed-binding(c), value);
    */
    binding-type-model-object(typed-binding(c))
      := extract-and-optimize-constant(value);
    delete-computation!(c);
    #t
  else
    #f
  end
end method;

define method constant-fold (c :: <keyword-default>)
  let merge = next-computation(c);
  let merge-temp = temporary(merge);
  if (~empty?(users(merge-temp)))
    let value = computation-value(c);
    let constant? = extractable-constant-value?(value);
    if (constant?)
      keyword-default-value-specifiers(c)[keyword-default-value-index(c) * 2 +
					    1]
	:= extract-and-optimize-constant(value);
      replace-temporary-in-users!(merge-temp,
				  keyword-default-value-keyword-variable(c));
      re-optimize(merge);
      delete-useless-computations(merge);
      #t
    else
      #f
    end if
  end if
end method;

define method constant-fold (c :: <temporary-transfer>)
  // after assignment conversion, all the temporary transfers can go away
  let tmp = computation-value(c);
  replace-computation-with-temporary!(c, tmp);
  #t
end method;

define method constant-fold (c :: <guarantee-type>)
  let static-type = static-guaranteed-type(c);
  if (static-type)
    let value = computation-value(c);
    if (guaranteed-joint?(type-estimate(value), static-type))
      replace-computation-with-temporary!(c, value);
      #t
    end
  else
    let type-t = guaranteed-type(c);
    let (type-constant?, the-type) = fast-constant-value?(type-t);
    if (type-constant?)
      static-guaranteed-type(c) := the-type;
      guaranteed-type(c) := #f;
      remove-user!(type-t, c);
      #t
    end
  end
end method;

define program-warning <run-time-type-error>
  slot condition-inferred-type,
    required-init-keyword: inferred-type:;
  slot condition-expected-type,
    required-init-keyword: expected-type:;
  format-string 
    "Type check can fail - %s inferred, %s expected.";
  format-arguments
    inferred-type, expected-type;
end program-warning;

define program-warning <run-time-result-type-error>
    (<run-time-type-error>)
  format-string 
    "Result type check can fail - %s inferred, %s expected.";
end program-warning;

define method run-time-type-error-class (c :: <check-type-computation>)
  <run-time-type-error>
end method;

define method run-time-type-error-class (c :: <result-check-type-computation>)
  <run-time-result-type-error>
end method;

define method evaluate-type-checks? (c :: <check-type>)
  let (type-constant?, the-type) = fast-constant-value?(type(c));
  if (type-constant? & instance?(the-type, <&type>))
    let the-estimate 
      = type-estimate(computation-value(c));
    if (guaranteed-joint?(the-estimate, the-type))
      #t
    else
      // if (guaranteed-disjoint?(the-estimate, the-type))
      if (effectively-disjoint?(the-estimate, the-type))
	note(<run-time-type-error>,
	     source-location: dfm-source-location(c),
	     context-id:      dfm-context-id(c),
	     inferred-type: the-estimate,
	     expected-type: the-type);
      end;
      #f
    end;
  end;
end method evaluate-type-checks?;

define method evaluate-type-checks? (c :: <constrain-type>)
  unless (type(c))
    let the-estimate 
      = type-estimate(computation-value(c));
    type-estimate-subtype?
      (the-estimate, as(<type-estimate>, dylan-value(#"<object>")))
  end;
end method evaluate-type-checks?;

// TODO: Calls to "values" aren't being type-inferred correctly at the 
// moment. When they are, beef up this checking appropriately. Things
// to do:
//
//   o Spot too few multiple values as a special case so that it can be
//     reported more usefully than "#f isn't of type xyz".
//
//   o Get more specific summary information somehow - distinguish 
//     multiple value bind checks from return type checks by class
//     so that error messages can be made more meaningful, preferably
//     reporting the variable names involved where possible.

// *** This has no callers?  (Which is good, since it has no implementation!)
// define method check-type-check 
//     (c :: <multiple-value-check-type-computation>, check-estimate)
// end method;

define function make-type-estimate-for-fixed-check (type-temp*)
  // Construct the type estimate which is used for fixed-return-value
  // type-checking done by <multiple-value-check-type>.  
  let object-te = as(<type-estimate>, dylan-value(#"<object>"));
  local method type-temporary-type-estimate (type-temp) => (te :: <type-estimate>)
          // Construct type-estimate for this temp, erring on side of generality
          let (type-constant?, type) = type-temp & fast-constant-value?(type-temp);
          if (type-constant? & instance?(type, <&type>))
            as(<type-estimate>, type)  // type-estimate for this temporary.
          else
            object-te                  // Won't provoke disjointness.
          end
        end;
  make-type-estimate
    (<type-estimate-values>,
     // Should check these aren't <bottom>.
     fixed: map(type-temporary-type-estimate, type-temp*),
     rest:  #f)
end;

define function trim-type-estimate-to-fixed-values 
  (te :: <type-estimate>, n :: <integer>) => (te :: <type-estimate>)
  // Extract exactly n values from te, and make a new values te.
  // Used to trim computation te to fixed values in optimization below.
  select (te by instance?)
    <type-estimate-bottom> 
      => te;
    <type-estimate-values> 
      => make-type-estimate
	   (<type-estimate-values>, 
	    fixed: map(curry(type-estimate-values-ref, te), 
		       range(from: 0, below: n)),
	    rest:  #f);
    <type-estimate-union>
      => make-type-estimate
	   (<type-estimate-union>, 
	    unionees: map(rcurry(trim-type-estimate-to-fixed-values, n),
			  type-estimate-unionees(te)));
  end
end;

define function evaluate-fixed-values-type-checks
  (c :: <multiple-value-check-type-computation>, values-te :: <type-estimate>)
    => (statically-checked? :: <boolean>)
  // See if the fixed values match.  If so, mark them as statically checked,
  // so the back end won't generate code for the type check.  #rest done 
  // elsewhere; see evaluate-type-checks(<multiple-value-check-type-rest>).
  // 
  // Do the check all together, as in:
  //   type-union(values(<a>, <b>), values(<b>, <a>)) vs values(<a>, <a>)
  // This will complain; position-by-position check wouldn't expose the error.
  let checked-types   = types(c);
  let ntypes          = size(checked-types);
  let check-estimate  = make-type-estimate-for-fixed-check(checked-types);
  let fixed-values-te = trim-type-estimate-to-fixed-values(values-te, ntypes);
  // Plain-vanilla disjointness of values types isn't quite right here:
  // * values(#rest <bottom>) and values(anything) are DISJOINT (their
  //   intersection is empty) but the first is a SUBTYPE of the second (each
  //   <bottom> is a subtype of every component of the other guy).
  // * We are perfectly able to elide the return-value type-check if we're never
  //   going to return!
  // * values(#rest <bottom>) is the canonical undefined return value, so we'll
  //   just check for that.
  //when (instance?(values-te, <type-estimate-bottom>))
  //  format-out("\n*** c = %s, values-te = %s", c, values-te);
  //  format-out("\n    ntypes = %d",            ntypes);
  //  format-out("\n    check-estimate = %s",    check-estimate);
  //  format-out("\n    fixed-values-te = %s",   check-estimate, fixed-values-te)
  //end;
  local method canonical-undefined-values? (te :: <type-estimate>)
	 => (canonical-undefined? :: <boolean>)
	  // Is this <bottom> or values(#rest <bottom>)?
	  select (te by instance?)
	    <type-estimate-bottom> => #t;
	    <type-estimate-values> => empty?(type-estimate-fixed-values(te)) &
		                      instance?(type-estimate-rest-values(te), 
						<type-estimate-bottom>);
	    otherwise              => #f;
	  end
	end;
  // if (~canonical-undefined-values?(values-te) &
  //     type-estimate-disjoint?(fixed-values-te, check-estimate))
  if (~canonical-undefined-values?(values-te) &
        effectively-disjoint?(fixed-values-te, check-estimate))
    // Can't possibly be returning the right thing, so complain.
    // NB: if values-te is bottom, no error since will never return.
    note(run-time-type-error-class(c),
         source-location: dfm-source-location(c),
         context-id:      dfm-context-id(c),
         inferred-type:   fixed-values-te,
         expected-type:   check-estimate)
  end;
  local method evaluate-single-type? (i :: <integer>) => (ev? :: <boolean>)
          // #t if ith value type-checks.  Mark it as statically done if so.
	  let type = checked-types[i];
	  if (type)                       // Not already checked
	    let (the-type-constant?, the-type) = fast-constant-value?(type);
	    if (the-type-constant?           & 
                instance?(the-type, <&type>) &
                type-estimate-values-element-subtype?
                  (values-te, i, as(<type-estimate>, the-type)))
	      // NB: succeeds if values-te is bottom, too -- no point in 
              //     checking type of result you'll never get!
	      remove-user!(type, c);
	      checked-types[i] := #f;   // don't bother checking this type
	      #t
	    end
          else
            #t
	  end
	end;
  every?(evaluate-single-type?, range(from: 0, below: ntypes))
end;

define method evaluate-type-checks? 
    (c :: <multiple-value-check-type-computation>)
  // If fixed types check statically, mark as checked.
  evaluate-fixed-values-type-checks(c, type-estimate(computation-value(c)))
end method evaluate-type-checks?;

define method evaluate-type-checks? (c :: <multiple-value-check-type-rest>)
  // If fixed & rest types statically check, then mark rest as statically checked.
  let check-rest-type = rest-type(c);
  let values-te       = type-estimate(computation-value(c));
  evaluate-fixed-values-type-checks(c, values-te) &  // Fixed types check?
  (~check-rest-type |                                // #f means already checked
    begin 
      let (rest-type-constant?, rest-type) = fast-constant-value?(check-rest-type);
      if (rest-type-constant? &                      // Type is constant
          instance?(rest-type, <&type>) &            //   and matches values-te
          type-estimate-values-rest-subtype?(values-te,
                                             size(types(c)),
                                             as(<type-estimate>, rest-type)))
        remove-user!(check-rest-type, c);            // Can remove check computation
        rest-type(c) := #f;
        #t
      else
        #f                                           // Can't remove it
      end
   end)
end;

// This all sux...

define inline method make-with-matching-temporary
    (env, class :: <class>, old-t :: <temporary>, #rest args)
  let (c, t)
    = apply(make-with-temporary, env, class, args);
  mvt-transfer-values!(old-t, t);
  c
end method;

define method copy-type-check 
    (c :: <single-value-check-type-computation>, 
       checked-ref :: <value-reference>)
 => (c-copy :: <single-value-check-type-computation>)
  let env = environment(c);
  let checked-c = checked-ref.generator;
  let loc 
    = (checked-c & computation-source-location(checked-c)) 
        | parent-source-location();
  make-with-matching-temporary
    (env, object-class(c), c.temporary,
     source-location: loc,
     value: checked-ref,
     type: type(c),
     temporary-class: temporary-class(temporary(c)))
end method;

define method copy-type-check 
    (c :: <assignment-check-type>,
       checked-ref :: <value-reference>)
 => (c-copy :: <assignment-check-type>)
  let env = environment(c);
  let checked-c = checked-ref.generator;
  let loc 
    = (checked-c & computation-source-location(checked-c)) 
        | parent-source-location();
  make-with-matching-temporary
    (env, object-class(c), c.temporary,
     source-location: loc,
     value: checked-ref,
     type: type(c),
     lhs-variable-name: lhs-variable-name(c),
     temporary-class: temporary-class(temporary(c)))
end method;

define method copy-type-check 
    (c :: <multiple-value-check-type-computation>,
       checked-ref :: <value-reference>)
 => (c-copy :: <multiple-value-check-type-computation>)
  let env = environment(c);
  let checked-c = checked-ref.generator;
  let loc 
    = (checked-c & computation-source-location(checked-c)) 
        | parent-source-location();
  make-with-matching-temporary
    (env, object-class(c), c.temporary,
     source-location: loc,
     value: checked-ref,
     types: copy-sequence(types(c)), // because they're modified
     temporary-class: temporary-class(temporary(c)))
end method;

define method copy-type-check 
    (c :: <multiple-value-check-type-rest>,
       checked-ref :: <value-reference>)
 => (c-copy :: <multiple-value-check-type-rest>)
  let env = environment(c);
  let checked-c = checked-ref.generator;
  let loc 
    = (checked-c & computation-source-location(checked-c)) 
        | parent-source-location();
  make-with-matching-temporary
    (env, object-class(c), c.temporary, 
     source-location: loc,
     value: checked-ref,
     types: copy-sequence(types(c)), // because they're modified
     rest-type: rest-type(c),
     temporary-class: temporary-class(temporary(c)))
end method;

define method constant-fold (c :: <check-type-computation>)
  let value-c = generator(computation-value(c));
  if (value-c & instance?(value-c, <if-merge>) 
        // The following is because we seem to have a bogus class hierarchy
        // here 8(
        // We mustn't propagate a constraint type above its station, since
        // the constraint is typically local (true within a particular
        // branch, say).
        & ~instance?(c, <constrain-type>)
        // The following is a blunt instrument that ensures that we don't
        // promote type checks inappropriately across conditionals or 
        // ahead of computations that might cause the check never to
        // get reached. We could be much smarter about this.
        & value-c == c.previous-computation)
    let merge-c :: <if-merge> = value-c;

    // Duplicate the type check in the branches of the merge.

    // Make a copy of the check computation that refers to the 
    // value generated by the branch.
    let left-ref = merge-left-value(merge-c);
    let left-check = copy-type-check(c, left-ref);

    // Insert at the end of the branch, and redirect the merge to
    // refer to its value.
    let left-c = merge-left-previous-computation(merge-c);
    insert-computation-before-reference!(merge-c, left-check, left-ref);
    merge-replace-left-value!(merge-c, left-ref, left-check.temporary);

    // Make a copy of the check computation that refers to the 
    // value generated by the branch.
    let right-ref = merge-right-value(merge-c);
    let right-check = copy-type-check(c, right-ref);

    // Insert at the end of the branch, and redirect the merge to
    // refer to its value.
    let right-c = merge-right-previous-computation(merge-c);
    insert-computation-before-reference!(merge-c, right-check, right-ref);
    merge-replace-right-value!(merge-c, right-ref, right-check.temporary);

    // Replace the original type check with the direct result of the
    // merge.
    replace-computation-with-temporary!(c, merge-c.temporary);

    // Re-optimize from the new type checks.
    re-optimize(left-check);
    re-optimize(right-check);
    #t
  elseif (evaluate-type-checks?(c))
    // remove it
    replace-computation-with-temporary!(c, computation-value(c));
    #t
  end
end method;

define method constant-fold (c :: <slot-value>)
  let instance-ref = computation-instance(c);
  let sd           = computation-slot-descriptor(c);
  let (constant?, instance) = fast-constant-value?(instance-ref);
  if (constant?)
    let spec = model-definition(sd);
    if ((spec-constant?(spec) & ~spec-volatile?(spec))
          | instance?(spec, <repeated-slot-definition>) // HACK: BAD DEF
          | instance?(instance-ref, <immutable-object-reference>)
          // | ^slot-getter(sd) == dylan-value(#"class-implementation-class")
          // | ^slot-getter(sd) == dylan-value(#"class-constructor")
          )
      let val = ^slot-value(instance, sd);
      unless (val == &unbound)
	let ref
	  = make-value-reference
	      (val, 
	       if (^slot-getter(sd) == dylan-value(#"tail"))
		 debug-assert
		   (instance?(instance-ref, <immutable-object-reference>),
		    "FOLDING TAIL OF MUTABLE REF %=", instance);
		 // maintain immutability
		 <immutable-object-reference>
	       else
		 <object-reference>
	       end if);
	replace-computation-with-temporary!(c, ref);
	#t
      end unless;
    end if;
  elseif (instance?(instance-ref, <stack-vector-temporary>) 
	    & ^slot-getter(sd) == dylan-value(#"size"))
    let ref = make-object-reference(number-values(instance-ref));
    replace-computation-with-temporary!(c, ref);
    #t
  else
    let type = type-estimate(instance-ref);
    if (instance?(type, <type-estimate-limited-collection>))
      select (^slot-getter(sd))
	dylan-value(#"dimensions")
	  => if (type-estimate-dimensions(type))
	       let dims = as(<simple-object-vector>, type-estimate-dimensions(type));
	       let ref  = make-value-reference(dims, <immutable-object-reference>);
	       replace-computation-with-temporary!(c, ref);
	       #t;
	     else 
	       #f
	     end if;
	dylan-value(#"element-type") 
	  => let ref = make-object-reference(as(<&type>, type-estimate-of(type)));
	     replace-computation-with-temporary!(c, ref);
             #t;
        otherwise
	  => #f;
      end select;
    else 
      #f
    end if
  end if;
end method;

/*
define method constant-fold (c :: <set-cell-value!>)
  #f;
end method;
*/
