Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///////////////////////////////////////////////////////////////////
///
///                   TYPING CALLS
///  
/// Now life gets more interesting....
///
///////////////////////////////////////////////////////////////////

define method initialize-node-type 
  (call :: <call>, css, work-agenda) => ()
  initialize-node-type-internal(call, css, work-agenda);
  maybe-propagate-arg-types(call, css, work-agenda);
end;

define method refine-initial-node-type 
    (call :: <call>, css :: <call-site-summary>, wa :: <work-agenda>) => ()
  let temp = call.temporary;
  let initial-type = lookup-type(temp, css, call);
  let new-type = infer-node-type(call, css, wa);
  let renamings = element(css.renaming-computations, call, default: #f);
  when (renamings)
    for (x in renamings)      
      let tmp = x.head;
      let renaming = x.tail;
      schedule-users-of-renaming(wa, tmp.users, renaming, css);
      delete-renamings(tmp, css, call);
    end;
  end;
  maybe-propagate-arg-types(call, css, wa);
  unless (^subtype?(new-type, initial-type))
    schedule-renaming-computations(temp, css, wa);
    css.pool[temp] :=  maybe-make-inferred-union(call, new-type, initial-type);
    schedule-users(wa, temp.users, css);
  end;
end;

define function schedule-users-of-renaming
  (agenda :: <work-agenda>, nodes :: <list>, renaming, summary)
  for (node in nodes)
    if (node.environment.lambda == summary.css-lambda)
      when (member?(renaming, element(summary.renaming-map, node, default: #()),
                    test: method (x, y :: <pair>) x == y.tail end))
        schedule-for-retyping(agenda, node, summary);
      end;
    else
      for-all-summaries (css in node.environment.lambda.call-site-summaries)
        schedule-users-of-renaming(agenda, list(node), renaming, css);
      end;
    end;
  end;
end;

define inline function get-arg-types 
  (args :: <vector>, css, comp) => (args :: <simple-object-vector>);
  map-as(<simple-object-vector>, 
         rcurry(lookup-type, css, comp),
         args);
end;

define inline function get-argument-types (c :: <call>, css)
  => (args :: <simple-object-vector>);
  get-arg-types(c.arguments, css, c);
end;
  
define method validate-args
  (fn :: <&method>, args :: <vector>, sig :: <&signature>, 
   css :: <call-site-summary>, call :: <function-call>)
  => (result :: one-of(default:, required:, rest:, keys:));
  if (call.call-iep?)
    // this must have already been validated
    if (sig.^signature-key?)
      keys:;
    elseif (sig.^signature-rest?)
      rest:;
    else
      required:;
    end
  else
    let num-args = args.size;
    let num-required = sig.^signature-number-required;
    let n = num-required - num-args;
    let rest? = sig.^signature-rest?;
    let key? = sig.^signature-key?;

    if (n > 0)
      typist-note(<too-few-arguments-in-call>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           function: fn,
           required-count: num-required,
           supplied-count: num-args);
      default:;
    elseif (~(rest? | key?))
      if (n < 0)
        typist-note(<too-many-arguments-in-call>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           function: fn,
           required-count: num-required,
           supplied-count: num-args);
        default:;
      elseif (n = 0)
        required:;
      end
    elseif (key? & ~even?(n))
      typist-note(<unbalanced-keyword-arguments-in-call>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           function: fn,
           keyword-supplied-count: n);
      break("unbal");
      default:;
    elseif (key?)
      keys:
    else
      rest:
    end;
  end
end;

define method validate-args
  (fn :: <&method>, args :: <vector>, sig :: <&signature>, 
   css :: <call-site-summary>, call :: <method-call>)
  => (result :: one-of(default:, required:, rest:, keys:));
  // this must have already been validated
  if (sig.^signature-key?)
    keys:;
  elseif (sig.^signature-rest?)
    rest:;
  else
    required:;
  end;
end;

define method validate-args
  (fn :: <&method>, args :: <vector>, sig :: <&signature>, 
   css :: <call-site-summary>, call :: <engine-node-call>)
  => (result :: one-of(default:, required:, rest:, keys:));
  // this must have already been validated
  if (sig.^signature-key?)
    keys:;
  elseif (sig.^signature-rest?)
    rest:;
  else
    required:;
  end;
end;

define method get-canonical-arg-types
  (call :: <function-call>, args, css :: <call-site-summary>, def)
  => (args :: <simple-object-vector>);
  // Why did AndyS write this?
  // let the-fn = lookup-type(call.function, css, call).^singleton-object;
  // let fn = if (instance?(the-fn, <&generic-function>)) def else the-fn end;
  let fn = def;
  if (call.call-iep?)
    let sig = fn.^function-signature;
    let num-req = sig.^signature-number-required;
    let action = validate-args(fn, args, sig, css, call);
    if ((action == rest:) | (action == keys:))
      if (args.size = num-req) // i.e. no rest arg!
        let new-args = make(<vector>, size: num-req + 1);
	replace-subsequence!(new-args, args, end: num-req);
        new-args[num-req] := cached-singleton(#[]);
        new-args
      else
        let rest-arg = args[num-req];
        if (rest-arg.object-class == <values-type>)
          let new-args = copy-sequence(args);
          new-args[num-req] :=
            make(<rest-values-type>, 
              types: rest-arg.fixed-types, rest-type: rest-arg.values-rest-type);
          new-args
        else
          args
        end
      end
    else
      args
    end;
  else
    get-canonical-arg-types-internal(fn, args, css, call);
  end;
end;

define method get-canonical-arg-types
  (call :: <primitive-call>, args, css :: <call-site-summary>, def)
  => (args :: <simple-object-vector>);
  // Why did AndyS write this?
  // let the-fn = lookup-type(call.arguments[0], css, call).^singleton-object;
  // let fn = if (instance?(the-fn, <&generic-function>)) def else the-fn end;
  let fn = def;
  get-canonical-arg-types-internal(fn, args, css, call);
end;

define method get-canonical-arg-types-internal 
  (fn :: <&method>, args, css :: <call-site-summary>, comp) 
  => (args :: <simple-object-vector>);
  let sig = fn.^function-signature;
  let action = validate-args(fn, args, sig, css, comp);
  select (action)
    default: => default-arg-types-from-signature(fn, sig);
    required: => args;
    rest: => construct-canonical-rest-arg-types(fn, args, sig, css, comp);
    keys: => construct-canonical-keys-arg-types(fn, args, sig, css, comp);
  end;
end;

/*
define method get-canonical-arg-types-internal
  (code :: <&code>, args :: <simple-object-vector>, css :: <call-site-summary>, comp) 
  => (args :: <simple-object-vector>);
  let fn = code.function;
  let sig = fn.^function-signature;
  let num-args = args.size;
  let num-required = sig.^signature-number-required;
  let rest? = sig.^signature-rest?;
  let key? = sig.^signature-key?;

  if (key? & (num-args > num-required))
    let new-args = make(<simple-object-vector>, size: num-args - 1);
    let i = 0;
    for (j from 0 below num-args)
      unless (j = num-required)
        new-args[i] := args[j];
        i := i + 1;
      end;
    end;
    new-args;
  elseif (rest? & (num-args > num-required))
    let stack-arg = args[num-required];
    if (instance?(stack-arg, <values-type>))
      let stack-v = stack-arg.fixed-types;
      let stack-v-size = stack-v.size;
      let new-args = make(<simple-object-vector>, size: num-args + stack-v-size - 1);
      for (i from 0 below num-required)
        new-args[i] := args[i];
      end;
      for (i from 0 below stack-v-size,
           j from num-required)
        new-args[j] := stack-v[i];
      end;
      new-args;
    else
      args;
    end;
  else
    args;
  end;
end;
*/

define method get-canonical-arg-types-internal
  (code :: <&code>, args :: <simple-object-vector>, css :: <call-site-summary>, comp) 
  => (args :: <simple-object-vector>);
  // if its been upgraded to here then the args are already `canonical`
  args;
end;


define inline function default-arg-types 
  (l :: <&method>) => (args :: <simple-object-vector>);
  default-arg-types-from-signature(l, l.^function-signature);
end;


define inline method get-required-from-signature(sig :: <&signature>)
 => (args :: <vector>, size :: <integer>)
  let args = sig.^signature-required;
  let real-size = sig.^signature-number-required;
  if (args.size = real-size)
    values(args, real-size)
  else
    values(copy-sequence(args, end: real-size), real-size)
  end
end;

define inline method get-keys-from-signature(sig :: <&signature>)
 => (args :: <vector>, size :: <integer>)
  let keys = sig.^signature-keys;
  let real-size = sig.^signature-number-keys;
  if (keys.size = real-size)
    values(keys, real-size)
  else
    values(copy-sequence(keys, end: real-size), real-size)
  end
end;

define inline method get-key-types-from-signature(sig :: <&signature>)
 => (args :: <vector>, size :: <integer>)
  let keys = sig.^signature-key-types;
  let real-size = sig.^signature-number-keys;
  if (keys.size = real-size)
    values(keys, real-size)
  else
    values(copy-sequence(keys, end: real-size), real-size)
  end
end;

define inline function default-arg-types-from-signature
  (l :: <&method>, sig :: <&signature>) => (args :: <simple-object-vector>);

  let required = get-required-from-signature(sig);
  let rest? = sig.^signature-rest?;
  let key? = sig.^signature-key?;
  if (key? & model-definition(l))
    // E.G. EXITs dont have a model def
    concatenate(required, vector(typist-sov-type()), 
                get-key-types-from-signature(sig));
  elseif (rest?)
    concatenate(required, vector(typist-sov-type()))
  else
    required;
  end;
end;

/* (98sep03,gts) expand-rest-type? was:
 *  define function expand-rest-type? (fn :: <&method>) => (b :: <boolean>)
 *      let md = model-definition(fn);
 *      if (md)
 *	let rest? = md.form-signature.spec-arguments-spec.spec-rest-variable-spec;
 *	rest? & rest?.expand-args?
 *      end
 *  end;

 *  But now it is algorithmic, to avoid a few slots.
 *  
 *  Look at the parameters of the <lambda>.  If one of them is a 
 *  <lexical-rest-variable>, look at the users of that rest param.
 *  If one of those users is a <values> computation, look at the users 
 *  of the result of the <values> computation.  If a user is an <exit>, 
 *  then we should expand type types.

 *  First of all, this is pretty fragile.

 *  Secondly, we may want to extend this for other cases where the rest
 *  variable type should be expanded.
 */

define function expand-rest-type? (fn :: <&lambda>) => (b :: <boolean>)
  // when find-element works efficiently on lists, can do:
  // let rest-param = find-element(parameters(fn),
  //                    rcurry(instance?, <lexical-rest-variable>));

  let rest-param = iterate loop (ps = parameters(fn))
                     if (~empty?(ps))
		       if (instance?(p, <lexical-rest-variable>))
			 p
		       else
		         loop(tail(ps))
		       end if
		   end iterate;

  // better would be some notion of "data sink" for the rest var. 

  if (rest-param)
    let users1 = users(rest-param);
    any?(rcurry(instance?, <exit>), users1)
      | any?(method(user) any?(rcurry(instance?, <exit>), users(user)) end,
	     users1)
  end if
end;

define function construct-canonical-rest-arg-types
  (fn :: <&method>, args :: <vector>, sig :: <&signature>, 
   css :: <call-site-summary>, comp) 
  => (args :: <simple-object-vector>);
  let num-args = args.size;
  let num-required = sig.^signature-number-required;
  let new-arg-list = make(<simple-object-vector>, size: num-required + 1);
  for (i from 0 below num-required)
    new-arg-list[i] := args[i];
  end;
  if (num-args = num-required)
    new-arg-list[num-required] := cached-singleton(#[]);
  elseif (expand-rest-type?(fn))
    new-arg-list[num-required] := 
       make(<rest-values-type>, types: copy-sequence(args, start: num-required));
  else
    new-arg-list[num-required] := typist-sov-type();
  end;
  new-arg-list;
end;


define function construct-canonical-keys-arg-types
  (fn :: <&method>, args :: <vector>, sig :: <&signature>, 
   css :: <call-site-summary>, comp) 
  => (args :: <simple-object-vector>);
  let num-args = args.size;
  let num-required = sig.^signature-number-required;
  let sig-keys = get-keys-from-signature(sig);
  let sig-key-types = get-key-types-from-signature(sig);
  let num-optionals = sig-keys.size;
  let total-args = num-required + num-optionals;
  let arg-list = make(<simple-object-vector>, size: total-args + 1);
  let keys-start = num-required + 1;

  // fills in the types for unsupplied key args (i.e. the places in arg-list which are still #f).
  local method fill-in-default-key-types()
          for (i from keys-start to total-args)
//             unless (arg-list[i])
               arg-list[i] := sig-key-types[i - keys-start];
//             end;
           end;
         end;
 
  // grab the required arg types
  for (i from 0 below num-required)
    arg-list[i] := args[i];
  end;

  fill-in-default-key-types();

  if (num-args = num-required)
    // deal with the keys
    arg-list[num-required] := cached-singleton(#[]);
//    fill-in-default-key-types();
  else
    if (expand-rest-type?(fn))
      arg-list[num-required] := 
         make(<rest-values-type>, types: copy-sequence(args, start: num-required));
    else
      arg-list[num-required] := typist-sov-type();
    end;
  end;

  check-for-unrecognized-keywords(comp, fn, args, sig);
  arg-list;
end;

define method ^method-generic-function (object :: <&method>)
  let def = model-definition(object);
  if (def)
    let name = form-variable-name(def);
    let model = lookup-model-object(name, reference?: #f);
    model
  end
end method;

define function check-for-unrecognized-keywords (comp, fn, args :: <vector>, sig :: <&signature>) => ();
  let num-args = args.size;
  let num-required = sig.^signature-number-required;
  let gf = ^method-generic-function(fn);

  unless ( num-args = num-required 
         | ~sig.^signature-key? 
         | sig.^signature-all-keys?
         | (gf & gf.^function-signature.^signature-all-keys?) )
    let sig-keys = get-keys-from-signature(sig);
    collecting (unrecognized-keys)
      block (exit)
        for (i from num-required below num-args by 2)
          let arg = args[i];
          if (instance?(arg, <&singleton>) & instance?(arg.^singleton-object, <symbol>))
            let index = find-key(sig-keys, curry(\==, arg.^singleton-object));
            if (index)
	    else
              collect-into(unrecognized-keys, arg.^singleton-object)
            end;
          else
            // don`t know what this is so all bets are off from here on (e.g. this could evaluate to 
            // a keyword which appears explicitly later. It would be wrong to accept the type of the key
            // in such a case.
            exit();
          end;
        end;
      end;

      let dodgy-keys = collected(unrecognized-keys);

      unless (empty?(dodgy-keys) )
        typist-note(<unrecognized-keyword-arguments-in-call>,
             source-location:  dfm-source-location(comp),
             context-id:       dfm-context-id(comp),
             function:         fn,
             supplied-keywords: dodgy-keys,
             recognized-keywords: sig-keys);
      end;
    end;
  end;
end;

define function check-arg-types!
  (fn :: <&method>, args :: <vector>, sig :: <&signature>, 
   css :: <call-site-summary>) 
  => (args :: <simple-object-vector>);
  let num-required = sig.^signature-number-required;
  let keys-start = num-required + 1;

  // check the required arg types
  for (i from 0 below num-required, 
       arg in args, sig-arg in sig.^signature-required)
    unless (^subtype?(arg, sig-arg))
      args[i] := sig-arg
    end
  end;

  if (sig.^signature-key?)
    let sig-key-types = sig.^signature-key-types;
    // Will there always be enough keys at this point??
    for (i from keys-start below args.size)
      let arg = args[i];
      let sig-arg = sig-key-types[i - keys-start];
      unless (^subtype?(arg, sig-arg))
        args[i] := sig-arg
      end
    end;
  end;

  args
end;

/// First lets do the easy ones...


define method infer-node-type 
  (sv :: <stack-vector>, css, w-l) => (result :: <&type>)  
  let sv-args = sv.arguments;
  if (empty?(sv-args))
    typist-sov-type();
  else
    make(<values-type>,
         types: get-arg-types(sv-args, css, sv),
         rest-type: #f);
  end;
end;

// utility to ensure type matches tempory
define method match-value-and-type (temp :: <temporary>, type :: <&type>)
  => (result-type :: <&type>);
  type;
end;

define method match-value-and-type (temp :: <temporary>, type :: <values-type>)
  => (result-type :: <&type>);
  let fixed = type.fixed-types;
  if (empty?(fixed))
    type.values-rest-type 
    // called function returns nothing, so (yuk, yuk, yuk) temp will be #f i.e. ^#f
    | typist-<false-type>();
  else
    fixed[0];
  end;
end;

define method match-value-and-type (temp :: <multiple-value-temporary>, type :: <values-type>)
  => (result-type :: <values-type>);
  type
end;

define method match-value-and-type (temp :: <multiple-value-temporary>, type :: <&type>)
  => (result-type :: <&type>)
  make(<values-type>, types: vector(type), rest-type: #f);
end;

define method match-value-and-type (temp :: <multiple-value-temporary>, type :: <&bottom-type>)
  => (result-type :: <&type>);
  type;
end;

  
define method match-value-and-type (temp == #f, type :: <&type>)
  => (result-type :: <&type>)
  type;
end;

/// Produce a type for a call given a signature and the result temporary of the call

// TODO: decide whether to issue a warning here and when limited collections are available 
//       we can do better (maybe) in the case where there was a declaration for the rest var.
// Only calling for 1 value
define method return-type-from-signature-and-call
  (s :: <&signature>, temp :: <temporary>) => (r :: <&type>)
  let vals = ^signature-values(s);
  if (s.^signature-number-values > 0)
    s.^signature-values[0];
  else
    ^signature-rest-value(s) 
    // called function returns nothing, so (yuk, yuk, yuk) temp will be #f i.e. ^#f
    | typist-<false-type>()
  end;
end;

// watch out for bottom, although it wont propagate from here
define method return-type-from-signature-and-call
  (s :: <&signature>, temp == #f) => (r :: <&type>)
  if (s.^signature-number-values > 0)
    s.^signature-values[0];
  else
    typist-<object-type>();
  end;
end;

// watch out for bottom. Mustn't wrap it in a values-type
define method return-type-from-signature-and-call
  (s :: <&signature>, temp :: <multiple-value-temporary>) => (r :: <&type>)
  let vals = s.^signature-values;
  let vals-size = s.^signature-number-values;
  if ((vals-size > 0) & ^subtype?(vals[0], typist-<bottom-type>()))
    vals[0];
  elseif (vals-size = vals.size)
    make(<values-type>, 
         types: ^signature-values(s), 
         rest-type: ^signature-rest-value(s))
  else
    make(<values-type>, 
	 types: copy-sequence(^signature-values(s), end: vals-size),
         rest-type: ^signature-rest-value(s));
  end;
end;

define variable *constant-fold-in-typist* = #t;

define generic maybe-fold (call :: <call>, css :: <call-site-summary>)
  => (type :: false-or(<&type>));

define function variable-slot-access?
    (function :: <&function>, folder-function :: <function>, call-arguments)
 => (well? :: <boolean>)
  // TODO: This ^iep thing is a hack and a half. Remove it when we can
  // prove it's not necessary.
  if (folder-function ~== ^iep)
    if (size(call-arguments) == 1)
      let arg = call-arguments.first;
      let class = ^object-class(arg); // Because we get a raw sometimes! 8(
      if (instance?(class, <&class>))
        let slotd = ^slot-descriptor(class, function);
        if (slotd & ^slot-setter(slotd))
          #t
        end;
      end
    elseif (size(call-arguments) == 2)
      let arg = call-arguments.second;
      let class = ^object-class(arg); // Because we get a raw sometimes! 8(
      if (instance?(class, <&class>))
        let slotd = ^slot-descriptor(class, function);
        if (slotd & ^slot-setter(slotd))
          #t
        end;
      end
    end
  end;
end function;


define method maybe-fold (c :: <primitive-call>, css :: <call-site-summary>)
  => (type :: false-or(<&type>));
  let arg-types = get-arg-types(arguments(c), css, c);
  when (every?(rcurry(instance?, <&singleton>), arg-types))
    let args = map(compose(compile-stage, ^singleton-object), arg-types);
    with-dependent-context ($compilation of model-creator(css.css-lambda))
      let (result-type, folded?) =
        block ()
          // TODO: DOESN"T HANDLE MULTIPLE-VALUE RETURNING PRIMITIVES
          values(cached-singleton(apply(c.primitive.primitive-value, args)), #t);
        exception (<error>)
          values(#f, #f);
        end block;
      when (folded?)
        record-primitive-result(result-type, <primitive-constant-folded>, c, arg-types, css);
      end;
    end;
  end;
end method;

define method record-primitive-result
  (result-type :: <&type>, record-type, call, arg-types, css)
  => (result :: <&type>);
  let fold-result = 
    create-computation-record
     (record-type, call, css, arg-types);
  fold-result.inferred-type := result-type;
  record-computation-record(css, call, fold-result);
  result-type;
end;

define method record-primitive-result
  (result-type :: <&singleton>, record-type, call, arg-types, css)
  => (result :: <&singleton>);
  let fold-result = 
    create-computation-record
     (record-type, call, css, arg-types, call-values: result-type.^singleton-object);
  fold-result.inferred-type := result-type;
  record-computation-record(css, call, fold-result);
  result-type;
end;

/// so now we can type a primitive call
define method infer-node-type 
  (call :: <primitive-call>, css, w-l) 
  => (result :: <&type>);  
   (*constant-fold-in-typist* & maybe-fold(call, css))
  | begin
      let (result-type) =
        select (call.primitive)
          dylan-value(#"primitive-apply")
            => infer-primitive-apply-type(call, css, w-l);
          dylan-value(#"primitive-values")
            => infer-primitive-values-type(call, css, w-l);
          otherwise
            => return-type-from-signature-and-call
                (call.primitive.primitive-signature, call.temporary);
        end; 
      let type = match-value-and-type(call.temporary, result-type);
      record-primitive-result(type, <primitive-call-record>, call, call.arguments, css);
    end;
end;

define method infer-node-type 
  (call :: <c-variable-pointer-call>, css, w-l) 
  => (result :: <&type>);  
//  record-primitive-result(<&raw-pointer>, <primitive-call-record>, call, call.arguments, css);
  <&raw-pointer>
end;


define function infer-primitive-apply-type 
  (call :: <primitive-call>, css :: <call-site-summary>, w-l) => (r :: <&type>);
  let obj = lookup-type(call.arguments[0], css, call);
  if (instance?(obj, <&singleton>))
    let fn = obj.^singleton-object;
    if (instance?(fn, <&function>))
      infer-apply-type-from-function
        (call, fn, copy-sequence(call.arguments, start: 1), css, w-l);
    else
       //TODO this should note an error
      return-type-from-signature-and-call(call.primitive.primitive-signature, call.temporary);
    end;
  else
    return-type-from-signature-and-call(call.primitive.primitive-signature, call.temporary);
  end;
end;

define function infer-primitive-values-type 
  (call :: <primitive-call>, css :: <call-site-summary>, w-l) => (r :: <&type>);
// gts -- rest-type should be <object>!
  make(<values-type>, types: #[], rest-type:  typist-<object-type>());
// orig:  make(<values-type>, types: #[], rest-type:  typist-sov-type());
end;


// This is ordered differently from the folder in optimization to fail asap
// doing as little work as possible
define method maybe-fold (c :: <function-call>, css :: <call-site-summary>)
  => (result :: false-or(<&type>));
  let fn-type = lookup-type(c.function, css, c);
  when (instance?(fn-type, <&singleton>))
    let function = fn-type.^singleton-object;
    when (instance?(function, <&function>))
      let compile-stage-function = lookup-compile-stage-function(function);
      when (compile-stage-function)
        let arg-types = get-arg-types(arguments(c), css, c);
        when (every?(rcurry(instance?, <&singleton>), arg-types))
          let args = map(^singleton-object, arg-types);
          unless (variable-slot-access?(function, compile-stage-function,args))
            let temp = c.temporary;
            with-dependent-context ($compilation of model-creator(css.css-lambda))
              let (result, folded?) = maybe-fold-call(c, temp, compile-stage-function, args);
              when (folded?)
                record-result(result, temp, c, function, arg-types, css);
              end;
            end;
          end;
        end;
      end;         
    end;
  end;
end method;

define method record-result(result, temp, call, function, arg-types, css)
  => (result :: <&type>);
  let result-class = 
    if (instance?(function, <&generic-function>)) 
      <generic-constant-folded>;
    else
      <method-constant-folded>;
    end;
  let fold-result = 
    create-function-call-record
     (result-class, function, arg-types, call, css, call-values: result);
  let result-type = cached-singleton(result);
  fold-result.inferred-type := result-type;
  record-computation-record(css, call, fold-result);
  result-type;
end;

define method record-result(result, t :: <multiple-value-temporary>, call, function, arg-types, css)
  => (result :: <&type>);
  let result-class = 
    if (instance?(function, <&generic-function>)) 
      <generic-constant-folded>;
    else
      <method-constant-folded>;
    end;
  let fold-result = 
    create-function-call-record
     (result-class, function, arg-types, call, css, call-values: result);

  let fixed-result-types = 
    map-as(<simple-object-vector>, cached-singleton, result);
  let result-type = make(<values-type>, types: fixed-result-types);
  let matched-result-type = match-value-and-type(t, result-type);
  fold-result.inferred-type := matched-result-type;
  record-computation-record(css, call, fold-result);
  matched-result-type;
end;

define function maybe-fold-call (call, t, function, arguments)
  block ()
    values(fold-call(t, function, arguments), #t)
  exception (e :: <error>)
    values(#f, #f)		// silently fail to fold
  end;
end;

define method fold-call (t == #f, function, arguments) 
  => (call-values)
  apply(function, arguments);
end;

define method fold-call 
  (t :: <multiple-value-temporary>, function, arguments) 
  => (call-values)
  let (#rest call-values) = apply(function, arguments);
  call-values
end;

define method fold-call (t :: <temporary>,
				  function, arguments) => (call-value)
  apply(function, arguments);
end;
 


define method infer-node-type 
  (call :: <function-call>, css, w-l) => (result :: <&type>)
  (*constant-fold-in-typist* & maybe-fold(call, css))
   | match-value-and-type
       (call.temporary, infer-call-type(call, lookup-type(call.function, css, call), css, w-l))
end;

define generic  infer-call-type
  (call :: <function-call>, fn :: <&type>, css, w-l) => (r :: <&type>);

define constant $the-empty-vector = as(<simple-vector>, #[]);

define method  infer-call-type
  (call :: <function-call>, fn-type :: <&type>, css, w-l) => (r :: <&type>)
  let result-type =
    if (call.temporary)
      make(<values-type>, 
           types: $the-empty-vector, 
           rest-type: typist-values-rest-type());
    else
      // nothing uses the result of this call so just return a handy type.
      typist-<unknown-type>();
    end;


  let dylan-function-type = dylan-value(#"<function>");

  if (guaranteed-disjoint?(fn-type, dylan-function-type))
    let dispatch-result = create-computation-record
                           (<illegal-call-record>, call, css, call.arguments);
    dispatch-result.inferred-type := result-type;
    record-computation-record(css, call, dispatch-result);

    if (instance?(css, <default-call-site-summary>))
      delayed-typist-note(css, call,
                  <non-function-in-call>, 
                  computation-record: dispatch-result,
                  source-location: dfm-source-location(call),
                  context-id: dfm-context-id(call),
                  type-estimate: fn-type);
    elseif (*warn-for-all-summaries*)
      delayed-typist-note(css, call,
                  <non-function-in-call>, 
                  computation-record: dispatch-result,
                  source-location: dfm-source-location(call),
                  context-id: dfm-context-id(call),
                  type-estimate: fn-type);
    end;
  end;

  result-type
end;

define method  infer-call-type
  (call :: <function-call>, fn-type :: <&singleton>, css, w-l) => (r :: <&type>)
  infer-call-type-from-function(call, fn-type.^singleton-object, css, w-l);
end;


define generic  infer-call-type-from-function
    (call :: <function-call> , function-obj , css, w-l) => (r :: <&type>);

define method infer-call-type-from-function
    (call :: <function-call> , function-obj , css, w-l) => (r :: <&type>);
  let dispatch-result = create-computation-record
                         (<illegal-call-record>, call, css, call.arguments);
  if (instance?(css, <default-call-site-summary>))
    delayed-typist-note(css, call, <non-function-in-call>, 
                computation-record: dispatch-result,
                source-location: dfm-source-location(call),
                context-id: dfm-context-id(call),
                type-estimate: lookup-type(call.function, css, call));
  elseif (*warn-for-all-summaries*)
    delayed-typist-note(css, call, <non-function-in-call>, 
                computation-record: dispatch-result,
                source-location: dfm-source-location(call),
                context-id: dfm-context-id(call),
                type-estimate: lookup-type(call.function, css, call));
  end;

  let result-type =
    if (call.temporary)
      make(<values-type>, 
           types: $the-empty-vector, 
           rest-type: typist-values-rest-type());
    else
      // nothing uses the result of this call so just return a handy type.
      typist-<unknown-type>();
    end;

  dispatch-result.inferred-type := result-type;
  record-computation-record(css, call, dispatch-result);
  result-type
end;



// Here`s the fun one 
// could call estimate-effective-method (gf, arg-types, call)

define variable *infer-through-non-locals* = #t;

// the tightness of the mode in which the GF was compiled can be got from
// gf.model-creator.form-compilation-record.compilation-record-library.library-description-compilation-mode (!!!!).
// This tells us that we could ct-dispatch it as long as the library binding settings
// are tight.
define function tight-mode? 
  (referer :: <&object>, referee :: <&object>)
  => (result :: <boolean>);
  referee.model-creator.form-compilation-record.compilation-record-library.library-description-compilation-mode == tight:
  | referer.model-creator.form-compilation-record.compilation-record-library.library-description-compilation-mode == tight:
end;


define inline function cross-library-call? 
  (referee :: <&object>)
  => (result :: <boolean>);
  referee.model-creator.form-compilation-record.compilation-record-library
  ~== current-top-level-library-description()
end;

define method  infer-call-type-from-function
  (call :: <function-call> , gf :: <&generic-function> , css, agenda) 
  => (r :: <&type>);
  let sig = gf.^function-signature; 
  let number-required = sig.^signature-number-required;
  let args = get-arg-types(call.arguments, css, call);

  if (number-required > call.arguments.size)
    typist-note(<too-few-arguments-in-call>,
         source-location: dfm-source-location(call),
         context-id:      dfm-context-id(call),
         function: gf,
         required-count: number-required,
         supplied-count: call.arguments.size);
    typist-<bottom-type>()

  elseif (~arguments-potentially-joint?(
             args, ^signature-required(sig), number-required))
    let dispatch-result = create-computation-record
                            (<illegal-call-record>, call, css, call.arguments);
    /* delayed- */ typist-note(/* css, call, */ <argument-types-mismatch-in-call>,
         computation-record: dispatch-result,
         source-location: dfm-source-location(call),
         context-id:      dfm-context-id(call),
         function: gf,
         required-types:  get-required-from-signature(sig),
         supplied-type-estimates: args);
    
    break("bad arg types, %=,%=, %=, %=,%=",
      call, gf, css, args, sig);

    let result-type = return-type-from-signature-and-call(sig, call.temporary);
    dispatch-result.inferred-type := result-type;
    record-computation-record(css, call, dispatch-result);
    result-type

  elseif (*infer-through-non-locals* & tight-mode?(css.css-lambda, gf))
    let cached-result = probe-gf-primary-cache(gf, args);
    if (cached-result)
      process-primary-cached-result(cached-result, call, css);
    else
      block (return) // so we can punt whenever we find we're in OPEN territory
        let (products, status) = CPA-expand-args(args);
        if (products)
          let final-result :: false-or(<dispatch-record>)= #f;
          // NB. There's always at least 1 element here
          for (i from 0 below products.size)
            let product = products[i];
            let (known?, dispatch-result) = 
              typist-all-applicable-methods-guaranteed-known?(gf, product, call, css);
            if (known?)
              let cached-result = probe-gf-secondary-cache(gf, product);  // gets a dispatch record
              if (cached-result)
                process-secondary-cache-result(cached-result, call, css);
                if (final-result)
                  final-result := dispatch-result-merge(final-result, cached-result);
                else
                  final-result := cached-result;
                end;
              else
                let dispatch-result = typist-estimate-effective-method(gf, product, call, css);
                process-dispatch-result(dispatch-result, gf, product, call, css, agenda);
                if (final-result)
                 final-result := dispatch-result-merge(final-result, dispatch-result);
                else
                  final-result := dispatch-result;
                end;
              end;
            else
              // Open GF: update the various caches
              let result-type = return-type-from-signature-and-call(sig, call.temporary);
              dispatch-result.input-types := as(<list>, args);   // gts, merge, as <list>
              dispatch-result.inferred-type := result-type;
              record-computation-record(css, call, dispatch-result);
              update-gf-primary-cache(gf, args, dispatch-result);
              return(result-type);
            end;
          end; // for (i from 0 below products.size)

          if (instance?(final-result, <no-applicable-methods>))
            delayed-typist-note(css, call, <no-applicable-methods-in-call>,
              computation-record: final-result,
              source-location: dfm-source-location(call),
              context-id:      dfm-context-id(call),
              function: gf,
              supplied-type-estimates: args);
	  end;

          final-result.input-types := as(<list>, args);    // gts, merge, as <list>
          record-computation-record(css, call, final-result);
          update-gf-primary-cache(gf, args, final-result);
          final-result.inferred-type;

        elseif (status == megamorphic:)
          let dispatch-result = create-function-call-record
                                 (<megamorphic-args>, gf, args, call, css);
          let result-type = return-type-from-signature-and-call(sig, call.temporary);
          dispatch-result.inferred-type := result-type;
          record-computation-record(css, call, dispatch-result);
          update-gf-primary-cache(gf, args, dispatch-result);
          result-type;
        elseif (status == bottom:)
          let dispatch-result = create-function-call-record
                                 (<bottom-in-arg-list>, gf, args, call, css);
          let result-type = typist-<bottom-type>();
          dispatch-result.inferred-type := result-type;
          record-computation-record(css, call, dispatch-result);
          update-gf-primary-cache(gf, args, dispatch-result);
          result-type;
        else
          error("Unrecognized status from CPA expansion: %s : %s", status, args);
        end; // IF (PRODUCTS)
      end; //BLOCK (RETURN)
    end;
  else
    return-type-from-signature-and-call(sig, call.temporary);
  end;
end;

define method  infer-call-type-from-function
  (call :: <apply> , f :: <&function> , css, agenda) 
  => (r :: <&type>);
  infer-apply-type-from-function(call, f, call.arguments, css, agenda);
end;

define method  infer-apply-type-from-function
  (call , gf :: <&generic-function>, supplied-args, css, agenda) 
  => (r :: <&type>);
  // TODO:
  let sig = gf.^function-signature;
  if (*infer-through-non-locals* & tight-mode?(css.css-lambda, gf))
    let apply-args = get-arg-types(supplied-args, css, call);
    let cached-result = probe-gf-primary-cache(gf, apply-args);
    if (cached-result)
      process-primary-cached-result(cached-result, call, css);
    else
      block (return) // so we can punt whenever we find we're in OPEN territory
        // TODO: if we know the last arg is disjoint from <sequence> we should note an error
        // The last arg might also be a singleton sequence so we should check for and exploit this if possible.
        let num-required = sig.^signature-number-required;
        let apply-arg-size = apply-args.size;
        unless (num-required < apply-arg-size)
          let dispatch-result = create-function-call-record(<failed-apply>, gf, apply-args, call, css);
          let result-type = return-type-from-signature-and-call(sig, call.temporary);
          dispatch-result.inferred-type := result-type;
          record-computation-record(css, call, dispatch-result);
          return(result-type);
        end;
        // drop the last arg (which should be a <sequence>) and then make the number of optionals even
        let additional = ash(ash(apply-arg-size - num-required - 1, -1), 1);
        let args = copy-sequence(apply-args, end: num-required + max(0, additional));
        let (products, status) = CPA-expand-args(args);
        if (products)
          let final-result :: false-or(<dispatch-record>)= #f;
          // NB. There's always at least 1 element here
          for (i from 0 below products.size)
            let product = products[i];
            let (known?, dispatch-result) = 
              typist-all-applicable-methods-guaranteed-known?(gf, product, call, css);
            if (known?)
              let cached-result = probe-gf-secondary-cache(gf, product);
              if (cached-result)
                process-secondary-cache-result(cached-result, call, css);
                if (final-result)
                  final-result := dispatch-result-merge(final-result, cached-result);
                else
                  final-result := cached-result;
                end;
              else
                let dispatch-result = typist-estimate-effective-method(gf, product, call, css);
                process-dispatch-result(dispatch-result, gf, product, call, css, agenda);
                if (final-result)
                 final-result := dispatch-result-merge(final-result, dispatch-result);
                else
                  final-result := dispatch-result;
                end;
              end;
            else
              // Open GF: update the various caches
              let result-type = return-type-from-signature-and-call(sig, call.temporary);
              dispatch-result.input-types := as(<list>, args);  // gts, merge, as <list>
              dispatch-result.inferred-type := result-type;
              record-computation-record(css, call, dispatch-result);
              update-gf-primary-cache(gf, apply-args, dispatch-result);
              return(result-type);
            end;
          end; // for (i from 0 below products.size)

          if (instance?(final-result, <no-applicable-methods>))
            delayed-typist-note(css, call, <no-applicable-methods-in-call>,
              computation-record: final-result,
              source-location: dfm-source-location(call),
              context-id:      dfm-context-id(call),
              function: gf,
              supplied-type-estimates: args);
	  end;

          final-result.input-types := as(<list>, args);  // gts, merge, as <list>
          record-computation-record(css, call, final-result);
          update-gf-primary-cache(gf, apply-args, final-result);
          final-result.inferred-type;

        elseif (status == megamorphic:)
          let dispatch-result = create-function-call-record
                                 (<megamorphic-args>, gf, apply-args, call, css);
          let result-type = return-type-from-signature-and-call(sig, call.temporary);
          dispatch-result.inferred-type := result-type;
          record-computation-record(css, call, dispatch-result);
          update-gf-primary-cache(gf, apply-args, dispatch-result);
          result-type;
        elseif (status == bottom:)
          let dispatch-result = create-function-call-record
                                 (<bottom-in-arg-list>, gf, apply-args, call, css);
          let result-type = typist-<bottom-type>();
          dispatch-result.inferred-type := result-type;
          record-computation-record(css, call, dispatch-result);
          update-gf-primary-cache(gf, apply-args, dispatch-result);
          result-type;
        else
          error("Unrecognized status from CPA expansion: %s : %s", status, args);
        end; // IF (PRODUCTS)
      end; //BLOCK (RETURN)
    end;
  else
    return-type-from-signature-and-call(sig, call.temporary);
  end;
end;

define method probe-gf-primary-cache (gf :: <&generic-function>, args :: <arg-types>)
  => (result :: false-or(<dispatch-record>));
  let result = element(gf.dispatch-result-cache, args, default: #f);
  if (result & ~instance?(result, <bottom-in-arg-list>))
    result
  elseif (cross-library-call?(gf))
    let cache = current-top-level-library-description().library-dispatch-result-cache;
    element(cache, pair(gf, args), default: #f)
  end;
end;

define method probe-gf-secondary-cache (gf :: <&generic-function>, args :: <arg-types>)
  => (result :: false-or(<dispatch-record>));
  let result = element(gf.effective-method-cache, args, default: #f);
  result
  | if (cross-library-call?(gf))
      let cache = current-top-level-library-description().library-dispatch-result-cache;
      element(cache, pair(gf, args), default: #f)
    end;
end;

define method update-gf-primary-cache
  (gf :: <&generic-function>, args :: <arg-types>, dispatch-result :: <dispatch-record>)
  => ();
  if (cross-library-call?(gf))
    let cache = current-top-level-library-description().library-dispatch-result-cache;
    element(cache, pair(gf, args)) := dispatch-result;
  else
    element(gf.dispatch-result-cache, args) := dispatch-result;
  end
end;

define method update-gf-secondary-cache
  (gf :: <&generic-function>, args :: <arg-types>, dispatch-result :: <dispatch-record>)
  => ();
  if (cross-library-call?(gf))
    let cache = current-top-level-library-description().library-dispatch-result-cache;
    element(cache, pair(gf, args)) := dispatch-result;
  else
    element(gf.effective-method-cache, args) := dispatch-result;
  end
end;

define method process-primary-cached-result
  (cached-result :: <dispatch-record>, call, css)
  => (type :: <&type>);
  let type = typist-<bottom-type>();
  for (called-css-or-type in cached-result.candidate-summaries)
    if (instance?(called-css-or-type, <&type>))
      type := maybe-make-inferred-union(call, type, called-css-or-type);
    else
      let called-css :: <call-site-summary> = called-css-or-type;
      type := maybe-make-inferred-union(call, type, called-css.result-type);
//    add-caller(css, called-css);
//    add-callee(called-css, css);
      record-dependency-new(called-css, call, css);
    end
  end;
  cached-result.inferred-type := type;
  let new = copy-dispatch-result(cached-result);
  new.computation := call;
  new.context := css;
  new.inferred-type := type;
  record-computation-record(css, call, new);
  type;
end;

define method process-primary-cached-result
  (cached-result :: <open-generic>, call, css)
  => (type :: <&type>);
  let new = copy-dispatch-result(cached-result);
  new.computation := call;
  new.context := css;
  new.inferred-type := cached-result.inferred-type;
  record-computation-record(css, call, new);
  new.inferred-type;
end;

define method process-primary-cached-result
  (cached-result :: <megamorphic-args>, call, css)
  => (type :: <&type>);
  let new = copy-dispatch-result(cached-result);
  new.computation := call;
  new.context := css;
  new.inferred-type := cached-result.inferred-type;
  record-computation-record(css, call, new);
  new.inferred-type;
end;


define method process-secondary-cache-result
  (cached-result :: <dispatch-record>, call, css)
  => ();
  let type = typist-<bottom-type>();
  for (called-css-or-type in cached-result.candidate-summaries)
    if (instance?(called-css-or-type, <&type>))
      type := maybe-make-inferred-union(call, type, called-css-or-type);
    else
      let called-css :: <call-site-summary> = called-css-or-type;
      type := maybe-make-inferred-union(call, type, called-css.result-type);
//    add-caller(css, called-css);
//    add-callee(called-css, css);
      record-dependency-new(called-css, call, css);
    end
  end;
  cached-result.inferred-type := type;
end;


define method process-dispatch-result
  (result :: <successful-dispatch>, gf, product, call, css, agenda);
  let (type, summary) = infer-call-type-from-function-internal
                         (call, result.dispatched-method, product, css, agenda); 
  result.inferred-type := type;
  result.candidate-summaries := list(summary);
  
  update-gf-secondary-cache(gf, product, result);

//  unless (instance?(type, <values-type>) | instance?(type, <&bottom-type>))
//    error("merging dispatch results did not yield a <values-type>");
//  end;

  result;
end;

define method process-dispatch-result
  (result :: <ambiguous-methods>, gf, product, call, css, agenda)

  // If the dispatching of an ambiguous call at run-time generates an error then
  // we should probably omit this for loop.  However, if one of the ambiguous methods is
  // chosen then taking the union of the result types is probably preferable to <bottom>.

  for (fn in result.ambiguous-methods)
    let sig = fn.^function-signature;
    let sig-required = get-required-from-signature(sig);
    let prod = constrain-product(sig-required, product);
    let (type, summary) = infer-call-type-from-function-internal
                           (call, fn, prod, css, agenda);
    result.inferred-type := maybe-make-inferred-union(call, result.inferred-type, type);
    result.candidate-summaries := pair(summary, result.candidate-summaries);
  end;

  update-gf-secondary-cache(gf, product, result);
  result;
end;

define method process-dispatch-result
  (result :: <no-applicable-methods>, gf, product, call, css, agenda)
  update-gf-secondary-cache(gf, product, result);
  result;
end;

define method process-dispatch-result
  (result :: <imprecise-argument-types>, gf, product, call, css, agenda);
  for (fn in result.potentially-applicable-methods)
    let sig = fn.^function-signature;
    let sig-required = get-required-from-signature(sig);
    let prod = constrain-product(sig-required, product);
    let (type, summary) = infer-call-type-from-function-internal
                           (call, fn, prod, css, agenda);
    result.inferred-type := maybe-make-inferred-union(call, result.inferred-type, type);
    result.candidate-summaries := pair(summary, result.candidate-summaries);
  end;
  update-gf-secondary-cache(gf, product, result);
  result;
end;

define method  infer-call-type-from-function
    (call :: <function-call> , code :: <&code> , css, w-l) => (r :: <&type>);
  // We've upgraded to here but still go back to the method
  infer-call-type-from-function(call, code.function, css, w-l);
end;

// A switch to control typing of calls to local methods
define variable *infer-local-call-types* = #t;

define inline function infer-call-type-from-function-internal
  (call :: <function-call>, def :: <&method>, args :: <sequence>, 
   css :: <call-site-summary>, agenda :: <work-agenda>, #key canonical-args?)
  => (type :: <&type>, new-css :: <call-site-summary>);

  let canonical-args = 
    if (canonical-args?)
      args
    else
      get-canonical-arg-types(call, args, css, def);
    end;

  let sig = def.^function-signature;

  let special-action = get-special-action(def);
  if (special-action)
    special-action(call, def, canonical-args, css);
  elseif (instance?(def, <&accessor-method>))
    // let new-css = get-call-site-summary(def, canonical-args);
    let type = return-type-from-signature-and-call(sig, call.temporary);
    // new-css.result-type := type;
    // values(type, new-css);
    values(type, type)
  elseif (constant-result?(sig))
    let type = return-type-from-signature-and-call(sig, call.temporary);
    values(type, get-default-call-site-summary(def));
  else

    // we need to worry about `recursive customization` if we do constant folding.
    // see chapter 5 of Agesen`s thesis for background
    let recursive-css = (*constant-fold-in-typist* 
                        & detect-recursive-blow-up(call, def, canonical-args, css));
    let new-css = recursive-css 
                  | begin
                      // We are going to make a new summary.  Better check the args are OK
                      check-arg-types!
                        (def, canonical-args, def.^function-signature, css);
/*
if (modulo(def.call-site-summaries.size, 20) = 19)
  format-out("Expanding summaries for %=\n", def);
  for (css in def.call-site-summaries, i from 0 below 10)
    format-out("  %=: %=\n", css.result-type, css)
  end;
end;
*/
                      get-call-site-summary(def, canonical-args);
                    end;
//    add-caller(css, new-css);
//    add-callee(new-css, css);
 
    if (dependents?(call.temporary))
      record-dependency-new(new-css, call, css);
    end;

    if (new-css.result-type == typist-<unknown-type>())
      unless (def.body) 
        regenerate-dfm-for(def);
      end;
      new-css.compressed? := #f;
      type-infer-lambda(def, new-css);
    end;

    let old-type = new-css.result-type;
    select (old-type by instance?)
      <unknown-type> => values(typist-<bottom-type>(), new-css);
      otherwise => values(old-type, new-css);
    end;
  end;
end;

/* This needs a bit of explaining. When we constant fold we can fall in the `recursive customization`
   trap. For us this is characterised by recursive calls which have singleton(s) with <REAL> base-type(s)
   in the same position in the argument tuple. These will potentially cause the typist not to terminate if they
   are used to produce a new constant in a corresponding position in a recursive call. Once we have detected this
   situation we can `relax' the type(s) by using the base-type which will prevent non-termination. The trick is to
   retain precision while keeping the cost of detection reasonably cheap. There are number of options:

   1. Punt as soon as we find <real> singletons in the arg tuple. This seems cheap skate, but its possible.
   2. Punt as soon as we find that we`ve already encountered a call with a <real> singleton in a corresponding
      position.
   3. Only punt if we already have a summary with a singleton in one of the indicated positions and there is a
      a path from that summary to the current summary. This is the most accurate, but unless we incurr a running
      cost to keep the search cheaper, this search could have an exhorbitant cost. And note we have to search until 
      we`ve searched everywhere or until we fail. The problem is the likely case (i.e. that the call is ok) is the most
      expensive.

*/

// Option 1

// Let's be really conservative for now.

define function boring-singleton? (x)
  instance?(x, <&singleton>) 
  & ( ^subtype?(x, dylan-value(#"<real>"))
    | ^subtype?(x, dylan-value(#"<symbol>"))
    | (^subtype?(x, dylan-value(#"<string>"))
      & x.^singleton-object.size > 0) )
end;

define function type-through-method? (f :: <&method>) => (res :: <boolean>)
  let definition = model-definition(f);
  if (definition)
    select (form-type-through-policy(definition))
      type-through: => #t;
      no-type-through: => #f;
      default-type-through: =>
        method-inlineable?(f) | ~cross-library-call?(f)
    end
  else
    #t
  end if;
end;

define function detect-recursive-blow-up (call, lambda, args, css)
  let summaries = lambda.call-site-summaries;
  let new-css = element(summaries, args, default: #f);
  if (new-css)
    new-css;
  elseif (~type-through-method?(lambda))
    get-default-call-site-summary(lambda);
  else 
    local method find-singleton-keys(args)
            let keys = #();
            for (i from 0 below args.size)
              when (boring-singleton?(args[i]))
                keys := pair(i, keys);
              end;
            end;
            unless (empty?(keys))
              keys;
            end;
          end;

    let singleton-keys = find-singleton-keys (args);

    when (singleton-keys)
      let default-css = get-default-call-site-summary(lambda);
      let default-args = default-css.arg-types;
      let default-size = default-args.size;
      let relaxed-arg-types = copy-sequence(args);
      for (i in singleton-keys)
	unless (i >= default-size) 
          let default-arg = default-args[i];
          if (instance?(default-arg, <&singleton>))
            relaxed-arg-types[i] := default-arg;
	  else
            relaxed-arg-types[i] := args[i].^singleton-object.^object-class;
          end
        end;
      end;
      let relaxed-css = get-call-site-summary(lambda, relaxed-arg-types);
      lambda.call-site-summaries[args] := relaxed-css;
      relaxed-css;
    end
  end
end;


/* Option 2

define function detect-recursive-blow-up (call, lambda, args, css)
  let summaries = lambda.call-site-summaries;
  let new-css = element(summaries, args, default: #f);
  if (new-css)
    new-css;
  elseif (summaries.size < 4)
    #f
  else 
    let real-type = dylan-value(#"<real>");
    local method singleton-real? (x)
            instance?(x, <&singleton>)
            & ^subtype?(x, real-type)
          end,
          method find-singleton-keys(args)
            let keys = #();
            for (i from 0 below args.size)
              when (singleton-real?(args[i]))
                keys := pair(i, keys);
              end;
            end;
            unless (empty?(keys))
              keys;
            end;
          end;

    let singleton-keys = find-singleton-keys (args);

    when (singleton-keys)
      block (return)
        for (summary in summaries)
          let css-arg-types = summary.arg-types;
          for (i in singleton-keys)
            when (i < css-arg-types.size & singleton-real?(css-arg-types[i]))
              let relaxed-arg-types = copy-sequence(args);
              for (i in singleton-keys)
                relaxed-arg-types[i] := args[i].^singleton-object.^object-class;
              end;
              let relaxed-css = get-call-site-summary(lambda, relaxed-arg-types);
              lambda.call-site-summaries[args] := relaxed-css;
              return(relaxed-css);
            end;
          end;
        end;
      end;
    end;
  end;
end;
*/

define inline function constant-result?(sig :: <&signature>) => (b :: <boolean>)
  let rest-val = sig.^signature-rest-value;
  block (return)
    if (~rest-val | instance?(rest-val, <&singleton>))
      let vals = sig.^signature-values;
      for (i from 0 below sig.^signature-number-values)
        let val = vals[i];
        unless (instance?(val, <&singleton>) | instance?(val, <&bottom-type>))
          return(#f)
        end;
      end;
      return(#t)
    end
  end
end;

define inline function result-not-used?(call, css) => (b :: <boolean>)
  let call-users = call.temporary.users;
  empty?(call-users)
  // Unfortunately the compiler sometimes generates misleading return nodes...
  | ( call-users.size = 1 
    & instance?(call-users[0], <return>)
    & begin
        let result-spec = css.css-lambda.signature-spec.spec-values-spec;
        ~result-spec.spec-rest-variable-spec & empty?(result-spec.spec-required-variable-specs)
      end )
end;


define method  infer-call-type-from-function
    (call :: <function-call> , d :: <&method> , css, w-l) => (r :: <&type>);
  let args = get-arg-types(call.arguments, css, call);
  let arg-types = get-canonical-arg-types(call, args, css, d);
  let sig = d.^function-signature;
  let number-required = sig.^signature-number-required;

  if (d == dylan-value(#"values"))
    if (call.call-iep?)
      unless (args.size = 1)
        break("values");
      end; 
      let call-type = 
        make(<values-type>, types: #[], rest-type: typist-<object-type>());
      call-type
    else
      let call-type = make(<values-type>, types: args);
      call-type
    end
  elseif (sig.^signature-number-required > args.size)
    // Error already noted by call to get-canonical-arg-types
    if (call.temporary)
      make(<values-type>, 
           types: $the-empty-vector, 
           rest-type: typist-values-rest-type());
    else
      // nothing uses the result of this call so just return a handy type.
      typist-<unknown-type>();
    end

  elseif (d == dylan-value(#"apply"))
    let obj = lookup-type(call.arguments[0], css, call);
    if (instance?(obj, <&singleton>))
      let fn = obj.^singleton-object;
      if (instance?(fn, <&function>))
        infer-apply-type-from-function
          (call, fn, copy-sequence(call.arguments, start: 1), css, w-l);
      else
        delayed-typist-note(css, call, <non-function-in-apply-call>, 
                    source-location: dfm-source-location(call),
                    context-id: dfm-context-id(call),
                    type-estimate: obj);
        return-type-from-signature-and-call(sig, call.temporary);
      end;
    else
      return-type-from-signature-and-call(sig, call.temporary);
    end

  elseif (~arguments-potentially-joint?
             (arg-types, ^signature-required(sig), number-required))
    let dispatch-result = create-computation-record
                            (<illegal-call-record>, call, css, call.arguments);
    /*delayed- */ typist-note(/* css, call, */ <argument-types-mismatch-in-call>,
         computation-record: dispatch-result,
         source-location: dfm-source-location(call),
         context-id:      dfm-context-id(call),
         function: d,
         required-types: get-required-from-signature(sig),
         supplied-type-estimates: arg-types);
    

//    break("arg mismatch2, %=, %=, %=, %=, %=", 
//          css, call, d, get-required-from-signature(sig), arg-types);

    let result-type =
      if (call.temporary)
        make(<values-type>, 
             types: $the-empty-vector, 
             rest-type: typist-values-rest-type());
      else
        // nothing uses the result of this call so just return a handy type.
        typist-<unknown-type>();
      end;

    dispatch-result.inferred-type := result-type;
    record-computation-record(css, call, dispatch-result);
    result-type

//  Damn - even if the result is not used we may need to type the body
//         because of side-effects on bind-exits for example
//  However, the treatment of <bind-exit> is totally bonkers at the moment 
//  anyway, and has been disabled.  So we might as well carry on with this
//  optimization anyway for the moment.
  elseif (result-not-used?(call, css))
    let result-type = typist-<unknown-type>();
    let call-record = create-function-call-record(<unique-method>, 
      d, args, call, css, method: d, summaries: vector(get-default-call-site-summary(d)));
    call-record.inferred-type := result-type;
    record-computation-record(css, call, call-record);
    result-type

  else
    let (result-type, summaries) =
      if ((*infer-local-call-types* & ~lambda-top-level?(d))
          |(*infer-through-non-locals* & tight-mode?(css.css-lambda, d)))
        if (args.size > 0)
          collecting (result-types)
            collecting (summaries)
              let (products, status) = CPA-expand-args(arg-types);
              if (products)
                for (i from 0 below products.size)
                  let product = 
                    check-product
                      (^signature-required(sig), products[i], number-required);

                  when (product)
                    let (type, summary) = 
                      infer-call-type-from-function-internal
                        (call, d, product, css, w-l, canonical-args?: #t);
                    collect-into(result-types, type);
                    collect-into(summaries, summary);
                  end;
                end for;
                let rts = collected(result-types);
                if (rts.size = 0)
                  values(typist-<bottom-type>(), #());
                elseif (rts.size = 1)
                  values(rts[0], as(<list>, collected(summaries)));
                else
                  values(reduce1(curry(maybe-make-inferred-union, call), rts), 
                         as(<list>, collected(summaries)));
                end;
              elseif (status == megamorphic:)
                values(return-type-from-signature-and-call(sig, call.temporary),
                       as(<list>, collected(summaries)));
              elseif (status == bottom:)
                values(typist-<bottom-type>(), as(<list>, collected(summaries)));
              else
                error("Unrecognized status from CPA expansion: %s : %s", status, args);
              end;
            end collecting; // summaries
          end collecting;  // result-types
        else
          // no args
          infer-call-type-from-function-internal
            (call, d, arg-types, css, w-l, canonical-args?: #t);
        end;
      else
        values(return-type-from-signature-and-call(sig, call.temporary), #());
      end;

    let call-record = create-function-call-record(<unique-method>, 
      d, args, call, css, method: d, summaries: summaries);
    call-record.inferred-type := result-type;
    
    record-computation-record(css, call, call-record);
    result-type;
  end
end;  


define method  infer-apply-type-from-function
    (call , d :: <&lambda> , supplied-args, css, w-l) => (r :: <&type>);
  // TODO 
  let apply-args = get-arg-types(supplied-args, css, call);
  let sig = d.^function-signature;
  let num-required = ^signature-number-required(sig);

  if (d == dylan-value(#"values"))
    make(<values-type>, 
          types: copy-sequence(apply-args, end: apply-args.size - 1), rest-type: typist-<object-type>());
  else
  let (result-type, summaries) =
    if ((*infer-local-call-types* & ~lambda-top-level?(d))
        |(*infer-through-non-locals* & tight-mode?(css.css-lambda, d)))

      // TODO: if we know the last arg is disjoint from <sequence> we should note an error
      // The last arg might also be a singleton sequence so we should check for and exploit this if possible.
      let apply-arg-size = apply-args.size;
      if (num-required >= apply-arg-size)
        let call-result = create-function-call-record
                           (<applied-method-record>, d, apply-args, call, css, summaries: #());
        let result-type = return-type-from-signature-and-call(sig, call.temporary);
        call-result.inferred-type := result-type;
        record-computation-record(css, call, call-result);
        values(result-type, #());
      else
        // drop the last arg (which should be a <sequence>)
        let additional = apply-arg-size - num-required - 1;
        // make the number of optionals even unless we are applying values
        let additional = 
	  if (d == dylan-value(#"values")) additional else ash(ash(additional, -1), 1) end;
        let args = copy-sequence(apply-args, end: num-required + max(additional, 0));

        if (args.size > 0)
          collecting (result-types)
            collecting (summaries)
              let (products, status) = CPA-expand-args(args);
              if (products)
                for (i from 0 below products.size)
                  let product = 
                    check-product(sig.^signature-required, products[i], num-required); 
                  when (product)
                    let (type, summary) = infer-call-type-from-function-internal
                                           (call, d, product, css, w-l);
                    collect-into(result-types, type);
                    collect-into(summaries, summary);
                  end;
                end for;
                let rts = collected(result-types);
                if (rts.size = 0)
                  values(typist-<bottom-type>(), #());
                elseif (rts.size = 1)
                  values(rts[0], as(<list>, collected(summaries)));
                else
                  values(reduce1(curry(maybe-make-inferred-union, call), rts), 
                         as(<list>, collected(summaries)));
                end;
              elseif (status == megamorphic:)
                values(return-type-from-signature-and-call(sig, call.temporary), #());
              elseif (status == bottom:)
                values(typist-<bottom-type>(), #());
              else
                error("Unrecognized status from CPA expansion: %s : %s", status, args);
              end;
            end collecting; // summaries
          end collecting;  // result-types
        else
          // no args
          infer-call-type-from-function-internal(call, d, args, css, w-l);
        end;
      end;
    else
      values(return-type-from-signature-and-call(sig, call.temporary), #());
    end;

  let call-record = create-function-call-record(<applied-method-record>, d, apply-args, call, css,
                                                summaries: summaries);
  call-record.inferred-type := result-type;
  record-computation-record(css, call, call-record);
  result-type;
  end
end;  

 
// Recovery of argument-types 

define method maybe-propagate-arg-types (call, css, work-agenda)
end;

define method maybe-propagate-arg-types (call :: <function-call>, css, work-agenda)
  let fn = lookup-type(call.function, css, call);
  maybe-propagate-arg-types-internal(call, fn, css, work-agenda)
end;

define method maybe-propagate-arg-types-internal(call, fn, css, work-agenda)
end;

define method maybe-propagate-arg-types-internal(call, fn :: <&singleton>, css, work-agenda)
  maybe-propagate-arg-types-internal(call, fn.^singleton-object, css, work-agenda);
end;

define method maybe-propagate-arg-types-internal(call, fn :: <&generic-function>, css, work-agenda)
  let result = get-computation-record(css, call);
  when (result)
    constrain-call-arg-types(result);
  end;
end;

define method maybe-propagate-arg-types-internal(call, fn :: <&method>, css, work-agenda)
  let supplied = get-arg-types(call.arguments, css, call);
  let required = get-required-from-signature(fn.^function-signature);
  recover-arg-types(call, supplied, required, css);
end;

define method constrain-call-arg-types (result :: <dispatch-record>)
end;

define method constrain-call-arg-types (result :: <illegal-call-record>)
end;

define method constrain-call-arg-types (result :: <open-generic>)
  let call = result.computation;
  let gf = lookup-type(call.function, result.context, call).^singleton-object;
  let required = get-required-from-signature(gf.^function-signature);
  recover-arg-types
    (result.computation, result.input-types, required, 
     result.context);
end;

define method constrain-call-arg-types (result :: <failed-sealed-generic>)
  let call = result.computation;
  let gf = lookup-type(call.function, result.context, call).^singleton-object;
  let required = get-required-from-signature(gf.^function-signature);
  recover-arg-types
    (result.computation, result.input-types, required, 
     result.context);
end;

define method constrain-call-arg-types (result :: <imprecise-argument-types>)
  let methods = result.potentially-applicable-methods;
  let sig = methods[0].^function-signature;
  let arg-specializers = copy-sequence(get-required-from-signature(sig));
  let limit = arg-specializers.size;
  for (m in methods.tail)
    let specs = m.^function-signature.^signature-required;
    for (j from 0 below limit)
      arg-specializers[j] := cached-type-union(arg-specializers[j] , specs[j]);
    end;
  end;
  recover-arg-types
    (result.computation, result.input-types, arg-specializers, result.context);
end;


define method recover-arg-types
  (call, supplied-args, required-arg-types, css)
  let args = call.arguments;
  unless (supplied-args.size < required-arg-types.size)
    for (i from 0 below required-arg-types.size)
      let arg = args[i];
      let required-arg-type = required-arg-types[i];
      let arg-type = supplied-args[i];
      when(arg.users.size > 1
           & ^subtype?(required-arg-type, arg-type) 
           & ~^subtype?(arg-type, required-arg-type))
        install-recovered-renaming(call, arg, required-arg-type, css);
      end;
    end;
  end;
end;

define method install-recovered-renaming
  (call, temp, type, css)
  let renaming = #f;
  let ifs-seen = 0;
  local method end? (comp)
          comp.object-class == <merge>
          & (ifs-seen = 0)
        end,

        method record-use(c :: <computation>)
          if (instance?(c, <if>))
            ifs-seen := ifs-seen + 1;
          elseif (c.object-class == <merge>)
            ifs-seen := ifs-seen - 1;
          end;
          when (member?(c, temp.users))
            unless (renaming)
              renaming := make(<recovered-renaming>,
                               value: temp,
                               comp: call,
                               css: css);
            end;
            let mapping = element(css.renaming-map, c, default: #f);
            if (mapping)
              css.renaming-map[c] := add(mapping, pair(temp, renaming));
            else
              css.renaming-map[c] := list(pair(temp, renaming));
            end;
          end;
        end;
  typist-walk-computations(record-use, call.next-computation, css, end?);
  when (renaming)
    css.renaming-computations[call] := pair(pair(temp, renaming),
                                            element(css.renaming-computations, call, default: #()));
    css.renamed-temporaries[temp] := pair(renaming,
                                          element(css.renamed-temporaries, temp, default: #()));
    css.pool[renaming] := type;

  end;

end;

/* 
  TODO: tidy  this up.
  stuff for supporting special actions on things like 'system-allocate-simple-instance` etc
*/


// (gts, comments) special action signature:
//  (<call>, <&method>, args::<sequence>, <call-site-summary>)
//  => (<&type>, <call-site-summary>

define variable *special-actions* = make(<table>);

define inline function get-special-action(x) => (action :: false-or(<function>));
  element(*special-actions*, x, default: #f); 
end;

// TODO: we need to check for illegal arguments in these special actions.

define function get-special-call-site-summary 
  (lambda :: <&method>, argument-types :: <arg-types>, result :: <&type>)
  => (r :: <call-site-summary>)
  let css = get-call-site-summary(lambda, argument-types, create-compressed?: #t);
  if (instance?(css.result-type, <unknown-type>))
    css.result-type := result;
  end;
  css
end;

// (gts) shouldn't this return a ccs too?
define function values-special-action
  (call, def, arg-types, css)
  make(<values-type>, types: arg-types);
end;

define function default-special-action
  (call, def, arg-types, css)
  let default-css = get-default-call-site-summary(def);
  if (instance?(default-css.result-type, <unknown-type>))
    unless (def.body) 
      regenerate-dfm-for(def);
    end;
    default-css.compressed? := #f;
    type-infer-lambda(def, default-css);
  end;
  values(default-css.result-type, default-css)
end;

define function constant-special-action
    (result :: <&type>) => (constant-function :: <function>)
  method (call, def, arg-types, css) 
    values(result, get-special-call-site-summary(def, arg-types, result))
  end
end;

define function setter-special-action
  (call, def, arg-types, css)
  let result = arg-types[0];
  // gts, this looks wrong -- 2nd return value should be a CSS:
  values(result, result)
end;

define function system-allocate-special-action
  (call, def, arg-types, css)
  let class-type = arg-types[0];
  let the-type = if (instance?(class-type, <&singleton>))
                   class-type.^singleton-object;
                 else
                   dylan-value(#"<object>");
                 end;
  let type = make(<values-type>, types: vector(the-type));
/*
  let new-css = get-call-site-summary(def, arg-types);
  if (instance?(new-css.result-type, <unknown-type>))
    unless (def.body) 
      regenerate-dfm-for(def);
    end;
    new-css.compressed? := #f;
    type-infer-lambda(def, new-css);
    new-css.result-type := type;
  end;
*/
  let new-css = get-default-call-site-summary(def);
//  add-caller(css, new-css);
//  add-callee(new-css, css);
  values(type, new-css);
end;

define function initialize-special-actions ()
  *special-actions*[dylan-value(#"system-allocate-simple-instance")] 
    := system-allocate-special-action;
  *special-actions*[dylan-value(#"system-allocate-simple-instance-i")] 
    := system-allocate-special-action;
// gts - what is this?     98may22
//  *special-actions*[dylan-value(#"system-allocate-instance-of-byte-size")] 
//    := system-allocate-special-action;
  *special-actions*[dylan-value(#"system-allocate-repeated-instance")] 
    := system-allocate-special-action;
//  *special-actions*[dylan-value(#"system-allocate-repeated-byte-instance")] 
//    := system-allocate-special-action;
  *special-actions*[dylan-value(#"values")] 
    := values-special-action;

  // Assume constant-folding deals with the interesting instances of the
  // following cases.
  *special-actions*[dylan-value(#"pair")] 
    := default-special-action;
  *special-actions*[dylan-value(#"==")]        
    := constant-special-action(dylan-value(#"<boolean>"));

  *special-actions*[dylan-value(#"list-next-state")]        
    := constant-special-action(dylan-value(#"<list>"));
  *special-actions*[dylan-value(#"list-finished-state?")]        
    := constant-special-action(dylan-value(#"<boolean>"));
  *special-actions*[dylan-value(#"list-current-element")]        
    := constant-special-action(dylan-value(#"<object>"));

//  instance? gets inlined, so we need a better approach than this...
//  for (lambda in dylan-value(#"instance?").^generic-function-methods-known)
//    *special-actions*[lambda] := constant-special-action(dylan-value(#"<boolean>"));
//  end;

  for (lambda in dylan-value(#"tail-setter").^generic-function-methods-known)
    unless (method-inlineable?(lambda))
      *special-actions*[lambda] := setter-special-action;
    end
  end;
  for (lambda in dylan-value(#"element-setter").^generic-function-methods-known)
    unless (method-inlineable?(lambda))
      *special-actions*[lambda] := setter-special-action;
    end
  end;

  let class-type = dylan-value(#"<class>");
  for (lambda in dylan-value(#"make").^generic-function-methods-known)
    if (lambda.^function-signature.^signature-required[0] == class-type)
      // I.e. this is the default make method
      *special-actions*[lambda] := 
        method (call, def, arg-types, css)
          let make-class = arg-types[0];
          let result = 
            if (instance?(make-class, <&singleton>))
              make-class.^singleton-object
            else
              class-type
            end;
          values(result, get-special-call-site-summary(def, arg-types, result))
        end;
    end
  end;
end;


define method guaranteed-method-precedes?
    (m1 :: <&method>, m2 :: <&method>, arg-te* :: <sequence>) 
  guaranteed-method-relationship(m1, m2, arg-te*) == $method1-precedes
end method;

// This is pretty much as per the DRM specification, but using the
// conservative "guaranteed-xxx" predicates.

define method guaranteed-method-relationship
    (m1 :: <&method>, m2 :: <&method>, arg-te* :: <sequence>) 
 => (relationship)
  let precedes-somewhere? = #f;
  let follows-somewhere? = #f;
  let specs1 = m1.^function-specializers;
  let specs2 = m2.^function-specializers;
  for (arg-te in arg-te*, spec1 in specs1, spec2 in specs2)
    case
      spec1 == spec2
        => ; // continue
      guaranteed-preceding-specializer?(spec1, spec2, arg-te)
	=> unless (^subtype?(spec2, spec1)) precedes-somewhere? := #t end;
      guaranteed-preceding-specializer?(spec2, spec1, arg-te)
        => follows-somewhere? := #t;
      otherwise
        => ; // continue
    end;
  end;
  if (precedes-somewhere?)
    if (follows-somewhere?)
      $methods-unordered
    else
      $method1-precedes
    end
  else
    if (follows-somewhere?)
      $method2-precedes
    else
      $methods-unordered
    end;
  end;
end method;

// Do we know all the methods on this generic function for the given
// argument type estimates.

define method all-applicable-methods-guaranteed-known? 
    (f :: <&generic-function>, arg-te* :: <sequence>) => (known?)
  // TODO: Consider dynamic screw cases.
  ^generic-function-sealed?(f)
    | begin
        local method domain-guaranteed-joint? (domain)
          let specializers = ^domain-types(domain);              
          arguments-guaranteed-joint?(arg-te*, specializers, specializers.size)
        end;
        any?(domain-guaranteed-joint?, ^generic-function-domains(f));
      end
end method;

// TODO: This test of domain inclusion is insufficient. If the generic is
// individually sealed over both branches of a test union type, we 
// should win here.

//   domain <integer>
//   domain <symbol>
//   sealed-over type-union(<integer>, <symbol>)?

// Coalescing domain declarations could be tricky to get right. It might
// be easier to expand any type estimate unions here? Unions aren't the
// only things, though. Limited integers have the same problem (although
// they could be considered unions). In fact, there are examples of 
// classes alone.

//   sealed <a> (<object>)
//   <b> (<a>)
//   <c> (<a>)
//   domain (<b>)
//   domain (<c>)
//   sealed-over <a> ?

// Easier if we canonicalize the type a union of known concrete subtypes
// I guess.

define method arguments-guaranteed-joint? 
    (arg-te* :: <sequence>, domain-type* :: <sequence>, sig-size :: <integer>) 
       => (joint?)
  block (return)
    for (arg-te in arg-te*, domain-type in domain-type*,
         i from 0 below sig-size)
      unless (guaranteed-joint?(arg-te, domain-type))
        return(#f)
      end unless;
    end for;
    #t
  end block;
  // every?(guaranteed-joint?, arg-te*, domain-type*);
end method;

define method arguments-potentially-joint? 
    (arg-te* :: <sequence>, domain-type* :: <sequence>, sig-size :: <integer>)
       => (joint?)
  block (return)
    for (arg-te in arg-te*, domain-type in domain-type*, 
         i from 0 below sig-size)
      unless (potentially-joint?(arg-te, domain-type))
        return(#f)
      end unless;
    end for;
    #t
  end block;
  // every?(potentially-joint?, arg-te*, domain-type*);
end method;


// Loop Calls

define method initialize-node-type 
  (call :: <loop-call>, css, work-agenda) => ()
  initialize-node-type-internal(call, css, work-agenda);
end;

define method infer-node-type 
  (call :: <loop-call>, css, w-l) => (result :: <&type>)  
  typist-<bottom-type>();
end;


// Can this be simplified as we now know we have a loop call?

define method refine-initial-node-type 
    (call :: <loop-call>, css :: <call-site-summary>, wa :: <work-agenda>) => ()
  let temp = call.temporary;
  let initial-type = lookup-type(temp, css, call);
  let new-type = infer-node-type(call, css, wa);
  let renamings = element(css.renaming-computations, call, default: #f);
  when (renamings)
    for (x in renamings)      
      let tmp = x.head;
      let renaming = x.tail;
      schedule-users-of-renaming(wa, tmp.users, renaming, css);
      delete-renamings(tmp, css, call);
    end;
  end;
  maybe-propagate-arg-types(call, css, wa);
  unless (^subtype?(new-type, initial-type))
    schedule-renaming-computations(temp, css, wa);
    css.pool[temp] :=  maybe-make-inferred-union(call, new-type, initial-type);
    schedule-users(wa, temp.users, css);
  end;
end;







