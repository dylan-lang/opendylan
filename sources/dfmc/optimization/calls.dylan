Synopsis: optimizing special calls
Author:   Jonathan Bachrach and Keith Playford
Module:   dfmc-optimization
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define &optimizer-function primitive-not
    (env, call, arguments)
  do-optimize-primitive-not(env, call, arguments)
end &optimizer-function;

define method replace-call-argument!
    (call :: <call>, new-arg :: <value-reference>, index :: <integer>)
  let args = arguments(call);
  remove-user!(args[index], call);
  add-user!(new-arg, call);
  args[index] := new-arg;
end method;

define method do-optimize-primitive-next-methods-parameter
    (env :: <environment>, call :: <primitive-call>,
     call-args :: <argument-sequence>)
  let lambda :: <&lambda> = lambda(lambda-environment(env));
  when (^function-signature(lambda))
    let specializers
      = map(curry(as, <type-estimate>), ^function-specializers(lambda));
    let gf = ^method-generic-function(lambda);
    when (gf & instance?(gf, <&generic-function>))
      let effectives = estimate-effective-methods(gf, specializers, lambda);
      unless (empty?(effectives)
                | any?(rcurry(instance?, <&accessor-method>), effectives))
        let effectives-ref
          = make-value-reference
              (tail(effectives), <immutable-object-reference>);
        replace-computation-with-temporary!(call, effectives-ref);
        #t;
      end unless;
    end when;
  end when;
end method;

define &optimizer-function primitive-next-methods-parameter
    (env :: <environment>, call :: <primitive-call>,
     arguments :: <argument-sequence>)
  do-optimize-primitive-next-methods-parameter(env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-instance?
    (env :: <environment>, call :: <primitive-call>,
     call-args :: <argument-sequence>)
  let type-ref = call-args[1];
  let (type-constant?, static-type) = fast-constant-value?(type-ref);
  if (type-constant?)
    let object = call-args[0];
    case
      guaranteed-joint?(type-estimate(object), static-type) =>
        let true-tmp = make-object-reference(#t);
        replace-computation-with-temporary!(call, true-tmp);
        #t;
      guaranteed-disjoint?(type-estimate(object), static-type) =>
        let false-tmp = make-object-reference(#f);
        replace-computation-with-temporary!(call, false-tmp);
        #t;
    end case
  end if;
end method;

define &optimizer-function primitive-instance?
    (env :: <environment>, call :: <primitive-call>,
     arguments :: <argument-sequence>)
  do-optimize-primitive-instance?(env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-not
    (env :: <environment>, call, call-args)
  let arg = call-args[0];
  let gen = generator(arg);
  if (instance?(gen, <primitive-call>))
    select (primitive(gen))
      dylan-value(#"primitive-not")
        => let (call-c, call-t)
             = make-with-temporary
                 (env, <primitive-call>,
                  primitive: dylan-value(#"primitive-as-boolean"),
                  arguments: copy-sequence(arguments(gen)));
           replace-call-computation!(env, call, call-c, call-c, call-t);
           re-optimize(call-c);
           #t;
      dylan-value(#"primitive-as-boolean")
        => replace-call-argument!(call, first(arguments(gen)), 0);
           re-optimize(gen);
           #t;
      otherwise
        => #f;
    end select;
  end if;
end method;

define &optimizer-function primitive-as-boolean
    (env, call, arguments)
  do-optimize-primitive-as-boolean(env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-as-boolean
    (env :: <environment>, call, call-args)
  let arg = call-args[0];
  let arg-type = type-estimate(arg);
  if (guaranteed-joint?(arg-type, dylan-value(#"<boolean>")))
    replace-computation-with-temporary!(call, arg);
    #t
  end if;
end method;

define &optimizer-function primitive-id?
    (env, call, arguments)
  do-optimize-primitive-id?(env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-id?
    (env :: <environment>, call, call-args)
  // PID?(X, #F) == X | PID?(#F, X) == X
  local method do-arg (arg, other-arg) => (did-it? :: <boolean>)
    let (arg-constant?, arg-raw-value) = fast-constant-value?(arg);
    if (arg-constant? & arg-raw-value == #f)
      let (new-call, new-ref)
        = make-with-temporary
          (env, <primitive-call>,
           primitive: dylan-value(#"primitive-not"),
           arguments: vector(other-arg));
      replace-computation!(call, new-call, new-call, new-ref);
      re-optimize(new-call);
      #t
    end if;
  end method;

  do-arg(call-args[1], call-args[0])
    | do-arg(call-args[0], call-args[1]);
end method;

define macro primitive-coercion-inverses-definer
  { define ?adjective:name primitive-coercion-inverses ?p1:name of ?p2:name }
    => { define function "do-optimize-" ## ?p1
             (env :: <environment>, call, arg)
           let arg-gen = generator(arg);
           // TODO: NO CLOSED OVER DATA
           if (arg-gen & env == environment(arg-gen))
             do-primitive-coercion-inverses
               (env, call, arg, arg-gen, dylan-value(?#"p2"), ?#"adjective");
           end if;
         end function;
         define &optimizer-function ?p1 (env, call, arguments)
           "do-optimize-" ## ?p1(env, call, arguments[0])
         end &optimizer-function }
  { define primitive-coercion-inverses ?p1:name and ?p2:name }
    => { define forward primitive-coercion-inverses ?p1 of ?p2;
         define reverse primitive-coercion-inverses ?p2 of ?p1}
end macro;

// If the arg to a primitive coercion was generated by the coercion's
// inverse, just output a reference to the argument to the generator.

define method do-primitive-coercion-inverses
    (env :: <environment>, call, arg, arg-gen :: <primitive-call>, p2, kind)
  if (primitive(arg-gen) == p2)
    replace-computation-with-temporary!(call, arguments(arg-gen)[0]);
    // format-out("users of %=: %=.\n", arg-gen, users(temporary(arg-gen)));
    #t
  else
    do-primitive-move-coercion(env, call, arg, arg-gen)
  end if
end method;

define method in-loop? (inside-c :: <computation>, outside-c :: <computation>)
  // format-out("  IN-LOOP?\n");
  iterate loop
      (prev :: false-or(<computation>) = previous-computation(inside-c))
    // format-out("    FOUND %=\n", prev);
    case
      instance?(prev, <loop>) => #t;
      prev == outside-c       => #f;
      ~prev                   => #f;
      otherwise               => loop(previous-computation(prev));
    end case;
  end iterate;
end method;

define method do-primitive-move-coercion
    (env :: <environment>, c :: <primitive-call>, arg, arg-gen)
  // format-out("MAYBE MOVING %= GEN %= NEXT-COMP %= ARG %= NUSERS %=\n", c, arg-gen, next-computation(arg-gen), arg, size(users(arg)));
  if (arg-gen &
      next-computation(arg-gen) ~== c &
      size(users(arg)) = 1 &
      in-loop?(c, arg-gen))
    // format-out("MOVING %=\n", c);
    redirect-previous-computations!(c, c.next-computation);
    redirect-next-computations!(c, c.previous-computation);
    insert-computation-after!(arg-gen, c);
    #t
  end if;
end method;

define method do-primitive-move-log-coercion
    (env :: <environment>,
     c :: <primitive-call>, call-args :: <argument-sequence>)
  let arg-0 = call-args[0];
  let arg-1 = call-args[1];
  let constant? = fast-constant-value?(arg-1);
  do-primitive-move-coercion(env, c, arg-0, generator(arg-0))
end method;

define function merge-replace-right-value!
    (merge-node :: <binary-merge>,
     old-ref :: false-or(<value-reference>),
     new-ref :: false-or(<value-reference>)) => ()
  merge-right-value(merge-node) := new-ref;
  remove-user!(old-ref, merge-node);
  add-user!(new-ref, merge-node);
end function;

define function merge-replace-left-value!
    (merge-node :: <binary-merge>,
     old-ref :: false-or(<value-reference>),
     new-ref :: false-or(<value-reference>)) => ()
  merge-left-value(merge-node) := new-ref;
  remove-user!(old-ref, merge-node);
  add-user!(new-ref, merge-node);
end function;

// For a primitive coercion of a temp generated by an if-merge,
// push the coercion into the two branches of the if,
// in the hopes that we will encounter the inverses handled above.
//                                                  (gts, 5/30/97)
define method do-primitive-coercion-inverses
    (env :: <environment>, call, arg, arg-gen :: <binary-merge>,
     inverse-coercion, kind)
  // only do this optimization if we push unboxing into the loop
  let tmp = temporary(arg-gen);
  if (kind == #"forward")

    let merge-node = arg-gen;
    let orig-lhs-ref = merge-left-value(merge-node);
    let orig-rhs-ref = merge-right-value(merge-node);

    // clone coercion primitive call

    let (new-lhs-call, new-lhs-ref)
      = make-with-temporary
        (env, <primitive-call>,
         primitive: primitive(call),
         arguments: vector(orig-lhs-ref));
    let (new-rhs-call, new-rhs-ref)
      = make-with-temporary
        (env, <primitive-call>,
         primitive: primitive(call),
         arguments: vector(orig-rhs-ref));

    // create an inverse coercion, for other users (besides this call)
    // of the merge value

    let (inverse-coercion-call, inverse-coercion-ref)
      = make-with-temporary
        (env, <primitive-call>,
         primitive: inverse-coercion,
         arguments: vector(arg));

    insert-computation-after!(merge-node, inverse-coercion-call);
    replace-temporary-in-users!
      (arg, inverse-coercion-ref, exclude: curry(\==, inverse-coercion-call));

    re-optimize(new-rhs-call);
    re-optimize(new-lhs-call);
    re-optimize(inverse-coercion-call);

    // Push new calls into left and right branches
    // and change merge to use new temps for input
    // Need to do each side completely so that insert-before-reference! works.
    // Hack lhs:
    insert-computation-before-reference!
      (merge-node, new-lhs-call, orig-lhs-ref);
    merge-replace-left-value!(merge-node, orig-lhs-ref, new-lhs-ref);

    // Hack rhs:
    insert-computation-before-reference!
      (merge-node, new-rhs-call, orig-rhs-ref);
    merge-replace-right-value!(merge-node, orig-rhs-ref, new-rhs-ref);

    // Current call (the coercion) is now obsolete.  Replace all users
    // of this call with the temporary gen'ed by the merge node.

    replace-computation-with-temporary!(call, arg);

    // NOW MERGING RAW VALUES

    re-optimize-type-estimate(merge-node);

    #t
  end if;
end method;

define method do-primitive-coercion-inverses
    (env :: <environment>, call, arg, arg-gen :: <loop-merge>,
     inverse-coercion, kind)
  local method number-function-calls (f :: <&lambda>)
          let count = 0;
          do-callers(method (x) count := count + 1; end, f);
          count
        end method;
  let loops-function = arg-gen.environment.lambda-environment.lambda;
  // avoid this optimization if loops haven't yet been completely converted
  // this is overly conservative because it could be that these remaining
  // calls never will get converted and that we might never do this opt.
  // the better solution would be to avoid doing these low-level opts until
  // the high-level opts have been performed.
  unless (number-function-calls(loops-function) > 1)
    next-method();
  end unless;
end method;

define method do-primitive-coercion-inverses
    (env :: <environment>, call, arg, arg-gen, p2, kind)
  do-primitive-move-coercion(env, call, arg, arg-gen);
  // #f
end method;

define primitive-coercion-inverses
  primitive-byte-character-as-raw and primitive-raw-as-byte-character;

define primitive-coercion-inverses
  primitive-single-float-as-raw and primitive-raw-as-single-float;

define primitive-coercion-inverses
  primitive-double-float-as-raw and primitive-raw-as-double-float;

define primitive-coercion-inverses
  primitive-unwrap-machine-word and primitive-wrap-machine-word;

define primitive-coercion-inverses
  primitive-cast-integer-as-raw and primitive-cast-raw-as-integer;

define primitive-coercion-inverses
  primitive-cast-pointer-as-raw and primitive-cast-raw-as-pointer;

// TODO: generalize the coercion macro to accommodate wrap-c-pointer
//
// define primitive-coercion-inverses
//   primitive-unwrap-c-pointer of primitive-wrap-c-pointer at 1;
//
// define primitive-coercion-inverses
//   primitive-wrap-c-pointer of primitive-unwrap-c-pointer at 0;

define function do-optimize-primitive-wrap-c-pointer
    (env :: <environment>, call, arg)
  let arg-gen = generator(arg);
  // TODO: NO CLOSED OVER DATA
  if (arg-gen & env == environment(arg-gen)
        & primitive(arg-gen) == dylan-value(#"primitive-unwrap-c-pointer"))
    replace-computation-with-temporary!(call, arguments(arg-gen)[0]);
    #t
  end if;
end function;

define &optimizer-function primitive-wrap-c-pointer (env, call, arguments)
  do-optimize-primitive-wrap-c-pointer(env, call, arguments[1])
end &optimizer-function;

define function do-optimize-primitive-unwrap-c-pointer
    (env :: <environment>, call, arg)
  let arg-gen = generator(arg);
  // TODO: NO CLOSED OVER DATA
  if (arg-gen & env == environment(arg-gen)
        & primitive(arg-gen) == dylan-value(#"primitive-wrap-c-pointer"))
    replace-computation-with-temporary!(call, arguments(arg-gen)[1]);
    #t
  end if;
end function;

define &optimizer-function primitive-unwrap-c-pointer (env, call, arguments)
  do-optimize-primitive-unwrap-c-pointer(env, call, arguments[0])
end &optimizer-function;

define function potentially-applicable-methods
    (gf :: <&generic-function>, #rest arg-te*)
  let guaranteed-known?
    = all-applicable-methods-guaranteed-known?(gf, arg-te*);
  if (guaranteed-known?)
    let methods-known
      = ^generic-function-methods-known(gf);
    guaranteed-sorted-applicable-methods(methods-known, arg-te*);
  else
    values(#(), #())
  end if
end function;

define function simple-make-c-pointer-internal-call?
    (call :: <simple-call>) => (well? :: <boolean>)
  let arguments = arguments(call);
  let (constant?, class) = fast-constant-value?(arguments[0]);
  // ONLY ADDRESS SLOT?
  if (constant? & size(^slot-descriptors(class)) == 1
        & empty?(^class-slot-descriptors(class))
        & ^repeated-slot-descriptor(class) == #f)
    // NO SPECIAL INITIALIZE METHODS
    let (leading-sorted, others)
      = potentially-applicable-methods
          (dylan-value(#"initialize"), as(<type-estimate>, class));
    if (empty?(others) & size(leading-sorted) == 1)
      let rest-arg  = arguments[2];
      let rest-args = maybe-vector-element-references(rest-arg);
      // ONLY ADDRESS ARGUMENT
      if (rest-args & size(rest-args) == 2 & constant-value(rest-args[0]) == #"address")
        #t
      else
        #f
      end if;
    else
      // format-out("LEADING %= OTHERS %=\n", leading-sorted, others);
      #f
    end if
  else
    #f
  end if;
end function;

define function do-optimize-make-c-pointer-internal
    (env :: <environment>, call, arguments)
  if (simple-make-c-pointer-internal-call?(call))
    let class = arguments[0];
    let mm-wrapper = ^class-mm-wrapper(constant-value(class));
    let (unwrap-call, unwrap-ref)
      = make-with-temporary
          (env, <primitive-call>,
           primitive: dylan-value(#"primitive-unwrap-machine-word"),
           arguments: vector(arguments[1]));
    let (wrap-call, wrap-ref)
      = make-with-temporary
          (env, <primitive-call>,
           primitive: dylan-value(#"primitive-wrap-c-pointer"),
           arguments: vector(make-object-reference(mm-wrapper), unwrap-ref));
    let (first-c, last-c) = join-1x1!(unwrap-call, wrap-call);
    replace-call-computation!(env, call, first-c, last-c, wrap-ref);
    re-optimize(unwrap-call);
    #t
  else
    #f
  end if
end function;

define &optimizer-function make-c-pointer-internal (env, call, arguments)
  do-optimize-make-c-pointer-internal(env, call, arguments)
end &optimizer-function;

define method ^type-estimate-class-of (te :: <type-estimate-class>)
  type-estimate-class(te)
end method;

define method ^type-estimate-class-of (te :: <type-estimate-limited-instance>)
  ^object-class(type-estimate-singleton(te))
end method;

define method ^type-estimate-class-of (te :: <type-estimate>)
  #f
end method;

define method do-optimize-primitive-repeated-slot-offset
    (env :: <environment>, call, call-args)
  // format-out("OPTIMIZING REPEATED SLOT OFFSET %=\n", arguments);
  let arg = call-args[0];
  let arg-type = type-estimate(arg);
  let class = ^type-estimate-class-of(arg-type);
  if (class)
    let repeated-slot = ^repeated-slot-descriptor(class);
    if (repeated-slot)
      let offset = slot-fixed-offset-in(repeated-slot, arg-type);
      if (offset)
        let result-c-tmp
          = make-object-reference(make-raw-literal(offset + 1));
        replace-computation-with-temporary!(call, result-c-tmp);
        #t
      end if
    end if
  end if
end method;

define &optimizer-function primitive-repeated-slot-offset
    (env, call, arguments)
  do-optimize-primitive-repeated-slot-offset(env, call, arguments)
end &optimizer-function;

////
//// MACHINE-WORD PRIMITIVE OPTIMIZATIONS
////

define variable *optimize-machine-word-primitives?* = #t;

define inline method do-optimize-machine-word-binary-noop-1
   (env :: <environment>,
    call :: <primitive-call>, enabled?, static-arg, dynamic-arg,
    identity-value :: <integer>)
  if (enabled?)
    let (constant?, raw-value) = fast-constant-value?(static-arg);
    if (constant?)
      let raw-value :: <integer> = as(<integer>, ^raw-object-value(raw-value));
      if (raw-value = identity-value)
        // format-out("NOOP\n");
        replace-computation-with-temporary!(call, dynamic-arg);
        #t
      end if;
    end if;
  end if;
end method;

define method do-optimize-machine-word-binary-noop
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>,
    identity-value :: <integer>,
    #key first? = #t, second? = #t)
  if (*optimize-machine-word-primitives?*)
    let arg-0 = call-args[0];
    let arg-1 = call-args[1];
    do-optimize-machine-word-binary-noop-1
          (env, call, first?,  arg-0, arg-1, identity-value)
      | do-optimize-machine-word-binary-noop-1
          (env, call, second?, arg-1, arg-0, identity-value)
  end if;
end method;

define method do-optimize-machine-word-partial-fold-binary-op
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>,
    combine :: <function>, companion-primitive-name :: <symbol>)
  if (*optimize-machine-word-primitives?*)
    let arg-0 = call-args[0];
    let arg-1 = call-args[1];
    let (constant?, value-1) = fast-constant-value?(arg-1);
    if (constant?)
      let gen = generator(arg-0);
      // format-out("TRYING PARTIAL CALL %= GEN %=\n", call, gen);
      if (primitive-call-to?(gen, companion-primitive-name))
        let ref = second(arguments(gen));
        let (constant?, value-2) = fast-constant-value?(ref);
        if (constant?)
          let combined-value = combine(value-1, value-2);
          if (combined-value)
            let (ignore-first, ignore-last, new-arg-1)
              = convert-object-reference-1(env, combined-value);
            // format-out("PARTIAL-FOLD %= %= -> %=\n", value-1, value-2, combined-value);
            replace-call-argument!(call, first(arguments(gen)), 0);
            replace-call-argument!(call, new-arg-1, 1);
            re-optimize(gen);
            #t
          end if;
        end if;
      end if;
    end if;
  end if;
end method;

define method machine-word-primitive-call-to-and-argument?
    (env :: <environment>, call, primitive-name :: false-or(<symbol>))
 => (next-gen, argument :: false-or(<integer>))
  // format-out("LOOKING FOR %=\n", primitive-name);
  if (~primitive-name | primitive-call-to?(call, primitive-name))
    let call-args = arguments(call);
    let arg-1 = call-args[1];
    let (constant?, raw-value-1) = fast-constant-value?(arg-1);
    // format-out("  FOUND CALL %= ARG %=\n", call, raw-value-1);
    if (constant?)
      let value-1 :: <integer> = as(<integer>, ^raw-object-value(raw-value-1));
      values(generator(call-args[0]), value-1)
    else
      values(#f, #f)
    end if;
  end if;
end method;

define method machine-word-primitives-call-to-and-arguments?
   (env :: <environment>, call :: <primitive-call>, test :: <function>,
    #rest primitive-names)
 => (ref :: false-or(<value-reference>))
  let primitive-call = call;
  let number-primitives = size(primitive-names);
  let test-arguments :: <simple-object-vector>
    = make(<vector>, size: number-primitives);
  iterate loop (i :: <integer> = 0, last-gen = #f, gen = call)
    if (i < number-primitives)
      let primitive-name  = primitive-names[i];
      let (next-gen, test-argument)
        = machine-word-primitive-call-to-and-argument?
            (env, gen, primitive-name);
      if (next-gen)
        test-arguments[i] := test-argument;
        loop(i + 1, gen, next-gen)
      end if;
    elseif (apply(test, test-arguments))
      temporary(last-gen)
    end if;
  end iterate;
end method;

define method do-optimize-machine-word-fold-tag/untag-ops
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>,
    test :: <function>, #rest primitive-names)
  if (*optimize-machine-word-primitives?*)
    let reference :: false-or(<value-reference>)
      = apply(machine-word-primitives-call-to-and-arguments?,
              env, call, test, primitive-names);
    if (reference)
      // format-out("TAG/UNTAG %=\n", reference);
      replace-computation-with-temporary!(call, reference);
      #t
    end if
  end if
end method;

define function raw-mw-op
    (op :: <function>, rx :: <&raw-machine-word>, ry :: <&raw-machine-word>)
 => (z :: <&raw-machine-word>)
  let x :: <integer> = as(<integer>, ^raw-object-value(rx));
  let y :: <integer> = as(<integer>, ^raw-object-value(ry));
  make-raw-literal(op(x, y))
end function raw-mw-op;

define method do-optimize-primitive-machine-word-logxor
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 0)
    | do-optimize-machine-word-fold-tag/untag-ops
        (env, call, call-args,
         method (xor :: <integer>, ior :: <integer>, shift :: <integer>)
           let max = ash(1, shift);
           let result = ior < max & xor < max & ior = xor;
           // format-out("matching XOR %= IOR %= SHIFT %= -> %=\n",
           //            xor, ior, shift, result);
           result
         end method,
         #f,
         #"primitive-machine-word-logior",
         #"primitive-machine-word-shift-left-signal-overflow")
    | do-optimize-machine-word-fold-tag/untag-ops
        (env, call, call-args,
         method (xor :: <integer>, ior :: <integer>, andy :: <integer>)
           let result = ior = xor & logand(andy, xor) = 0;
           // format-out("matching XOR %= IOR %= AND %= -> %=\n",
           //            xor, ior, andy, result);
           result
         end method,
         #f,
         #"primitive-machine-word-logior",
         #"primitive-machine-word-logand")
    | do-primitive-move-log-coercion(env, call, call-args)
end method;

define &optimizer-function primitive-machine-word-logxor
    (env, call, arguments)
  do-optimize-primitive-machine-word-logxor(env, call, arguments)
end &optimizer-function;

define method machine-word-primitive-call-to-and-matching-argument?
   (env :: <environment>, primitive-arg :: <value-reference>,
    primitive-name :: <symbol>, test :: <function>, test-arg :: <integer>)
 => (gen :: false-or(<primitive-call>))
  let gen = generator(primitive-arg);
  if (primitive-call-to?(gen, primitive-name))
    let arg-1 = arguments(gen)[1];
    let (constant?, raw-value-1) = fast-constant-value?(arg-1);
    if (constant?)
      let value-1 :: <integer> = as(<integer>, ^raw-object-value(raw-value-1));
      // format-out("MATCHING %= TEST-ARG %= VALUE %=\n",
      //            test, test-arg, value-1);
      if (test(value-1, test-arg))
        gen
      end if
    end if;
  end if;
end method;

define method machine-word-primitives-call-to-and-matching-arguments?
   (env :: <environment>, call-args :: <argument-sequence>,
    #rest primitive-names-and-tests)
 => (gen :: false-or(<primitive-call>))
  let arg-0 = call-args[0];
  let arg-1 = call-args[1];
  let (constant?, raw-test-arg) = fast-constant-value?(arg-1);
  if (constant?)
    let test-arg :: <integer> = as(<integer>, ^raw-object-value(raw-test-arg));
    iterate loop (i :: <integer> = 0)
      if (i < size(primitive-names-and-tests))
        let primitive-name = primitive-names-and-tests[i];
        let test           = primitive-names-and-tests[i + 1];
        let generator
           = machine-word-primitive-call-to-and-matching-argument?
               (env, arg-0, primitive-name, test, test-arg);
        if (generator)
          generator
        else
          loop(i + 2)
        end if;
      end if;
    end iterate;
  end if;
end method;

define method do-optimize-machine-word-obsoleted-by-right-shift
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>,
    #rest primitive-names-and-tests)
  let generator :: false-or(<primitive-call>)
    = apply(machine-word-primitives-call-to-and-matching-arguments?,
            env, call-args, primitive-names-and-tests);
  if (generator)
    // format-out("RIGHT SHIFTED\n");
    replace-call-argument!(call, arguments(generator)[0], 0);
    re-optimize(generator);
    #t
  end if
end method;

define method do-optimize-machine-word-obsoleted-by-logand
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>,
    #rest primitive-names-and-tests)
  let generator :: false-or(<primitive-call>)
    = apply(machine-word-primitives-call-to-and-matching-arguments?,
            env, call-args, primitive-names-and-tests);
  if (generator)
    // format-out("LOGAND'D\n");
    replace-call-argument!(call, arguments(generator)[0], 0);
    re-optimize(generator);
    #t
  end if
end method;

define function obsolete-loganded-logior?
    (x :: <integer>, test-arg :: <integer>) => (res :: <boolean>)
  logand(x, test-arg) = 0
end function;

define function obsolete-right-shifted-logior?
    (x :: <integer>, test-arg :: <integer>) => (res :: <boolean>)
  logand(x, lognot(ash(1, test-arg) - 1)) = 0
end function;

define function obsolete-right-shifted-logand?
    (x :: <integer>, test-arg :: <integer>) => (res :: <boolean>)
  let result = logand(lognot(x), (ash(1, test-arg) - 1)) = 0;
  // format-out("OBSO AND %= %= %= %= -> %=\n",
  //            x, test-arg, lognot(x), (ash(1, test-arg) - 1), result);
  result
end function;

define method do-optimize-primitive-machine-word-logand
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-partial-fold-binary-op
        (env, call, call-args, curry(raw-mw-op, logand),
         #"primitive-machine-word-logand")
    | do-optimize-machine-word-obsoleted-by-logand
        (env, call, call-args,
         #"primitive-machine-word-logior", obsolete-loganded-logior?)
end method;

define &optimizer-function primitive-machine-word-logand
    (env, call, arguments)
  do-optimize-primitive-machine-word-logand(env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-machine-word-logior
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 0)
    | do-optimize-machine-word-partial-fold-binary-op
        (env, call, call-args, curry(raw-mw-op, logior),
         #"primitive-machine-word-logior")
    | do-optimize-machine-word-fold-tag/untag-ops
        (env, call, call-args,
         method (ior-1 :: <integer>, xor :: <integer>, ior-2 :: <integer>)
           let result = ior-1 = xor & xor = ior-2;
           // format-out("matching IOR-1 %= XOR %= IOR-2 %= -> %=\n",
           //            ior-1, xor, ior-2, result);
           result
         end,
         #f,
         #"primitive-machine-word-logxor",
         #"primitive-machine-word-logior");
end method;

define &optimizer-function primitive-machine-word-logior
    (env, call, arguments)
  do-optimize-primitive-machine-word-logior(env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-machine-word-multiply-signal-overflow
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 1);
end method;

define &optimizer-function primitive-machine-word-multiply-signal-overflow
    (env, call, arguments)
  do-optimize-primitive-machine-word-multiply-signal-overflow
    (env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-machine-word-divide-signal-overflow
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 1);
end method;

define &optimizer-function primitive-machine-word-divide-signal-overflow
    (env, call, arguments)
  do-optimize-primitive-machine-word-divide-signal-overflow
    (env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-machine-word-add-signal-overflow
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 0);
end method;

define &optimizer-function primitive-machine-word-add-signal-overflow
    (env, call, arguments)
  do-optimize-primitive-machine-word-add-signal-overflow
    (env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-machine-word-subtract-signal-overflow
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 0);
end method;

define &optimizer-function primitive-machine-word-subtract-signal-overflow
    (env, call, arguments)
  do-optimize-primitive-machine-word-subtract-signal-overflow
    (env, call, arguments)
end &optimizer-function;

define method do-optimize-machine-word-shifts
     (env :: <environment>,
      call :: <primitive-call>, call-args :: <argument-sequence>,
      other-shift-name :: <symbol>)
  let arg-0 = call-args[0];
  let arg-1 = call-args[1];
  let gen   = generator(arg-0);
  // format-out("OPTIMIZING SHIFT INVERSES %= %=\n", call, gen);
  if (primitive-call-to?(gen, other-shift-name))
    // format-out("FOUND %=\n", other-shift-name);
    let (constant?, raw-amount-0) = fast-constant-value?(arg-1);
    if (constant?)
      // format-out("  SHIFT AMOUNT %=\n", raw-amount-0);
      let call-args = arguments(gen);
      let arg-1     = call-args[1];
      let (constant?, raw-amount-1) = fast-constant-value?(arg-1);
      if (constant?)
        // format-out("  SHIFT AMOUNT %=\n", raw-amount-1);
        let amount-0 = as(<integer>, ^raw-object-value(raw-amount-0));
        let amount-1 = as(<integer>, ^raw-object-value(raw-amount-1));
        if (amount-0 = amount-1)
          replace-computation-with-temporary!(call, call-args[0]);
          #t
        end if;
      end if;
    end if;
  end if;
end method;

define method do-optimize-primitive-machine-word-shift-right
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 0, first?: #f)
    | do-optimize-machine-word-obsoleted-by-right-shift
        (env, call, call-args,
         #"primitive-machine-word-logior", obsolete-right-shifted-logior?,
         #"primitive-machine-word-logand", obsolete-right-shifted-logand?)
    | do-optimize-machine-word-shifts
        (env, call, call-args,
         #"primitive-machine-word-shift-left-signal-overflow")
end method;

define &optimizer-function primitive-machine-word-shift-right
    (env, call, arguments)
  do-optimize-primitive-machine-word-shift-right(env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-machine-word-shift-left-signal-overflow
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  do-optimize-machine-word-binary-noop(env, call, call-args, 0, first?: #f)
end method;

define &optimizer-function primitive-machine-word-shift-left-signal-overflow
    (env, call, arguments)
  do-optimize-primitive-machine-word-shift-left-signal-overflow
    (env, call, arguments)
end &optimizer-function;

define method do-optimize-primitive-machine-word-bit-field-extract
   (env :: <environment>,
    call :: <primitive-call>, call-args :: <argument-sequence>)
  let (constant?, raw-offset) = fast-constant-value?(call-args[0]);
  if (constant?)
    let offset = as(<integer>, ^raw-object-value(raw-offset));
    if (offset = 0)
      let (constant?, raw-size) = fast-constant-value?(call-args[1]);
      if (constant?)
        let size = as(<integer>, ^raw-object-value(raw-size));
        let mask = ash(1, size) - 1;
        let (ignore-first, ignore-last, mask-ref)
          = convert-object-reference-1(env, make-raw-literal(mask));
        let (call-c, call-t)
          = make-with-temporary
              (env, <primitive-call>,
               primitive: dylan-value(#"primitive-machine-word-logand"),
               arguments: vector(call-args[2], mask-ref));
        replace-call-computation!(env, call, call-c, call-c, call-t);
        re-optimize(call-c);
      end if;
    end if;
  end if;
end method;

define &optimizer-function primitive-machine-word-bit-field-extract
    (env, call, arguments)
  do-optimize-primitive-machine-word-bit-field-extract(env, call, arguments)
end &optimizer-function;


//// VALUES

define method do-optimize-values (env :: <environment>, call, call-args)
  // format-out("OPTIMIZING VALUES %=\n", call-args);
  let values-args = maybe-vector-element-references(call-args[0]);
  let (required, rest)
    = if (values-args)
        let args = copy-sequence(values-args);
        let padded-values
          = apply(pad-multiple-values,
                  call.environment,
                  temporary-value-context(call.temporary),
                  args);
        values(padded-values, #f)
      else
        values(#[], call-args[0])
      end if;
  let (c, t) = convert-values(env, required, rest);
  replace-call-computation!(env, call, c, c, t);
  #t
end method;

define &optimizer-function values (env, call, arguments)
  do-optimize-values(env, call, arguments)
end &optimizer-function;

define &optimizer-function identity (env, call, arguments)
  // we only get here if there is a single arg
  let (c, t) = convert-values(env, arguments, #f);
  replace-call-computation!(env, call, c, c, t);
  #t
end &optimizer-function;

define program-warning <non-sequence-last-argument-in-apply>
  slot condition-type-estimate,
    required-init-keyword: type-estimate:;
  format-string
    "Last argument in apply call is not a sequence - inferred type is %=.";
  format-arguments type-estimate;
end program-warning;

define method temporary-class
    (tmp == #f) => (res :: false-or(<class>))
  #f
end method;

define method temporary-class
    (tmp :: <multiple-value-temporary>) => (res :: false-or(<class>))
  <multiple-value-temporary>
end method;

define method temporary-class
    (tmp :: <temporary>) => (res :: false-or(<class>))
  <temporary>
end method;

define method temporary-class
    (tmp :: <named-temporary-mixin>) => (res :: false-or(<class>))
  <named-temporary>
end method;

define function call-temporary-class
    (call :: <function-call>) => (res :: false-or(<class>))
  temporary-class(temporary(call))
end function;

define method do-optimize-apply (env :: <environment>, call, call-args)
  let env = call.environment;
  // The generic call consistency checking code will pick up and report
  // on too few argument to apply.
  if (size(call-args) > 1)
    // HACK: SHOULD JUST INSERT AN AS(<SOV>, ...)
    let apply-args-ref = call-args[1];
    let apply-args = maybe-vector-element-references(apply-args-ref);
    if (apply-args)
      let args = copy-sequence(apply-args);
      let last-arg = last(args);
      let last-arg-type = type-estimate(last-arg);
      if (guaranteed-joint?
            (last-arg-type, dylan-value(#"<simple-object-vector>")))
        let (apply-call, apply-tmp)
          = make-with-temporary
             (env, <apply>,
              function: call-args[0],
              arguments: args,
              temporary-class: call-temporary-class(call));
        replace-computation!(call, apply-call, apply-call, apply-tmp);
        generator(apply-args-ref) & re-optimize(generator(apply-args-ref));
        re-optimize(apply-call);
        #t
      else
        if (guaranteed-disjoint?(last-arg-type, dylan-value(#"<sequence>")))
          note(<non-sequence-last-argument-in-apply>,
               source-location: dfm-source-location(call),
               context-id:      dfm-context-id(call),
               type-estimate:   last-arg-type);
        end;
        #f
      end if
    else
      #f
    end if;
  else
    #f
  end if;
end method;

define &optimizer-function apply (env, call, arguments)
  do-optimize-apply(env, call, arguments)
end &optimizer-function;

define method do-optimize-size (env :: <environment>, call, call-args)
  let env  = call.environment;
  let arg  = call-args[0];
  if (instance?(arg, <stack-vector-temporary>))
    replace-call-with-values(list(number-values(arg)), call, temporary(call));
  elseif (instance?(type, <type-estimate-limited-collection>)
        & type-estimate-size(type))
    replace-call-with-values(list(type-estimate-size(type)), call, temporary(call));
  else
    #f
  end if;
end method;

define &optimizer-function size (env, call, arguments)
  do-optimize-size(env, call, arguments)
end &optimizer-function;

define method do-optimize-dimensions (env :: <environment>, call, call-args)
  let env  = call.environment;
  let arg  = call-args[0];
  let type = type-estimate(arg);
  if (instance?(type, <type-estimate-limited-collection>)
        & type-estimate-dimensions(type))
    replace-call-with-values(list(type-estimate-dimensions(type)), call, temporary(call));
  else
    #f
  end if;
end method;

define &optimizer-function dimensions (env, call, arguments)
  do-optimize-dimensions(env, call, arguments)
end &optimizer-function;

define method do-optimize-element-type (env :: <environment>, call, call-args)
  let env  = call.environment;
  let arg  = call-args[0];
  let type = type-estimate(arg);
  if (instance?(type, <type-estimate-limited-collection>))
    replace-call-with-values
      (list(as(<&type>, type-estimate-of(type))), call, temporary(call));
  else
    #f
  end if;
end method;

define &optimizer-function element-type (env, call, arguments)
  do-optimize-element-type(env, call, arguments)
end &optimizer-function;

define method pointer-argument? (ref :: <value-reference>)
  guaranteed-disjoint?(type-estimate(ref), dylan-value(#"<value-object>"));
end method;

define method do-optimize-id (env :: <environment>, call, arguments)
  // check second arg first, since it is often a constant
  if (pointer-argument?(arguments[1]) | pointer-argument?(arguments[0]))
    // format-out("OPTIMIZING ID?\n");
    let (id-c, id-t)
      = make-with-temporary
          (env, <primitive-call>,
           primitive: dylan-value(#"primitive-id?"),
           arguments: arguments);
    re-optimize-local-users(env, temporary(call));
    replace-call-computation!(env, call, id-c, id-c, id-t);
    re-optimize(id-c);
    #t
  end if;
end method;

define &optimizer-function \== (env, call, arguments)
  do-optimize-id(env, call, arguments)
end &optimizer-function;

/*
define method do-optimize-<
    (env :: <environment>, call :: <simple-call>,
     arguments :: <argument-sequence>)
  let (constant?, value) = fast-constant-value?(arguments[1]);
  if (constant?)
    let te = type-estimate(arguments[0]);
    if (instance?(te, <type-estimate-limited-integer>)
          & ~type-estimate-instance?(value - 1, te))
      format-out("OPT %=\n", call);
      replace-computation-with-temporary!
        (call, make-object-reference(#f));
    end if;
  end if;
end method;

define &optimizer-function \< (env, call, arguments)
  do-optimize-<(env, call, arguments)
end &optimizer-function;
*/

define sideways method ^instance?-function (s :: <&singleton>)
  let obj = s.^singleton-object;
  let te = type-estimate(obj);
  let vo = dylan-value(#"<value-object>");
  if (guaranteed-disjoint?(te, vo))
    #"singleton-pointer-id?-instance?"
  elseif (guaranteed-joint?(te, vo))
    #"singleton-value-object-instance?"
  else
    #"singleton-instance?"
  end if
end method;

define function do-optimize-nary
    (env :: <environment>, call, call-arguments, binary-function,
     number-required :: <integer>,
     #key identity-value, type-name = #"<object>")
  if (size(call-arguments) = number-required + 1)
    let rest-args =
      maybe-vector-element-references(call-arguments[number-required]);
    if (rest-args)
      let real-arguments =
        if (number-required = 0)
          rest-args
        else
          concatenate(copy-sequence(call-arguments,
                                    end: number-required), rest-args)
        end if;
      let n-arguments = size(real-arguments);
      select (n-arguments)
        0 =>
          if (number-required = 0)
            let (first, last, temp) =
              convert-object-reference-1(env, identity-value);
            replace-call-computation!(env, call, first, last, temp);
            #t
          end if;
        1 =>
          let (int-first, int-last, int-t) =
            convert-object-reference-1(env, dylan-value(type-name));
          let (check-c, check-t) =
            make-with-temporary(env, <check-type>,
                                value: real-arguments[0],
                                type: int-t);
          let (first-c, last-c)
            = join-2x1!(int-first, int-last, check-c);
          replace-call-computation!(env, call, first-c, last-c, check-t);
          re-optimize(check-c);
          #t;
        otherwise =>
          let (first, last, function-t) =
            convert-object-reference-1(env, dylan-value(binary-function));
          let (call-c, temp) =
            make-with-temporary
              (env, <simple-call>,
               function: function-t,
               arguments: vector(real-arguments[0], real-arguments[1]));
          let calls = list(call-c);
          let (first, last) = join-2x1!(first, last, call-c);
          for (i from 2 below n-arguments)
            let (call-c, call-t) =
              make-with-temporary
                (env, <simple-call>,
                function: function-t,
                arguments: vector(temp, real-arguments[i]));
            calls := pair(call-c, calls);
            let (_first, _last) = join-2x1!(first, last, call-c);
            first := _first;
            last  := _last;
            temp  := call-t;
          end;
          // do them in the same order they were generated
          for (c in calls) re-optimize(c) end;
          replace-call-computation!(env, call, first, last, temp);
          #t;
      end;
    end;
  end;
end;

define function do-optimize-logxxx
    (env :: <environment>, call, call-arguments, binary-function, identity-value)
  do-optimize-nary
    (env, call, call-arguments, binary-function, 0,
     identity-value: identity-value, type-name: #"<integer>");
end function;

define &optimizer-function logior (env, call, arguments)
  do-optimize-logxxx(env, call, arguments, #"binary-logior", 0)
end &optimizer-function;

define &optimizer-function logxor (env, call, arguments)
  do-optimize-logxxx(env, call, arguments, #"binary-logxor", 0)
end &optimizer-function;

define &optimizer-function logand (env, call, arguments)
  do-optimize-logxxx(env, call, arguments, #"binary-logand", -1)
end &optimizer-function;

// These optimizers for the sake of the make protocol.

define function constant-class?
    (ref) => (well? :: <boolean>, value :: false-or(<&class>))
  let (constant?, class) = constant-value?(ref);
  if (constant? & instance?(class, <&class>))
    values(#t, class)
  else
    values(#f, #f)
  end
end function;

define program-warning <attempt-to-instantiate-abstract-class>
  slot condition-abstract-class,
    required-init-keyword: abstract-class:;
  format-string
    "Illegal instantiation of the abstract class %=.";
  format-arguments
    abstract-class;
end program-warning;

define &optimizer-function allocate-instance
    (env, call, arguments)
  block (return)
    if (size(arguments) ~== 2) return(#f) end;
    let (constant?, class) = constant-class?(arguments.first);
    if (~constant?) return(#f) end;
    ^ensure-class-complete(class);
    if (^class-abstract?(class))
      note(<attempt-to-instantiate-abstract-class>,
           source-location: dfm-source-location(call),
           context-id:      dfm-context-id(call),
           abstract-class:  class);
      let (err-f, err-l, err-t)
        = convert-error-call
            (env,
             temporary-value-context(call.temporary),
             format-to-string
               ("Illegal instantiation of the abstract class %=.",
                class));
      replace-computation!(call, err-f, err-l, err-t);
      return(#f);                        // gts, 10/97
    end if;
    if (^repeated-slot-descriptor(class)) return(#f) end;
    // We have a simplish class.
    let (no-op1, no-op2, allocator-ref)
      = convert-object-reference-1
          (call.environment, dylan-value(#"system-allocate-simple-instance"));
    let (allocate-call, allocate-tmp)
      = make-with-temporary
           (call.environment, <simple-call>,
            function: allocator-ref,
            arguments: vector(arguments[0]),
            temporary-class: call-temporary-class(call));
    replace-computation!(call, allocate-call, allocate-call, allocate-tmp);
    re-optimize(allocate-call);
    #t
  end
end &optimizer-function;

define &optimizer-function defaulted-initialization-arguments
    (env, call, arguments)
  block (return)
    // return(#f); // Off temporarily
    if (size(arguments) ~== 1)
      return(#f)
    end;
    let (constant?, class) = constant-class?(arguments.first);
    if (~constant?)
      return(#f)
    end;
    ^ensure-class-complete(class);
    let default-init-args = ^defaulted-initialization-arguments-slot(class);
    if (~(instance?(default-init-args, <simple-object-vector>)
            & empty?(default-init-args)))
      return(#f)
    end;
    // We know that there are no default initargs, so fold away this call
    // to the empty vector.
    let (first, last, temp)
      = convert-object-reference-1(env, default-init-args);
    replace-computation!(call, first, last, temp);
    re-optimize-users(first);
    #t
  end;
end &optimizer-function;

define function constant-empty-vector? (ref) => (well? :: <boolean>)
  let (constant?, value) = constant-value?(ref);
  constant? & instance?(value, <simple-object-vector>) & empty?(value)
end function;

define method primitive-call-to?
    (c :: false-or(<computation>), name :: <symbol>) => (res :: <boolean>)
  instance?(c, <primitive-call>) & primitive(c) == dylan-value(name)
end method;

define method stack-vector?
    (ref :: <value-reference>)
 => (well? :: <boolean>, maybe-vector :: false-or(<simple-object-vector>),
     env :: false-or(<environment>), copy? :: <boolean>)
  let gen = generator(ref);
  case
    instance?(ref, <immutable-object-reference>)
      => values(#t, map(make-object-reference, reference-value(ref)), #f, #f);
    instance?(ref, <stack-vector-temporary>)
      => values(#t, arguments(gen), environment(gen), #f);
    primitive-call-to?(gen, #"primitive-copy-vector")
      => let ref = first(arguments(generator(ref)));
         if (instance?(ref, <stack-vector-temporary>))
           values(#t, arguments(generator(ref)), environment(gen), #t)
         else
           values(#f, #f, #f, #f)
         end if;
    otherwise
      => values(#f, #f, #f, #f)
  end case;
end method;

define function do-optimize-concatenate-2
    (env :: <environment>, call, arguments)
  block (return)
    if (size(arguments) ~== 2)
      return(#f);
    end;
    let arg1 = arguments.first;
    let arg2 = arguments.second;
    // format-out("arg1 %= arg2 %=\n", arg1, arg2);
    case
      constant-empty-vector?(arg1)
        => replace-computation-with-temporary!(call, arg2);
           re-optimize-users(arg2);
           #t;
      constant-empty-vector?(arg2)
        => replace-computation-with-temporary!(call, arg1);
           re-optimize-users(arg1);
           #t;
      otherwise
        => let (stack?, vec1, env1, copy1?) = stack-vector?(arg1);
           if (stack?)
             let (stack?, vec2, env2, copy2?) = stack-vector?(arg2);
             if (stack? & (~env1 | ~env2 | env1 == env2))
               let (call-c, call-t)
                 = generate-stack-vector
                     (call.environment, concatenate(vec1, vec2));
               let (copy-c, copy-t)
                 = if (copy1? | copy2?)
                     make-with-temporary
                       (env, <primitive-call>,
                        primitive: dylan-value(#"primitive-copy-vector"),
                        arguments: vector(call-t));
                   else
                     values(#f, call-t)
                   end if;
               let (first, last) = join-1x1!(call-c, copy-c);
               replace-call-computation!(env, call, first, last, copy-t);
               #t
             end if;
           else
             #f;
           end if;
    end;
  end;
end function;

define &optimizer-function concatenate-2
    (env, call, arguments)
  do-optimize-concatenate-2(env, call, arguments)
end &optimizer-function;

define &optimizer-function generic-logior (env, call, arguments)
  do-optimize-logxxx(env, call, arguments, #"generic-binary-logior", 0)
end &optimizer-function;

define &optimizer-function generic-logxor (env, call, arguments)
  do-optimize-logxxx(env, call, arguments, #"generic-binary-logxor", 0)
end &optimizer-function;

define &optimizer-function generic-logand (env, call, arguments)
  do-optimize-logxxx(env, call, arguments, #"generic-binary-logand", -1)
end &optimizer-function;

define &optimizer-function max (env, call, arguments)
  do-optimize-nary
    (env, call, arguments, #"binary-max", 1)
end &optimizer-function;

define &optimizer-function min (env, call, arguments)
  do-optimize-nary
    (env, call, arguments, #"binary-min", 1)
end &optimizer-function;

define method maybe-optimize-function-call (c, f, arguments)
  #f
end method;

define inline function do-maybe-optimize-function-call
    (c :: <function-call>, f :: <&lambda>, arguments :: <simple-object-vector>)
  let optimize = lookup-optimizer-function(f);
  when (optimize)
    optimize(c.environment, c, arguments);
  end when;
end function;

define method maybe-optimize-function-call
    (c :: <function-call>, f :: <&code>, arguments :: <simple-object-vector>)
  do-maybe-optimize-function-call(c, f.function, arguments)
end method;

define method maybe-optimize-function-call
    (c :: <function-call>, f :: <&lambda>, arguments :: <simple-object-vector>)
  unless (method-upgrade?(f)) // otherwise wait for upgrade
    do-maybe-optimize-function-call(c, f, arguments)
  end unless;
end method;

define method maybe-optimize-function-call
    (c :: <primitive-call>, f :: <&primitive>, arguments :: <simple-object-vector>)
  let optimize = lookup-optimizer-function(f);
  if (optimize)
    block ()
      optimize(c.environment, c, arguments)
    exception (<error>)
      #f
    end;
  end if;
end method;


define &optimizer-function map (env, call, arguments)
  do-optimize-map (env, call, arguments)
end &optimizer-function;

define function do-optimize-map(env, call, arguments)
  let (arg-constant?, arg-value) = constant-value?(arguments[2]);
  if (arg-constant? & empty?(arg-value))
    let (first, last, function-t) =
      convert-object-reference-1(env, dylan-value(#"type-for-copy"));
    let (tfc-call-c, tfc-temp) =
      make-with-temporary
        (env, <simple-call>,
         function: function-t,
         arguments: vector(arguments[1]));
    let (tfc-first, tfc-last) = join-2x1!(first, last, tfc-call-c);

    let (first, last, function-t) =
      convert-object-reference-1(env, dylan-value(#"map-as-one"));
    let (map-call-c, temp) =
      make-with-temporary
        (env, <simple-call>,
         function: function-t,
         arguments: vector(tfc-temp, arguments[0], arguments[1]));
    let (map-first, map-last) = join-2x1!(first, last, map-call-c);
    let (first, last) = join-2x2!(tfc-first, tfc-last, map-first, map-last);
    replace-call-computation!(env, call, first, last, temp);
    analyze-calls(tfc-call-c);
    analyze-calls(map-call-c);
    #t
  end;
end;


define function do-optimize-multi-collection
    (env, call, arguments, function-one)
  let (arg-constant?, arg-value) = constant-value?(arguments.last);
  if (arg-constant? & empty?(arg-value))
    let (first, last, function-t) =
      convert-object-reference-1(env, dylan-value(function-one));
    let (new-call-c, temp) =
      make-with-temporary
        (env, <simple-call>,
         function: function-t,
         arguments: copy-sequence(arguments, end: arguments.size - 1));
    let (first, last) = join-2x1!(first, last, new-call-c);
    replace-call-computation!(env, call, first, last, temp);
    analyze-calls(new-call-c);
    #t
  end;
end;

define &optimizer-function do (env, call, arguments)
  do-optimize-multi-collection (env, call, arguments, #"do-one")
end &optimizer-function;

define &optimizer-function any? (env, call, arguments)
  do-optimize-multi-collection (env, call, arguments, #"any?-one")
end &optimizer-function;

define &optimizer-function every? (env, call, arguments)
  do-optimize-multi-collection (env, call, arguments, #"every?-one")
end &optimizer-function;

/*
Need to optimize map-into as well?
*/

define &optimizer-function class-constructor-atomically
    (env, call, arguments)
  let (constant?, class) = fast-constant-value?(arguments.first);
  if (constant?)
    // format-out(">>> Upgrading make call: %=\n", class);
    let constructor = ^class-constructor(class);
    let (first, last, ref)
      = convert-reference(call.environment, $single, constructor);
    replace-computation!(call, first, last, ref);
    #t
  else
    #f
  end;
end &optimizer-function;

define method maybe-replace-type-accessor!
    (env :: <environment>, call :: <call>,
     type :: false-or(<&type>), found? :: <boolean>)
  if (found?)
    replace-computation-with-temporary!
      (call, make(<object-reference>, value: type));
    #t
  else
    #f
  end if;
end method;

define &optimizer-function function-required-type
    (env :: <environment>, call :: <simple-call>, arguments :: <argument-sequence>)
  let (type, found?)
    = ^function-required-type*
         (first(arguments), constant-value(second(arguments)));
  maybe-replace-type-accessor!(env, call, type, found?)
end;

define &optimizer-function function-key-type
    (env :: <environment>, call :: <simple-call>, arguments :: <argument-sequence>)
  let (type, found?)
    = ^function-key-type*
         (first(arguments), constant-value(second(arguments)));
  maybe-replace-type-accessor!(env, call, type, found?)
end;

define &optimizer-function function-value-type
    (env :: <environment>, call :: <simple-call>, arguments :: <argument-sequence>)
  let (type, found?)
    = ^function-value-type*
         (first(arguments), constant-value(second(arguments)));
  maybe-replace-type-accessor!(env, call, type, found?)
end;

define &optimizer-function function-rest-value-type
    (env :: <environment>, call :: <simple-call>, arguments :: <argument-sequence>)
  let (type, found?) = ^function-rest-value-type*(first(arguments), #f);
  maybe-replace-type-accessor!(env, call, type, found?)
end;

/// TODO: SHOULD BE SPED UP TO CONS/CPU LESS

define &optimizer-function immutable-type-vector
    (env :: <environment>, call :: <simple-call>, arguments :: <argument-sequence>)
  let type-refs = maybe-vector-element-references(call);
  if (type-refs)
    let types = map(constant-value, type-refs);
    if (every?(rcurry(^instance?, dylan-value(#"<type>")), types))
      let types = as-sig-types(types);
      replace-computation-with-temporary!
        (call, make(<object-reference>, value: mapped-model(types)));
      #t
    else
      #f
    end if;
  else
    #f
  end if;
end;
