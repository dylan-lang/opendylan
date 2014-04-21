Module:   dfmc-optimization
Author:   Keith Playford and Paul Haahr
Synopsis: Inline function bodies
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Simple inlining pass

// define compilation-pass try-inlining,
//   visit: computations,
//   optimization: low,
//   after: analyze-calls,
//   before: single-value-propagation,
//   triggered-by: analyze-calls,
//   trigger: analyze-calls,
//   trigger: single-value-propagation;

define generic try-inlining (c :: <computation>);
define generic try-inlining-call (c :: <function-call>, function);
define generic function-used-once? (function) => (used-once? :: <boolean>);

define method try-inlining (c :: <computation>)
  #f
end method try-inlining;

define method inline-method-value (ref :: <method-reference>)
  reference-value(ref)
end method;

define method inline-method-value (ref :: <value-reference>)
  #f
end method;

define method inline-method-value (ref :: <temporary>)
  let generator = ref.generator;
  if (instance?(generator, <make-closure>))
    computation-closure-method(generator)
  end if
end method;

define function call-inline-effective-function (c :: <function-call>)
  let funct = inline-method-value(c.function);
  if (funct & call-iep?(c))
    iep(function(funct))
  else
    funct
  end if;
end function;

define method try-inlining (c :: <function-call>)
  try-inlining-call(c, call-inline-effective-function(c))
end method try-inlining;

define method try-inlining-call (c :: <function-call>, function)
  // <apply> nodes must be inlined, but the rules are different

  // Calls where the function is something other than an IEP
  // probably shouldn't ever be inlined.
  #f
end method try-inlining-call;

//// INLINING POLICY

define method try-inlining-call (c :: <simple-call>, code :: <&iep>)
  // TODO: size heuristic
  // TODO: inline letrec-bound functions
  let fun = code.function;
  if (lambda-top-level?(fun))
    if (method-inlineable?(fun)
          & call-inlining-depth(c) < $max-inlining-depth)
      inline-call-copied(c, code);
    end if;
  elseif (function-reference-used-once?(c.function) // the temporary
          & function-used-once?(code)             // the value
          & ~inner-environment?(environment(c), environment(code)))
    inline-call(c, code)
  elseif (lambda-inlineable?(fun) == #t) // currently only bind-exit returns
    with-dfm-copier-environment (environment(code))
      inline-call-copied(c, code)
    end with-dfm-copier-environment;
    // #f
  else
    #f
  end if;
end method try-inlining-call;

define method function-used-once?
    (f :: <&lambda>) => (used-once? :: <boolean>);
  (size(users(f)) + size(users(f.iep))) = 1
end method function-used-once?;

define method function-used-once? (f :: <&iep>) => (used-once? :: <boolean>)
  function-used-once?(f.function)
end method function-used-once?;

define function function-reference-used-once?
    (ref :: <value-reference>) => (used-once? :: <boolean>);
  local method user-count
            (code :: <value-reference>) => (res :: <integer>)
          let count :: <integer> = 0;
          for (user in users(code))
            unless (instance?(user, <initialize-closure>))
              count := count + 1;
            end unless;
          end for;
          count
        end method;
  user-count(ref) = 1
end function;


//// Inlining tools

// Currently, only ieps may be inlined, because the xep code is
// not (yet) represented in DFM code.  xep->iep conversion is
// handled by upgrading the calls in analyze-calls.

define method inline-call (c :: <function-call>, f :: <&lambda>)
  #f
end method inline-call;

// At it's heart, let-conversion should just be inlining plus removal
// of the original lambda expression. Since inlining will normally
// involve making a copy of the function's flow graph and splicing
// it into place, an optimisation where the original code is no
// longer needed is to re-use that.

define function replace-call-computation!
    (env :: <lexical-environment>,
     call :: <call>, first :: <computation>, last :: <computation>,
     ref :: false-or(<value-reference>))
  // format-out("MATCHING %= TO %=\n", temporary(call), ref);
  let (first, new-last, ref)
    = match-values-with-temporary
        (env, temporary(call), first, last, ref);
  unless (new-last == last)
    re-optimize(new-last);
  end unless;
  replace-computation!(call, first, new-last, ref);
end function;

define method maybe-update-inlined-next-methods
    (c :: <function-call>, f :: <&lambda>, mapped-body)
  when (^next?(f))
    assert(instance?(c, <method-call>) | instance?(c, <method-apply>),
           "calling method %= inappropriately %=", f, c);
    let nmcs =
      collecting ()
        walk-lambda-computations
          (method (c)
             when (primitive-call-to?(c, #"primitive-next-methods-parameter"))
               collect(c)
             end when
           end method,
           mapped-body);
      end collecting;
    for (nmc in nmcs)
      // REPLACE NEXT-METHODS QUERY IN BODY WITH NEXT-METHODS FROM CALL
      replace-computation-with-temporary!(nmc, next-methods(c));
    end for;
  end when;
end method;

define method do-inline-call
    (c :: <function-call>, f :: <&iep>, mapped :: <function>)
  let f = function(f);
  let target-env = lambda-environment(c.environment);
  let (mapped-body) = move-code-into!(f, target-env, mapped);
  let (first, last, return-c)
    = extract-lambda-body-extent(mapped-body, target-env);
  if (mapped == identity)
    f.body := #f;
  end if;
  redirect-arguments!(c, f, mapped);
  let return-t = return-c.computation-value;
  re-optimize-users(c.temporary);
  let (first, last, tmp)
    = if (~c.temporary | instance?(c.temporary, <multiple-value-temporary>))
        // is the call's temporary expecting different values than the
        // fn's return temp is delivering?
        if (c.temporary & instance?(c.temporary, <multiple-value-temporary>)
            & (required-values(c.temporary) ~= required-values(return-t)
               | rest-values?(c.temporary) ~= rest-values?(return-t)))
          let (adjust, adjust-temp) =
            make-with-temporary
              (target-env,
               if (rest-values?(c.temporary))
                 <adjust-multiple-values-rest> else <adjust-multiple-values>
               end,
               value: return-t,
               number-of-required-values: required-values(c.temporary),
               temporary-class: <multiple-value-temporary>);
          adjust-temp.required-values := required-values(c.temporary);
          adjust-temp.rest-values? := rest-values?(c.temporary);
          re-optimize(adjust);
          join-2x1-t!(first, last, adjust, adjust-temp);
        else
          values(first, last, return-t);
        end if;
      else
        let (extract-c, extract-t)
          = make-with-temporary
              (target-env, <extract-single-value>,
               temporary-class: temporary-class(c.temporary),
               value: return-t);
        re-optimize(extract-c);
        join-2x1-t!(first, last, extract-c, extract-t);
      end if;
  replace-call-computation!(target-env, c, first, last, tmp);
  delete-computation!(return-c);
  maybe-update-inlined-next-methods(c, f, mapped-body);
  #t
end method;

define method inline-call (c :: <function-call>, f :: <&iep>)
  if (*colorize-dispatch*)
    color-dispatch(c, #"inlining")
  end;
  re-optimize-users(c.function); // MAYBE DELETE MAKE/INIT-CLOSURE IF PRESENT
  let f-body = f.body;
  let inherited-location
    = if (~computation-source-location(f-body))
        parent-source-location()
      else
        #f
      end;
  walk-lambda-computations
    (method (c :: <computation>)
       item-status(c) := $queueable-item-absent;
       computation-source-location(c)
         := computation-source-location(c) | inherited-location
     end,
     f-body);
  do-inline-call(c, f, identity);
end method inline-call;

define method inline-call-copied (c :: <function-call>, f :: <&iep>)
  let code = f.function;
  ensure-method-dfm(code);
  if (code.body)
    if (*colorize-dispatch*)
      color-dispatch(c, #"inlining")
    end;
    dynamic-bind (*inlining-depth* = call-inlining-depth(c) + 1)
      let copier = current-dfm-copier(estimated-copier-table-size(code));
      do-inline-call(c, f, curry(deep-copy, copier));
    end dynamic-bind;
  else
    lambda-inlineable?(code) := #f;
    debug-out(#"inlining", "LOST %='s BODY FOR INLINING", code);
  end if;
end method;

define method extract-lambda-body-extent (body, env) => (first, last, return-c)
  let bind-c = body;
  let return-c = bind-return(bind-c);
  check-type(bind-c, <bind>);
  check-type(return-c, <return>);
  if (bind-c.next-computation == return-c)
    let nop = make-in-environment(env, <nop>);
    values(nop, nop, return-c)
  else
    let first = bind-c.next-computation;
    let last = return-c.previous-computation;
    first.previous-computation := #f;
    last.next-computation      := #f;
    remove-computation-references!(bind-c);
    values(first, last, return-c)
  end if
end method;

define program-warning <ambiguous-copy-down-method>
  slot condition-method, required-init-keyword: meth:;
  slot condition-other-methods, required-init-keyword: other-methods:;
  format-string "Multiple applicable copy-down methods for %s, picking one at random";
  format-arguments meth;
end;

define program-warning <ambiguous-copy-down-method-option>
  slot condition-method, required-init-keyword: meth:;
  format-string "One option was %s";
  format-arguments meth;
end;

define program-warning <unknown-copy-down-method-domain>
  slot condition-method, required-init-keyword: meth:;
  format-string "Domain not fully known for copy-down method %s";
  format-arguments meth;
end;


define serious-program-warning <missing-copy-down-method>
  slot condition-method, required-init-keyword: meth:;
  format-string "Failed to find an applicable copy-down method for %s";
  format-arguments meth;
end;

define method find-copy-downable-methods
    (m :: <&copy-down-method>) => (sorted, others)
  let gf         = m.^method-generic-function;
  let sig        = m.^function-signature;
  let req-size   = sig.^signature-number-required;
  let req        = copy-sequence(sig.^signature-required, end: req-size);
  let req-te*    = map(curry (as, <type-estimate>), req);
  let methods-known = ^generic-function-methods-known(gf);
  unless (all-applicable-methods-guaranteed-known?(gf, req-te*))
    note(<unknown-copy-down-method-domain>,
         meth: m,
         source-location: m.model-source-location);
  end;
  // Lose all methods that are known statically always to be more
  // specific than ourselves, leaving only methods known to be
  // less specific and those that are potentially more or less
  // specific.
  let methods
    = choose(method (them :: <&method>)
               them == m
                 | ~guaranteed-method-precedes?(them, m, req-te*)
             end method,
             methods-known);
  guaranteed-sorted-applicable-methods(methods, req-te*);
end method;

// markt, copy-down inlining, first cut (still generates a type-check warning)
define compiler-sideways method copy-down-body (m :: <&copy-down-method>) => ()
  let (sorted-applicable-methods, others) = find-copy-downable-methods(m);
  local method real-method (a) ~instance?(a, <&copy-down-method>) & a end;

  let meth = any?(real-method, sorted-applicable-methods) |
              begin
                let others = choose(real-method, others);
                if (others.size == 1)
                  others.first
                else
                  let lib = model-library(m);
                  let kludge = any?(method (a) model-library(a) == lib & a end,
                                    others);
                  if (kludge)
                    note(<ambiguous-copy-down-method>,
                         meth: m,
                         other-methods: others,
                         source-location: m.model-source-location,
                         subnotes: map(method (m)
                                         make(<ambiguous-copy-down-method-option>,
                                              meth: m,
                                              source-location: m.model-source-location)
                                       end,
                                       others));
                    kludge
                  end;
                end;
              end;
  if (~meth)
    note(<missing-copy-down-method>,
         meth: m,
         source-location: m.model-source-location);
  else
    let  callee = meth.^iep ;
    let  f = callee.function ;

    // this required to set up copy source method ready for splicing:
    ensure-method-dfm (f) ;

    debug-assert(f.body, "No body to copy-down?");
    f.lambda-copied-down? := #t;

    //format-out ("copy-down inlining body of %s for %s\n", f, m) ;

    really-run-compilation-passes (f);

    let  target-env = m.environment.lambda-environment ;
    let  mapper = curry (deep-copy, make (<dfm-copier>));
    ensure-optimization-queue (m);

    while (queue-pop(m.optimization-queue)) end; // EMPTY BOGUS SCHTUFF

    let stale-temporaries
      = collecting ()
          for-temporary (tmp in target-env)
            unless (member?(tmp, m.parameters))
              collect(tmp)
            end unless;
          end for-temporary;
        end collecting;

    for (tmp in stale-temporaries)
      remove-temporary!(target-env, tmp);
    end for;

    let  mapped-body = move-code-into! (f, target-env, mapper) ;
    let  (first, last, return-c)
      = extract-lambda-body-extent (mapped-body, target-env) ;
    redirect-args! (m.parameters, f.parameters, mapper) ;

    for (param in mapper(f.parameters))
      remove-temporary!(target-env, param);
    end for;

    // markt: fix for keyword default vector.  Shame not to share these,
    // but that is more complex to get right for the linker.
    if (instance?(f, <&keyword-method>))
      m.keyword-specifiers := mapper (f.keyword-specifiers) ;
    end if;

    let  bind-comp = m.body ;
    let  return-t  = return-c.computation-value ;

    return-c.previous-computation := #f ;

    join-2x1! (first, last, return-c);

    bind-comp.next-computation := #f ;
    join-2x2! (bind-comp, bind-comp, first, last) ;

    re-optimize (return-c) ;
    m.body-spec := f.body-spec ;

    run-optimizations (m);
    #f
  end
end;


// markt, slightly more general version for copy-down
define function redirect-args! (args, params, mapped :: <function>) => ()
  for (parameter in params, argument in args)
    replace-temporary-in-users! (parameter.mapped, argument)
  end
end;

define method redirect-arguments!
    (c :: <function-call>, f :: <&lambda>, mapped :: <function>) => ()
  redirect-args!(c.arguments, f.parameters, mapped)
end method redirect-arguments!;

define method move-code-into!
    (f :: <&lambda>, env :: <environment>, mapped :: <function>)
 => (mapped-body)
  // THESE SHOULD BE IN ENVIRONMENT.DYLAN
  let f-env = f.environment;
  when (f-env)
    let mapped-env  = mapped(f-env);
    for (entry in mapped-env.entries)
      env.entries := add!(env.entries, entry);
    end for;
    // TODO: MANAGE THESE
    // for (loop in mapped-env.loops)
    //   env.loops := add!(env.loops, loop);
    // end for;
    for (inner in mapped-env.inners)
      env.inners := add!(env.inners, inner);
      inner.outer := env;
    end for;
    for-temporary (tmp in lambda-environment(mapped-env))
      add-temporary!(env, tmp);
      tmp.frame-offset := next-frame-offset(env);
      tmp.environment  := env;
    end for-temporary;
  end;
  let mapped-body = mapped(f.body);
  let lambda      = env.lambda;

  // Make sure the new items are added to the optimization queue in the
  // correct order.  We save the current entries, collect the new ones, and
  // then add the new ones to the old so they end up in the correct order.

  let old-q = lambda.optimization-queue;
  lambda.optimization-queue := make(<optimization-queue>, code: mapped-body);

  walk-lambda-computations(method (c :: <computation>)
                             c.environment := env;
                             re-optimize-into!(c, lambda);
                           end, mapped-body);
  walk-lambda-computations(method (c :: <computation>)
                             if (instance?(c, <call>))
                               do(node-id,
                                  choose(rcurry(instance?,
                                                type-union(<object-reference>,
                                                           <temporary>)),
                                         c.arguments));
                             end;
                             //ensures that the right node (whose environment
                             //changed in the walk-lambda-computations above)
                             //is now traced...
                             c.next-computation := c.next-computation;
                           end, mapped-body);

  let mapped-q = lambda.optimization-queue;
  lambda.optimization-queue := old-q;
  iterate xfer (qhead = queue-pop(mapped-q))
    when (qhead)
      add-to-queue!(old-q, qhead);
      xfer(queue-pop(mapped-q))
    end when;
  end iterate;

  mapped-body
end method;


