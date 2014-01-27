Module: dfmc-flow-graph
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define compilation-pass analyze-environments,
//   visit: functions,
//   mandatory?: #t;

define method analyze-environments
    (f :: <&lambda>) => (changed? :: <boolean>);
  compute-closure(f.environment);
  // convert-closure(f.environment);
  // access(dfmc-back-end, print-method-out)(f);
  for (entry in f.environment.entries)
    analyze-block(f.environment, entry);
  end for;
  ~empty?(f.environment.closure)
end method;

define method analyze-block
    (env :: <lambda-lexical-environment>, entry :: <entry-state>)
 => (local? :: <boolean>)
  // TODO: no intervening blocks (currently overly conservative)
  entry.local-entry-state?
    := every?(curry(local-exit?, env), entry.exits)
         & size(env.entries) == 1;
end method;

define method local-exit?
    (env :: <lambda-lexical-environment>, exit :: <exit>)
 => (local? :: <boolean>)
  exit.environment == env
end method;

// If a temporary has references outside its generating environment
// after all the optimizations have been run, it has been closed over.
// The temporary itself is flagged as closed over, and then it is added
// to the set of temporaries in the closure of the referencing
// environment. In order for the temporary to be available at the
// point of creation of that closure, it must also be added to the
// closure set of the environment that creates that closure if it
// isn't the generating environment, and so on up the chain in any
// environment between the generator and the referencer.

define method closure-call? (t :: <temporary>, c :: <simple-call>)
  let f-t = function(c);
  t == f-t & instance?(generator(f-t), <make-closure>)
    & ~member?(f-t, arguments(c))
end method;

define method closure-call? (t, c)
  #f
end method;

define method closure-setup? (c :: <computation>) => (well? :: <boolean>)
  instance?(c, <initialize-closure>)
end method;

define constant <closure-entry-strength> = false-or(<integer>);
define constant $no-closure-entry     = #f;
define constant $weak-closure-entry   = 1;
define constant $strong-closure-entry = 2;

define function merge-closure-entry-strengths
    (s1 :: <closure-entry-strength>, s2 :: <closure-entry-strength>)
 => (s :: <closure-entry-strength>)
  if (s1)
    if (s2) max(s1, s2) else s1 end
  else
    s2
  end if
end function;

define method compute-closure (env :: <lambda-lexical-environment>) => ()
  // format-out("ENV %=\n", lambda(env));
  for-temporary (tmp in env)
    // format-out("  TMP %=\n", tmp);
    for (reference in tmp.users)
      // format-out("    REF %=\n", reference);
      let reference-environment = reference.environment;
      unless (reference-environment == env)
        // format-out("      REF %=\n", reference);
        let strength
          = if (closure-call?(tmp, reference) | closure-setup?(reference))
              $weak-closure-entry
            else
              $strong-closure-entry
            end if;
        close-over(tmp, reference-environment, env, strength);
      end unless;
    end for;
  end for-temporary;
end method;

define method ref-users (ref :: <value-reference>) => (user)
  users(ref)
end method;

define method ref-users (ref :: <make-closure>) => (user)
  users(ref.temporary)
end method;

define function do-over-lambda-users
    (f :: <function>, env :: <lambda-lexical-environment>) => ()
  local method do-over (code)
          for (ref in references(code))
	    for (c in ref-users(ref))
	      f(c.environment)
	    end for
	  end for
	end method;
  let lambda = lambda(env);
  do-over(lambda); do-over(lambda.iep);
end function;

define function do-over-lambda-using-computations
    (f :: <function>, env :: <lambda-lexical-environment>) => ()
  local method do-over (code)
          for (ref in references(code))
	    for (c in ref-users(ref))
	      if (~closure-setup?(c))
  	        f(c)
              end;
	    end for
	  end for
	end method;
  let lambda = lambda(env);
  do-over(lambda); do-over(lambda.iep);
end function;

define function close-over-lambda-users
    (tmp :: <temporary>,
     reference-environment :: <lambda-lexical-environment>,
     home-environment :: <lambda-lexical-environment>,
     strength :: <closure-entry-strength>) => ()
  do-over-lambda-users
    (method (env)
       close-over(tmp, env, home-environment, strength);
     end,
     reference-environment);
end function;

define function close-over
    (tmp :: <temporary>,
     reference-environment :: <lambda-lexical-environment>,
     home-environment :: <lambda-lexical-environment>,
     strength :: <closure-entry-strength>) => ()
  if (reference-environment == home-environment
       | ~lambda-used?(reference-environment.lambda))
    // Do nothing.
  elseif (member?(tmp, reference-environment.closure))
    // Merge in the given closure strength.
    tmp.closed-over?
      := merge-closure-entry-strengths(tmp.closed-over?, strength);
  elseif (lookup-alias(reference-environment.lifture, tmp))
    // An alias has been made since we last looked, so there can no longer
    // be any references.
  else
    // format-out("TMP %= CLOSED-OVER BY %= FROM %=\n",
    //            tmp, lambda(reference-environment),
    //            lambda(home-environment));
    // If all calls to the referencing function are known, we may choose
    // to lambda lift instead of close.
    if (reference-liftable?(tmp, reference-environment))
      lift-reference(tmp, reference-environment);
      close-over-lambda-users
        (tmp, reference-environment, home-environment, strength);
    else
      reference-environment.closure
        := add-new!(reference-environment.closure, tmp);
      tmp.closed-over?
        := merge-closure-entry-strengths(tmp.closed-over?, strength);
      close-over-lambda-users
        (tmp, reference-environment, home-environment, strength);
      let reference-lambda = reference-environment.lambda;
      // if (lambda-used?(reference-lambda))
        // If there's now a creation point alias, use that within this
        // environment instead.
        let create-env
          = reference-lambda.references.first.environment;
        let alias = lookup-alias(create-env.lifture, tmp);
        if (alias)
          alias.closed-over?
            := merge-closure-entry-strengths(alias.closed-over?, strength);
          for (user in tmp.users)
            let user-env = user.environment;
            if (nested-environment?(user-env, reference-environment))
              // format-out("convert-closure: close alias: %=\n", user);
              replace-temporary-references!(user, tmp, alias);
            end;
          end;
          for-all-lambdas (f in reference-environment.lambda)
            let f-env = f.environment;
            if (member?(tmp, f-env.closure))
              f-env.closure := add-new!(remove!(f-env.closure, tmp), alias);
            end;
          end;
          // Recompute the closure of the creation environment to handle the
          // new references.
          compute-closure(create-env);
        end;
      // end;
    end;
  end if;
end function;

define function lambda-make-closure
    (lambda :: <&lambda>) => (res :: false-or(<make-closure>))
  block (return)
    for (user in users(lambda))
      if (instance?(user, <make-closure>))
        return(user)
      end if;
    end for;
    #f
  end block;
end function;

define function reference-liftable?
    (tmp :: <temporary>, ref-env :: <lambda-lexical-environment>)
 => (well? :: <boolean>)
  ~cell?(tmp)
    & ~instance?(tmp, <entry-state>)
    & ~instance?(tmp.generator, <make-closure>)
    & block (return)
        let f = ref-env.lambda;
        let make-closure = f.references.first;
        let f-ref = make-closure.temporary;
        do-over-lambda-using-computations
          (method (c :: <computation>)
             if (~closure-call?(f-ref, c))
               // Do we also need to check that they can all see the
               // closed over temps?
               // format-out("convert-closure: non-call %=.\n", c);
               return(#f)
             end
           end,
           ref-env);
        #t
      end
end function;

define function lift-reference
    (closed-ref :: <temporary>, ref-env :: <lambda-lexical-environment>)
 => ()
  // format-out("convert-closure: lifting with lifture: %=\n",
  //            ref-env.lifture);
  // format-out("convert-closure: %s from %s in:\n",
  //             closed-ref, closed-ref.environment.lambda);
  // format-out("convert-closure: ref %=\n", ref-env.lambda);
  // We need to add an argument to the function, and modify all
  // callers. Any callers within the scope of the function
  // are modified to refer to the new argument. Otherwise,
  // they refer to the original.
  // for (closed-ref in closed-ref*)
  // format-out("convert-closure: fixing %=\n", closed-ref);
  let new-arg-name
    = name(closed-ref) | dylan-variable-name(#"implicit-argument");
  let new-temp = add-function-argument(ref-env, new-arg-name);
  ref-env.lifture
    := add-alias(ref-env.lifture, closed-ref, new-temp);
  do-over-lambda-using-computations
     (method (c :: <simple-call>)
        let alias = lookup-alias(c.environment.lifture, closed-ref);
        if (alias)
          // format-out("convert-closure: alias fixing %=\n", c);
          add-call-argument(c, alias);
        else
          // format-out("convert-closure: no-alias fixing %=\n", c);
          add-call-argument(c, closed-ref);
        end;
      end,
      ref-env);
  for (user in closed-ref.users)
    let alias = lookup-alias(user.environment.lifture, closed-ref);
    if (alias)
      // format-out("convert-closure: alias user: %=\n", user);
      replace-temporary-references!(user, closed-ref, alias);
    end;
  end;
  for-all-lambdas (f in ref-env.lambda)
    let f-env = f.environment;
    if (member?(closed-ref, f-env.closure))
      f-env.closure := remove!(f-env.closure, closed-ref);
    end;
  end;
  // Force a look at this new variable and any new references to the old
  // variable.
  // format-out
  //   ("convert-closure: redo ref %=\n", ref-env.lambda);
  compute-closure(ref-env);
  // format-out
  //   ("convert-closure: redo def %=\n", closed-ref.environment.lambda);
  compute-closure(closed-ref.environment);
  // format-out
  //   ("convert-closure: done redo def %=\n", closed-ref.environment.lambda);
end function;

define function lookup-alias (lifture :: <list>, tmp) => (alias)
  if (empty?(lifture))
    #f
  elseif (lifture.head == tmp)
    lifture.tail.head
  else
    lookup-alias(lifture.tail.tail, tmp)
  end
end function;

define function add-alias
    (lifture :: <list>, tmp, aliased-tmp) => (lifture :: <list>)
  pair(tmp, pair(aliased-tmp, lifture))
end function;

define function nested-environment?
    (test-env :: <lexical-environment>, target-env :: <lexical-environment>)
 => (well? :: <boolean>)
  block (found)
    for (env = test-env then outer(env), while: env)
      if (env == target-env) found(#t) end;
    end;
    #f
  end;
end function;

define method add-function-argument
    (env :: <lambda-lexical-environment>, name)
 => (arg :: <lexical-required-variable>)
  let f = env.lambda;
  let object-type = dylan-value(#"<object>");
  // Hack the function object.
  let sig = ^function-signature(f);
  let sig-n-required = ^signature-number-required(sig);
  let new-sig
     = make(<&signature>,
            number-required: sig-n-required + 1,
            required: concatenate(vector(object-type),
                                  copy-sequence(^signature-required(sig),
                                                end: sig-n-required)),
            key?: ^signature-key?(sig),
            keys: ^signature-keys(sig),
            key-types: if (^signature-key?(sig))
                         ^signature-key-types(sig)
                       else
                         #[]
                       end,
            values: ^signature-values(sig),
            rest-value?: ^signature-rest-value(sig) & #t,
            rest-value: ^signature-rest-value(sig),
            number-values: ^signature-number-values(sig),
            rest?: ^signature-rest?(sig),
            // next?: ^signature-next?(sig),
            next?: #f,
            sealed-domain?: ^signature-sealed-domain?(sig));
  ^function-signature(f) := new-sig;
  let sig-spec = signature-spec(f);
  let new-sig-spec
    = make(<signature-spec>,
           argument-required-variable-specs:
             concatenate(vector(make(<required-variable-spec>,
                                     variable-name: name)),
                         spec-argument-required-variable-specs(sig-spec)),
           argument-next-variable-spec:
             spec-argument-next-variable-spec(sig-spec),
           argument-rest-variable-spec:
             spec-argument-rest-variable-spec(sig-spec),
           argument-key?:
             spec-argument-key?(sig-spec),
           argument-key-variable-specs:
             spec-argument-key-variable-specs(sig-spec),
           value-required-variable-specs:
             spec-value-required-variable-specs(sig-spec),
           value-rest-variable-spec:
             spec-value-rest-variable-spec(sig-spec));
  signature-spec(f) := new-sig-spec;
  let arg = make(<lexical-required-variable>,
                 name: name,
                 environment: env,
                 specializer: object-type);
  let argv = vector(arg);
  f.parameters
    := concatenate(argv, f.parameters);
  env.temporaries
    := concatenate-as(<stretchy-vector>, argv, env.temporaries);
  arg
end method;

define method add-call-argument
    (c :: <simple-call>, arg :: <value-reference>) => ()
  add-user!(arg, c);
  c.arguments := concatenate(vector(arg), c.arguments);
end method;
