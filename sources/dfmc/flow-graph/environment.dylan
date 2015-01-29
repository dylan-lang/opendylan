Module: dfmc-flow-graph
Author: Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Variables

// TODO: Work out which of these should and can be abstract and which
// concrete.

define /* abstract */ class <lexical-variable>
    (<named-temporary-mixin>, <temporary>, <binding>)
end class;

define method rest-variable? (var :: <lexical-variable>)
  #f
end method;

define method keyword-variable? (var :: <lexical-variable>)
  #f
end method;

define class <lexical-specialized-variable> (<lexical-variable>)
  slot specializer, required-init-keyword: specializer:;
end class;

// !@#$ super hack -- happens on rest parameters
define method specializer (object) dylan-value(#"<object>") end;

define class <lexical-local-variable> (<lexical-specialized-variable>)
end class;

define class <lexical-required-variable> (<lexical-specialized-variable>)
end class;

define /* abstract */ class <lexical-optional-variable> (<lexical-variable>)
end class;

define class <lexical-rest-variable> (<lexical-optional-variable>) end class;

define method rest-variable? (variable :: <lexical-rest-variable>)
  #t
end method;

define class <lexical-keyword-variable> (<lexical-optional-variable>)
end class;

define method keyword-variable? (variable :: <lexical-keyword-variable>)
  #t
end method;

//// Environments

define sideways method frame-size (env :: <module>) => (size :: <integer>) 0 end;
define sideways method frame-size-setter (value, env :: <module>) 0 end;

define method outer (environment :: <module>)
  #f
end method;

define class <lexical-environment> (<environment>)
  slot outer :: false-or(<lexical-environment>),
    required-init-keyword: outer:;
  // slot cached-lexical-variables-in-scope = #f;
end class;

define inline method top-level-environment? (env :: <lexical-environment>)
  ~outer(env)
end method;

/// TODO: THIS HACK WILL ONLY WORK UNTIL WE USE LIMITED VECTORS OF TMPS

define constant $weak-temporaries = as(<stretchy-vector>, #[#"weak"]);

define inline function weak-temporaries?
    (tmps :: <stretchy-object-vector>) => (well? :: <boolean>)
  size(tmps) = 1 & tmps[0] == #"weak"
end function;

define dood-class <lambda-lexical-environment> (<lexical-environment>)
  slot lambda :: false-or(<&lambda>), required-init-keyword: lambda:;
  weak slot temporaries :: <stretchy-object-vector> = make(<stretchy-vector>),
    reinit-expression: $weak-temporaries;
  // slot temporaries :: <stretchy-object-vector> = make(<stretchy-vector>);
  slot rare-environment-data :: <simple-object-vector> = #[];
end dood-class;

define method ensure-lambda-body (fun :: <&lambda>) => ()
  let env = environment(fun);
  when (env & weak-temporaries?(temporaries(env)))
    for-all-lambdas (lambda in fun)
      let env = environment(lambda);
      temporaries(env) := compute-temporaries(env);
    end for-all-lambdas;
  end when;
end method;

define method approximate-number-temporaries (fun :: <&lambda>) => (res :: <integer>)
  2 ^ lambda-log-number-temporaries(fun)
end method;

define method compute-temporaries
    (env :: <lambda-lexical-environment>) => (res :: <stretchy-object-vector>)
  let fun  = lambda(env);
  let tmps = make(<stretchy-vector>,
                  capacity: approximate-number-temporaries(fun));
  for (parameter in parameters(fun))
    environment(parameter) := env;
    add!(tmps, parameter)
  end for;
  frame-size(env) := size(tmps);
  local method maybe-add-temporary!
            (c :: <computation>, tmp :: false-or(<temporary>))
          when (tmp)
            generator(tmp)    := c;
            environment(tmp)  := env;
            frame-offset(tmp) := next-frame-offset(env);
            add!(tmps, tmp)
          end when;
        end method;
  for-computations (c :: <computation> previous pc :: false-or(<computation>) in fun)
    environment(c)          := env;
    previous-computation(c) := pc;
    maybe-add-temporary!(c, temporary(c));
    when (instance?(c, <block>))
      maybe-add-temporary!(c, entry-state(c));
    end when;
  end for-computations;
  tmps
end method;

define class <top-level-environment> (<lambda-lexical-environment>)
end class;

define method compute-temporaries
    (env :: <top-level-environment>) => (res :: <stretchy-object-vector>)
  make(<stretchy-object-vector>)
end method;

define method number-temporaries
    (e :: <lambda-lexical-environment>) => (res :: <integer>)
  size(temporaries(e))
end method;

define inline method clear-temporaries!
    (env :: <lambda-lexical-environment>)
  size(env.temporaries) := 0;
end method;

define inline method remove-temporary!
    (env :: <lambda-lexical-environment>, t :: <temporary>)
  remove!(env.temporaries, t);
  trace-dfm-node(#"remove-temporary", t, #f);
end method;

define inline method add-temporary!
    (env :: <lambda-lexical-environment>, t :: <temporary>)
  add!(env.temporaries, t)
end method;

define sealed inline method initialize
    (x :: <lambda-lexical-environment>, #key, #all-keys)
  next-method();
  bindings(x) := make(<variable-name-table>);
end method;

define rare-slots (<lambda-lexical-environment>, rare-environment-data)
  slot bindings :: false-or(<variable-name-table>) = #f;
  slot variable-assignments :: false-or(<table>) = #f;
  slot inners :: <list>  = #(); // FOR NESTED LAMBDAS
  slot entries :: <list> = #(); // FOR BLOCKS
  slot loops :: <list>   = #(); // FOR LOOP CONVERSION
  slot closure :: <list> = #(); // FOR CLOSED OVER TEMPORARIES
  slot lifture :: <list> = #(); // FOR LIFTED CLOSED OVER TEMPORARIES
end rare-slots;

ignore(remove-inners!);
ignore(remove-lifture!);

define method strip-bindings (env :: <lambda-lexical-environment>)
  remove-bindings!(env);
end method;

define method strip-assignments (env :: <lambda-lexical-environment>)
  remove-variable-assignments!(env);
end method;

// define method broken (s, #rest a)
//   // apply(break, s, a);
// end method;

define method strip-environment (env :: <lambda-lexical-environment>)
  strip-bindings(env);
  strip-assignments(env);
  remove-entries!(env);
  remove-loops!(env);
  let number-temporaries = size(temporaries(env));
  lambda-log-number-temporaries(lambda(env))
    := if (number-temporaries = 0)
         0
       else
         round/(log(as(<float>, number-temporaries)), log(2.0))
       end if;
  /*
  let tmps* = compute-temporaries(env);
  let tmps  = temporaries(env);
  unless (size(tmps*) = size(tmps))
    for (tmp in tmps)
      when (used?(tmp) & ~member?(tmp, tmps*))
        format-out("IN %= RECONSTRUCTED TEMPORARIES %= INVALID %= MISSING %=\n",
                   lambda(env), tmps*, tmps, tmp);
        broken("BAD RECONSTRUCTION %= %=", lambda(env), tmp);
      end when;
    end for;
  end unless;
  */
end method;

define method assignments (tmp :: <lexical-variable>) => (res :: <list>)
  let tbl = variable-assignments(environment(tmp));
  if (tbl)
    element(tbl, tmp, default: #())
  else
    #()
  end if;
end method;

define method assignments-setter
    (value :: <list>, tmp :: <lexical-variable>) => (res :: <list>)
  let tbl :: <table>
    = variable-assignments(environment(tmp))
        | (variable-assignments(environment(tmp)) := make(<table>));
  if (value == #())
    remove-key!(tbl, tmp);
    value
  else
    element(tbl, tmp) := value;
  end if;
end method;

define method frame-size
    (env :: <lambda-lexical-environment>) => (res :: <integer>)
  number-temporaries(env)
end method;

define method frame-size-setter
    (new-value, env :: <lambda-lexical-environment>)
end method;

define method lambda-loop (f :: <&lambda>) => (res)
  first(f.environment.loops, default: #f)
end method;

define method lambda-loop-setter (loop, f :: <&lambda>)
  let env = f.environment;
  // TODO: MANAGE THESE
  // env.loops := add!(env.loops, loop);
  env.loops := list(loop);
end method;

define constant $top-level-environment
  = make(<top-level-environment>, lambda: #f, outer: #f);

define method lambda-environment (env :: <lexical-environment>)
  env
end;

define method add-inner!
    (env :: <top-level-environment>,
     inner :: <lambda-lexical-environment>)
end method;

define method add-inner!
    (env :: <module>, inner :: <lambda-lexical-environment>)
end method;

define method add-inner!
    (env :: <lambda-lexical-environment>, inner :: <lambda-lexical-environment>)
  env.inners := add-new!(env.inners, inner);
end method;

define class <local-lexical-environment> (<lexical-environment>)
  constant slot binding-id, required-init-keyword: id:;
  constant slot binding-type, required-init-keyword: type:;
  constant slot binding-value, required-init-keyword: value:;
  // SAVE SLOTS
  // slot %lambda-environment :: false-or(<lambda-lexical-environment>) = #f,
  //   init-keyword: environment:;
end class;

define function make-local-lexical-environment
    (name :: <variable-name-fragment>, value, type, env :: <environment>)
 => (new-env :: <local-lexical-environment>)
  make(<local-lexical-environment>,
       id: name,
       type: type,
       value: value,
       outer: env)
end;

define method lambda-environment (env :: <local-lexical-environment>)
  lambda-environment(outer(env))
end;

/* SAVE SLOTS
define method lambda-environment (env :: <local-lexical-environment>)
  %lambda-environment(env)
    | (%lambda-environment(env) := lambda-environment(outer(env)))
end;

define method lambda-environment-setter
    (value :: <lambda-lexical-environment>, env :: <local-lexical-environment>)
  %lambda-environment(env) := value;
end;
*/

define method add-inner!
    (env :: <local-lexical-environment>, inner :: <lambda-lexical-environment>)
  add-inner!(env.outer, inner)
end method;

define method all-environments (environment :: <environment>)
  collecting ()
    iterate loop (environment = environment)
      collect(environment);
      if (environment.outer)
        loop(environment.outer);
      end if;
    end iterate;
  end collecting;
end method;

define method next-frame-offset (env :: <lambda-lexical-environment>)
  let offset = env.frame-size;
  env.frame-size := env.frame-size + 1;
  offset
end method;

define method add-variable!
    (env :: <lambda-lexical-environment>,
     name :: <variable-name-fragment>,
     variable :: <lexical-variable>)
  env.bindings[name] := variable;
end method;

define method lookup
    (env :: <lambda-lexical-environment>, name :: <variable-name-fragment>,
     #rest options, #key default, reference? = #t)
 => (binding, type, environment)
  let v = element(env.bindings, name, default: #f);
  if (v)
    values(v, #f, env)
  else
    apply(lookup, env.outer, name, options)
  end
end method;

define method lookup
    (env :: <local-lexical-environment>, name :: <variable-name-fragment>,
     #rest options, #key default, reference? = #t)
 => (binding, type, environment)
  if (same-name-when-local?(binding-id(env), name))
    values(binding-value(env), binding-type(env), env)
  else
    apply(lookup, env.outer, name, options)
  end
end method;

define function lookup-in-top-level-environment
    (name :: <variable-name-fragment>, default, reference?)
 => (binding, type, environment)
  let env = outer-lexical-environment();
  let val = if (env) element(env, name, default: $unfound)
            else $unfound end;
  if (found?(val))
    values(make(<interactor-binding>, name: name, interactor-id: val), #f, #f)
  else
    values(lookup-binding(name, reference?: reference?), #f, #f)
  end;
end;

define method lookup
    (env == #f,
     name :: <variable-name-fragment>,
     #key default, reference? = #t)
 => (binding, type, environment)
  lookup-in-top-level-environment(name, default, reference?)
end method;

define method lookup
    (env :: <top-level-environment>,
     name :: <variable-name-fragment>,
     #key default, reference? = #t)
 => (binding, type, environment)
  lookup-in-top-level-environment(name, default, reference?)
end method;


define method inner-environment?
    (maybe-inner :: <environment>, maybe-outer :: <environment>)
  block (return)
    for (e = maybe-inner then e.outer, while: e)
      if (e == maybe-outer)
        return(#t);
      end if;
    finally
      #f
    end for
  end block
end method inner-environment?;

define generic lambda-has-free-lexical-references?
    (object :: <&lambda-or-code>) => (free-references? :: <boolean>);

define function closure-self-reference?
    (t :: <temporary>, lambda-env :: <lambda-lexical-environment>)
 => (res :: <boolean>)
  let c = generator(t);
  instance?(c, <make-closure>)
    & lambda-env == environment(computation-closure-method(c))
end function;

define function closure-self-referencing?
    (lambda-env :: <lambda-lexical-environment>)
 => (res :: <boolean>)
  any?(rcurry(closure-self-reference?, lambda-env), closure(lambda-env))
end function;

define method closure-size
    (environment :: <lambda-lexical-environment>) => (res :: <integer>)
  let closure = environment.closure;
  let closure-size = size(closure);
  iterate loop (count = 0, index = 0)
    if (index >= closure-size)
      count
    else
      let self? = #f
        /* closure-self-reference?(closure[index], environment) */;
      loop(count + if (self?) 0 else 1 end, index + 1)
    end if
  end iterate;
end method;

define method closure-offset
    (environment :: <lambda-lexical-environment>, tmp :: <temporary>)
  let closure = environment.closure;
  let closure-size = closure.size;
  iterate check (offset = 0, index = 0)
    if (index >= closure-size)
      #f
    // elseif (closure-self-reference?(tmp, environment))
    //   check(offset, index + 1)
    elseif (closure[index] == tmp)
      offset
    else
      check(offset + 1, index + 1)
    end if
  end iterate;
end method;

define method closure-offset (lambda :: <&lambda>, tmp :: <temporary>)
  if (tmp.closed-over?)
    closure-offset(lambda.environment, tmp)
  end if
end method;

define method lambda-has-free-lexical-references? (lambda :: <&lambda>)
 => (free-references? :: <boolean>)
  // look upward, since we know there isn't much that way --
  // this is only used for <definition>, which only occurs at "top level"
  // if this is ever called from elsewhere, it may need to get cleverer to
  // be sufficiently fast
  let lambda-env = environment(lambda);
  block (found)
    for (outer-env = outer(lambda-env) then outer(outer-env),
         while: outer-env)
      if (instance?(outer-env, <lambda-lexical-environment>))
        for-temporary (t in outer-env)
          for (use in users(t))
            if (inner-environment?(environment(use), lambda-env))
              // some use of an outer temporary is inside this lambda
              found(#t)
            end;
          end;
        end;
      end;
    end;
    #f
  end;
end;

define method lambda-has-free-lexical-references? (code :: <&code>)
 => (free-references? :: <boolean>)
  lambda-has-free-lexical-references?(function(code))
end;

define method extract-lambda (lambda :: <&lambda>) => ()
  // splice lambda out of the outer lambda
  // must be known to have no free lexical references
  // XXX: might violation of this precondition lead to the
  // mysterious "are live on entry to lambda" errors?
  let env = environment(lambda);
  let outer-env = lambda-environment(outer(env));
  let outer-outer-env = outer(outer-env);
  inners(outer-env) := remove!(inners(outer-env), env);
  outer(env) := outer-outer-env;
  if (outer-outer-env)
    add-inner!(lambda-environment(outer-outer-env), env);
  end;
  lambda-top-level?(lambda) := #t;
  run-compilation-passes(lambda);
end;

define method extract-lambda (code :: <&code>) => ()
  extract-lambda(function(code))
end;

// Walk the lexical variables of the environment in inside out order.

// define generic do-lexical-variables-in-scope
//     (f :: <function>, env :: false-or(<environment>)) => ();
//
// define method do-lexical-variables-in-scope
//     (f :: <function>, env == #f) => ()
// end method;
//
// define method do-lexical-variables-in-scope
//     (f :: <function>, env :: <module>) => ()
// end method;
//
// define method do-lexical-variables-in-scope
//     (f :: <function>, env :: <lexical-environment>) => ()
//   do-lexical-variables-in-scope(f, env.outer);
// end method;
//
// define method do-lexical-variables-in-scope
//     (f :: <function>, env :: <lambda-lexical-environment>) => ()
//   for (var in env.bindings) f(var) end;
//   next-method();
// end method;
//
// define method do-lexical-variables-in-scope
//     (f :: <function>, env :: <local-lexical-environment>) => ()
//   f(binding-value(env));
//   next-method();
// end method;
//
// define method lexical-variables-in-scope
//     (env :: <environment>) => (variables :: <list>)
//   env.cached-lexical-variables-in-scope
//     | (env.cached-lexical-variables-in-scope
//          := collecting ()
//               do-lexical-variables-in-scope(method (var) collect(var) end, env);
//             end);
// end method;

// Hacks!!!

define sideways method classify-word-in (context :: <pair>, word)
  classify-word-in(tail(context), word);
end method;

define sideways method fragment-module
    (form :: <variable-name-fragment>) => (module :: <module>)
  let module = fragment-context(form);
  if (module)
    if (instance?(module, <pair>))
      // Local environment + module.
      tail(module);
    else
      module
    end;
  else
    dylan-implementation-module();
  end;
end method;
