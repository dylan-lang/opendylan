Module:   dfmc-flow-graph
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: check invariants in the DFM flow graph
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// framework

define class <invariant-violation> (<simple-error>)
end class <invariant-violation>;

define thread variable *checker-message* = #f;

define method invariant-error (message, #rest arguments)
  error(make(<invariant-violation>,
	     format-string:
	       if (*checker-message*)
		 concatenate(*checker-message*, message)
	       else
		 message
	       end if,
	     format-arguments: arguments))
end method invariant-error;

define method ensure (condition, message, #rest arguments) => ();
  unless (condition)
    apply(invariant-error, message, arguments)
  end unless;
  values()
end method ensure;


/// driver

define method ensure-invariants (f :: <&lambda>, #key before, after) => ();
  local method make-message (string, name)
          // should just use format
          concatenate(string, " ", as-lowercase(as(<string>, name)), ": ")
        end method make-message;
  dynamic-bind (*checker-message* =
		  case
		    before => make-message("before", before);
		    after  => make-message("after",  after);
		      otherwise #f
		  end case)
    ensure-invariants*(f);
  end dynamic-bind;
  values()
end method ensure-invariants;

define method ensure-invariants* (f :: <&lambda>) => ();
  for-lambda (sub-f in f)
    ensure-invariants*(sub-f);
  end for-lambda;
  check-lambda(f);
  for-computations (c in f)
    ensure-environment-invariants(c, f);
    check-computation(c);
  end for-computations;
end method ensure-invariants*;


/// checks

// still needed:
//   liveness analysis (merge nodes)
//   multiple value checks
//   final-state invariants (register allocation checks)

define function check-lambda (f) => ();
  let e = f.environment;
  let outer-lambda = lambda-environment(e.outer);
  if (instance?(outer-lambda, <lambda-lexical-environment>))
    ensure(member?(e, outer-lambda.inners),
	   "%= has outer lambda environment %=, but is not among %=",
	   e, outer-lambda, outer-lambda.inners);
  end if;
  for (sub-e in e.inners)
    ensure(lambda-environment(sub-e.outer) == e,
	   "%= is an inner environment of %=, but its outer lambda environment is %=",
	   sub-e, e, lambda-environment(sub-e.outer));
  end for;
end function check-lambda;

define method check-computation (c :: <computation>) => ();
  // TODO: need equivalent 
  // check-computation-counts(c, next-computations, previous-computations);
  // check-computation-counts(c, previous-computations, next-computations);
end method check-computation;


/*
/// wiring checks

define method check-computation-counts (c :: <computation>, forwards, backs)
  for (forward-c in c.forwards)
    let to = count(c.forwards, forward-c);
    let fro = count(forward-c.backs, c);
    ensure(to == fro,
           "%= has %= %= links to %= with %= %= links back",
	   c, to, forwards, forward-c, fro, backs);
  end for;
end method check-computation-counts;
*/

/// environment checks

define method ensure-environment-invariants
    (c :: <computation>, f :: <&lambda>)
  ensure(c.environment == f.environment,
         "%= is in %= but has a different environment", c, f);
  if (c.temporary)
    ensure(c.temporary.generator == c,
           "%= defines %=, but is not the registered definer",
	   c, c.temporary);
    check-environment(c.temporary, c, f);
  end if;
  do-used-temporaries
    (method (t)
       if (t)
	 ensure(member?(c, t.users),
		"%= uses %=, but is not a registered user", c, t);
	 check-environment(t, c, f);
       end if;
     end,
     c);
end method ensure-environment-invariants;

define method check-environment (temporary, computation, function)
  let t-env = temporary.environment;
  block (exit)
    for (env = function.environment then env.outer, while: env)
      if (env == t-env)
	exit();
      end if;
    end for;
    invariant-error("%= uses %= in %=, but environment %= not in scope",
	            computation, temporary, function, temporary.environment);
  end block
end method check-environment;


/// check model objects

define method check-computation (c :: <make-closure>) => ();
  next-method();
  check-object(c.computation-closure-method)
end method check-computation;

define method check-object (o :: <object>) => ();
  values()
end method check-object;

define method check-object (o :: <&lambda>) => ();
  unless (lambda-top-level?(o))
    check-lambda(o)
  end;
end method check-object;

define method check-object (o :: <&code>) => ();
  check-object(o.function)
end method check-object;

/// specific computations

define method check-computation (c :: <nop-computation>) => ();
  next-method();
  ensure(~c.temporary | ~c.temporary.used?,
         "temporary %= generated by %= is used", c.temporary, c)
end method check-computation;

define method check-computation (c :: <binary-merge>) => ();
  next-method();
  ensure(if (c.temporary.multiple-values?)
	   multiple-values?(merge-left-value(c)) &
	     multiple-values?(merge-right-value(c))
	 else
	   ~multiple-values?(merge-left-value(c)) &
	     ~multiple-values?(merge-right-value(c))
	 end if,
         "disagreement on multiple values in %=", c)
end method check-computation;

define method check-computation (c :: <temporary-transfer-computation>)
 => ();
  next-method();
  ensure(if (c.temporary.multiple-values?)
	   multiple-values?(computation-value(c))
	 else
	   ~multiple-values?(computation-value(c))
	 end if,
         "disagreement on multiple values in %=", c)
end method check-computation;

define method check-computation (c :: <definition>) => ();
  next-method();
  ensure(instance?(c.assigned-binding, <module-binding>),
         "definition requires a module binding in %=", c)
end method check-computation;

define method check-computation (c :: <values>) => ();
  next-method();
  let temp = c.temporary;
  ensure(~temp | temp.multiple-values?,
         "need a multiple value result temporary in %=", c)
end method check-computation;

define method check-computation (c :: <extract-value-computation>) => ();
  next-method();
  ensure(c.computation-value.multiple-values?,
         "need a multiple value input temporary in %=", c)
end method check-computation;

define method check-computation (c :: <return>) => ();
  next-method();
  ensure(c.computation-value.multiple-values?,
         "need a multiple value input temporary in %=", c)
end method check-computation;

define method check-computation (c :: <exit>) => ();
  ensure(member?(c, c.entry-state.exits),
	 "%= is not a member of its entry-state.exits %=",
	 c, c.entry-state.exits);
end method check-computation;


/*
/// move to utilities?  maybe functional-extensions?

define method count (collection :: <collection>, value, #key test = \==)
  let n = 0;
  for (e in collection)
    if (test(e, value))
      n := n + 1;
    end if;
  finally
    n
  end for
end method count;
*/
