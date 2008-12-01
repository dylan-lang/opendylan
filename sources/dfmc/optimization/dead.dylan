module:   dfmc-optimization
author:   Paul Haahr
synopsis: dead-code elimination, of various sorts
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Delete useless environments.

// Where inlining has removed all references to a sublambda of a function,
// delete its obsolete entry in the enclosing environment.

// define compilation-pass delete-useless-environments,
//   visit: functions,
//   optimization: low,
//   after: delete-useless-computations,
//   before: analyze-environments;

define method delete-useless-environments (f :: <&lambda>)
  let env = f.environment;
  collecting (used)
    for (inner-env in env.inners)
      let inner-lambda = inner-env.lambda;
      // If any aspect of the function is used, it must be preserved since
      // the iep and xep will make reference to it.
      if (lambda-used?(inner-lambda))
        collect-into(used, inner-env);
      else
        // format-out("Eliminating environment of: %=\n", inner-env.lambda);
      end;
    end;
    env.inners := collected(used);
  end;
  #t
end method delete-useless-environments;

// define compilation-pass delete-useless-computations,
//   visit: computations,
//   optimization: low,
//   after: single-value-propagation,
//   before: analyze-environments;

define method really-delete-useless-computations (c :: <computation>)
  if (instance?(next-computation(c), <if-merge>))
    re-optimize(c);
  end if;
  if (*colorize-dispatch*)
    if (instance?(c, <function-call>))
      color-dispatch(c, #"eliminated");
    end;
  end;
  delete-computation!(c);
  #t
end method;

define method delete-useless-computations (c :: <computation>)
  if (useless?(c))
    format-out("deleting %=\n", c);
    really-delete-useless-computations(c)
  end if
end method delete-useless-computations;

define method delete-useless-computations (c :: <if-merge>)
  if (useless?(c))
    let status = #f;
    c.temporary := #f;
    if (merge-left-value(c))
      remove-user!(merge-left-value(c), c);  merge-left-value(c)  := #f;
      status := #t;
    end if;
    if (merge-right-value(c))
      remove-user!(merge-right-value(c), c); merge-right-value(c) := #f;
      status := #t;
    end if;
    let if-c = previous-computation(c);
    if (consequent(if-c) == alternative(if-c))
      delete-computation!(c);
      delete-computation!(if-c);
    end if;
    status
  end if
end method delete-useless-computations;

//// nop and unused instruction

define generic useless? (c :: <computation>) => (boolean :: <boolean>);

define method useless? (c :: <computation>) => (well? :: <boolean>)
  // This case is normally detected by the code that decrements the
  // use count on the temporary, in remove-user! below.  However,
  // with a computation that's created with no uses and whose value
  // is ignored, we never get this effect.
  let t = c.temporary;
  // format-out("c = %=; t.users = %=\n", c, if (t) t.users else #[] end);
  ~(t & t.used?) & c.side-effect-free?
end method useless?;

define method useless? (c :: <nop>) => (res :: singleton(#t))
  #t
end method useless?;

// a bind-exit node is useless if there are no exits

define method useless? (c :: <bind-exit>) => (res :: <boolean>)
  empty?(exits(entry-state(c)))
end method;

// an unwind-protect block is useless if:
//  (a) there's nothing to protect (no body), or
//  (b) nothing to do on unwind (no cleanup).
define method useless? (c :: <unwind-protect>) => (res :: <boolean>)
  // gts-debug("cleanups", "useless?(<unwind-protect>)(%=,%=,%=).\n",
  //   c, has-body?(c), has-cleanups?(c));
  (~ has-body?(c))
    | (~ has-cleanups?(c))
end method;

define method useless? (c :: <if>) => (res :: <boolean>)
  // c.consequent == c.alternative
  #f
end method useless?;

//// side-effect-free?

define method side-effect-free? (c :: <computation>)
  #f
end method;

define method side-effect-free? (c :: <primitive-call>)
  ~primitive-side-effecting?(primitive(c))
end method;

define method side-effect-free? (c :: <make-closure>)
  #t
end method;

define method side-effect-free? (c :: <variable-reference>)
  #t
end method;

define method side-effect-free? (c :: <keyword-default>)
  #t
end method;

define method side-effect-free? (c :: <stack-vector>)
  #t
end method;

define method side-effect-free? (c :: <slot-value>)
  #t
end method;

define method side-effect-free? (c :: <repeated-slot-value>)
  #t
end method;

define method side-effect-free? (c :: <temporary-transfer>)
  #t
end method;

define method side-effect-free? (c :: <binary-merge>)
  #t
end method;

define method side-effect-free? (c :: <values>)
  #t
end method side-effect-free?;

define method side-effect-free? (c :: <extract-value-computation>)
  #t
end method side-effect-free?;

define method side-effect-free? (c :: <adjust-multiple-values-computation>)
  #t
end method side-effect-free?;

define compiler-sideways method remove-user! (t :: <temporary>, user)
  next-method();
  if (~used?(t) & t.generator)
    re-optimize(t.generator);
  end;
end method;

define compiler-sideways method remove-user! (ref :: <object-reference>, user)
  next-method();
  if (~used?(ref))
    remove-user!(reference-value(ref), ref);
  end;
end method;
