module: dfmc-flow-graph
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <optimization-queue> = <queue>;

define method init-optimization-queue (code :: <&lambda>) => ()
  let queue :: <optimization-queue>
    = make(<optimization-queue>, // estimated number of computations
           capacity: truncate(number-temporaries(code) * 1.5));
  code.optimization-queue := queue;
  if (code.body)
    walk-lambda-computations
      (method (item :: <queueable-item-mixin>)
         item.item-status := $queueable-item-absent;
         add-to-queue!(queue, item);
       end,
       code.body);
  end if;
  reverse-queue!(queue);
end method;

// queue computation for further optimization attempts. This is
// typically used to enable reconsideration of  computations which are
// *potentially* subject to optimization as a result optimization of
// another computation.

define generic re-optimize (c :: false-or(<computation>)) => ();

define compiler-open generic re-optimize-type-estimate
  (c :: <computation>) => ();

// by default we just add the computation back to the queue.
define method re-optimize-into (c :: <computation>, lambda :: <&lambda>) => ()
  let q = lambda.optimization-queue;
  // format-out("--- RE-OPT %= INT %= ---\n", c, lambda);
  re-optimize-type-estimate(c);
  if (q)
    add-to-queue!(q , c);
    // print-queue-out(q);
  elseif (lambda.body)                // make sure it's not dead
    init-optimization-queue(lambda)
  end if;
end method;

define method re-optimize-into! (c :: <computation>, lambda :: <&lambda>) => ()
  re-optimize-into(c, lambda);
end method;

/// GENERAL

define method re-optimize (c == #f) => ()
end method;

define method re-optimize (c :: <computation>) => ()
  re-optimize-into(c, c.environment.lambda);
end method;

define method re-optimize (c :: <merge>) => ()
  if (c.item-status == $queueable-item-absent)
    next-method();
    re-optimize-users(c.temporary)
  end if
end method;

define method re-optimize (c :: <make-closure>) => ()
  next-method();
  re-optimize-users(c.temporary);
end method;

define method re-optimize (c :: <values>) => ()
  next-method();
  re-optimize-users(c.temporary)
end method;

define method re-optimize (c :: <temporary-transfer-computation>) => ()
  next-method();
  re-optimize-users(c.temporary)
end method;

define method re-optimize (c :: <extract-value-computation>) => ()
  next-method();
  re-optimize-users(c.temporary)
end method;

define method re-optimize (c :: <adjust-multiple-values-computation>) => ()
  next-method();
  re-optimize-users(c.temporary)
end method;

define method re-optimize (c :: <function-call>) => ()
  next-method();
  unless (c.compatibility-state == $compatibility-checked-compatible)
    c.compatibility-state := $compatibility-unchecked; // HACK: ADD NOTHER ONE
  end unless;
  unless (c.compatibility-state == $compatibility-checked-incompatible)
    dispatch-state(c) := $dispatch-untried;
  end unless;
end method;

/// USERS

define method re-optimize-users (o :: <object>)
  re-optimize(o)
end method;

define method re-optimize-users (o :: <referenced-object>)
  for (c in o.users)
    re-optimize-users(c);
  end;
end method;

/// LOCAL USERS

define function in-environment?
    (env :: <lexical-environment>, c :: <computation>) => (b :: <boolean>)
  iterate loop (e = c.environment)
    if (e == env)
      #t
    elseif (e)
      loop(e.outer)
    else
      #f
    end if
  end iterate;
end function;

define method re-optimize-if-local (env :: <lexical-environment>, c) => ()
end method;

define method re-optimize-if-local
    (env :: <lexical-environment>, c :: <computation>) => ()
  if (in-environment?(env, c))
    re-optimize(c);
  end if;
end method;

define method re-optimize-local-users (env :: <lexical-environment>, o)
end method;

define method re-optimize-local-users
    (env :: <lexical-environment>, o :: <computation>)
  re-optimize-if-local(env, o);
end method;

define method re-optimize-local-users
    (env :: <lexical-environment>, o :: <referenced-object>)
  for (c in o.users)
    re-optimize-local-users(env, c);
  end;
end method;

/// USED FUNCTION

define method maybe-re-optimize-used-function
    (env :: <lexical-environment>, o :: <&lambda-or-code>)
  let f = o.function;
  unless (lambda-top-level?(f))
    re-optimize-users(f);
    re-optimize-users(f.^iep);
  end unless;
end method;

define method maybe-re-optimize-used-function (env :: <lexical-environment>, o)
end method;

/// GENERATORS

define method re-optimize-generators (s :: <vector>)
  for (t in s)
    re-optimize(t.generator);
  end;
end method;
