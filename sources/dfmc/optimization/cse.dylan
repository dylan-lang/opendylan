Module: dfmc-optimization
Author: Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Naive CSE. Only attempts CSE within a single code segment - no attempt
// is made to do CSE across local function calls.

// Potential optimisations (not done):
//
//   o Hoist subexpressions common to all branches of a conditional,
//     and that are always reached, above the conditional. Saves 
//     space, but could also save time if further eliminations can
//     then be done in code following the conditional.
//
//       e.g. if (...) a * b else a * b * c end
//
//   o Subfunctions created within the scope of an expression are 
//     ammenable to CSE with expressions computed before their 
//     creation in the enclosing body. This could require extra
//     values to be closed over (assumes temporary/variable 
//     unification). We could do this anyway and rely on an 
//     orthogonal optimisation which decides whether closing over
//     a value is "worth the effort", or whether it would be 
//     just as fast to simply recompute it.
//
//       e.g. let val = a * b + c * d;
//            let f = lambda (x) x * a * b end;
//            ...
//
//   o Have as a general goal to increase the scope of an 
//     expression if doing so increases CSE oportunities.
//     For example, moving simple computations ahead of a
//     bind where they don't depend on the bind to allow
//     the bound values (perhaps functions) to see them.
//
//       e.g. let f = lambda (x) x * a * b end;
//            f(a * b); // can compute a * b above and share


//// CSE states.

// Need a good reusable-computation hash and lookup table.

define primary class <cse-state> (<object>)
  constant slot reusable-computations :: <stretchy-object-vector> 
    = make(<stretchy-vector>);
  constant slot dominating-state :: false-or(<cse-state>) = #f,
    init-keyword: dominating-state:;
end class;

define sealed domain make (singleton(<cse-state>)); 
define sealed domain initialize (<cse-state>);

define primary class <cse-if-state> (<cse-state>)
  constant slot cse-if :: <if>,
    required-init-keyword: if:;
end class;

define class <cse-consequent-if-state> (<cse-if-state>)
end class;

define constant $cse-consequent-if-direction = #"consequent";

define method cse-if-state-direction 
    (state :: <cse-consequent-if-state>) => (res :: <symbol>)
  $cse-consequent-if-direction
end method;

define class <cse-alternative-if-state> (<cse-if-state>)
end class;

define constant $cse-alternative-if-direction = #"alternative";

define method cse-if-state-direction 
    (state :: <cse-alternative-if-state>) => (res :: <symbol>)
  $cse-alternative-if-direction
end method;

// We lookup and install if necessary in one go in the hope of saving
// some time.

define function intern-reusable-computation
    (c :: <computation>, state :: <cse-state>)
 => (canonical-c :: <computation>)
  block (return)
    for (search-state = state then dominating-state(search-state),
         while: search-state)
      for (reusable-c in reusable-computations(search-state))
        if (equivalent-computation?(c, reusable-c))
          return(reusable-c);
        end if;
      end for;
    end for;
    add!(reusable-computations(state), c);
    c
  end block;
end function;

//// CSE driver.

define method share-common-subexpressions (f :: <&lambda>) => ()
  let code = f.body;
  if (code)
    cse-walk-computations(code, #f, make(<cse-state>));
  else
    format-out("CSE found empty function: %=\n", f);
  end if;
end method;

define method cse-walk-computations
    (c :: <computation>, last :: false-or(<computation>), 
     state :: <cse-state>) 
 => ()
  iterate loop (c = c)
    unless (c == last)
      cse-walk-computation(c, state);
      let cc = c.next-computation;
      if (cc)
        loop(cc);
      end if;
    end unless;
  end iterate;
end method;

define method cse-walk-computation 
    (c :: <computation>, state :: <cse-state>) => ()
  if (reusable-computation?(c))
    let canonical-c = intern-reusable-computation(c, state);
    if (c ~== canonical-c)
      // Is replacing computations mid-walk this way safe?
      replace-computation-with-temporary!(c, canonical-c.temporary);
    end;
  end;
end method;

define method cse-lookup-reusable-if-test 
    (state :: <cse-state>, tst :: <value-reference>)
 => (res :: false-or(<symbol>))
  if (reusable-computation?(tst))
    block (return)
      for (search-state = state then dominating-state(search-state),
	   while: search-state)
        when (instance?(state, <cse-if-state>) & (tst == state.cse-if.test))
          return(cse-if-state-direction(state));
        end when;
      end for;
    end block;
  end if;
end method;

define method cse-walk-computation
    (c :: <if>, state :: <cse-state>) => ()
  let direction?
    = cse-lookup-reusable-if-test(state, c.test);
  select (direction?)
    $cse-consequent-if-direction  => constant-fold-if(c, #t);
    $cse-alternative-if-direction => constant-fold-if(c, #f);
    otherwise                     => ;
  end select;
  let consequent-state 
    = make(<cse-consequent-if-state>, dominating-state: state, if: c);
  cse-walk-computations
    (c.consequent, c.next-computation, consequent-state);
  let alternative-state 
    = make(<cse-alternative-if-state>, dominating-state: state, if: c);
  cse-walk-computations
    (c.alternative, c.next-computation, alternative-state);
end method;

define method cse-walk-computation
    (c :: <loop>, state :: <cse-state>) => ()
  let body-state
    = make(<cse-state>, dominating-state: state);
  cse-walk-computations
    (c.loop-body, c.next-computation, body-state);
end method;

define method cse-walk-computation
    (c :: <bind-exit>, state :: <cse-state>)  => ()
  let exit-state 
    = make(<cse-state>, dominating-state: state);
  cse-walk-computations(c.body, c.next-computation, exit-state);
end method;

define method cse-walk-computation
    (c :: <unwind-protect>, state :: <cse-state>) => ()
  let body-state 
    = make(<cse-state>, dominating-state: state);
  cse-walk-computations(c.body, c.next-computation, body-state);
  let cleanups-state 
    = make(<cse-state>, dominating-state: state);
  cse-walk-computations(c.cleanups, c.next-computation, cleanups-state);
end method;

//// CSE predicates.

// Reusability and equivalence.

// General methods.

define method reusable-computation? 
    (c :: <computation>) => (well? :: <boolean>)
  #f
end method;

define method reusable-computation? 
    (c :: <boolean>) => (well? :: <boolean>)
  #t
end method;

define method equivalent-computation? 
    (c1 :: <object>, c2 :: <object>)
 => (well? :: <boolean>)
  c1 == c2
end method;

define method equivalent-computation? 
    (c1 :: <computation>, c2 :: <computation>)
 => (well? :: <boolean>)
  c1 == c2
end method;

define method reusable-computation? 
    (c :: <loop-merge>) => (well? :: <boolean>)
  #t
end method;

define method reusable-computation? 
    (c :: <temporary>) => (well? :: <boolean>)
  reusable-computation?(c.generator)
end method;

// Binding references.

define method reusable-computation? 
    (c :: <binding-reference>) => (well? :: <boolean>)
  constant?(referenced-binding(c))
end method;

define method equivalent-computation?
    (ref1 :: <binding-reference>, ref2 :: <binding-reference>)
 => (well? :: <boolean>)
  referenced-binding(ref1) == referenced-binding(ref2)
end method;

// Object references.

define method reusable-computation? 
    (c :: <object-reference>) => (well? :: <boolean>)
  #t
end method;

define method equivalent-computation?
    (ref1 :: <object-reference>, ref2 :: <object-reference>)
 => (well? :: <boolean>)
  equivalent-computation?(reference-value(ref1), reference-value(ref2))
end method;

define method equivalent-computation? 
    (c1 :: <&raw-machine-word>, c2 :: <&raw-machine-word>)
 => (well? :: <boolean>)
  ^raw-object-value(c1) == ^raw-object-value(c2)
end method;

// Primitive calls.

define method reusable-computation? 
    (c :: <primitive-call>) => (well? :: <boolean>)
  // Some ugly subclasses of <primitive-call> don't actually have a
  // primitive! 8/
  c.primitive 
    & primitive-stateless?(c.primitive)
    & every?(reusable-computation?, c.arguments)
end method;

define method equivalent-computation?
    (c1 :: <primitive-call>, c2 :: <primitive-call>)
 => (well? :: <boolean>)
  c1.primitive == c2.primitive
    & every?(equivalent-computation?, c1.arguments, c2.arguments)
end method;

// Slot access.

define method reusable-computation? 
    (c :: <slot-value>) => (well? :: <boolean>)
  ~(^slot-setter(computation-slot-descriptor(c)))
end method;

define method equivalent-computation?
    (c1 :: <slot-value>, c2 :: <slot-value>)
 => (well? :: <boolean>)
  computation-slot-descriptor(c1) == computation-slot-descriptor(c2)
    & equivalent-computation?
        (computation-instance(c1), computation-instance(c2))
end method;
