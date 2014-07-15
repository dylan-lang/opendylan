Module:   dfmc-execution
Author:   Jonathan Bachrach, Paul Haahr, Keith Playford
Synopsis: Evaluation of DFM programs
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// MACHINE STATE

define class <machine-state> (<object>)
  constant slot state-closure = #f, init-keyword: closure:;
  constant slot state-temporaries :: <simple-object-vector>, // could be a repeated slot
    required-init-keyword: temporaries:;
end class <machine-state>;

define constant $uninitialized-temporary
  = make(<unknown>, name: #"uninitialized-temporary");

define method unchecked-frame-fetch
    (state :: <machine-state>, offset :: <integer>)
  state.state-temporaries[offset]
end method;

/* TODO: OBSOLETE?
define method frame-fetch (state :: <machine-state>, offset :: <integer>)
  let value = unchecked-frame-fetch(state, offset);
  if (value == $uninitialized-temporary)
    error("fetched uninitialized temporary")
  end if;
  value
end method;
*/

define method frame-fetch-setter
    (new-value, state :: <machine-state>, offset :: <integer>)
  state.state-temporaries[offset] := new-value
end method;

define method make
    (class :: subclass(<machine-state>), #rest initargs, #key frame-size = 0)
 => (res :: <machine-state>)
  apply(next-method, class,
        temporaries: make(<simple-object-vector>,
                          size: frame-size, fill: $uninitialized-temporary),
        initargs)
end method make;

define class <value-cell> (<object>)
  slot value-cell-value, init-keyword: value:;
end class;

define method unchecked-fetch
    (the-state :: <machine-state>, object :: <temporary>)
  let offset = closure-offset(the-state.state-closure, object);
  if (offset)
    let data = the-state.state-closure[offset];
    if (object.cell?)
      data.value-cell-value
    else
      data
    end if
  else
    unchecked-frame-fetch(the-state, object.frame-offset)
  end if;
end method;

define method fetch
    (state :: <machine-state>, object :: <temporary>)
  let value = unchecked-fetch(state, object);
  if (value == $uninitialized-temporary)
    error("fetched uninitialized temporary")
  end if;
  value
end method;

define method fetch-setter
    (new-value, the-state :: <machine-state>, object :: <temporary>)
  if (object.used?)
    let offset = object.cell? & closure-offset(the-state.state-closure, object);
    if (offset)
      the-state.state-closure[offset].value-cell-value := new-value
    else
      frame-fetch(the-state, object.frame-offset) := new-value
    end if
  end if
end method;

define method unchecked-fetch
    (state :: <machine-state>, binding :: <module-binding>)
  binding.binding-value-slot
end method;

define method fetch
    (state :: <machine-state>, binding :: <module-binding>)
  binding.binding-value-slot
end method;

define method fetch-setter
    (value, state :: <machine-state>, variable :: <module-binding>)
  variable.binding-value-slot := value
end method;

define method unchecked-fetch
    (state :: <machine-state>, object :: <object-reference>)
  reference-value(object)
end method;

define method fetch
    (state :: <machine-state>, object :: <object-reference>)
  unchecked-fetch(state, object)
end method;

define method unchecked-fetch
    (state :: <machine-state>, object :: <defined-constant-reference>)
  unchecked-fetch(state, referenced-binding(object))
end method;

define method fetch
    (state :: <machine-state>, object :: <defined-constant-reference>)
  unchecked-fetch(state, object)
end method;

//// EXECUTION

define method execute (state :: <machine-state>, c :: <bind>)
  execute(state, c.next-computation);
end method;

define method create-closure
    (lambda :: <&lambda>, data :: <simple-object-vector>, sig :: <&signature>)
  make(<&method>,
       debug-name: lambda.^debug-name,
       signature:   ^function-signature(lambda),
       environment: lambda.environment,
       body: lambda.body,
       data: data)
end method;

define method create-closure
    (code :: <&iep>, data :: <simple-object-vector>, sig :: <&signature>)
  create-closure(code.function, data, sig).iep
end method;

define method create-closure
    (code :: <&xep>, data :: <simple-object-vector>, sig :: <&signature>)
  create-closure(code.function, data, sig).xep
end method;

define method create-closure-value (state :: <machine-state>, object)
  unchecked-fetch(state, object)
end method;

define method create-closure-value
    (state :: <machine-state>, object :: <lexical-variable>)
  let value = next-method();
  if (object.cell?)
    make(<value-cell>, value: value)
  else
    value
  end if
end method;

define method create-closure-data
    (state :: <machine-state>, lambda :: <&lambda>)
  map-as(<vector>, curry(create-closure-value, state),
         lambda.environment.closure)
end method;

define method create-closure-data
    (state :: <machine-state>, code :: <&code>)
  map-as(<vector>, curry(create-closure-value, state),
         code.environment.closure)
end method;

define method execute (state :: <machine-state>, c :: <make-closure>)
  let lambda = computation-closure-method(c);
  let sigtmp = computation-signature-value(c);
  let sigval
    = if (sigtmp) fetch(state, sigtmp) else ^function-signature(lambda) end;
  create-closure(lambda, create-closure-data(state, lambda), sigval);
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <variable-reference>)
  fetch(state, c.temporary) := fetch(state, c.referenced-binding);
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <set!>)
  let new-value = fetch(state, c.computation-value);
  fetch(state, c.assigned-binding) := new-value;
  if (c.temporary)
    fetch(state, c.temporary) := new-value;
  end if;
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <definition>)
  // this only applies to module-scoped variables
  let new-value = fetch(state, c.computation-value);
  c.assigned-binding.binding-value-slot := new-value;
  if (c.temporary)
    fetch(state, c.temporary) := new-value;
  end if;
  execute(state, c.next-computation)
end method;

define method execute (state :: <machine-state>, c :: <temporary-transfer-computation>)
  fetch(state, c.temporary) := fetch(state, c.computation-value);
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <function-call>)
  let function = fetch(state, c.function);
  fetch(state, c.temporary)
    := execute-call-using-function(state, function, c);
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <primitive-call>)
  let function = c.primitive;
  fetch(state, c.temporary)
    := run-stage
         (apply(compile-stage(function),
            map(compose(compile-stage, curry(fetch, state)),
                c.arguments)));
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <slot-value>)
  fetch(state, c.temporary)
    := ^slot-value(fetch(state, computation-instance(c)),
                   computation-slot-descriptor(c));
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <slot-value-setter>)
  fetch(state, c.temporary)
    := ^slot-value-setter
          (fetch(state, computation-new-value(c)),
           fetch(state, computation-instance(c)),
           computation-slot-descriptor(c));
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <repeated-slot-value>)
  fetch(state, c.temporary)
    := ^repeated-slot-value
          (fetch(state, computation-instance(c)),
           computation-slot-descriptor(c),
           ^raw-object-value(fetch(state, computation-index(c))));
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <repeated-slot-value-setter>)
  fetch(state, c.temporary)
    := ^repeated-slot-value-setter
          (fetch(state, computation-new-value(c)),
           fetch(state, computation-instance(c)),
           computation-slot-descriptor(c),
           ^raw-object-value(fetch(state, computation-index(c))));
  execute(state, c.next-computation);
end method;

// define variable *arguments* = make(<stretchy-vector>);
// define variable *optional-arguments* = make(<stretchy-vector>);

define method execute-call-using-function-and-arguments
    (state :: <machine-state>,
     function :: <&lambda>, arguments :: <stretchy-vector>)
  execute-call-using-function-and-arguments(state, function.xep, arguments)
end method;

define method execute-call-using-function-and-arguments
    (state :: <machine-state>,
     code :: <&iep>, arguments :: <stretchy-vector>)
  let function = code.function;
  let new-state
    = make(state.object-class,
           frame-size: function.frame-size, closure: function);
  for (argument in arguments,
       variable in function.parameters)
    fetch(new-state, variable) := argument;
  end for;
  execute(new-state, function.body);
end method;

define function process-keyword-arguments-into
    (new-arguments :: <stretchy-vector>,
     f :: <&lambda>, arguments :: <sequence>)
  let signature = ^function-signature(f);
  let all-keys? = ^signature-all-keys?(signature);
  let number-required = ^signature-number-required(signature);
  for (i from arguments.size - 1 to
         number-required by -2)
    let keyword = arguments[i - 1];
    block (break)
      for (j from 0 below f.keyword-specifiers.size by 2,
           k from number-required + 1)
        if (keyword == f.keyword-specifiers[j])
          new-arguments[k] := arguments[i];
          break();
        end if;
      end for;
    end block;
  end for;
end;

define method execute-call-using-function-and-arguments
    (state :: <machine-state>,
     code :: <&xep>, arguments :: <stretchy-vector>)
  let function = code.function;
  let new-state
    = make(state.object-class,
           frame-size: function.frame-size, closure: function);
  let signature = ^function-signature(function);
  let number-required = signature.^signature-number-required;
  let number-arguments = arguments.size;
  for (index from 0 below number-required,
       argument in arguments,
       variable in function.parameters)
    fetch(new-state, variable) := argument;
  end for;
  if (^signature-optionals?(signature))
    let rest = make(<vector>, size: number-arguments - number-required);
    for (i from number-required below number-arguments)
      rest[i - number-required] := arguments[i];
    end for;
    fetch(new-state, function.parameters[number-required]) := run-stage(rest);
    if (^signature-key?(signature))
      // *optional-arguments*.size := function.parameters.size;
      let optional-arguments = make(<stretchy-vector>, size: function.parameters.size);
      let key-specs = function.keyword-specifiers;
      // fill in defaults
      for (j from 1 by 2,
           i from number-required + 1 below function.parameters.size)
        optional-arguments[i] := key-specs[j];
      end for;
      process-keyword-arguments-into
        (optional-arguments, function, arguments);
      for (i from number-required + 1 below function.parameters.size)
        fetch(new-state, function.parameters[i]) := optional-arguments[i];
      end for;
    end if;
  end if;
  execute(new-state, function.body);
end method;

define method execute-call-using-function
    (state :: <machine-state>, function :: <&code>, c :: <simple-call>)
  // *arguments*.size := c.arguments.size;
  let new-arguments = make(<vector>, size: c.arguments.size);
  execute-call-using-function-and-arguments
    (state, function,
     map-into(new-arguments, curry(fetch, state), c.arguments));
end method;

define method execute-call-using-function
    (state :: <machine-state>, function :: <&code>, c :: <apply>)
  // *arguments*.size := 0;
  let new-arguments = make(<stretchy-vector>);
  for (argument in c.arguments,
       index from 0 below c.arguments.size - 1)
    add!(new-arguments, fetch(state, argument));
  end for;
  concatenate!(new-arguments, compile-stage(fetch(state, c.arguments.last)));
  execute-call-using-function-and-arguments(state, function, new-arguments);
end method;

define method execute-call-using-function
    (state :: <machine-state>, function :: <&lambda>, c :: <function-call>)
  execute-call-using-function(state, function.xep, c)
end method;

define method execute (state :: <machine-state>, c :: <stack-vector>)
  fetch(state, c.temporary)
    := run-stage
         (map-as(<vector>, compose(compile-stage, curry(fetch, state)),
                 c.arguments));
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <loop-call>)
  execute(state, c.loop-call-loop);
end method;

define constant %false = #f;

define method execute (state :: <machine-state>, c :: <if>)
  if (^raw-object-value(fetch(state, c.test)) == %false)
    execute(state, c.alternative)
  else
    execute(state, c.consequent)
  end if;
end method;

define method execute (state :: <machine-state>, c :: <nop-computation>)
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <return>)
  fetch(state, c.computation-value)
end method;

define method execute (state :: <machine-state>, c :: <unwind-protect>)
  block ()
    execute(state, c.body);
  cleanup
    execute(state, c.cleanups);
  end block;
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <bind-exit>)
  block (return)
    fetch(state, c.entry-state)
      := method (exit-value)
           fetch(state, c.temporary) := compile-stage(exit-value);
           return()
         end method;
    execute(state, c.body);
  end block;
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <exit>)
  let return = fetch(state, c.entry-state);
  return(fetch(state, c.computation-value))
end method;

define method execute (state :: <machine-state>, c :: <end-exit-block>)
  // terminate interpreter thread
  #"bogus-value-from-execute-<end-exit-block>"
end method;

define method execute (state :: <machine-state>, c :: <end-protected-block>)
  // terminate interpreter thread
  #"bogus-value-from-execute-<end-protected-block>"
end method;

define method execute (state :: <machine-state>, c :: <end-cleanup-block>)
  // terminate interpreter thread
  #"bogus-value-from-execute-<end-cleanup-block>"
end method;

/// multiple values

define method execute (state :: <machine-state>, c :: <values>)
  fetch(state, c.temporary)
    := begin
         let fixed = map-as(<simple-object-vector>,
                            curry(fetch, state), c.fixed-values);
         if (c.rest-value)
           concatenate(fixed, fetch(state, c.rest-value))
         else
           fixed
         end if
       end;
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <extract-single-value>)
  let mv = fetch(state, c.computation-value);
  fetch(state, c.temporary) := element(mv, c.index, default: %false);
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>, c :: <extract-rest-value>)
  let mv = fetch(state, c.computation-value);
  fetch(state, c.temporary)
    := run-stage(if (c.index > mv.size)
                   #[]
                 else
                   copy-sequence(mv, start: c.index)
                 end if);
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>,
                       c :: <adjust-multiple-values>)
  let mv = fetch(state, c.computation-value);
  let count = size(mv);
  let n = number-of-required-values(c);
  fetch(state, c.temporary) :=
    if (count = n)
      mv
    elseif (count > n)
      copy-sequence(mv, end: n)
    else
      replace-subsequence!(make(<simple-object-vector>,
                                size: n,
                                fill: #f),
                           mv,
                           end: count)
    end;
  execute(state, c.next-computation);
end method;

define method execute (state :: <machine-state>,
                       c :: <adjust-multiple-values-rest>)
  let mv = fetch(state, c.computation-value);
  let count = size(mv);
  let n = number-of-required-values(c);
  fetch(state, c.temporary) :=
    if (count >= n)
      mv
    else
      replace-subsequence!(make(<simple-object-vector>,
                                size: n,
                                fill: #f),
                           mv,
                           end: count)
    end;
  execute(state, c.next-computation);
end method;

/// types

define method execute (state :: <machine-state>, c :: <check-type>)
  // TODO: check the type!!!
  next-method();                // do the temporary transfer
end method;

define method execute (state :: <machine-state>, c :: <multiple-value-check-type>)
  // TODO: check the types!!!
  next-method();                // do the temporary transfer
end method;

define method execute (state :: <machine-state>, c :: <multiple-value-check-type-rest>)
  // TODO: check the types!!!
  next-method();                // do the temporary transfer
end method;

/// cell for assignment

define class <xcell> (<object>)
  slot cell-value, init-keyword: value:;
end class <xcell>;

define method execute (state :: <machine-state>, c :: <make-cell>)
  fetch(state, c.temporary)
    := make(<xcell>, value: fetch(state, c.computation-value));
  execute(state, c.next-computation)
end method execute;

define method execute (state :: <machine-state>, c :: <get-cell-value>)
  fetch(state, c.temporary) := fetch(state, c.computation-cell).cell-value;
  execute(state, c.next-computation)
end method execute;

define method execute (state :: <machine-state>, c :: <set-cell-value!>)
  fetch(state, c.temporary)
    := (fetch(state, c.computation-cell).cell-value
          := fetch(state, c.computation-value));
  execute(state, c.next-computation)
end method execute;

//// PUBLIC INTERFACE

define method eval-using-class
    (class :: subclass(<machine-state>), lambda :: <&lambda>)
  let state
    = make(class,
           frame-size: lambda.environment.frame-size, closure: lambda);
  apply(values, execute(state, lambda.body))
end method eval-using-class;

define compiler-sideways method eval (lambda :: <&method>)
  let number-required = ^signature-number-required(^function-signature(lambda));
  if (number-required ~= 0)
    error("Can only eval 0 argument methods - %= requires %= arguments.",
          lambda, number-required);
  end;
  eval-using-class(<machine-state>, lambda)
end method;


//// TIME INDEPENDENT EVAL

// This variation on the evaluation engine is used for evaluation
// when we only want a result if the expression is guaranteed to
// always return that result.
//
// This routine needs to be able to run on code in nested environments
// without having fully converted the surrounding environments.  That
// probably requires more fixes in fetch.

define class <constant-eval-machine-state> (<machine-state>)
end class <constant-eval-machine-state>;

define compiler-sideways method constant-eval (lambda :: <&lambda>)
  eval-using-class(<constant-eval-machine-state>, lambda)
end method constant-eval;

define constant $unknown-non-constant
  = make(<unknown>, name: #"non-constant value");

define method unchecked-fetch
    (state :: <constant-eval-machine-state>, binding :: <module-binding>)
  $unknown-non-constant
end method unchecked-fetch;

define method fetch
    (state :: <constant-eval-machine-state>, binding :: <module-binding>)
  $unknown-non-constant
end method fetch;

define method execute
    (state :: <constant-eval-machine-state>, c :: <primitive-call>)
  fetch(state, c.temporary) := $unknown-non-constant;
  // TODO: model this accurately
  /* if (c.primitive.side-effect-free? & c.primitive.state-independent?)
       next-method()
     else
       unknown
     end if; */
  execute(state, c.next-computation);
end method execute;


/// execution engine patches to test multiple value spilling

define constant *multiple-value-area* = make(<stretchy-vector>);

define class <mv-test-state> (<machine-state>)
end class <mv-test-state>;

define method fetch
    (the-state :: <mv-test-state>, object :: <multiple-value-temporary>)
  as(<vector>, *multiple-value-area*)
end method fetch;

define method fetch-setter
    (new-value, the-state :: <mv-test-state>,
     object :: <multiple-value-temporary>)
  unless (new-value == *multiple-value-area*)
    *multiple-value-area*.size := new-value.size;
    for (i from 0, e in new-value)
      *multiple-value-area*[i] := e;
    end for;
  end unless;
  *multiple-value-area*
end method fetch-setter;
