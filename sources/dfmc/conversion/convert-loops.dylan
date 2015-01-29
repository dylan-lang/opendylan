module:  dfmc-conversion
culprit: mitchell
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// At the moment we only compile for loops "by hand".  Other loops are
// currently defined via tail-recursive methods.
// TODO: investigate whether it is worth explicitly generating DFM for
// the other loops.

// We generate essentially the same DFM as the previous macros-based approach
// produced after optimization, although hopefully much more quickly.

//
// AST for for loops
//

define constant <for-clauses> = limited(<vector>, of: <for-clause>);

define class <for-statement> (<object>)
  constant slot for-clauses :: <for-clauses>,
    required-init-keyword: clauses:;
  constant slot end-clause :: false-or(<end-clause>),
    required-init-keyword: end-clause:;
  constant slot for-body, required-init-keyword: body:;
  constant slot for-finally, required-init-keyword: finally:;
end;

define class <for-clause> (<object>)
  constant slot bound-variable, required-init-keyword: bv:;
end;

define constant $empty-for-clause = make(<for-clause>, bv: #f);

define class <explicit-clause>(<for-clause>)
  constant slot init-value, required-init-keyword: value:;
  constant slot next-value, required-init-keyword: next:;
end;

define class <numeric-clause>(<for-clause>)
  constant slot start-value, required-init-keyword: from:;
  constant slot bound-value, required-init-keyword: bound:;
  constant slot bound-direction :: one-of(#"none", #"to", #"above", #"below"),
    required-init-keyword: direction:;
  constant slot increment-value, required-init-keyword: by:;
end;

define class <collection-clause>(<for-clause>)
  constant slot collection-value, required-init-keyword: value:;
  constant slot collection-keyed-by, required-init-keyword: keyed-by:;
  constant slot collection-set-by, required-init-keyword: set-by:;
  constant slot collection-using, required-init-keyword: using:;
end;

define class <end-clause>(<object>)
  constant slot end-clause-kind :: one-of(#"until", #"while"),
    required-init-keyword: kind:;
  constant slot end-test, required-init-keyword: test:;
end;


//
// Converter for for loops, which generates AST and then converts it to DFM
//

define &converter \for
  { \for (?header) ?fbody end }
    => convert(env, context, build-for-statement(header, fbody));
fbody:
  { ?main:body } => pair(main, #{ #f });
  { ?main:body \finally ?val:body } => pair(main, val);
header:
  { }
    => #();
  { ?v:variable ?keyset in ?e1:expression ?using, ?header:* }
    => pair(make(<collection-clause>,
                 bv: v, value: e1, keyed-by: keyset.head, set-by: keyset.tail,
                 using: using),
            header);
  { ?v:variable = ?e1:expression then ?e2:expression, ?header:* }
    => pair(make(<explicit-clause>, bv: v, value: e1, next: e2),
            header);
  { ?v:variable from ?e1:expression ?to, ?header:* }
    => pair(make(<numeric-clause>,
                 bv: v, from: e1, direction: to[0], bound: to[1], by: to[2]),
            header);
  { while: ?test:expression } => list(pair(#"while", test));
  { until: ?test:expression } => list(pair(#"until", test));
using:
  { } => #{ forward-iteration-protocol };
  { using ?protocol:expression } => protocol;
keyset:
  { } => pair(#f, #f);
  { keyed-by ?kv:variable set-by ?sv:variable } => pair(kv,sv);
  { set-by ?sv:variable keyed-by ?kv:variable } => pair(kv,sv);
  { keyed-by ?kv:variable } => pair(kv,#f);
  { set-by ?sv:variable } => pair(#f,sv);
to:
  { to ?limit:expression ?by }    => vector(#"to", limit, by);
  { above ?limit:expression ?by } => vector(#"above", limit, by);
  { below ?limit:expression ?by } => vector(#"below", limit, by);
  { ?by } => vector(#"none", #f, by);
by:
  { } => #{ 1 }
  { by ?step:expression } => step
end &converter;

define function build-for-statement(header, fbody)
  if (empty?(header))
    fbody.tail
  else
    let hl = header.last;
    let (for-clauses, end-clause) =
      if (instance?(hl, <pair>))
        let hs = header.size - 1;
        let v = make(<for-clauses>, size: hs, fill: $empty-for-clause);
        for (e in header, i from 0 below hs) v[i] := e end;
        let end-clause = make(<end-clause>, kind: hl.head, test: hl.tail);
        values(v, end-clause)
      else
        values(as(<for-clauses>, header), #f)
      end;

    make(<for-statement>, clauses: for-clauses, end-clause: end-clause,
          body: fbody.head, finally: fbody.tail)
  end
end;



//
// Utility definitions.
//

// Utility functions that could be moved to flow-graph/utilities?

/*
define function join-1+!(comp, #rest args)
  let sz = args.size;
  iterate loop (first = comp, last = comp, index = 0)
    if (index >= sz) values(first, last)
    else
      let (first, last) =
        join-2x1!(first, last, args[index]);
      loop(first, last, index + 1)
    end;
  end;
end;
*/

define function join-2+!(comp-first, comp-last, #rest args)
  let sz = args.size;
  iterate loop (first = comp-first, last = comp-last, index = 0)
    if (index >= sz) values(first, last)
    else
      let (first, last) =
        join-2x2!(first, last, args[index], args[index + 1]);
      loop(first, last, index + 2)
    end;
  end;
end;


// TODO: modify conversion/convert to use this function when processing
// conditionals to avoid duplication.

define function generate-if
  (env :: <environment>, test-temp,
   then-first, then-last, then-temp, else-first, else-last, else-temp)
    => (first :: false-or(<computation>), last :: false-or(<computation>),
        ref :: <value-reference>);

  let if-c = make-in-environment
               (env, <if>, test: test-temp,
                consequent: then-first, alternative: else-first);

  let then-last = then-last | if-c;
  let else-last = else-last | if-c;

  let (merge, temporary) =
    make-with-temporary
      (env, <if-merge>, previous-computation: if-c,
       left-previous-computation:  then-last, left-value:  then-temp,
       right-previous-computation: else-last, right-value: else-temp);

  if (then-first)
    previous-computation(then-first) := if-c;
  else
    consequent(if-c) := merge;
  end if;
  if (else-first)
    previous-computation(else-first) := if-c;
  else
    alternative(if-c) := merge;
  end if;

  next-computation(if-c)      := merge;
  next-computation(then-last) := merge;
  next-computation(else-last) := merge;

  values(if-c, merge, temporary)
end;


// To enable us to embed temporaries etc in fragments we need to
// extend convert-object reference.

define method convert-object-reference
  (env :: <environment>, context :: <value-context>,
   object :: <value-reference>)
    => (first :: false-or(<computation>), last :: false-or(<computation>),
        ref :: <value-reference>)
  values(#f, #f, object)
end;


//
// Iteration states
//

// When processing a for loop we maintain a state record for each iteration
// variable.

// TODO: tighten up types, and return types on methods.

define class <for-bv-state>(<object>)
  constant slot for-bv-clause :: <for-clause>, required-init-keyword: clause:;
  slot for-bv-spec;
  slot for-bv-type-temp :: false-or(<value-reference>) = #f;
  slot for-bv-variable;
  slot for-bv-current-temp :: false-or(<value-reference>);
end;

define class <explicit-bv-state>(<for-bv-state>)
//  slot for-bv-init-temp :: false-or(<value-reference>);
  slot for-bv-next;
end;

define class <numeric-bv-state>(<for-bv-state>)
//  slot for-bv-start-temp :: false-or(<value-reference>);
  slot for-bv-bound-temp :: false-or(<value-reference>);
  slot for-bv-increment-temp :: false-or(<value-reference>);
  slot for-bv-to-direction-temp :: false-or(<value-reference>);
  slot for-bv-non-negative-bound? :: <boolean> = #f;
end;

define class <collection-bv-state>(<for-bv-state>)
  slot for-bv-collection-temp :: false-or(<value-reference>);

  slot for-bv-initial-state-temp :: false-or(<value-reference>);
  slot for-bv-limit-temp :: false-or(<value-reference>);
  slot for-bv-next-state-temp :: false-or(<value-reference>);
  slot for-bv-finished-state?-temp :: false-or(<value-reference>);
  slot for-bv-current-key-temp :: false-or(<value-reference>);
  slot for-bv-current-element-temp :: false-or(<value-reference>);
  slot for-bv-current-element-setter-temp :: false-or(<value-reference>);
end;


// STEP 1
//
// Execute the expressions that are executed just once, in left to right
// order as they appear in the for statement.  These expressions include
// the types of all the bindings, and the expressions init-value,
// collection, start, bound, and increment.  If the value of collection
// is not a collection, an error is signaled. The default value for
// increment is 1.

define function step-1
    (env :: <environment>, object :: <for-statement>)
        => (first :: false-or(<computation>), last :: false-or(<computation>),
            states);

  let number-of-bvs = object.for-clauses.size;
  let states = make(<vector>, size: number-of-bvs);
  let acc-first = #f; let acc-last = #f;

  for (fc in object.for-clauses, i from 0)
    let (init-first, init-last, state, start) = generate-step-1(fc, env);
    states[i] := state;

    let specs = parse-value-bindings(fc.bound-variable);
    let spec = (specs.spec-value-required-variable-specs)[0];
    let type-expression = spec-type-expression(spec);
    let (first, last, type)
      = if (state.for-bv-type-temp)
          values(init-first, init-last, state.for-bv-type-temp)
        else
          let (first, last, type) =
            convert-type-expression(env, type-expression);
          state.for-bv-type-temp := type;
          join-2x2-t!(first, last, init-first, init-last, type);
        end if;

    state.for-bv-spec := spec;

    state.for-bv-current-temp := start;

    let (first, last) = join-2x2!(acc-first, acc-last, first, last);
    acc-first := first; acc-last := last;
  end;

  values(acc-first, acc-last, states)
end;


define method generate-step-1(ec :: <explicit-clause>, env :: <environment>)
  let state = make(<explicit-bv-state>, clause: ec);

  let (first, last, initial) = convert(env, $single, ec.init-value);
//  state.for-bv-init-temp := initial;
  state.for-bv-next := ec.next-value;

  values(first, last, state, initial);
end;


define method generate-step-1(nc :: <numeric-clause>, env :: <environment>)
  let state = make(<numeric-bv-state>, clause: nc);

  let (c-f, c-l, start) = convert(env, $single, nc.start-value);
//  state.for-bv-start-temp := start;

  let (c-f, c-l) =
    if (nc.bound-value)
      let (b-f, b-l, bound) = convert(env, $single, nc.bound-value);
      state.for-bv-bound-temp := bound;
      join-2x2!(c-f, c-l, b-f, b-l);
    else
      values(c-f, c-l)
    end;

  let (i-f, i-l, increment) = convert(env, $single, nc.increment-value);
  state.for-bv-increment-temp := increment;
  let (c-f, c-l) = join-2x2!(c-f, c-l, i-f, i-l);

  let (incr-constant?, incr-value) = constant-value?(increment);
  let (n-f, n-l, downwards?) =
    if (incr-constant? & instance?(incr-value, <integer>))
      let downwards? = (incr-value < 0);
      let (n-f, n-l, downwards?-tmp) =
        convert(env, $single, if (downwards?) #{ #t } else #{ #f } end);
      let (start-constant?, start-value) = constant-value?(start);
      if (start-constant? & instance?(start-value, <integer>))
        let (min, max) =
          if (downwards?)
            values(#f, start-value)
          else
            state.for-bv-non-negative-bound? := #f; // (start-value >= 0);
            values(start-value, #f);
          end if;
        let (first, last, type) =
          convert-type-expression
            (env,
             #{ <integer> }
             // #{ limited(<integer>, min: ?min, max: ?max) }
             );
        state.for-bv-type-temp := type;
        join-2x2-t!(n-f, n-l, first, last, downwards?-tmp);
      else
        values(n-f, n-l, downwards?-tmp)
      end if;
    else
      convert(env, $single, #{ ?increment < 0 });
    end;
  state.for-bv-to-direction-temp := downwards?;
  let (c-f, c-l) = join-2x2!(c-f, c-l, n-f, n-l);


  values(c-f, c-l, state, start);
end;

define constant $fip-setters =
  vector(for-bv-initial-state-temp-setter, for-bv-limit-temp-setter,
         for-bv-next-state-temp-setter,    for-bv-finished-state?-temp-setter,
         for-bv-current-key-temp-setter,   for-bv-current-element-temp-setter,
         for-bv-current-element-setter-temp-setter);

define method generate-step-1(cc :: <collection-clause>, env :: <environment>)
  let state = make(<collection-bv-state>, clause: cc);

  let (c-f, c-l, collection) = convert(env, $single, cc.collection-value);
  state.for-bv-collection-temp := collection;

  let using = cc.collection-using;

  // TODO: COULD BE MORE PRECISE IN VALUE-CONTEXT HERE
  let (fip-f,fip-l,fip) = convert(env, $all-rest, #{ ?using(?collection) });
  let (c-f, c-l) = join-2x2!(c-f, c-l, fip-f, fip-l);

  for (i from 0, setter in $fip-setters)
    let (comp, esv-t) =
      make-with-temporary(env, <extract-single-value>, value: fip, index: i);
    setter(esv-t, state);
    join-2x1!(c-f, c-l, comp);  c-l := comp;
  end;

  values(c-f, c-l, state, state.for-bv-initial-state-temp);
end;



// STEP 2
//
// Create the iteration bindings of explicit step and numeric clauses.
//
//   For each explicit step clause, create the binding for the value of
//   init-value.  If the binding is typed and the value is not of the
//   specified type, signal an error.
//
//   For each numeric clause, create the binding for the value of start.
//   If the binding is typed and the value is not of the specified type,
//   signal an error.

define function step-2(env :: <environment>, states)
  let number-of-bvs = states.size;

  let first = #f; let last = #f;

  let new-env =
    reduce(
      method(old-env, state :: <for-bv-state>)
        let (new-env, comp) = generate-step-2(old-env, state);
        let (f, l) = join-2x1!(first, last, comp); first := f; last := l;
        new-env
      end, env, states);

  let initials =
    collecting (as <simple-object-vector>)
      for (i from 0 below size(states))
        collect(states[i].for-bv-current-temp);
      end for;
    end collecting;

  let merges = make(<simple-object-vector>, size: number-of-bvs);
  let loop-c = make-in-environment(env, <loop>, merges: merges);
  let (first, last) = join-2x1!(first, last, loop-c);

  let body-f = #f; let body-l = #f;
  for (i from 0 below number-of-bvs)
    let state = states[i];
    let loop-merge-c =
      make-in-environment(
        new-env, <loop-merge>,
        loop: loop-c, parameter: initials[i],
        argument: state.for-bv-variable,
        temporary: state.for-bv-variable);  // need to set prev comps later

    state.for-bv-current-temp := state.for-bv-variable;
    state.for-bv-variable.generator := loop-merge-c;
    let (f, l) = join-2x1!(body-f, body-l, loop-merge-c);
    body-f := f; body-l := l;
    merges[i] := loop-merge-c;
  end;

  values(first, last, new-env, loop-c, body-f, body-l);
end;


define method generate-step-2(env :: <environment>, state :: <for-bv-state>)
  let (new-env, variable) =
    bind-local-variable(env,
      spec-variable-name(state.for-bv-spec), state.for-bv-type-temp);
  state.for-bv-variable := variable;

  let (check-c, check-t) =
    make-with-temporary(env, <check-type>,
      value: state.for-bv-current-temp, type: state.for-bv-type-temp);
  state.for-bv-current-temp := check-t;

  values(new-env, check-c)
end;

define method generate-step-2(env :: <environment>, state :: <collection-bv-state>)
  state.for-bv-variable :=
    make(<temporary>, environment: lambda-environment(env));
  values(env, #f)
end;



// STEP 3
//
// Check numeric and collection clauses for exhaustion.  If a clause is
// exhausted, go to step 9.
//
//   A collection clause is exhausted if its collection has no next
//   element.
//
//   A numeric clause is exhausted if a bound is supplied and the value of
//   the clause is no longer in bounds.  If above is specified, the clause
//   will be in bounds as long as the value is greater than the bounds.  If
//   below is specified, the clause will be in bounds as long as the value
//   is less than the bounds.  If to is specified with a positive or zero
//   increment, the clause will be in bounds as long as it is less than or
//   equal to the bounds.  If to is specified with a negative increment,
//   the clause will be in bounds as long as it is greater than or equal to
//   the bounds.


define function step-3(env :: <environment>, states)
  let number-of-bvs = states.size;

  local method generate(i)
    let (test-f, test-l, test) = generate-step-3(env, states[i]);

    if (i = number-of-bvs - 1)
      values(test-f, test-l, test)
    else
      let (else-f, else-l, else-t) = generate(i + 1);
      let (constant?, value) = constant-value?(else-t);
      if (constant? & value == #f)
        if (else-f) remove-computation-block-references!(else-f, #f) end;
        values(test-f, test-l, test)
      else
        let (then-f, then-l, then-t) = convert(env, $single, #{ #t });

        let (if-f, if-l, if-t) =
          generate-if(env, test,
                      then-f, then-l, then-t, else-f, else-l, else-t);
        let (first, last) = join-2x2!(test-f, test-l, if-f, if-l);
        values(first, last, if-t);
      end
    end
  end;

  if (number-of-bvs > 0)
    generate(0)
  else
    convert(env, $single, #{ #f });
  end;
end;


define method generate-step-3(env :: <environment>, state :: <explicit-bv-state>)
  convert(env, $single, #{ #f });
end;

define method generate-step-3(env :: <environment>, state :: <numeric-bv-state>)
  // Returns #t if exhausted.
  let nc :: <numeric-clause> = state.for-bv-clause;
  if (nc.bound-value)
    let index = state.for-bv-current-temp;
    let bound = state.for-bv-bound-temp;

    let (comp-f, comp-l, temp) =
      select (nc.bound-direction)
        #"below" =>
          convert(env, $single,
                  if (state.for-bv-non-negative-bound?)
                    #{ element-range-check(?index, ?bound) }
                  else
                    #{ ?index < ?bound }
                  end if);
        #"above" => convert(env, $single, #{ ?bound < ?index });
        #"to" =>
          let (then-first, then-last, then-t) =
            convert(env, $single, #{ ?index < ?bound });
          let (else-first, else-last, else-t) =
            convert(env, $single, #{ ?bound < ?index });
          generate-if(env, state.for-bv-to-direction-temp,
                      then-first, then-last, then-t,
                      else-first, else-last, else-t)
      end;

    if (nc.bound-direction == #"to")
      values(comp-f, comp-l, temp)
    else
      // Need to negate the result.  Why is there no primitive for this?
      let (neg-f, neg-l, temp) =
        convert(env, $single, #{ primitive-id?(?temp, #f) });

      let (first, last) = join-2x2!(comp-f, comp-l, neg-f, neg-l);
      values(first, last, temp);
    end

  else
    convert(env, $single, #{ #f });
  end;
end;

define method generate-step-3(env :: <environment>, state :: <collection-bv-state>)

  let finished? = state.for-bv-finished-state?-temp;
  let collection = state.for-bv-collection-temp;
  let current-state = state.for-bv-current-temp;
  let limit = state.for-bv-limit-temp;

  convert(env, $single, #{ ?finished?(?collection, ?current-state, ?limit) })
end;



// STEP 4
//
// For each collection clause create the iteration binding for the next
// element of the collection for that clause.  Fresh bindings are created
// each time through the loop (i.e., the binding is not assigned the new
// value).  If the binding is typed and the value is not of the specified
// type, signal an error.


define function step-4(env :: <environment>, states)
  let first = #f;  let last = #f;
  let new-env = env;

  for (state in states)
    if (instance?(state, <collection-bv-state>))
      let (new-env+variable, variable) =
        bind-local-variable(new-env,
                            spec-variable-name(state.for-bv-spec),
                            state.for-bv-type-temp);
      new-env := new-env+variable;
      state.for-bv-variable := variable;

      let collection = state.for-bv-collection-temp;
      let iteration-state = state.for-bv-current-temp;

      let (comp, temp) =
        make-with-temporary(new-env, <simple-call>,
          function: state.for-bv-current-element-temp,
          arguments: vector(collection, iteration-state));
      let (f, l) = join-2x1!(first, last, comp); first := f;  last := l;

      let comp =
        make-in-environment(new-env, <check-type>,
          value: temp, type: state.for-bv-type-temp, temporary: variable);
      variable.generator := comp;
      let (f, l) = join-2x1!(first, last, comp); first := f;  last := l;

      let clause = state.for-bv-clause;

      if (clause.collection-keyed-by)
        let specs = parse-value-bindings(clause.collection-keyed-by);
        let spec = (specs.spec-value-required-variable-specs)[0];

        let (new-env+variable, variable) =
          bind-local-variable(new-env, spec-variable-name(spec), #f);
        new-env := new-env+variable;

        let comp =
          make-in-environment(new-env, <simple-call>,
            function: state.for-bv-current-key-temp, temporary: variable,
            arguments: vector(collection, iteration-state));
        variable.generator := comp;
        let (f, l) = join-2x1!(first, last, comp); first := f;  last := l;
      end;

      if (clause.collection-set-by)
        let specs = parse-value-bindings(clause.collection-set-by);
        let spec  = (specs.spec-value-required-variable-specs)[0];

        let (new-env+variable, variable) =
          bind-local-variable(new-env, spec-variable-name(spec), #f);
        new-env := new-env+variable;

        let (ignore-1, ignore-2, current-element-setter) =
          convert-reference(env, $single,
            dylan-value(#"%curry-current-element-setter"));
        let setter = state.for-bv-current-element-setter-temp;
        let comp =
          make-in-environment(new-env, <simple-call>,
            function: current-element-setter, temporary: variable,
            arguments: vector(collection, iteration-state, setter));
        variable.generator := comp;
        let (f, l) = join-2x1!(first, last, comp); first := f;  last := l;
      end;
    end;
  end;

  values(new-env, first, last)
end;


// STEP 5
//
// If end-test is supplied, execute it.  If the value of end-test is
// false and the symbol is while:, go to step 9.  If the value of
// end-test is true and the symbol is until:, go to step 9.

define function step-5(env :: <environment>, object, s678-first, s678-last, s8-temp)
  if (object.end-clause)
    let (test-first, test-last, test-temp) =
      convert(env, $single, object.end-clause.end-test);

    let (then-first_, then-last_, then-temp_) =
      convert(env, $single, #{ #f });

    let (if-first, if-last, temp) =
      if (object.end-clause.end-clause-kind == #"until")
        generate-if(env, test-temp,
                    then-first_, then-last_, then-temp_,
                    s678-first, s678-last, s8-temp);
      else
        generate-if(env, test-temp,
                    s678-first, s678-last, s8-temp,
                    then-first_, then-last_, then-temp_);
      end;

    let (s5678-first, s5678-last) =
      join-2x2!(test-first, test-last, if-first, if-last);

    values(s5678-first, s5678-last, temp)
  else
    values(s678-first, s678-last, s8-temp)
  end;
end;


// STEP 6
//
// Execute the expressions in the body in order.  The expressions in the
// body are used to produce side-effects.


// STEP 7
//
// Obtain the next values for explicit step and numeric clauses.  Values
// are obtained in left to right order, in the environment produced by
// step 6.
//
//   For each explicit step clause, execute next-value.
//
//   For each numeric clause, add the increment to the current value of the
//   binding, using +.

define method step-7(env :: <environment>, object, states)
  let first = #f;  let last = #f;

  for (fc in object.for-clauses, i from 0)
    let (f, l) = generate-step-7(env, states[i]);
    let (f, l) = join-2x2!(first, last, f, l);  first := f; last := l;
  end;

  values(first, last)
end;


define method generate-step-7(env :: <environment>, state :: <explicit-bv-state>)
  let (first, last, temp) = convert(env, $single, state.for-bv-next);

  let (check, temp) =
    make-with-temporary(env, <check-type>,
      value: temp, type: state.for-bv-type-temp);
  state.for-bv-current-temp := temp;
  join-2x1!(first, last, check)
end;


define method generate-step-7(env :: <environment>, state :: <numeric-bv-state>)
  let index = state.for-bv-current-temp;
  let increment = state.for-bv-increment-temp;
  let (first, last, temp) = convert(env, $single, #{ ?index + ?increment });

  let (check, temp) =
    make-with-temporary(env, <check-type>,
                        value: temp, type: state.for-bv-type-temp);
  state.for-bv-current-temp := temp;
  join-2x1!(first, last, check)
end;


define method generate-step-7(env :: <environment>, state :: <collection-bv-state>)
  let (comp, temp) = make-with-temporary(env, <simple-call>,
    function: state.for-bv-next-state-temp,
    arguments: vector(state.for-bv-collection-temp,
                      state.for-bv-current-temp));
  state.for-bv-current-temp := temp;
  values(comp, comp)
end;


// STEP 8
//
// Create the iteration bindings of explicit step and numeric clauses for
// the values obtained in step 7.  For each clause, if a binding type is
// supplied and the next value for that clause is not of the specified
// type, signal an error.  Fresh bindings are created each time through
// the loop (i.e., the binding is not assigned the new value).  After the
// bindings have been created, go to step 3.


define function step-8(env :: <environment>, states, loop-comp)
  let (comp, loop-temp)
    = make-with-temporary
        (env, <loop-call>, loop: loop-comp,
         merges: loop-merges(loop-comp));

  for (merge in loop-merges(loop-comp), i :: <integer> from 0)
    loop-merge-call(merge) := comp;
    replace-temporary-references!
      (merge, loop-merge-argument(merge),
       states[i].for-bv-current-temp);
  end for;

  values(comp, comp, loop-temp)
end;


// STEP 9
//
// Execute the expressions in the result-body in order.  Bindings created
// in step 2 and 8 are visible during the execution of result-body, but
// bindings created in step 4 ( the iteration bindings of collection
// clauses) are not visible during the execution of result-body.  The
// values of the last expression in the result-body are returned as the
// values of the for statement.  If there are no expressions in the
// result-body, for returns #f.

define function step-9(env :: <environment>, context, object, temp)
  if (object.for-finally)
    convert(env, context, object.for-finally);
  else
    match-values-with-context(env, context, #f, #f, temp)
  end
end;


// CONVERT

define method convert
    (env :: <environment>,
     context :: <value-context>,
     object :: <for-statement>)
        => (first :: false-or(<computation>),
            last :: false-or(<computation>),
            ref :: false-or(<value-reference>));

  let (s1-f, s1-l, states) = step-1(env, object);
  let (s2-f, s2-l, s2-env, loop-comp, body-f, body-l) = step-2(env, states);
  let (s3-f, s3-l, s3-t) = step-3(s2-env, states);
  let (s24-env, s4-f, s4-l) = step-4(s2-env, states);
  let (s6-f, s6-l, s6-t) = convert(s24-env, $ignore, object.for-body);
  let (s7-f, s7-l) = step-7(s24-env, object, states);
  let (s8-f, s8-l, s8-t) = step-8(s24-env, states, loop-comp);
  let (s678-f, s678-l) = join-2+!(s6-f, s6-l, s7-f, s7-l, s8-f, s8-l);

  let (then-f, then-l, then-t) = convert(s24-env, $single, #{ #f });
  let (s5678-f, s5678-l, else-t) =
    step-5(s24-env, object, s678-f, s678-l, s8-t);
  let (else-f, else-l) = join-2x2!(s4-f, s4-l, s5678-f, s5678-l);

  let (if-f, if-l, if-t) =
      generate-if(s24-env, s3-t,
                  then-f, then-l, then-t, else-f, else-l, else-t);

  let (s9-f, s9-l, s9-t) = step-9(s2-env, context, object, if-t);

  let end-c = make-in-environment(env, <end-loop>, loop: loop-comp);
  let (body-f, body-l) =
    join-2+!(body-f, body-l, s3-f, s3-l, if-f, if-l, s9-f, s9-l, end-c, end-c);

  loop-body(loop-comp) := body-f;
  previous-computation(body-f) := loop-comp;

  let (comp-f, comp-l) =
    join-2+!(s1-f, s1-l, s2-f, s2-l);

  values(comp-f, comp-l, s9-t)
end;

// WHILE & UNTIL

define &converter \while
  { \while (?wtest:expression) ?wbody:body end }
    => convert-loop(env, context, wtest, wbody, #t);
end &converter;

define &converter \until
  { \until (?utest:expression) ?ubody:body end }
    => convert-loop(env, context, utest, ubody, #f);
end &converter;

define method convert-loop
    (env :: <environment>,
     context :: <value-context>,
     test, body, while? :: <boolean>)
        => (first :: false-or(<computation>),
            last :: false-or(<computation>),
            ref :: false-or(<value-reference>));
  let loop-c
    = make-in-environment(env, <loop>, merges: #[]);

  let (body-f, body-l) = convert(env, $ignore, body);
  let (continue-c, continue-t)
    = make-with-temporary(env, <loop-call>, loop: loop-c, merges: #[]);
  let (body-f, body-l) = join-2x1!(body-f, body-l, continue-c);

  let (test-f, test-l, test-t) = convert(env, $single, test);

  let (if-f, if-l) =
    if (while?)
      generate-if(env, test-t, body-f, body-l, #f, #f, #f, #f);
    else
      generate-if(env, test-t, #f, #f, #f, body-f, body-l, #f);
    end;
  let (if+test-f, if+test-l) = join-2x2!(test-f, test-l, if-f, if-l);

  let end-c = make-in-environment(env, <end-loop>, loop: loop-c);
  let (body-f, body-l) = join-2x1!(if+test-f, if+test-l, end-c);
  loop-body(loop-c) := body-f;
  previous-computation(body-f) := loop-c;

  match-values-with-context(env, context, loop-c, loop-c, #f)
end convert-loop;
