Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Inference of the types in the typist.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// Minimal static type inference: recurse until you ground out on data.
///

///
/// A few tools.
///

// *** Dependency: on the return, or last instruction?
define function type-estimate-body(body-first :: <computation>,
                                   cache :: <type-cache>,
                                   #key before) => (te :: <type-estimate>)
  // Type the computations in a body (i.e., sequence of DFM instructions).
  // Goes up to the next ender, or before: if supplied.  The purpose of the
  // latter is to find an <end-exit-block>, which is not an <end>.
  walk-computations(rcurry(type-estimate-in-cache, cache), body-first, before);
  type-estimate-in-cache(case
                           before    => previous-computation(before);  // <end-exit-block>
                           otherwise => final-computation(body-first); // has an <end>
                         end,
                         cache)
end;

///
/// Machine to figure out the return value type(s) from a function type.
///

define generic function-valtype (fntype, cache :: <type-cache>)
 // Figure out values type for this function type.
 => (valtype :: <type-estimate>);

define method function-valtype (fntype :: <type-estimate-limited-function>,
                                cache :: <type-cache>)
   => (valtype :: <type-estimate>);
  // Easy to extract values type from a limited function
  type-estimate-values(fntype)
end;

define method function-valtype (fntype :: <type-estimate-limited-instance>,
                                cache  :: <type-cache>)
   => (valtype :: <type-estimate>);
  // Coerce singleton to limited function & try again.
  let fn = type-estimate-singleton(fntype);
  if (instance?(fn, <&callable-object>))
    // It's guaranteed to be some kind of function
    let (sig, cl, body) =
      select (fn by instance?)
        <&lambda>    => values(^function-signature(fn),
                               &object-class(fn),
                               // body(fn)
                               #f);
        <&code>      => values(^function-signature(function(fn)),
                               &object-class(fn),
                               // body(function(fn))
                               #f);
        <&function>  => values(^function-signature(fn),
                               &object-class(fn),
                               #f);
        <&primitive> => values(primitive-signature(fn),
                               &object-class(fn),
                               #f);
      end;
    function-valtype(type-estimate-function-from-signature(sig, cl, cache,
                                                           body: body),
                     cache)
  else
    // It's not a callable thing; you'll never return.
    make(<type-estimate-bottom>)
  end
end;

define method function-valtype (fntype :: <type-estimate-union>,
                                cache :: <type-cache>)
   => (valtype :: <type-estimate>);
  // Unions of function objects get processed recursively.
  make(<type-estimate-union>,
       unionees: map(rcurry(function-valtype, cache),
                     type-estimate-unionees(fntype)))
end;

define method function-valtype (fntype :: <object>, cache :: <type-cache>)
   => (valtype :: <type-estimate>);
  // Anything else just punts to the general case.  E.g., class <function>
  // goes here.  We could try to check for disjointness from <callable-object>,
  // but that happense in the rest of the compiler
  // (see optimization/dispatch.dylan).
  make(<type-estimate-values>)
end;

///
/// Manipulating values and unions of values.
///

define generic single-value?(vtype) => (single? :: <boolean>);

define method single-value?(vtype :: <type-estimate-values>)
    => (single? :: <boolean>)
  // Is this really only 1 return value?
  size(type-estimate-fixed-values(vtype)) = 1 &
       type-estimate-rest-values(vtype) == #f
end;

define method single-value?(vtype :: <type-estimate-union>)
 => (single? :: <boolean>)
  // Is this a union of single-value types?
  every?(single-value?, type-estimate-unionees(vtype))
end;

define generic first-value(vtype) => (first :: <type-estimate>);

define method first-value(vtype :: <type-estimate-values>)
 => (val :: <type-estimate>)
  // First value of a multiple value type.
  case
    size(type-estimate-fixed-values(vtype)) ~== 0
      => type-estimate-fixed-values(vtype)[0]; // First fixed value
    type-estimate-rest-values(vtype)
      => type-estimate-rest-values(vtype);     // No fixed value; use rest
    otherwise
      => make(<type-estimate-limited-instance>, singleton: &false)
  end
end;

define method first-value(vtype :: <type-estimate-union>)
 => (val :: <type-estimate>)
  // Union of first values of components.
  make(<type-estimate-union>,
       unionees: map-as(<unionee-sequence>,
                        first-value,
                        type-estimate-unionees(vtype)))
end;

///
/// How to type-estimate a call.
///

define inline function type-estimate-call-stupidly-from-fn
       (call :: <call>, fn, cache :: <type-cache>)
    => (te :: false-or(<type-estimate>))
  // Type of function-object.
  let fntype  = type-estimate-in-cache(fn, cache);
  function-valtype(fntype, cache); // Values type or union thereof.
end function;

define method type-estimate-call-from-site(call :: <function-call>,
                                           cache :: <type-cache>)
    => (te :: false-or(<type-estimate>))
  let (constant?, fn)  = constant-value?(call.function);
  if (instance?(fn, <&generic-function>))
    select (fn)
      dylan-value(#"make") =>
        let (c?, arg-type)
          = constant-value?(first(arguments(call)));
        let type
          = if (c? & ^instance?(arg-type, dylan-value(#"<type>")))
              arg-type
            else
              dylan-value(#"<object>")
            end if;
        make(<type-estimate-values>,
             fixed: vector(as(<type-estimate>, type)));
      dylan-value(#"element"), dylan-value(#"element-no-bounds-check") =>
        let collection-te = type-estimate(first(arguments(call)));
        if (instance?(collection-te, <type-estimate-limited-collection>))
          make(<type-estimate-values>,
               fixed: vector(as(<type-estimate>, type-estimate-of(collection-te))))
        else
          type-estimate-call-stupidly-from-fn(call, function(call), cache)
        end if;
      dylan-value(#"element-setter"), dylan-value(#"element-no-bounds-check-setter") =>
        // This might have fewer arguments than expected, like when rcurry() is involved.
        if (size(arguments(call)) > 1)
          let collection-te = type-estimate(second(arguments(call)));
          if (instance?(collection-te, <type-estimate-limited-collection>))
            make(<type-estimate-values>,
                 fixed: vector(as(<type-estimate>, type-estimate-of(collection-te))))
          else
            type-estimate-call-stupidly-from-fn(call, function(call), cache)
          end if;
        else
          type-estimate-call-stupidly-from-fn(call, function(call), cache)
        end if;
      otherwise =>
        type-estimate-call-stupidly-from-fn(call, function(call), cache)
    end select;
  else
    type-estimate-call-stupidly-from-fn(call, function(call), cache)
  end if;
end method;

define method ^make-return-class-from-signature
    (fn :: <&method>) => (res :: false-or(<&class>))
  let sig   = ^function-signature(fn);
  let rtype = first(^signature-required(sig));
  let vtype = first(^signature-values(sig));
  let class = select (rtype by instance?)
                <&singleton> => ^singleton-object(rtype);
                <&subclass>  => ^subclass-class(rtype);
                otherwise    => #f;
              end select;
  if (class & ~^subtype?(vtype, class))
    class
  else
    vtype
  end if
end method;

define method ^make-method? (fn :: <&method>) => (well? :: <boolean>)
  let binding = model-variable-binding(fn);
  binding & binding == dylan-binding(#"make")
end method;

define method ^make-method? (fn) => (well? :: <boolean>)
  #f
end method;

define method ^element-method? (fn :: <&method>) => (well? :: <boolean>)
  let binding = model-variable-binding(fn);
  binding
    & (binding == dylan-binding(#"element")
         | binding == dylan-binding(#"element-no-bounds-check"))
end method;

define method ^element-setter-method? (fn :: <&method>) => (well? :: <boolean>)
  let binding = model-variable-binding(fn);
  binding
    & (binding == dylan-binding(#"element-setter")
         | binding == dylan-binding(#"element-no-bounds-check-setter"))
end method;


// We'd much rather like to look at fn.parameters, but we don't
// always have it.  So re-compute formal argument order (incomplete,
// but sufficient for the only use case so far).
define function find-parameter-value (wanted-name, sig-spec, arguments)
 => (constant?, value);
  let index = 0;
  let arg-index =
    block(return)
      for (spec in sig-spec.spec-value-required-variable-specs)
        if (spec.spec-variable-name.fragment-name = wanted-name)
          return(index);
        end;
        index := index + 1;
      end;
      if (sig-spec.spec-argument-optionals?)
        index := index + 1;
      end;
      for (spec in sig-spec.spec-argument-key-variable-specs)
        if (spec.spec-keyword-expression.fragment-value = wanted-name)
          return(index);
        end;
        index := index + 1;
      end;
      #f
    end;
  if (arg-index)
    constant-value?(arguments[arg-index])
  else
    values(#f, #f)
  end
end;

define method type-estimate-call-from-site(call :: <method-call>,
                                           cache :: <type-cache>)
    => (te :: false-or(<type-estimate>))
  let (constant?, fn)  = constant-value?(call.function);
  local method get-parameter (name)
          let (c?, val) = find-parameter-value(name, signature-spec(fn), call.arguments);
          val
        end;
  if (instance?(fn, <&method>))
    if (^make-method?(fn))
      let sig-class
        = ^make-return-class-from-signature(fn);
      let (c?, arg-type)
        = constant-value?(first(arguments(call)));
      let type-estimate
        = if (c? & ^subtype?(arg-type, sig-class))
            if (^subtype?(arg-type, dylan-value(#"<collection>")))
              // try to improve type prediction for collections, esp. limited collections
              let (element-type, class) = lookup-any-limited-collection-element-type(arg-type);
              let element-type-parm = get-parameter(#"element-type");
              if (element-type-parm)
                element-type := element-type-parm;
              end;
              unless (element-type)
                element-type := dylan-value(#"<object>");
              end;
              let size = #f;
              let dimensions = #f;
              unless (^subtype?(arg-type, dylan-value(#"<stretchy-collection>")))
                let unsupplied = dylan-value(#"unsupplied-object");
                let ts = get-parameter(#"size");
                unless (ts == unsupplied)
                  size := ts;
                end;
                let td = get-parameter(#"dimensions");
                unless (td == unsupplied)
                  dimensions := td;
                end;
              end;
              if (element-type | size | dimensions)
                make(<type-estimate-limited-collection>,
                     class:          class | arg-type,
                     concrete-class: arg-type,
                     of:             as(<type-estimate>, element-type),
                     size:           size,
                     dimensions:     dimensions & as(limited(<vector>, of: <integer>), dimensions));
              else
                as(<type-estimate>, arg-type);
              end if
            else
              as(<type-estimate>, arg-type);
            end if
          else
            as(<type-estimate>, sig-class);
          end if;
      make(<type-estimate-values>,
           fixed: vector(type-estimate))
    elseif (^element-method?(fn))
      let collection-te = type-estimate(first(arguments(call)));
      if (instance?(collection-te, <type-estimate-limited-collection>))
        make(<type-estimate-values>,
             fixed: vector(as(<type-estimate>, type-estimate-of(collection-te))))
      else
        next-method()
      end if
    elseif (^element-setter-method?(fn))
      // This might have fewer arguments than expected, like when rcurry() is involved.
      if (size(arguments(call)) > 1)
        let collection-te = type-estimate(second(arguments(call)));
        if (instance?(collection-te, <type-estimate-limited-collection>))
          make(<type-estimate-values>,
               fixed: vector(as(<type-estimate>, type-estimate-of(collection-te))))
        else
          next-method()
        end if
      else
        next-method()
      end if
    else
      next-method()
    end if;
  else
    next-method()
  end if;
end method;

define method type-estimate-call-from-site(call :: <primitive-call>,
                                           cache :: <type-cache>)
    => (te :: false-or(<type-estimate>))
  let fn = primitive(call);
  block (return)
    // special cases
    when (fn == dylan-value(#"primitive-object-allocate-filled"))
      // type returned is actually contained in second argument
      let (c?, wrapper) = constant-value?(second(arguments(call)));
      when (c?)
        let iclass = ^mm-wrapper-implementation-class(wrapper);
        let class  = ^iclass-class(iclass);
        return(make(<type-estimate-values>,
                    fixed: vector(as(<type-estimate>, class))))
      end when
    end when;
    type-estimate-call-stupidly-from-fn(call, fn, cache)
  end block;
end method;

// *** Dependency: on return value.
define function type-estimate-call-stupidly(call :: <call>, cache :: <type-cache>)
    => (te :: <type-estimate>)
  if (temporary(call))
    // Just look at the values declaration in the thing being called.
    let valtype = type-estimate-call-from-site(call, cache);
    if (multiple-values?(temporary(call)))
      // Takes multiple values.
      valtype
    else
      first-value(valtype)
    end
  else
    // call value not used -- no one cares what the result type is
    make(<type-estimate-values>)
  end
end;

///
/// Coercing a <&signature> to a <type-estimate-limited-function>.
///

/// TODO: GET THIS TO WORK WITH LIMITED VECTORS
define constant <&types>         = <simple-object-vector>;
//   = limited(<vector>, of: <&type>);
define constant <type-estimates> = <simple-object-vector>;
//   = limited(<vector>, of: <type-estimate>);

define function type-estimate-function-from-signature
    (sig :: <&signature>, class :: <&class>, cache :: <type-cache>, #key body)
    => (te :: <type-estimate>)
  // Construct a limited function type-estimate based on the model signature
  local  method lift (x :: <&type>) => (te :: <type-estimate>)
           as(<type-estimate>, x)
         end,
         method lift-sequence
                (x :: <&types>, number-required :: <integer>)
             => (te* :: <type-estimates>)
           let requireds :: <simple-object-vector>
             = make(<type-estimates>, size: number-required);
           for (e in x, i :: <integer> from 0 below number-required)
             requireds[i] := lift(e)
           end for;
           requireds
         end;
  make(<type-estimate-limited-function>,
       class:     class,
       requireds: lift-sequence(as(<&types>, ^signature-required(sig)),
                                ^signature-number-required(sig)),
       rest?:     ^signature-rest?(sig),
       // ^signature-keys? = #t iff accepts keys
       // ^signature-keys is nonempty iff there are local keys.
       // (remember #key #all-keys takes keys but none locally)
       // *** Redundancy in <signature> among keys, keys?, and all-keys? ?
       keys:      when (~empty?(^signature-keys(sig)))
                    let tbl = make(<object-table>);
                    for (key in ^signature-keys(sig))
                      // *** NB: key types not recorded in signatures, for
                      //     reasons that are obscure to me!
                      tbl[key] := as(<type-estimate>, dylan-value(#"<object>"));
                    end;
                    tbl
                  end,
       all-keys?: ^signature-all-keys?(sig),
       vals:      if (body)
                    // Prefer body to values in signature
                    type-estimate-body(body, cache)
                  else
                     make(<type-estimate-values>,
                          // Could also do type-estimate-body,
                          // if function body is available (e.g., <&lambda>).
                          fixed: lift-sequence(^signature-values(sig),
                                               ^signature-number-values(sig)),
                          rest:  when (^signature-rest-value(sig))
                                   lift(^signature-rest-value(sig))
                                 end)
                  end)
end;

define function type-estimate-datum (obj) => (te :: <type-estimate>)
  // Make a singleton out of obj.
  make(<type-estimate-limited-instance>, singleton: obj)
end;

define function lift-model-named(name :: <symbol>) => (te :: <type-estimate>)
  // Evaluate name in Dylan, lift resulting model type as type-estimate.
  as(<type-estimate>, dylan-value(name))
end;

///
/// The type inference rule compiler.
///

///
/// Rule syntax: (nonterminals in UPPER-CASE, {... | ...} for alternatives)
///
/// RULE-FORM  ::= define type-inference-rules RULE-GROUP TYPE-RULES end;
/// RULE-GROUP ::= NAME
/// TYPE-RULES ::= TYPE-RULE; ...  (ok, actually 0 or more)
//
/// TYPE-RULE  ::= LHS CONNECTIVE RHS
///
/// CONNECTIVE ::= == | <- | <-*
///
/// LHS        ::= OBJ :: DFM-TYPE
/// OBJ        ::= NAME
/// DFM-TYPE   ::= NAME
///
/// RHS        ::= EXPRESSION | EXPRESSION, RHS
///
/// Rule semantics:
///
/// * On the LHS:
///
///   - OBJ is a symbol, which will go into the arglist of a method.  The rest
///     of the type-rule is in its scope.  It is the object whose type is
///     being inferred.
///
///   - DFM-TYPE is a type of some kind of object in the DFM, i.e., a subtype
///     of <dfm-ref>.
///
///   - The LHS advertises a way to compute the type of OBJ, given the knowledge
///     that its representation in the DFM is of type DFM-TYPE.  E.g.,
///
///           pcall :: <primitive-call> == ...rhs involving pcall...
///
///     purports to tell you how to compute the type of a <primitive-call>
///     computation, to be referred to as pcall in the rhs.
///
/// * The LHS is the trigger of the rule, i.e., the thing to be matched before
///   the rule can fire.  If the LHS is OBJ :: DFM-TYPE, then the rule defines
///   a method with signature:
///
///         type-estimate-infer(OBJ :: DFM-TYPE, cache :: <type-cache>).
///
/// * There are two types of connectives between the LHS and RHS:
///
///   - A rule of the form:
///
///       OBJ :: DFM-TYPE == ...rhs...
///
///     defines a "basis rule" which calculates directly the type of OBJ.  The
///     RHS should return a <type-estimate> which will be union'd with what's
///     already known in the cache about OBJ.
///
///     To keep the justification semantics straight, the RHS really should be
///     a single expression that just constructes a <type-estimate>, and which
///     does not call type-estimate &c in the process.
///
///     E.g., the type of an <&object> is some class <stype>:
///
///       x :: <&object> == make(<type-estimate-class>, class: &object-class(x))
///
///   - A rule of the form:
///
///       OBJ :: DFM-TYPE <- ...rhs...
///
///     defines an "induction rule" in which the typist recurses.  The RHS
///     should be a new DFM object, on which type-estimate-in-cache(_, _)
///     will be called recursively to produce the type of OBJ.  Thus OBJ defers
///     to some other object to determine its own type.
///
///     The RHS can be a ,-separated sequence of objects, in which case the
///     type generated is the union of the types of each of them.
///
///     E.g., the type of an <method-reference> is the type of the object
///     to which it refers:
///
///         obj-ref :: <method-reference> <- value(obj-ref);
///
///     The other kind of induction rule uses the connective <-*.  In this case,
///     the last RHS object is assumed to be a list.  Kind of like apply.
///
///     E.g., the type of a <merge> is the union of the sources:
///
///         merge :: <merge> <-* sources(merge);
///
///   In either case, the RHS can refer to the variable cache, which contains
///   the current state of the mapping from objects to types.
///

// For desperate debugging mode.
// define thread variable *step-depth*      :: <integer> = -1;
define thread variable *tracing-infer?*  :: <boolean> = #f; // Show type inference
define thread variable *stepping-infer?* :: <boolean> = #f; // Show + single-step

define macro with-infer-stepping
  { with-infer-stepping (?dfm-type:name) ?forms:body end }
  // => { with-infer-stepping-internal(?dfm-type, method () ?forms end) }
  => { ?forms }
end;

/*
define function with-infer-stepping-internal(dfm-type, body-fn :: <function>)
   => (te :: <type-estimate>)
  // Trace the inference rules, waiting for a character at each rule entry.
  if (*stepping-infer?* | *tracing-infer?*)
    local method step-indent () => ()
            format-out("\n");
            for (i from 0 below *step-depth*)
              write-element(*standard-output*, '|')
            end
          end,
          method step-infer-in (dfm-type) => ()
            step-indent();
            format-out("%= :: %=", *current-lhs*, dfm-type);
            when (*stepping-infer?*)
              read(*standard-input*, 1)
            end
          end,
          method step-infer-out (answer) => ()
            step-indent();
            format-out("answer: %=", answer)
          end;
    dynamic-bind (*step-depth* = *step-depth* + 1)
      step-infer-in(dfm-type);
      let answer = body-fn();
      step-infer-out(answer);
      answer
    end
  else
    body-fn()
  end
end;
*/

define inline function type-union-with-sources
    (start-type :: <type-estimate>, sources :: <sequence>, cache :: <type-cache>)
    => (te :: <type-estimate>)
  // Start from start-type, and union in types of all the other sources.
  // Good for combining <merge> sources, variable assignments, etc.
  reduce(method (so-far, next-rhs)
           *current-rhs* := add!(*current-rhs*, next-rhs);
           type-estimate-union(so-far, type-estimate-in-cache(next-rhs, cache))
         end,
         start-type,
         sources)
end;

define macro type-infer-rhs
  // Either execute rhs code (basis rule) or recurse (induction rule).
  { type-infer-rhs(ET, ?rhs:expression) }               // Basis rule.
    => { ?rhs }

  { type-infer-rhs(LT, ?rhs:expression) }               // Induction rule (1).
    => { let the-rhs = ?rhs;
         *current-rhs* := add!(*current-rhs*, the-rhs);
         type-estimate-in-cache(the-rhs, ?=cache) }

  { type-infer-rhs(LT, ?rhs1:expression, ?rhs-more:*) } // Induction rule (2).
    => { type-estimate-union(type-infer-rhs(LT, ?rhs1),
                             type-infer-rhs(LT, ?rhs-more)) }

  { type-infer-rhs(LTS, ?rhs:expression) }              // Induction rule (3).
    => { type-union-with-sources(make(<type-estimate-bottom>), ?rhs, ?=cache) }

  { type-infer-rhs(LTS, ?rhs1:expression, ?rhs-more:*) }// Induction rule (4).
    => { type-estimate-union(type-infer-rhs(LT, ?rhs1),
                             type-infer-rhs(LTS, ?rhs-more)) }
end;

// define type-inference-rules empty-rule-set end;            // Empty Test case
define macro type-inference-rules-definer
  // Dispatch on form of first rule, generating code; recurse on rest.
  { define type-inference-rules ?rule-group:name end } => { }    // No rules

  { define type-inference-rules ?rule-group:name
      ?lhs:name :: ?dfm-type:name ?op-and-rhs;                   // First rule
      ?more-rules:*                                              // More rules
    end }
  => { define method type-estimate-infer                         // Expand first
         (?lhs :: ?dfm-type, ?=cache :: <type-cache>) => (te :: <type-estimate>)
         dynamic-bind (?=*current-rule* = ?#"rule-group",// Rule currently firing
                       ?=*current-lhs*  = ?lhs,          // Trigger pattern on lhs
                       ?=*current-rhs*  = #())           // Precious bodily fluids
          with-infer-stepping (?dfm-type)
           let answer = type-infer-rhs(?op-and-rhs);             // Compute type
           type-estimate-update-cache(?lhs, ?=cache, answer);    // Update cache
           answer                                                // Return type
          end
         end
       end;
       define type-inference-rules ?rule-group                   // Expand rest
         ?more-rules
       end }
op-and-rhs:   // ?#"op" blows up for some obscure reason
  { ==  ?rhs } => { ET, ?rhs }  // Basis rule
  { <-* ?rhs } => { LTS, ?rhs } // Induction rule, last rhs is a sequence
  { <-  ?rhs } => { LT, ?rhs  } // Induction rule
rhs: // ,-separated expressions Should be able to do: { ??item:expression, ... }
  { }                        => { }
  { ?item:expression }       => { ?item }
  { ?item:expression, ?rhs } => { ?item, ?rhs }
end;

///
/// Type inference rules.  Twisted way to do abstract semantic interpretation!
///

define type-inference-rules type-infer-punt
  // Rules which punt on roots of DFM heterarchy.  Anything else will error.
  // <nop>, <if>, some <end>s, <bind>, return no values in
  // any reasonable sense, so they have no type.
  ignore :: <dfm-ref> == make(<type-estimate-bottom>) // Bottom is punt type
end;

///
/// Type inference for DFM instructions.  There should be a rule here to
/// trigger on every type of <computation>, or else use the punt rule above.
///

define type-inference-rules type-infer-references
  // Rules about reference-like instructions.
  var-ref :: <variable-reference> <- referenced-binding(var-ref); // Defer to var
  int-ref :: <interactor-binding-reference> <- referenced-binding(int-ref); // Defer to var
  clo-ref :: <make-closure>       == if (computation-signature-value(clo-ref))
                                       // *** ? use signature?
                                       lift-model-named(#"<method>")
                                     else
                                       let m = computation-closure-method(clo-ref);
                                       *current-rhs* := add!(*current-rhs*, m);
                                       type-estimate-in-cache(m, cache);
                                     end;
end;

define type-inference-rules type-infer-assigns
  // Rules about assignment-like instructions.
  assign :: <assignment>          <- computation-value(assign); // Defer to RHS
  update :: <conditional-update!> == lift-model-named(#"<boolean>");
  txfer  :: <temporary-transfer-computation>
    <- computation-value(txfer);  // Defer to value
  // <definition> and <set!> are both <assignment>s.
  // <multiple-value-spill> & <multiple-value-unspill> are txfers.
end;

define type-inference-rules type-infer-merges
  // Rules about <merge> & <if> instructions.
  // <if> is pure control, so it gets bottom via the punt rule.
  // "It's the <merge>, stupid." (With apologies to James Carville.)
  // Unary merges all do some sort of inference, so they have heir own rules.
  merge :: <binary-merge> <- merge-left-value(merge), merge-right-value(merge)
end;

define type-inference-rules type-infer-calls
  // Rules about the various kinds of call instructions.
  // Type of call is return-type of callee, _if_ it returns.  Can't infer arg
  // types before call, since it might signal a run-time <type-error>.  _Can_
  // infer post-call arg types, though, when we do flow-dependent types.
  // The real version of this will use function templates.
  // *** <primitive-indirect-call>
  call :: <c-variable-pointer-call> == lift-model-named(#"<raw-pointer>");
  call :: <call>                    == type-estimate-call-stupidly(call, cache); // ***
  call :: <stack-vector>            == lift-model-named(#"<simple-object-vector>");
  call :: <loop-call>               == make(<type-estimate-bottom>);
  call :: <any-slot-value>
    == as(<type-estimate>, ^slot-type(computation-slot-descriptor(call)));
  call :: <any-repeated-slot-value>
    == begin
         let stype = ^slot-type(computation-slot-descriptor(call));
         let ltype =
           if (stype == dylan-value(#"<object>"))
             let instance-te = type-estimate(computation-instance(call));
             if (instance?(instance-te, <type-estimate-limited-collection>))
               type-estimate-of(instance-te)
             end;
           end;
         ltype | as(<type-estimate>, repeated-representation(stype))
       end;
end;

/* [gts, 2/98, wait until harp backend ready]
define type-inference-rules type-infer-c-ffi
  call :: <begin-with-stack-structure> == lift-model-named(#"<raw-pointer>");
end;
*/

define type-inference-rules type-infer-blocks
  // Various concrete subclasses of <block>.
  // * A <bind-exit>'s <temporary> these days contains only the result of a call
  //   to the escape continuation; the body result is explicitly merged after
  //   the <end-exit-block>, so it'll get typed on demand.  Used to type the
  //   body here, but there were problems with type-estimate-body using the
  //   <end-exit-block> as a guard -- couldn't take previous-computation of it.
  // * An <unwind-protect>'s <temporary> is there, but ununsed.  Use a
  //   side-effect to fill the cache with the body & cleanups.
  b-x :: <bind-exit>      <-* exits(entry-state(b-x)); // *** Escaped?
  u-p :: <unwind-protect> ==  type-estimate-body(body(u-p), cache); // ***
end;

define type-inference-rules type-infer-ends
  // Various kinds of terminating computations, and related things.
  // NB: the method on <end-block> overrides the one on <end> since the <e*b>s
  //     are built on (<end-block>, <end>) in that order.
  ndr :: <end>       <- computation-value(ndr);       // <return>, <exit>
  ebl :: <end-block> == make(<type-estimate-bottom>); // <eeb>, <epb>, <ecb>
  ebl :: <end-loop>  == make(<type-estimate-bottom>);
  ext :: <exit>      ==
    begin // *** Dependency!
      let value-type = type-estimate-in-cache(computation-value(ext), cache);
      if (instance?(value-type, <type-estimate-limited-collection>))
        make(<type-estimate-values>, rest: type-estimate-of(value-type))
      else
        make(<type-estimate-values>) // values(#rest <object>).
      end
    end;
end;

///
/// Guiding principles re <bottom> and multiple values:
///
/// * values(#rest <bottom>), or "bottoms all the way down," is the maximally
///   undefined return value.  No matter which value you try to look at, you get
///   <bottom>.  Even the ones you don't look at are <bottom>.
///
/// * values(...anything..., <bottom>, ...anything...) normalizes to
///   values(#rest <bottom>).
///
/// * Extracting a value from values(#rest <bottom>) always gives <bottom>, not
///   <bottom> union singleton(#f), since if you never return, you never default
///   the return value.
///
/// * Adjusting a values(#rest <bottom>) stays at values(#rest <bottom>), so
///   no matter what value you later try to extract, you get <bottom>.
///

define function type-estimate-values-element-subtype?
    (values-te :: <type-estimate>, index :: <integer>, te :: <type-estimate>)
 => (subtype? :: <boolean>)
  // Is the indexth value of values-te a subtype of te?  Slightly complex
  // because values-te could be a union of values type-estimates.
  local method single-value-subtype? (values-te :: <type-estimate-values>)
         => (indexth-te-subtype? :: <boolean>)
          let fixed-te* = type-estimate-fixed-values(values-te);
          let rest-te   = type-estimate-rest-values(values-te);
          let single-te =
            if (index < size(fixed-te*))
              // The value extraction is guaranteed to succeed with type:
              fixed-te*[index]
            elseif (~rest-te)
              // No rest type, so must default to #f.
              make(<type-estimate-limited-instance>, singleton: &false)
            elseif (instance?(rest-te, <type-estimate-bottom>))
              // values(#rest <bottom>), can't default #f because never return
              make(<type-estimate-bottom>)
            else
              // TODO: Where does value padding happen? If in this
              // instruction, the inferred type should be the rest type
              // (if present) union singleton(#f). That's what I've done
              // here to be conservative.
              // The value extraction may succeed, or be defaulted,
              // resulting in type:
              type-estimate-union(rest-te,
                                  make(<type-estimate-limited-instance>,
                                       singleton: &false))
            end;
          type-estimate-subtype?(single-te, te)
        end;
  select (values-te by instance?)
    <type-estimate-bottom> => #t;  // *** Not strictly <boolean>.
    <type-estimate-values> => single-value-subtype?(values-te);
    <type-estimate-union>  => every?(single-value-subtype?,
                                     type-estimate-unionees(values-te));
  end
end;

define function type-estimate-values-rest-subtype?
    (values-te :: <type-estimate>, index :: <integer>, te :: <type-estimate>)
 => (subtype? :: <boolean>)
  // Is every value of values-te after index a subtype of te?  Slightly complex
  // because values-te could be a union of values type-estimates.
  local method single-rest-subtype? (values-te :: <type-estimate-values>)
          let fixed-te* = type-estimate-fixed-values(values-te);
          let rest-te = type-estimate-rest-values(values-te);
          (~rest-te | type-estimate-subtype?(rest-te, te)) &
            // every type between index and size must be a subtype
            block (exit)
              for (i from index below size(fixed-te*))
                unless (type-estimate-subtype?(fixed-te*[i], te))
                  exit(#f)
                end
              end;
              #t
            end
        end;
  select (values-te by instance?)
    <type-estimate-bottom> => #t;
    <type-estimate-values> => single-rest-subtype?(values-te);
    <type-estimate-union>  => every?(single-rest-subtype?,
                                     type-estimate-unionees(values-te));
  end
end;


/*  Save this for use in <extract-rest-value> type inference below.
define method type-estimate-rest-value (te :: <type-estimate-values>,
                                        index :: <integer>)
 => (rest-value-te :: false-or(<type-estimate>))
  let fixed-te* = type-estimate-fixed-values(te);
  let rest-te = type-estimate-rest-values(te);
  if (index < size(fixed-te*))
    // The value extraction is guaranteed to succeed with type:
    // TODO? This does too much consing.  I assume it almost never
    //       happens, so it shouldn't matter.
    let fixed-type-estimates :: <type-variables>
      = copy-sequence(fixed-te*, start: index);
    make(<type-estimate-union>,
         unionees: if (rest-te)
                     add!(rest-te, fixed-type-estimates)
                   else
                     fixed-type-estimates
                   end)
  else
    rest-te
  end
end;

define method type-estimate-rest-value (te :: <type-estimate-union>,
                                        index :: <integer>)
 => (rest-value-te :: false-or(<type-estimate>))
  // this assumes that all unionees are <type-estimate-values>
  // probably it's an error if that's not true
  // this conses too much, but #rest values aren't used much
  let unionees :: <unionee-sequence>
    = map(rcurry(type-estimate-rest-value, index),
          type-estimate-unionees(te));
  if (every?(\~, unionees))
    #f
  else
    make(<type-estimate-union>,
         unionees: remove(unionees, #f))
  end
end;
*/

define method type-estimate-of (te :: <type-estimate-class>)
  let class = type-estimate-class(te);
  if (^subtype?(class, dylan-value(#"<collection>")))
    make(<type-estimate-class>, class: dylan-value(#"<object>"))
  else
    error("Trying to take type-estimate-of on a non-collection %=", te);
  end if
end method;

// *** Dependencies!
define type-inference-rules type-infer-multiple-values
  val :: <values> == make(<type-estimate-values>,
                          fixed: map(rcurry(type-estimate-in-cache, cache),
                                     fixed-values(val)),
                          rest: when (rest-value(val))
                                  type-estimate-of
                                    (type-estimate-in-cache(rest-value(val), cache))
                                end);
  xsv :: <extract-single-value> ==
    begin
      let values-te = type-estimate-in-cache(computation-value(xsv), cache);
      let index = index(xsv);
      local method estimate-single-value (te :: <type-estimate-values>)
             => (indexth-te :: <type-estimate>)
              // *** Share with type-estimate-values-element-subtype?, above.
              let fixed-te* = type-estimate-fixed-values(te);
              let rest-te   = type-estimate-rest-values(te);
              if (index < size(fixed-te*))
                // The value extraction is guaranteed to succeed with type:
                fixed-te*[index];
              elseif (~rest-te)
                // No #rest value, so default to #f
                make(<type-estimate-limited-instance>, singleton: &false)
              elseif (instance?(rest-te, <type-estimate-bottom>))
                // Something like values(#rest <bottom>), so result is bottom
                // because it can't be defaulted to #f.
                make(<type-estimate-bottom>)
              else
                // TODO: Where does value padding happen? If in this
                // instruction, the inferred type should be the rest type
                // (if present) union singleton(#f). That's what I've done
                // here to be conservative.
                // The value extraction may succeed, or be defaulted,
                // resulting in type:
                type-estimate-union(rest-te,
                                    make(<type-estimate-limited-instance>,
                                         singleton: &false))
              end
            end;
        select (values-te by instance?)
        <type-estimate-bottom> => values-te;
        <type-estimate-values> => estimate-single-value(values-te);
        <type-estimate-union>  => make(<type-estimate-union>,
                                       unionees: map(estimate-single-value,
                                                     type-estimate-unionees(
                                                       values-te)));
      end
    end;
  xrv :: <extract-rest-value>
    == // We're accessing a rest vector, but there's no point in
       // doing anything flash until limited collections are really
       // flying in the typist, hence the following.
       // TODO: Make this a limited vector of type when possible.
       lift-model-named(#"<simple-object-vector>");
  // <multiple-value-spill> & <multiple-value-unspill> are txfers.
end;

define type-inference-rules type-infer-adjust-multiple-values
  adj :: <adjust-multiple-values> ==
    begin
      let values-te = type-estimate-in-cache(computation-value(adj), cache);
      let  n        = number-of-required-values(adj);
      local method adjust-mv (te :: <type-estimate-values>)
              // Ensure there are EXACTLY n values.
              let fixed-te*         = type-estimate-fixed-values(te);
              let rest-te           = type-estimate-rest-values(te);
              let values-fixed-size = size(fixed-te*);
              if (values-fixed-size = n)
                // Fixed values supply exactly as many as we want
                if (rest-te)
                  // Lose the rest value
                  make(<type-estimate-values>, fixed: fixed-te*, rest: #f)
                else
                  // No rest value to lose, so no change.
                  te
                end
              elseif (n < values-fixed-size)
                // Fixed values supply more than needed; lose rest & some fixed.
                make(<type-estimate-values>,
                     fixed: copy-sequence(fixed-te*, end: n),
                     rest: #f)
              else
                // n > values-fixed-size, so fixed values insufficient for our
                // needs.  Fill with rest value or singleton(#f).
                let fill-te =
                  if (~rest-te)
                    // No rest arg, so fill is singleton(#f).
                    make(<type-estimate-limited-instance>, singleton: &false)
                  elseif (instance?(rest-te, <type-estimate-bottom>))
                    // values(#rest <bottom>), so stay bottom!
                    make(<type-estimate-bottom>)
                  else
                    // Use rest value union singleton(#f).
                    type-estimate-union(rest-te,
                                        make(<type-estimate-limited-instance>,
                                             singleton: &false))
                  end;
                make(<type-estimate-values>,
                     fixed: concatenate(fixed-te*,
                                        make(<list>,
                                             size: n - values-fixed-size,
                                             fill: fill-te)),
                     rest: #f)
              end
            end;
      select (values-te by instance?)
        <type-estimate-bottom> => values-te;
        <type-estimate-values> => adjust-mv(values-te);
        <type-estimate-union>  => make(<type-estimate-union>,
                                       unionees: map(adjust-mv,
                                                     type-estimate-unionees(
                                                       values-te)));
      end
    end;
  adj :: <adjust-multiple-values-rest> ==
    begin
      let values-te = type-estimate-in-cache(computation-value(adj), cache);
      let n         = number-of-required-values(adj);
      local method adjust-mv (te :: <type-estimate-values>)
              // Ensure there are at LEAST n fixed values.
              let fixed-te*         = type-estimate-fixed-values(te);
              let rest-te           = type-estimate-rest-values(te);
              let values-fixed-size = size(fixed-te*);
              if (values-fixed-size >= n)
                // Fixed values will suffice for our needs.
                te
              else
                // Need to pad out fixed values with rest or #f.
                let fill-te =
                  if (~rest-te)
                    // No rest type, so pad with #f
                    make(<type-estimate-limited-instance>, singleton: &false)
                  elseif (instance?(rest-te, <type-estimate-bottom>))
                    // Sucked into the values(#rest <bottom>) black hole.
                    make(<type-estimate-bottom>)
                  else
                    // Otherwise use rest type union singleton(#f).
                    type-estimate-union(rest-te,
                                        make(<type-estimate-limited-instance>,
                                             singleton: &false))
                  end;
                make(<type-estimate-values>,
                     fixed: concatenate(fixed-te*,
                                        make(<list>,
                                             size: n - values-fixed-size,
                                             fill: fill-te)),
                     rest: rest-te)
              end
            end;
      select (values-te by instance?)
        <type-estimate-bottom> => values-te;
        <type-estimate-values> => adjust-mv(values-te);
        <type-estimate-union> => make(<type-estimate-union>,
                                      unionees: map(adjust-mv,
                                                    type-estimate-unionees(
                                                      values-te)));
      end
    end;
end;

define generic constant-value? (ref)
  => (constant? :: <boolean>, value :: <object>);

define method constant-value?
  (ref :: <object-reference>)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <object-reference>.
  values(#t, reference-value(ref))
end method;

define method constant-value?
  (ref :: <defined-constant-reference>)
   => (constant-value? :: <boolean>, constant-value)
  // Extract the constant from an <defined-constant-reference>.
  // TODO: DOESN'T HANDLE FALSE
  let value = computation-value(ref);
  if (value)
    let (inlineable?, inline-value) = inlineable?(value);
    if (inlineable?)
      values(#t, inline-value)
    else
      values(#f, #f)
    end if
  else
    values(#f, #f)
  end if;
end method;

define method constant-value?
    (ref :: <temporary>) => (constant-value? :: <boolean>, constant-value)
  // If this temporary is estimated as a singleton, extract the constant.
  let type = type-estimate(ref);
  if (instance?(type, <type-estimate-limited-instance>))
    values(#t, type-estimate-singleton(type))
  else
    values(#f, #f)
  end
end method;

define method constant-value?
  (ref :: <value-reference>) => (constant-value? :: <boolean>, constant-value)
  // Other kinds of <value-reference>s are not constants.
  values(#f, #f)
end method;

define method constant-value-in-cache?
    (ref :: <value-reference>, cache)
  => (constant? :: <boolean>, value :: <object>);
  constant-value?(ref)
end method;

define method constant-value-in-cache?
    (ref :: <temporary>, cache)
 => (constant? :: <boolean>, value :: <object>);
  let type = type-estimate-in-cache(ref, cache);
  if (instance?(type, <type-estimate-limited-instance>))
    values(#t, type-estimate-singleton(type))
  else
    values(#f, #f)
  end
end method;


define function poor-mans-check-type-intersection
    (value-type :: <type-estimate>, temp :: false-or(<value-reference>),
       cache :: <type-cache>)
 => (intersection :: <type-estimate>)
  // TODO: take intersection of value type and checked type
  //       for now, do a poor-man's intersection
  if (temp)
    let (the-type-constant?, the-type) = constant-value-in-cache?(temp, cache);
    if (the-type-constant? & instance?(the-type, <&type>))
      let checked-type = as(<type-estimate>, the-type);
      if (type-estimate-subtype?(value-type, checked-type))
        // Value-type is more specific
        value-type
      else
        // Checked-type is more specific
        checked-type
      end
    else
      // don't know the type being checked at compile time
      value-type
    end
  else
    // optimizer has determined that type check is superfluous
    value-type
  end
end;

define type-inference-rules type-infer-checks
  // <check-type>s are a type check + temporary transfer
  ct :: <check-type> ==
    poor-mans-check-type-intersection(type-estimate-in-cache(computation-value(ct), cache),
                                      type(ct),
                                      cache);
  // <constrain-type>s are a constraint + temporary transfer
  // See description in optimization/assignment for motivation.
  ct :: <constrain-type> ==
    begin
      let values-te = type-estimate-in-cache(computation-value(ct), cache);
      let pruned-values-te =
        if (ct.type)
          poor-mans-check-type-intersection(values-te, ct.type, cache)
        else
          let false
            = make(<type-estimate-limited-instance>, singleton: &false);
          type-difference(values-te, false) | false;
        end;
      /*
      unless (type-estimate-subtype?(values-te, pruned-values-te))
        format-out(">>> Pruning type for %=:\n  Before %=\n  After  %=\n",
                   computation-value(ct), values-te, pruned-values-te);
      end;
      */
      pruned-values-te
    end;
  ct :: <multiple-value-check-type> ==
    begin
      let values-te          = type-estimate-in-cache(computation-value(ct), cache);
      let checked-types      = types(ct);
      let checked-fixed-size = size(checked-types);
      local method mv-intersection (te :: <type-estimate-values>)
              // Intersect te elements with checked-types, in order.  Make sure
              // we have exactly that many values, trimming #rest type.
              let fixed-te*         = type-estimate-fixed-values(te);
              let values-fixed-size = size(fixed-te*);
              let result-fixed      = make(<vector>, size: checked-fixed-size);
              // Fill result-fixed w/intersection of inferred & checked types.
              map-into(result-fixed,
                       rcurry(poor-mans-check-type-intersection, cache),
                       fixed-te*, checked-types);
              when (values-fixed-size < checked-fixed-size)
                // Insufficient fixed values, so pad with rest value.
                let rest-te          = type-estimate-rest-values(te);
                let rest-values-type =
                  if (~rest-te)
                    // No rest type, so use default #f.
                    make(<type-estimate-limited-instance>, singleton: &false)
                  elseif (instance?(rest-te, <type-estimate-bottom>))
                    // <bottom> contagion
                    make(<type-estimate-bottom>)
                  else
                    // Union rest-te with singleton(#f).
                    type-estimate-union(rest-te,
                                        make(<type-estimate-limited-instance>,
                                             singleton: &false))
                  end;
                for (i from values-fixed-size below checked-fixed-size)
                  // Pad out checked types with intersection of checked
                  // type and the fill type.
                  result-fixed[i] :=
                    poor-mans-check-type-intersection(rest-values-type,
                                                      checked-types[i],
                                                      cache)
                end
              end;
              // Result-fixed now contains the types we want.
              make(<type-estimate-values>,
                   fixed: as(<list>, result-fixed), // Someday use <sequence>...
                   rest: #f)
            end;
      select (values-te by instance?)
        <type-estimate-bottom> => values-te;
        <type-estimate-values> => mv-intersection(values-te);
        <type-estimate-union>  => make(<type-estimate-union>,
                                       unionees: map(mv-intersection,
                                                     type-estimate-unionees(
                                                       values-te)));
      end
    end;
  ct :: <multiple-value-check-type-rest> ==
    begin
      let values-te          = type-estimate-in-cache(computation-value(ct), cache);
      let checked-types      = types(ct);
      let checked-fixed-size = size(checked-types);
      local method mv-intersection (te :: <type-estimate-values>)
              // Intersect te elements with checked-types, in order.  Make sure
              // we have AT LEAST that many values, preserving #rest type.
              let fixed-te*         = type-estimate-fixed-values(te);
              let values-fixed-size = size(fixed-te*);
              let result-fixed      = make(<vector>,
                                           size: max(values-fixed-size,
                                                     checked-fixed-size));
              // Fill result-fixed w/ intersection of inferred & checked types.
              map-into(result-fixed,
                       rcurry(poor-mans-check-type-intersection, cache),
                       fixed-te*, checked-types);
              let rest-te = type-estimate-rest-values(te);
              when (values-fixed-size < checked-fixed-size)
                // Insufficient fixed values, so pad with rest value.
                let rest-values-type =
                  if (~rest-te)
                    // No rest type, so default to singleton(#f).
                    make(<type-estimate-limited-instance>, singleton: &false)
                  elseif (instance?(rest-te, <type-estimate-bottom>))
                    // <bottom> contagion
                    make(<type-estimate-bottom>)
                  else
                    // Use rest type union singleton(#f)
                    type-estimate-union(rest-te,
                                        make(<type-estimate-limited-instance>,
                                             singleton: &false))
                  end;
                for (i from values-fixed-size below checked-fixed-size)
                  // Pad out checked types with intersection of checked
                  // type and the fill type.
                  result-fixed[i] :=
                    poor-mans-check-type-intersection(rest-values-type,
                                                      checked-types[i],
                                                      cache)
                end
              end;
              // Now make sure rest of fixed types, if any, are compatible
              // with checked-rest-type.
              let checked-rest-type = rest-type(ct);
              for (i from checked-fixed-size below values-fixed-size)
                result-fixed[i] :=
                  poor-mans-check-type-intersection(fixed-te*[i],
                                                    checked-rest-type,
                                                    cache)
              end;
              // Result-fixed now contains the fixed types.  Also need to
              // check if the rest type, if any, is compatible.
              make(<type-estimate-values>,
                   fixed: as(<list>, result-fixed),
                   rest: rest-te &
                         poor-mans-check-type-intersection(rest-te,
                                                           checked-rest-type,
                                                           cache))
            end;
      select (values-te by instance?)
        <type-estimate-bottom> => values-te;
        <type-estimate-values> => mv-intersection(values-te);
        <type-estimate-union>  => make(<type-estimate-union>,
                                       unionees: map(mv-intersection,
                                                     type-estimate-unionees(
                                                       values-te)));
      end
    end;
end;

// Assume except is just singleton(#f) for now!

define method type-difference
  (type :: <type-estimate>, except :: <type-estimate-limited-instance>)
  type
end;

define method type-difference
  (type :: <type-estimate-class>, except :: <type-estimate-limited-instance>)
  if (type.type-estimate-class == dylan-value(#"<boolean>"))
    make(<type-estimate-limited-instance>, singleton: &true);
  else
    type
  end
end;

define method type-difference
  (type :: <type-estimate-union>, except :: <type-estimate-limited-instance>)
  collecting (unionees)
    for (te :: <type-estimate> in type-estimate-unionees(type))
      let pruned-te = type-difference(te, except);
      if (pruned-te) collect-into(unionees, pruned-te) end;
    end;
    let unionees :: <simple-object-vector>
      = as(<simple-object-vector>, collected(unionees));
    select (unionees.size)
      0 => #f;
      1 => unionees[0];
      otherwise => make(<type-estimate-union>, unionees: unionees)
    end;
  end collecting;
end;

define method type-difference
  (type :: <type-estimate-limited-instance>,
     except :: <type-estimate-limited-instance>)
  if (type-estimate-singleton(type) == type-estimate-singleton(except))
    #f
  else
    type
  end;
end method;

define type-inference-rules type-infer-cells
  // The Marquess of Queensbury Rules.
  //
  // By popular demand, boxes have the same type as their contents.
  // This is a storage issue instead of a type issue, and the compiler is the
  // one generating all the boxes, so elaborating a boxed type would just check
  // for compiler code-generation bugs anyway.
  //
  // Readers of the box get a type from original value + all assigns. (reflow)
  // Writers of the box get a type from what they write there.
  mb :: <make-cell>
    <-  computation-value(mb);
  gv :: <get-cell-value>
    <-* generator(computation-cell(gv)), assignments(temporary(generator(computation-cell(gv))));
  sv :: <set-cell-value!>
    <-  computation-value(sv);
end;

define type-inference-rules type-infer-guarantee-type
  gt :: <guarantee-type> ==
    begin
      let static-type = static-guaranteed-type(gt);
      if (static-type)
          as(<type-estimate>, static-type)
      else
        poor-mans-check-type-intersection(type-estimate-in-cache(computation-value(gt),
                                                                 cache),
                                          guaranteed-type(gt),
                                          cache)
      end
    end;
end;

///
/// Type inference on variables (subclasses of <binding> and <temporary>).
///

define type-inference-rules type-infer-variables
  // Rules about variable-like things:
  // <temporary>, <multiple-value-temporary>, <entry-state>
  // <binding>, <module-binding>, <lexical-required-variable>,
  // <lexical-keyword-variable>, <lexical-rest-variable>,
  // <lexical-specialized-variable>.
  // Note that any <temporary> with a generator is handled by type-infer, now.
  mb   :: <module-binding>
    == case
         constant?(mb) // module constant
           => let (val, computed?)
                   = binding-constant-model-object(mb, error-if-circular?: #f);
              case
                // Initializer has been computed, so use its result type.
                computed? & inlineable?(val)
                  => *current-rhs* := add!(*current-rhs*, val);
                     type-estimate-in-cache(val, cache);
                computed? & ^instance?(val, dylan-value(#"<object>"))
                  // Not inlineable, but might be able to extract a type from val.
                  // Also not a raw type, which would cause problems.
                  => // This does what type-estimate would have done with val,
                     // but using ^object-class rather than a singleton type.
                     *current-rhs* := add!(*current-rhs*, val);
                     cache[val]    := make(<type-variable>);
                     let answer = as(<type-estimate>, ^object-class(val));
                     type-estimate-update-cache(val, cache, answer);
                     answer;
                // Initializer not computed, so have to punt to declared type.
                otherwise
                  => type-infer-using-declared-type(cache, mb);
              end;
         // Otherwise a module variable: type from decls, initializer, assigns.
         // NB: Raw variables REQUIRE decls, since they're not <object>s!
         // Exported: must believe decl (can't find all assigns).
         // TODO: SMARTNESS: This queries exported/created from the module,
         // not the library. The test should be a test for escaping the
         // library, not the module.
         // TODO: Module variables can also escape a library by being named
         // in a macro.
         exported?(mb)
           => type-infer-using-declared-type(cache, mb);
         otherwise
           => type-infer-using-declared-type(cache, mb);
           /*
           // Unexported module variable: take union of all assigns (including
           // initializer), which is the "real" type.  NB: type-safety with
           // the declared type is enforced with assignment type checks, so
           // there's no need to try to intersect them here.
           // Further weirdness: optimizers might have removed an assignment,
           // leaving behind a model object.  So initial type should be type of
           // model object or bottom if none.
           let model = binding-model-object(mb, default: $unfound,
                                            error-if-circular?: #f);
           type-union-with-sources(case
                                     found?(model) =>
                                       *current-rhs* := add!(*current-rhs*, model);
                                       type-estimate-in-cache(model, cache);
                                     otherwise => make(<type-estimate-bottom>);
                                   end,
                                   map(computation-value, assignments(mb)),
                                   cache);
         */
       end;
  // A fallback for lexical variables which are a subclasses of <temporary>, but
  // don't always have their generator set, until all particular cases are
  // filled in.
  // N.B. - any <temporary> with a generator is filtered out before here
  lv :: <lexical-variable> == lift-model-named(#"<object>");
  lsv :: <lexical-specialized-variable> // includes required & local vars
    == type-union-with-sources
         (begin
            let spec = specializer(lsv); // Start @ decl
            select (spec by instance?)
              <&type>   => as(<type-estimate>, spec);
              otherwise => lift-model-named(#"<object>"); // Dynamic?
            end
          end,
            assignments(lsv),
          cache);
  // lkv    :: <lexical-keyword-variable> == ***;
  lrv :: <lexical-rest-variable>
    == lift-model-named(#"<simple-object-vector>");
  // We know nothing...
  ib :: <interactor-binding>
    == lift-model-named(#"<object>");
end;

define function type-infer-using-declared-type
    (cache, mb :: <module-binding>) => (answer :: <type-estimate>)
  let (type-val, computed?)
    = binding-constant-type-model-object
        (mb, error-if-circular?: #f);
  if (computed?)
    as(<type-estimate>, type-val);
  else
    lift-model-named(#"<object>");
  end;
end function;

///
/// Type inference on various kinds of data (subclasses of <&object>).
///

define type-inference-rules type-infer-data
  // Rules about data objects.
  obj :: <&top>      == type-estimate-datum(obj);
  obj :: <heap-deferred-model> == type-estimate-datum(obj);
  // *** All of these are because of mapped types, which are implemented
  //     directly in the compiler (hence not under <&object>), and because
  //     the emulator doesn't do type-unions meaningfully.
  obj :: <number>           == type-estimate-datum(obj);
  obj :: <boolean>          == type-estimate-datum(obj);
  obj :: <character>        == type-estimate-datum(obj);
  obj :: <string>           == type-estimate-datum(obj);
  obj :: <symbol>           == type-estimate-datum(obj);
  obj :: <vector>           == type-estimate-datum(obj);
  obj :: <list>             == type-estimate-datum(obj);
  obj :: <mapped-unbound>   == type-estimate-datum(obj);
end;
