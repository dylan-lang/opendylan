module: dfmc-optimization
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function analyze-dynamic-extent-for (f :: <&method>) => ()
  get-extent-of-parameters-for(f);
end;

/// Dynamic extent analysis:
//
//   This file contains a fairly simple-minded dynamic-extent analysis.
// For each lambda we compute the dynamic extent of each parameter. As
// part of the analysis we optimise #rest parameters that have dynamic
// extent by removing the redundant copy-vector.  We also tag any
// closures that we detect have dynamic extent.  The analysis is fairly
// simple-minded in at least a couple of ways:
//
// 1) In the case of recursive lambdas we don't attempt to solve the
// dataflow equations via iteration, but break the loop in a conservative
// fashion.  For #rest parameter and closure analysis this doesn't appear
// too restrictive, but if other optimizations started using the dynamic
// extent information then we may find the analysis is too conservative.
//
// 2) A parameter is treated as having dynamic extent only if it cannot
// escape from the method.  So if a method M, with parameter X, passes X
// to the identity function, and then discards the result, the parameter
// will not be viewed as having dynamic extent.  A more refined analysis
// would keep track of the dependencies between parameters and results.
// However, initial results suggest that things like the forward
// iteration protocol (see below) currently account for much more of the
// precision loss than a more refined analysis.
//
// The main problems that arise with this simple extent analysis are due
// to generic functions, particularly open ones.  We run the analysis
// late in the optimization phase, so any calls to known methods have
// already been resolved.  If we have a call to a generic function, and
// we know all the applicable methods, then we take the intersection of
// all their extents in the obvious way.  Suppose method M1 has dynamic
// extent on parameters 0 and 1, and M2 has dynamic extent on parameters
// 0 and 2.  If they are both part of the generic function M, and a call
// to M can be statically resolved to one of M1 or M2, then we would
// deduce that the call would allow the first argument to have dynamic
// extent, but not the others.
//
// The problem is more serious when open generics are involved.  In the
// worst case this means we must assume that no arguments can have
// dynamic extent if we call a generic where all applicable methods are
// not known.  This means that even simple expressions like "X = Y" would
// prevent X and Y from having dynamic extent if the types of X and Y
// were not very constrained.  One approach that can help here is to
// associate some dynamic extent information with the generic and then
// ensure that all methods added to the generic satisfy these
// constraints.  So we might declare that both arguments to any method on
// \= must have dynamic extent. Obviously this would conflict with the
// DRM, although one could make the case that certain methods should be
// documented as being functional.  We would also need to ensure that any
// methods added via add-method satisfied the constraints.  The problem
// here is perhaps similar to that of enforcing the make method guarantee
// when methods can be added dynamically.  At the moment the only
// constraint on a generic function that we can express is that all
// parameters must have dynamic extent, via the dynamic-extent modifier.
// The code will then check that any methods added at compile-time adhere
// to this constraint.  Thus one could allow generic functions like
// equality to interact "correctly" with extent analysis, with a slight
// modification to the DRM, but we have not added the appropriate
// modifiers to the Dylan library at this time.
//
// The nastiest problem is due to the current definition of the forward
// iteration protocol.  If it was required to be functional then
// everything would be fine.  We could not deduce that a collection
// passed to the fip had dynamic extent, as the collection could be
// passed out as part of the result.  However, with a slight alteration
// to the for macro, using something like
//
//   let dy = collection; %dynamic-extent(dy); let (...) = fip(dy);
//
// we could ensure that any use of a collection in a for loop did not
// prevent it from having dynamic-extent.  This is because we can ensure
// that in this case the results of the call to the fip cannot escape the
// for loop.  Unfortunately, the DRM places no such constraint on the
// fip, although it is almost inconceivable that someone would write a
// method on the fip that wasn't functional.  The net effect is that any
// use of a collection in a for loop prevents it from having dynamic
// extent unless we have a fairly precise type for the collection which
// allows the call to the fip to be expanded out.  As many of the
// higher-order functions such as map and do depend on the fip, any use
// of these methods will also prevent a collection from having dynamic
// extent.  These methods are not currently inlined, and so this causes a
// problem even if we have an accurate type for the collection.  At the
// moment this problem seems to be the biggest cause of parameters not
// being flagged as having dynamic extent when they should have.

define constant $none-with-dynamic-extent
  = as(limited(<vector>, of: <integer>), #[]);

// COMPUTE-DYNAMIC-EXTENT-OF-LAMBDA-PARAMETERS
//
//   Given a lambda, construct a conservative estimate of which parameters can
//   safely have dynamic extent.
//

define generic compute-dynamic-extent-of-lambda-parameters
    (f :: <&method>) => (extent :: <dynamic-extent>);

define method compute-dynamic-extent-of-lambda-parameters
    (f :: <&method>) => (extent :: <dynamic-extent>)
//  format-out("Processing %=\n", f);
//  print-method-out(f);

  let initial-extent = f.parameters-dynamic-extent;

  if (initial-extent)
    // The extent has already been computed, e.g. inherited from GF
    initial-extent
  else
    f.parameters-dynamic-extent := $none-with-dynamic-extent;  // Crude mechanism to prevent looping
    // Calculate extent
    if (f.parameters)
      let extent :: limited(<vector>, of: <integer>) =
        as(limited(<vector>, of: <integer>),
           choose-by(really-dynamic-extent?, f.parameters, range(from: 0)));
      if (*colorize-dispatch*) color-extent(f, extent) end;
      f.parameters-dynamic-extent := (extent.size = f.parameters.size) | extent
    else
      // Occasionally we encounter a lambda with no parameters, presumably
      // because the optimizer is being called on something that's not
      // initialized yet?  Anyhow, we just take a pessimistic view in this case
      f.parameters-dynamic-extent := $none-with-dynamic-extent
    end;
  end
end;


define function color-extent(f :: <&method>, extent :: <dynamic-extent>)
  let m = model-definition(f);

  if (m & extent)
    let sig-spec = m.form-signature;
    local method color (o)
      let loc = o & o.spec-variable-name.fragment-source-location;
      if (loc)
        color-location(loc, #"dynamic-extent")
      end
    end;

    let reqs = sig-spec.spec-argument-required-variable-specs;

    let reqs-size = reqs.size;

    if (extent == #t | member?(reqs-size, extent))
      color(sig-spec.spec-argument-rest-variable-spec)
    end;
  end
end;


define method compute-dynamic-extent-of-lambda-parameters
    (f :: <&lambda>) => (extent :: <dynamic-extent>)
  let extent = next-method();
  let binding = model-variable-binding(f);
  if (binding)
    let gf = binding-model-object(binding, error-if-circular?: #f);
    if ( instance?(gf, <&generic-function>)
       & gf.parameters-dynamic-extent
       & f.parameters-dynamic-extent ~= #t )
      // Need to fix this when GF extent becomes more refined.
      format-out("The parameters to %= must have dynamic extent\n", f);
    end;
  end;

  // Check to see if the body allocates any closures that can have dynamic
  // extent, and tag them if found.

  if (f.body)
    walk-computations(
      method (c)
        if ( c.object-class == <make-closure>
           & (c.closure-has-dynamic-extent?
                := really-dynamic-extent?(c.temporary))
           & *colorize-dispatch* )
          color-closure(c)
        end
      end, f.body, #f);
  end;

  extent
end;


define function color-closure(c :: <make-closure>)
  let body = c.computation-closure-method.body;
  if (body & dfm-source-location(body))
    let loc = dfm-source-location(body);
    // We really only want to color the start of the method.  This mess tries to do
    // that (but without much success...)
    let so = loc.source-location-start-offset;
    let sol = so.source-offset-line;
    let soc = so.source-offset-column;
    let real-loc =
      if (c.computation-closure-method.^debug-name)
        // Want to color just the method name, but how???
        make-source-location(
          loc.source-location-record,
          0, sol, soc, 0, sol, soc + 5) // Need to replace 0s here
      else  // Just color "method"
        make-source-location(
          loc.source-location-record,
          0, sol, soc, 0, sol, soc + 6) // Need to replace 0s here
      end;
    color-location(real-loc, #"dynamic-extent")
  end if;
end function;


define function get-extent-of-parameters-for
    (f :: <&method>) => (extent :: <dynamic-extent>)
  f.parameters-dynamic-extent
  | begin
      really-run-compilation-passes(f);
      compute-dynamic-extent-of-lambda-parameters(f)
    end
end;



// USED-WITH-DYNAMIC-EXTENT?
//
//   Returns #t if all uses of a temporary in a computation can safely have
//   dynamic extent.  We aim to be conservative, i.e. erring towards #f.

define generic used-with-dynamic-extent?
  (comp :: <computation>, temp) => (dynamic? :: <boolean>);

define method used-with-dynamic-extent?
  (comp :: <computation>, temp) => (dynamic? :: <boolean>)
  // The default is to return #f so that we are conservative
  #f
end;

define method used-with-dynamic-extent?
  (comp :: <stack-vector>, temp) => (dynamic? :: <boolean>)
  // Even though the vector has dynamic extent, it doesn't mean the contents have.
  // In a more refined analysis I guess we could try to track uses of the vector,
  // but we are unlikely to win in many cases.
  #f
end;

define method used-with-dynamic-extent?
  (comp :: <any-slot-value>, temp) => (dynamic? :: <boolean>)
  #t
end;

define method used-with-dynamic-extent?
  (comp :: <slot-value-setter>, temp) => (dynamic? :: <boolean>)
  #f
end;

define method used-with-dynamic-extent?
  (comp :: <repeated-slot-value-setter>, temp) => (dynamic? :: <boolean>)
  #f
end;

define method used-with-dynamic-extent?
  (comp :: <initialize-closure>, temp) => (dynamic? :: <boolean>)
  #t
end;

// In the following "transfer" cases we check to see if uses of the
// computation's temporary have dynamic extent.

define method used-with-dynamic-extent?
  (comp :: <temporary-transfer-computation>, temp) => (dynamic? :: <boolean>)
  really-dynamic-extent?(comp.temporary)
end;

define method used-with-dynamic-extent?
  (comp :: <values>, temp) => (dynamic? :: <boolean>)
  really-dynamic-extent?(comp.temporary)
end;

define method used-with-dynamic-extent?
  (comp :: <merge>, temp) => (dynamic? :: <boolean>)
  really-dynamic-extent?(comp.temporary)
end;

define method used-with-dynamic-extent?
  (comp :: <primitive-call>, temp) => (dynamic? :: <boolean>)
  if (comp.arguments[0] == temp)
    let var = comp.primitive.model-variable-name;
    if (var & var.fragment-identifier = #"primitive-apply")
      #t
    else
      comp.primitive.primitive-dynamic-extent?
    end
  else
    comp.primitive.primitive-dynamic-extent?
  end;
end;

define method used-with-dynamic-extent?
  (comp :: <function-call>, temp) => (dynamic? :: <boolean>)

  if (comp.function == temp)
    // Calling a function could have the side-effect of embedding the method
    // inside something else, causing temp to have indefinite extent.  So we
    // should return #f here.  However, this (rare) case will be detected
    // elsewhere, when the assignment takes place, so it is better to return #t
    #t
  else
    let typ = type-estimate(comp.function);

    let dynamic-extent-of-parameters =
        get-extent-of-parameters-in-call(comp, typ, temp);

//    format-out("Dynamic extent of %= = %=\n",
//      comp.function, dynamic-extent-of-parameters);

    dynamic-extent-of-parameters == #t
    | (~empty?(dynamic-extent-of-parameters)
      & begin
          let arg-positions
            = choose-by(curry(\=, temp), comp.arguments, range(from: 0));
          every?(method (a) member?(a, dynamic-extent-of-parameters) end,
                 arg-positions)
        end )
  end
end;

define method used-with-dynamic-extent?
    (comp :: <apply>, temp) => (dynamic? :: <boolean>)
  if (comp.arguments.last == temp) #t else next-method() end
end;


// GET-EXTENT-OF-PARAMETERS-IN-CALL
//
//   Calculates the dynamic extent of the parameters to a call

define generic get-extent-of-parameters-in-call
  (call :: <call>, typ :: <type-estimate>, temp) => (extent :: <dynamic-extent>);

define method get-extent-of-parameters-in-call
    (call :: <call>, typ :: <type-estimate>, temp) => (extent :: <dynamic-extent>)
  $none-with-dynamic-extent // Let's be conservative...
end;

define method get-extent-of-parameters-in-call
    (call :: <call>, typ :: <type-estimate-limited-function>, temp)
        => (extent :: <dynamic-extent>)
  $none-with-dynamic-extent // The function could do anything, so take the worst case...
end;

define method get-extent-of-parameters-in-call
    (call :: <call>, typ :: <type-estimate-union>, temp)
        => (extent :: <dynamic-extent>)
  // TODO: return the intersection of the extents of each component.  The
  // typist needs to export more before we can do this.
  $none-with-dynamic-extent
end;

define method get-extent-of-parameters-in-call
    (call :: <function-call>, typ :: <type-estimate-limited-instance>, temp)
        => (extent :: <dynamic-extent>)

  local method intersect-extents(so-far, fun :: <&method>)
    let fun-extent = get-extent-of-parameters-for(fun);
    if (fun-extent == #t) so-far
    elseif (so-far == #t) fun-extent
    else
      choose(rcurry(member?, fun-extent), so-far)
    end
  end;
  local method corresponds-to-a-model-parameter(temp, call, model)
         => (model-parameter? :: <boolean>)
          if (instance?(model, <&function>))
            if (call-congruent?(call))
              #t
            else
              let signature = ^function-signature(model);
              let required-count = ^signature-number-required(signature);
              find-key(call.arguments, curry(\==, temp)) < required-count;
            end if
          end if;
        end;

  let model = typ.type-estimate-singleton;

  if (corresponds-to-a-model-parameter(temp, call, model))
    case
      instance?(model, <&method>) =>
        get-extent-of-parameters-for(model);

      instance?(model, <&generic-function>) =>
        // When we can't statically dispatch and all the applicable methods
        // are known at compile-time we construct the intersection of the
        // extents of the applicable methods.  At the moment the dispatch
        // optimization discards the list of applicable methods so as a temporary
        // measure we recompute it.
        // TODO: fix this...
        let arg-te* = argument-type-estimates(call);
        if (all-applicable-methods-guaranteed-known?(model, arg-te*))
          let (leading-sorted, others)
            = guaranteed-sorted-applicable-methods
                (^generic-function-methods-known(model), arg-te*);
          let extent-of-leading-sorted =
            reduce(intersect-extents, #t, leading-sorted);
          reduce(intersect-extents, extent-of-leading-sorted, others)
        else
          model.parameters-dynamic-extent | $none-with-dynamic-extent
        end;

      otherwise => $none-with-dynamic-extent;
    end case
  else
    $none-with-dynamic-extent
  end if
end method;



// REALLY-DYNAMIC-EXTENT?
//
//   Calculates whether a temporary has dynamic extent

define generic really-dynamic-extent?(temp :: <temporary>) => (dynamic? :: <boolean>);

define method really-dynamic-extent?(temp :: <temporary>) => (dynamic? :: <boolean>)
  let tde = temp.dynamic-extent?;

  if (tde == #"unknown")
    temp.dynamic-extent? := #"processing";
    let env = temp.environment.lambda-environment;
    local method used-with-dynamic-extent-in-same-lambda?
              (use :: <computation>) => (b :: <boolean>)
            let used-env = use.environment.lambda-environment;
            (used-env == env | closure-really-dynamic-extent?(lambda(used-env)))
              & used-with-dynamic-extent?(use, temp)
          end method;
    temp.dynamic-extent? :=
      every?(used-with-dynamic-extent-in-same-lambda?, temp.references)
  else
    tde == #t  // i.e. return #f if we detect a loop
  end
end;

define method closure-really-dynamic-extent? (lambda :: <&lambda>) => (dynamic? :: <boolean>)
  block (return)
    for (ref in references(lambda))
      when (instance?(ref, <make-closure>))
        return(really-dynamic-extent?(temporary(ref)))
      end when;
    end for;
    #t
  end block;
end method;

/*
define method closure-really-dynamic-extent? (temp :: <temporary>) => (dynamic? :: <boolean>)
  local method used-as-call? (ref :: <computation>)
          (instance?(ref, <function-call>) & ref.function == temp)
            | (instance?(ref, <initialize-closure>) & ref.computation-closure == temp)
        end method;
  really-dynamic-extent?(temp)
  //   | every?(used-as-call?, temp.references)
end method;
*/

// We can view unboxed and interned objects as having dynamic extent

define method really-dynamic-extent?(temp :: <lexical-specialized-variable>)
    => (dynamic? :: <boolean>)
  select (temp.specializer)
    dylan-value(#"<integer>")  => #t;
    dylan-value(#"<byte-character>") => #t;
    dylan-value(#"<boolean>") => #t;
    dylan-value(#"<symbol>") => #t;     // Check that this is sound...
    otherwise => next-method()
  end;
end;


// In the case of a #rest parameter we optimize away the copy-vector if we can
// determine the parameter has dynamic extent.

define method really-dynamic-extent?
    (rest-param :: <lexical-rest-variable>) => (dynamic? :: <boolean>);
  let uses = rest-param.references;
  if (uses.size == 1)
    let copy-computation = uses[0];
    if (instance?(copy-computation, <primitive-call>)
       & copy-computation.primitive == dylan-value(#"primitive-copy-vector"))
      let dyn-ext? = really-dynamic-extent?(copy-computation.temporary);
      if (dyn-ext?)
        replace-computation-with-temporary!(copy-computation, rest-param);
        // Ideally I'd like to color the #rest parameter, but the
        // source-location stuff doesn't appear to provide this information
        // in the case of local methods... So, I'll keep the facility for
        // dumping a textual description for now.
      end;
      dyn-ext?
    else // There must have been a %dynamic-extent
      #t
    end
  else // There must have been a %dynamic-extent
    #t
  end;
end;
