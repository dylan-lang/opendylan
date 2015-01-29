Module:   dfmc-conversion
Synopsis: The method definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Method class framework.

define constant $compiler-method-class-map = make(<table>);

define method define-compiler-method-class
    (tag :: <symbol>, method-class) => ()
  element($compiler-method-class-map, tag) := method-class
end method;

define method lookup-compiler-method-class (tag :: <symbol>) => (method-class)
  element($compiler-method-class-map, tag, default: #f)
    | error("Failed to resolve the compiler method-class %=.", tag);
end method;

define-compiler-method-class(#"simple", <&method>);
define-compiler-method-class(#"initializer", <&initializer-method>);
define-compiler-method-class(#"getter", <&getter-method>);
define-compiler-method-class(#"setter", <&setter-method>);
define-compiler-method-class(#"repeated-getter", <&repeated-getter-method>);
define-compiler-method-class(#"repeated-setter", <&repeated-setter-method>);
define-compiler-method-class(#"copy-down-method", <&copy-down-method>) ;

//// Method modeling.

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <method-definition>) => ()
  form-evaluation-tried-and-failed?(form) := #f;
  if (form-dynamic?(form))
    compute-and-install-form-dynamic-init-method(form);
  else
    compute-and-install-form-model-objects-statically(form);
  end;
end method;

define function get-form-generic-definition (form) => (gf-def, gf-static?)
  let binding = form-variable-binding(form);
  let gf-def  = binding-definition(binding, default: #f);
  let gf-static?
    = gf-def & if (current-library-description?(form-library(gf-def)))
                 // Have to be slightly careful about evaluation order, hence
                 // the implicitly defined check shortcut which is a precondition
                 // for model computation of the generic looking back at its
                 // methods.
                 form-implicitly-defined?(gf-def)
                   | binding-model-object(binding)
               else
                 binding-model-object(binding)
               end;
  values(gf-def, gf-static?)
end function;

define method compute-and-install-form-model-objects-statically
    (form :: <method-definition>) => ()
  let name = form-variable-name(form);
  let (gf-def, gf-static?) = get-form-generic-definition(form);
  if (~gf-static?)
    // Oops, we have to be dynamic after all.
    debug-out(#"dynamic", ">>> GF forced retreat to the dynamic case for %=\n", form);
    form-evaluation-tried-and-failed?(form) := #t;
    compute-and-install-form-dynamic-init-method(form);
  else
    // We're at least partly static;  the g.f. is known.  But if the method isn't, we have
    // to add it at load time, and check congruency too.
    let model = compute-form-model-object(form, name);
    let model
      = if (model
             & (form-equivalent-method-definition(gf-def) == form
                  | check-model-at-definition(model)))
          // The equivalent method definition check gets around potential
          // circularity problems.
          model
        else
          #f
        end;
    let lib = library-description-model(form-library(form));
    if (model)
      lambda-top-level?(model) := #t;
      form-model(form) := model;
    else
      form-evaluation-tried-and-failed?(form) := #t;
    end;
    let method-locally-defined? = form-library(gf-def) == form-library(form);
    unless (form-compile-stage-only?(gf-def)
              | (method-locally-defined? & model)
              | (~model & form-handled-by-make-when-dynamic?(form))
              // | (model & ~method-locally-defined?
              //      & single-method-generic-function?(lookup-model-object(name, reference?: #f)))
              )
      let code
        = (with-expansion-source-form(form)
             let mcode = if (~model)
                           let signature-and-body = form-signature-and-body-fragment(form);
                           #{  generic-method (?form) ?signature-and-body end }
                         else
                           #{ ?model }
                         end if;

             let gf-runtime-sealed? = (form-sealable?(gf-def) | form-compiler-open?(gf-def));

             // Do we, at runtime, need to add <method-domain> domain for this method?  We
             // do if it's a sealed method, and one is not going to be added statically in
             // the gf model.
             let mdomain? = ~gf-runtime-sealed? & form-sealed?(form);

             // We only need to check congruency for dynamically computed methods.
             let check-congruency? = if (model) #f else #t end;

             // Do we check sealing?
             // - We skip it (thus skipping the other hairier computations) if the gf is runtime-sealed.
             // - We can also skip it if we are in the defining library of the generic, as there's no
             //   way anyone could have put on a domain to exclude us.  (Being not in incremental
             //   definition mode we don't have to worry about out-of-order definitions.
             // - Finally, we can also skip it if some specialized type is defined in our own
             //   library, which has the same consequence.  This last check could be smarter for
             //   dynamically computed methods by looking at those specializers it can rather than
             //   just punting entirely in that case.
             let check-sealing? = (~gf-runtime-sealed?
                                     & ~method-locally-defined?
                                     & (~model
                                          | all-types-known-imported?
                                              (model-library(model),
                                               ^function-signature(model))));

             if (~check-congruency? & check-sealing? & ~mdomain?)
               let definer = dylan-value(#"%add-method");
               #{ ?definer(?name, ?mcode, ?lib) }
             elseif (gf-runtime-sealed? & check-congruency? & ~check-sealing? & ~mdomain?)
               let definer = dylan-value(#"%add-dynamic-method");
               #{ ?definer(?name, ?mcode) }
             else
               let definer = dylan-value(#"%add-a-method");
               #{ ?definer(?name, ?mcode, ?lib, ?check-congruency?, ?check-sealing?, ?mdomain?) }
             end if
            end with-expansion-source-form);
      let init-model = convert-top-level-initializer(code);
      form-init-method(form) := init-model;
    end unless;
  end if;
end method;

/*
define function ^method-generic-function-definition (object :: <&method>)
  let binding = model-variable-binding(object);
  binding-definition(binding);
end function;
*/

// Despite its name, this function actually gets used to create lambdas
// sometimes (by the C-callable constructor).

define method compute-method-explicitly
    (method-class :: <class>,
     form, name, signature-spec, body-spec,
     #rest options, #key, #all-keys)
      => (model :: <&method>)
  apply(^make,
        method-class,
        definition: form,
        body-spec: body-spec,
        // debug-name:
        //   ~form & name & mapped-model(as-lowercase(as(<string>, name))),
        compiler-debug-name: name,
        signature-spec: signature-spec,
        options)
end method;

define method install-method-signature
    (m :: <&lambda>, form :: <method-defining-form>, sig :: <&signature>)
 => ()
  ^function-signature(m) := sig
end method;

define method install-method-signature
    (m :: <&accessor-method>,
       form :: <method-defining-form>, sig :: <&signature>)
 => ()
  ^function-signature(m) := sig;
  // ^method-slot-descriptor(m) := compute-signature(form, signature-spec);
end method;

/*
define method install-method-signature
    (m :: <&getter-method>,
     form :: <method-defining-form>,
     signature-spec :: <signature-spec>)
  let req-spec = spec-argument-required-variable-specs(spec);
  let class-spec = req-spec[0];
  ^top-level-eval(class-spec
  if (instance?(method-object, <&lambda>))
    ^function-signature(method-object)
      := compute-signature(form, signature-spec);
  end if;
end method;
*/

define compiler-sideways method compute-form-model-object
    (form :: <method-defining-form>, name :: <variable-name-fragment>)
      => (model :: false-or(<&method>))
  let signature-spec = form-signature(form);
  let (sig-object, static?) = compute-signature(form, signature-spec);
  if (static?)
    let method-class
      = lookup-compiler-method-class(form-class(form));
    let method-object
      = compute-method-explicitly
          (method-class, form, name, signature-spec, form-body(form));
    install-method-signature(method-object, form, sig-object);
    // One final check here. If we get this far but it's an accessor
    // method and the class as a whole is dynamic, we still can't
    // have the method.
    if (instance?(method-object, <&accessor-method>))
      let class
        = ^signature-required(sig-object)
             [accessor-method-dispatch-arg(method-object)];
      if (^ensure-slots-initialized(class))
        // Install the slot descriptor before we return.
        let gf = lookup-model-object(name, default: #f, reference?: #f);
        if (gf)
          let slotd = ^slot-descriptor(class, gf);
          ^method-slot-descriptor(method-object) := slotd;
          method-object
        end;
      end;
    else
      method-object
    end;
  end;
end method;

define compiler-sideways method form-top-level-methods
    (form :: <method-definition>) => (methods :: <sequence>)
  let inits = next-method();
  let model = form-model(form);
  if (model)
    pair(model, inits)
  else
    inits
  end;
end method;

define compiler-sideways method retract-form-model-objects (form :: <method-definition>) => ()
  library-description-system-gf-init-code(form-library(form)) := #f;
  next-method()
end method;


// Note that maybe-compute-and-install-method-dfm is the client entry
// point, while compute-and-install-method-dfm is for implementors of
// the protocol. This allows us to relieve specific implementing methods
// of setting up the appropriate context, since we can guarantee that
// this has been done before compute-and-install-method-dfm has been
// called.

define method maybe-compute-and-install-method-dfm (m :: <&method>) => ()
end method;

define method maybe-compute-and-install-method-dfm (m :: <&lambda>) => ()
  unless (m.body | m.lambda-optimized?) // OR RETRACTED BUT 1ST GROKKED
    with-simple-abort-retry-restart
      ("Skip generating DFM for this method",
       "Retry generating DFM for this method")
      with-dependent-context($compilation of model-creator(m))
        compute-and-install-method-dfm(m);
      end;
    end;
  end unless;
end method;

define method retract-method-dfm (m) => ()
end method;

define method slot-initial-value-method? (f :: <&method>) => (res :: <boolean>)
  let creator = model-creator(f);
  instance?(creator, <class-definition>)
    | (~lambda-top-level?(f)
         & instance?(creator, <method-definition>)
         & form-class(creator) == #"initializer")
end method;

define method dodgy-method? (f :: <&lambda>) => (well? :: <boolean>)
  ~model-has-definition?(f) & lambda-top-level?(f)
end method;

define method method-indirectly-inlineable? (f :: <&lambda>) => (well? :: <boolean>)
  unless (lambda-top-level?(f))
    local method outer-lambda (f :: <&lambda>) => (res :: false-or(<&lambda>))
            let env       = environment(f);
            let outer-env = env & lambda-environment(outer(env));
            outer-env & lambda(outer-env)
          end method;
    iterate loop (outer :: false-or(<&lambda>) = outer-lambda(f))
      if (~outer)
        #f
      elseif (lambda-top-level?(outer))
        method-inlineable?(outer)
      else
        loop(outer-lambda(outer))
      end if;
    end iterate;
  end unless;
end method;

define method method-fragments-strippable?
    (x :: <&lambda>) => (well? :: <boolean>)
  #t
end method;

define variable *strip-enabled?* = #t;

define method method-dfm-strippable?
    (x :: <&lambda>) => (well? :: <boolean>)
  when (*strip-enabled?*)
    lambda-initializer?(x)
      | ~(method-inlineable?(x) | method-indirectly-inlineable?(x)
            | slot-initial-value-method?(x) | dodgy-method?(x) | lambda-copied-down?(x))
  end when;
end method;

define compiler-sideways method retract-body-fragments (m :: <&lambda>) => ()
  when (method-fragments-strippable?(m))
    when (lambda-body(m)) body-spec(m) := #f; end;
    let form = model-definition(m);
    if (instance?(form, <method-defining-form>))
      strip-incremental-slots(form);
    end if;
  end when;
end method;

define compiler-sideways method strip-incremental-slots (x :: <&lambda>)
  lambda-heap(x) := #f;
  retract-body-fragments(x);
  strip-incremental-slots(^iep(x));
end method;

define compiler-sideways method strip-incremental-slots (x :: <&iep>)
  code(x) := #f;
end method;

define method retract-method-dfm (m :: <&lambda>) => ()
  if (method-dfm-strippable?(m) & lambda-body(m))
    m.parameters         := #f;
    m.body               := #f;
    m.environment        := #f;
    m.users              := #();
    m.optimization-queue := #f;
    m.lambda-body        := #f;
  // format-out("RETRACTING %=\n", m);
  else
    lambda-body(m) & (m.optimization-queue := #f);
    when (lambda-top-level?(m))
      m.users            := #();
    end when;
  end if;
end method;

define method compute-and-install-method-dfm
    (method-object :: <&method>) => ()
end method;

define method compute-and-install-method-dfm
    (method-object :: <&lambda>) => ()
  let body = compute-method-body(method-object);
  if (body)
    with-parent-source-location  (model-source-location(method-object))
      convert-lambda-into*($top-level-environment, method-object, body);
      // format-out("COMPUTING DFM FOR %=\n", method-object);
    end;
    retract-body-fragments(method-object);
    // one of this and the one below is redundant
    lambda-optimized?(method-object) := #f;
  end if;
end method;

define method compute-method-body (m :: <&lambda>)
  body-spec(m)
end method;

/// SHOULD HAPPEN AFTER OPTIMIZATION BUT CONTROL WOULD HAVE TO
/// CHANGE TO NOT RETRACT UNTIL AFTER OPTIMIZATION DERIVED INLINEABILITY

define constant $maximum-inlining-cost = 0;

define generic computation-inlining-cost
    (c :: <computation>) => (res :: <integer>);

define method computation-inlining-cost
    (c :: <computation>) => (res :: <integer>)
  1
end method;

define method computation-inlining-cost
    (c :: <bind>) => (res :: <integer>)
  0
end method;

define method computation-inlining-cost
    (c :: <return>) => (res :: <integer>)
  0
end method;

define method computation-inlining-cost
    (c :: <nop-computation>) => (res :: <integer>)
  0
end method;

define method computation-inlining-cost
    (c :: <temporary-transfer-computation>) => (res :: <integer>)
  0
end method;

define method update-lambda-inlineable? (f :: <&lambda>)
  when (lambda-inlineable?(f) == #"unknown")
    let definition = model-definition(f);
    let inlineable?
      = if (lambda-top-level?(f) & definition
              & form-inline-policy(definition) == #"default-inline"
              & empty?(f.environment.inners)
              & ~instance?(f, <&copy-down-method>))
          let cost :: <integer> = 0;
          let inlineable?
            = block (return)
                walk-lambda-computations // ESTIMATE INLINING COST
                  (method (c)
                     walk-computation-references
                       (method (c, ref, object)
                          ignore(c); ignore(ref);
                          unless (inlineable?(object))
                            return(#f)
                          end unless
                        end method,
                        c);
                     cost := cost + computation-inlining-cost(c);
                     when (cost > $maximum-inlining-cost)
                       return(#f)
                     end when;
                   end method,
                   f.body);
                #t
              end block;
          // when (inlineable?)
          //   format-out("LAMBDA %= INLINEABLE %=\n", f, cost);
          // end when;
          inlineable?
        else
          #f
        end if;
    lambda-inlineable?(f) := inlineable?;
  end when;
end method;

define method method-inlineable? (f :: <&method>) => (res :: <boolean>)
  let definition = model-definition(f);
  if (definition)
    update-lambda-inlineable?(f);
    assert(lambda-inlineable?(f) ~== #"unknown",
           "uninitialized lambda-inlineable? slot %=", f);
    let policy = form-inline-policy(definition);
    ~(policy == #"not-inline" | policy == #"default-inline")
      | model-compile-stage-only?(f)
      | (policy ~== #"not-inline" & lambda-inlineable?(f) == #t)
  end if;
end method;

define method ensure-method-optimized (f :: <&method>)
  maybe-compute-and-install-method-dfm(f) ;
  run-compilation-passes(f);
end method;

define method ensure-method-model (f :: <&method>)
end method;

define method ensure-method-model (f :: <&lambda>)
  if (f.lambda-optimized?)  // quick check
    ensure-lambda-body(f);
  else
    block ()
      ensure-method-optimized(f)
    cleanup
      let ld = current-top-level-library-description();
      when (ld ~== model-library(f) & method-fragments-strippable?(f))
        retract-method-dfm(f);
      end when;
    end block;
  end if;
end method;

define method ensure-optimized-method-model (f :: <&method>)
  ensure-method-model (f)
end method;

define method ensure-method-dfm (f :: <&method>)
end method;

define method force-method-model (f :: <&lambda>)
  // retract-method-dfm(f);      // MAKE SURE CLEAN START
  lambda-optimized?(f) := #f; // FORCE DFM TO BE BUILT
end method;

define method ensure-method-dfm (f :: <&lambda>)
  unless (body(f))
    force-method-model(f);
  end unless;
  ensure-method-model(f);
end method;

define method ensure-method-dfm-or-heap (f :: <&method>)
end method;

define method ensure-method-dfm-or-heap (f :: <&lambda>)
  unless (lambda-heap(f) | body(f))
    force-method-model(f);
    maybe-compute-and-install-method-dfm(f);
  end unless;
end method;

// markt - copy-down support

define compiler-open generic copy-down-body (m :: <&copy-down-method>) => () ;

define method compute-and-install-method-dfm (m :: <&copy-down-method>) => ()
  next-method () ;
  let bind = m.body ;
  if (bind)
    copy-down-body (m)  // this is in optimization/inlining, but exported from conversion
  else
    format-out ("seem not to have dummy-body for a copy-down %s\n", m)
  end;
end;

//// Dynamic incremental version

define method compute-and-install-form-dynamic-init-method
    (form :: <method-definition>) => ()
  // Filter out accessor methods, which are created and added dynamically
  // within make.
  if (~form-handled-by-make-when-dynamic?(form))
    next-method();
  end;
end method;

define method compute-form-dynamic-init-code
    (form :: <method-definition>) => (computed-method)
  let name               = form-variable-name(form);
  let ld                 = form-library(form);
  let lib                = library-description-model(ld);
  let signature-and-body = form-signature-and-body-fragment(form);
  let definer
    = if (form-sealed?(form))
        dylan-value(#"%define-sealed-method");
      else
        dylan-value(#"%define-method");
      end if;
  (with-expansion-source-form (form)
     #{ ?definer(?name, generic-method (?form) ?signature-and-body end, ?lib) }
  end with-expansion-source-form)
end method;
