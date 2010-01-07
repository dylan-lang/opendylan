Module:   dfmc-conversion
Synopsis: Shared code between define constant and define variable.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function empty-method? (m :: <&method>) => (empty? :: <boolean>)
  // method returns no values, has no side-effects
  let body = body(m);
  if (body)
    let vals = next-computation(body);
    instance?(vals, <values>) &
      empty?(fixed-values(vals)) &
      ~rest-value(vals) &
      instance?(next-computation(vals), <return>)
  else
    #t // no body so it's empty
  end if;
end function;

// TODO: figure out which other model objects want definitions

define method install-definition (model-object, form, name) => ()
end method;

define method install-definition 
    (model-object, form :: <booted-constant-definition>, name) => ()
  model-definition(model-object) := form;
end method;

define method install-definition 
    (model-object :: <&type>, form :: <binding-defining-form>, name) => ()
  unless (model-has-definition?(model-object))
    model-definition(model-object) := form;
  end unless;
end method;

define method install-definition 
    (model-object :: <&namespace>, form :: <binding-defining-form>, name) => ()
  unless (model-has-definition?(model-object))
    model-definition(model-object) := form;
  end unless;
end method;

define method install-definition 
    (model-object :: <&signature>, form :: <binding-defining-form>, name) => ()
  unless (model-has-definition?(model-object))
    model-definition(model-object) := form;
  end unless;
end method;

define method install-definition 
    (model-object :: <&method>, form :: <binding-defining-form>, name) => ()
  unless (model-has-definition?(model-object))
    model-definition(model-object) := form;
    unless (^debug-name(model-object))
      ^debug-name(model-object) := mapped-model(as-lowercase(as(<string>, name)));
    end unless;
  end unless;
end method;

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <binding-defining-form>) => ();
  let names = form-variable-names(form);
  let init-model :: <&method> =
    convert-top-level-initializer
    (generate-binding-defining-fragment(form),
     debug-name: 
       generate-variable-names-debug-name(names));
  maybe-compute-and-install-method-dfm(init-model);
  // Type infer init-model
  type-initializer-method(init-model);
  run-compilation-passes(init-model);
  let complete? :: <boolean> = #t ;
  unless(empty-method?(init-model))
    form-init-method(form) := init-model;
    complete? := #f ;
  end;
  // now set definitions where appropriate
  for (variable-name in names)
    if (form-defines-variable?(form, variable-name))
      let binding = lookup-binding(variable-name, reference?: #f);
      let (model-object, computed?) =
	untracked-binding-model-object-if-computed(binding);
      if (computed?)
	install-definition (model-object, form, variable-name);
      end;
    end;
  end;
end method;

// TODO: May have to extend this to other things like FFI definitions.
define method form-required-static? 
    (form :: <literal-value-binding-defining-form>) => (well? :: <boolean>)
  // The magic constants defined for libraries and modules.
  instance?(form-parent-form(form), <namespace-defining-form>)
end method;

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <literal-value-binding-defining-form>) => ();
  // Thread variable locations always have to be dynamically allocated and
  // initialized, so... 
  // We don't want to fold away constants in loose mode either, so if
  // the definition's dynamic, don't expose its value.
  if ((form-thread?(form) | form-dynamic?(form) | form-redefinition?(form))
        & ~form-required-static?(form))
    next-method();
  else
    let name = form-variable-name(form);
    let model-object =
      make-compile-time-literal(fragment-value(form-init-expression(form)));
    let var-type =
      lookup-constant-model-object
      (#f,
       spec-type-expression
	 (first(spec-value-required-variable-specs(form-bindings-spec(form)))));
    if (var-type &
	  ^instance?(model-object, var-type))
      define-model-object-and-type(name, model-object, var-type);
      install-definition (model-object, form, name);
    else
      next-method();
    end;
  end;
end method;

// Special cases for constant method definitions.
// Treat them more like define method.
// This is mostly to keep stack depth down while doing lazy compilation.

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <constant-method-definition>) => ()
  form-evaluation-tried-and-failed?(form) := #f;
  if (form-dynamic?(form) | form-redefinition?(form))
    next-method(); // compute-and-install-form-dynamic-init-method(form);
  else
    let name = form-variable-name(form);
    if (form-defines-variable?(form, name))
      let model = compute-form-model-object(form, name);
      if (model)
        lambda-top-level?(model) := #t;
        define-model-object(name, model);
      else
	form-evaluation-tried-and-failed?(form) := #t;
        next-method();
      end;
    end;
  end if;
end method;

define compiler-sideways method form-top-level-methods 
    (form :: <constant-method-definition>) => (methods :: <sequence>)
  let binding = form-variable-binding(form);
  let model = untracked-binding-model-object-if-computed(binding);
  if (form == untracked-binding-definition(binding, default: #f) & model)
    list(model)
  else
    next-method()
  end
end method;

// Transform:
//
//   define xxx (x, y) = foo();
//
// to:
//
//   let (x-tmp, y-tmp) = foo();
//   %initialize-binding(x, x-tmp);
//   %initialize-binding(y, y-tmp);

define method form-init-binding-expression (form) => (form)
  form-init-expression(form)
end method;

define method form-init-binding-expression 
    (form :: <constant-method-definition>) => (form)
  let init-expression = form-init-expression(form);
  if (method-fragment?(init-expression))
    let method-form = fragment-argument(init-expression);
    #{ constant-method (?form) ?method-form end }
  else
    init-expression
  end if;
end method;

define method generate-binding-defining-fragment
    (form :: <binding-defining-form>) => (defining-fragment)
  with-expansion-source-form (form)
    let bindings-spec = form-bindings-spec(form);
    let init-expression = form-init-binding-expression(form);
    let all-var-specs
      = concatenate(spec-value-required-variable-specs(bindings-spec),
                    if (spec-value-rest?(bindings-spec)) 
                      list(spec-value-rest-variable-spec(bindings-spec))
                    else
                      #()
                    end);
    let type-bindings = map(generate-type-binding, all-var-specs);
    let tmp-binding = map(generate-tmp-binding, all-var-specs, type-bindings);
    let binding-init 
      = map(curry(generate-binding-init, form), all-var-specs, type-bindings);
    let type-binding-decls 
      = map(method (type-binding, var-spec)
              let type-expression = spec-type-expression(var-spec);
              #{ let ?type-binding :: <type> = ?type-expression; } 
            end,
            type-bindings,
            all-var-specs);
    as-body
      (#{ ??type-binding-decls ...
          let (??tmp-binding, ...) = ?init-expression;
          ??binding-init; ... });
  end;
end method;

define method generate-type-binding (spec :: <required-variable-spec>)
  let name = spec-variable-name(spec);
  #{ "_" ## ?name ## "-type_" }
end method;

define method generate-tmp-binding 
    (spec :: <required-variable-spec>, type-name)
  let name = spec-variable-name(spec);
  #{ ?name ## "-tmp" :: ?type-name }
end method;

define method generate-tmp-binding 
    (spec :: <rest-variable-spec>, type-name)
  let name = spec-variable-name(spec);
  if (type-name)
    #{ #rest ?name ## "-tmp" :: ?type-name }
  else
    #{ #rest ?name ## "-tmp" }
  end
end method;

define method generate-binding-init 
    (form, spec :: <variable-spec>, type-name)
  let name = spec-variable-name(spec);
  if (form-defines-variable?(form, name))
    #{ %initialize-binding-type(?name, ?type-name);
       %initialize-binding(?name, ?name ## "-tmp") }
  else
    #{ #f }
  end;
end method;

// Generate a debug name from a set of variables.

define method generate-variable-names-debug-name (names)
  let strings = map(curry(as, <string>), names);
  local method splice (s1, s2)
    concatenate(s1, "/", s2);
  end;
  concatenate(reduce1(splice, strings), "-initializer");
end method;
