Module:   dfmc-namespace
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Top-level module bindings.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: should move these to common.
define macro with-library-context
  { with-library-context (?:expression) ?:body end }
    => { do-with-library-context(method () ?body end, ?expression) }
end macro;

define macro with-dood-context
  { with-dood-context (?ld:expression) ?:body end }
  =>
  { if (*interactive-compilation-layer*)
      dynamic-bind (*interactive-compilation-layer* = #f,
                    *library-description* = #f)
        with-library-context (?ld)
          ?body
        end
      end
    else
      with-library-context (?ld)
        ?body
      end
    end }
end macro;



//// Bindings.

define abstract compiler-open class <binding> (<object>)
end class;

define compiler-open generic rest-variable? (binding);
define compiler-open generic keyword-variable? (binding);

define method rest-variable? (binding :: <binding>) 
  #f
end method;

define method keyword-variable? (binding :: <binding>) 
  #f
end method;

define abstract dood-class <module-binding> 
    (<dood-dfmc-object>, <binding>, <variable-name>)
  lazy slot shadowable-binding-local-dependents :: <sequence> = #[];
end;

// Seal construction over the <module-binding> world.

define sealed domain make (subclass(<module-binding>));
define sealed domain initialize (<module-binding>);

define inline function binding-local-dependents (b :: <module-binding>)
 => (deps :: <sequence>);
  binding-local-dependents-in-context(current-library-description(), b)
end;

define inline function register-binding-dependent (b :: <module-binding>, dep)
  register-binding-dependent-in-context(current-library-description(), b, dep)
end;

define inline function unregister-binding-dependent (b :: <module-binding>, dep)
  unregister-binding-dependent-in-context(current-library-description(), b, dep)
end;

// moved to library-description.dylan, gts, 98jun26:
// define method binding-local-dependents-in-context
//     (ld :: <project-library-description>, binding :: <module-binding>)

// moved to library-description.dylan, gts, 98jun26:
// define method register-binding-dependent-in-context
//     (ld :: <project-library-description>, b :: <module-binding>, dep)

// moved to library-description.dylan, gts, 98jun26:
// define method unregister-binding-dependent-in-context
//    (ld :: <project-library-description>, b :: <module-binding>, dep)

//// Module bindings.

define macro binding-properties-definer
  { define ?adjectives:* binding-properties of ?class:name ?:name (?supers:*)
      ?slots:*
     end }
    => { binding-properties-class (?adjectives) ?name (?supers) ?slots end;
         binding-properties-accessors (?class) ?slots end }
end macro;

define macro binding-properties-class
  { binding-properties-class (?adjectives:*) ?:name (?supers:*) ?cslots end }
    => { define ?adjectives dood-class ?name (?supers) ?cslots end }
cslots:
  { } => { }
  { ?cslot:*; ...} => { ?cslot; ...}
cslot:
  { ?mods:* slot ?slot-name :: ?type:expression = ?init:expression, ?rest:* }
    => { ?mods slot ?slot-name :: ?type = ?init, ?rest }
slot-name:
  { ?:name } => { "shadowable-" ## ?name }
end macro;

define macro binding-properties-accessors
  { binding-properties-accessors (?class:name) end } => { }
  { binding-properties-accessors (?class:name)
      ?mods:* slot ?accessor:name :: ?type:expression = ?default:expression,
          ?rest:*;
      ?more:*
    end }
    => { define method ?accessor (object :: ?class) => (val :: ?type)
           let p
             = binding-properties-in-context
                 (current-library-description(), object, #f);
           if (p) "shadowable-" ## ?accessor(p) else ?default end
         end method;
         define method ?accessor ## "-setter" (value, object :: ?class)
           let p
             = binding-properties-in-context
                 (current-library-description(), object, #t);
           "shadowable-" ## ?accessor ## "-setter" (value, p)
         end method;
         binding-properties-accessors (?class) ?more end }
end macro;

define constant <definitions>           = <list>;
define constant <models>                = <definitions>;
define constant <definitions-or-models> = type-union(<definitions>, <models>);

define abstract binding-properties of <module-binding>
     <module-binding-properties> (<dood-dfmc-object>)
  lazy slot binding-local-modifying-definitions :: <definitions> = #(),
    init-keyword: modifying-definitions:;
  lazy slot %binding-local-modifying-models :: false-or(<models>) = #f;
end;

ignore(%binding-local-modifying-models);

define method shadowable-binding-previous-definition 
    (x :: <module-binding-properties>) => (res)
  #f
end method;

define method shadowable-binding-previous-definition-setter 
    (value, x :: <module-binding-properties>)
end method;

define inline function binding-value-slot
    (binding :: <module-binding>) => (res)
  untracked-binding-model-object-if-computed(binding)
end function;

define inline function binding-value-slot-setter
    (value, binding :: <module-binding>) => (res)
  debug-assert(value == untracked-binding-model-object-if-computed(binding),
               "unsupported use of binding-value-slot-setter");
  value
end function;

// Seal construction over the <module-binding-properties> world.

define sealed domain make (subclass(<module-binding-properties>));
define sealed domain initialize (<module-binding-properties>);

define method dood-disk-object 
    (dood :: <dood>, b :: <module-binding>) => (disk-object)
  let pdeps = b.private-shadowable-binding-local-dependents;
  // check if lazy cause then already vector cause in db
  unless (dood-lazy-value?(pdeps)) 
    let deps = b.shadowable-binding-local-dependents;
    let vdeps = as(<vector>, deps);
    unless (vdeps == deps)
      b.shadowable-binding-local-dependents := vdeps;
    end;
  end unless;
  next-method()
end;

define method named? (binding :: <module-binding>) => (yes :: singleton(#t))
 #t
end;

define generic binding-canonical-binding
    (binding :: <module-binding>) => (canonical-binding);

define generic binding-variable-name 
    (binding :: <module-binding>) => (variable-name);

define generic remove-binding-definition
    (binding :: <module-binding>, definition) => ();

define generic binding-model-object
    (binding :: <module-binding>, #key default, error-if-circular?)
 => (model);

define generic binding-accessible-to-other-libraries?
    (binding :: <module-binding>) => (result :: <boolean>);


define method binding-compilation-record
    (binding :: <module-binding>) => (cr :: false-or(<compilation-record>))
  let definition = untracked-binding-definition(binding, default: $unfound);
  found?(definition) & form-compilation-record(definition)
end method;

define program-warning <serious-duplicate-definition>
  slot condition-binding, required-init-keyword: binding:;
  slot condition-active-definition, required-init-keyword: active:;
  slot condition-duplicate-definition, required-init-keyword: duplicate:;
  format-string "Duplicate definition %= for %= ignored, conflicts with %=";
  format-arguments duplicate, binding, active;
end;

define serious-program-warning <imported-binding-definition>
  slot condition-binding, required-init-keyword: binding:;
  format-string "Attempt to define variable %s owned by another library, definition ignored.";
  format-arguments binding;
end;

define sideways method form-incremental? 
    (form :: <top-level-form>) => (well? :: <boolean>)
  *interactive-compilation-layer* ~== #f
end method;

define sideways method form-redefinition? 
    (form :: <variable-defining-form>) => (well? :: <boolean>)
  binding-previously-defined?(form-variable-binding(form))
end method;

define method add-definition (name, definition) => ()
  let binding = lookup-binding(name, reference?: #f);
  let old = untracked-binding-definition(binding, default: $unfound);
  binding-defined?(binding) := #t;
  if (binding-imported-into-library?(binding))
    note(<imported-binding-definition>, 
         source-location: form-source-location(definition),
         binding: binding);
    add-local-duplicate-definition(binding, definition);
  elseif (unfound?(old))
    note-adding-definition(binding, definition);
    binding.binding-active-definition := definition;
  elseif (form-incremental?(definition))
    // format-out("Redefining %= as %=\n", definition, old);
    remove-definition(name, old);
    note-adding-definition(binding, definition);
    binding.binding-previous-definition := old;
    binding.binding-active-definition := definition;
  elseif (defined-after?(definition, old)
            | (form-implicitly-defined?(old) &
                 current-library-description?(form-original-library(old))))
    debug-assert(current-library-description?(form-original-library(old)));
    retract-top-level-form(old);
    add-definition(name, definition);
  else
    note(<serious-duplicate-definition>,
         source-location: form-source-location(definition),
         binding: binding,
         active: old,
         duplicate: definition);
    add-local-duplicate-definition(binding, definition);
  end;
end method;

// Does no checks, triggers no dependencies. Only called when the binding
// has been determined to be undefined.
define method add-missing-definition 
    (name :: <variable-name-fragment>, def :: <variable-defining-form>) => ()
  let binding = lookup-binding(name, reference?: #f);
  binding.binding-active-definition := def;
end method;

define method remove-definition (name, definition) => ()
  let binding = untracked-lookup-binding(name);
  remove-binding-definition(binding, definition);
end method;

define method retract-modifying-models (binding :: <module-binding>) => ()
  %binding-local-modifying-models(binding) := #f;
end method;

define method add-modifying-definition (name, definition) => ()
  let binding = lookup-binding(name, reference?: #f);
  debug-assert(~member?(definition, binding.binding-local-ignored-definitions));
  retract-imported-binding(binding); // clear cache
  retract-modifying-models(binding); // clear cache
  note-adding-modifying-definition(binding, definition);
  binding-local-modifying-definitions(binding) 
    := add-local-definition(binding-local-modifying-definitions(binding), definition);
end method;

define compiler-open generic add-local-definition
  (definitions :: <definitions>, definition) => (new-definitions :: <definitions>);

define method add-local-definition
  (definitions :: <definitions>, definition) => (new-definitions :: <definitions>)
  add!(definitions, definition);
end method;

define method ignore-modifying-definition (name, definition) => ()
  let binding = untracked-lookup-binding(name);
  add-local-ignored-definition(binding, definition);
end method;

define method remove-modifying-definition (name, definition) => ()
  let binding = untracked-lookup-binding(name);
  let ignored? = remove-local-ignored-definition(binding, definition);
  unless (ignored?)
    retract-imported-binding(binding); // clear cache
    retract-modifying-models(binding); // clear cache
    note-removing-modifying-definition(binding, definition);
    binding-local-modifying-definitions(binding) 
      := remove!(binding-local-modifying-definitions(binding), definition);
  end;
end method;

define method lookup-certain-modifying-models 
    (name, form-predicate :: <function>, #key imported-only? = #f)
 => (models :: <models>)
  binding-certain-modifying-models
    (lookup-binding(name, reference?: #f), form-predicate, 
       imported-only?: imported-only?)
end method;

define method untracked-binding-defined-methods
    (binding :: <module-binding>) => (methods)
  #f
end method;

define method untracked-binding-defined-methods-setter
    (new-methods, binding :: <module-binding>)
 => (definitions)
end method;

define method untracked-binding-defined-domains
    (binding :: <module-binding>) => (domains)
  #f
end method;

define method untracked-binding-defined-domains-setter
    (new-domains, binding :: <module-binding>)
 => (definitions)
end method;

define method eval-certain-modifying-definitions!
    (models-or-forms :: <definitions>, form-predicate :: <function>)
 => (models :: <models>)
  // patch in certain evaluated definitions
  iterate loop (pair :: <list> = models-or-forms)
    unless (empty?(pair))
      let model-or-definition = head(pair);
      when (form-predicate(model-or-definition))
        head(pair) := untracked-ensure-form-model(model-or-definition);
      end when;
      loop(tail(pair))
    end unless;
  end iterate;
  models-or-forms
end method;

define inline method shadowable-binding-certain-local-modifying-models 
    (properties :: <module-binding-properties>, form-predicate :: <function>)
 => (models :: <models>)
  let models-or-forms
    = shadowable-%binding-local-modifying-models(properties)
        | (shadowable-%binding-local-modifying-models(properties)
             := copy-sequence
                  (shadowable-binding-local-modifying-definitions(properties)));
  // TODO: COULD HAVE A BIT TO SAY WHETHER ALL DEFINITIONS ARE EVALUATED
  eval-certain-modifying-definitions!(models-or-forms, form-predicate);
  models-or-forms
end method;

define method binding-certain-local-modifying-models 
    (binding :: <module-binding>, form-predicate :: <function>)
 => (models :: <models>)
  let p
    = binding-properties-in-context
       (current-library-description(), binding, #f);
  if (p) 
    shadowable-binding-certain-local-modifying-models(p, form-predicate)
  else
    #() 
  end
end method;

define method binding-definition
    (binding :: <module-binding>, #key default = unsupplied()) => (definition)
  note-binding-dependency(binding, dep$active-definition);
  untracked-binding-definition(binding, default: default)
end method;

define method binding-modifying-definitions (binding :: <module-binding>)
 => (definitions :: <definitions>)
  note-binding-dependency(binding, dep$modifying-definitions);  
  untracked-binding-modifying-definitions(binding);
end method;

define method binding-certain-modifying-models 
    (binding :: <module-binding>, form-predicate :: <function>,
       #key imported-only? = #f)
 => (models :: <models>)
  note-binding-dependency(binding, dep$modifying-definitions);  
  untracked-binding-certain-modifying-models
    (binding, form-predicate, imported-only?: imported-only?);
end method;

define method binding-defined-methods 
    (binding :: <module-binding>) => (methods)
  note-binding-dependency(binding, dep$modifying-definitions);  
  untracked-binding-defined-methods(binding);
end method;

define method binding-defined-methods-setter
    (new-methods, binding :: <module-binding>) => (methods)
  note-binding-dependency(binding, dep$modifying-definitions);  
  untracked-binding-defined-methods(binding) := new-methods
end method;

define method binding-defined-domains 
    (binding :: <module-binding>) => (domains)
  note-binding-dependency(binding, dep$modifying-definitions);  
  untracked-binding-defined-domains(binding);
end method;

define method binding-defined-domains-setter
    (new-domains, binding :: <module-binding>) => (domains)
  note-binding-dependency(binding, dep$modifying-definitions);  
  untracked-binding-defined-domains(binding) := new-domains
end method;


define method variable-defined? (name) => (defined?)
  defined?(lookup-binding(name, reference?: #f))
end method;

define function form-defines-variable?(form, name)
  let binding = untracked-lookup-binding(name);
  form == untracked-binding-definition(binding, default: #f)
end function;

define compiler-open generic form-ignored-internal? (form);
define compiler-open generic form-ignored? (form);

define method form-ignored-internal? (form :: <modifying-form>)
  // Simplification so we can use LOCAL-ignored-definitions
  debug-assert(current-library-description?(form-library(form)),
               "General case not implemented");
  let binding = form-variable-binding(form);
  member?(form, binding.binding-local-ignored-definitions)
end method;

// This method must be overridden by multiple-binding forms
define method form-ignored-internal? (form :: <variable-defining-form>)
  ~form-defines-variable?(form, form-variable-name(form))
end method;

// Don't really have a general way to officially ignore other forms..
define method form-ignored-internal? (form :: <top-level-form>)
  #f
end method;

define function binding-previously-defined? 
    (binding :: <module-binding>) => (well? :: <boolean>)
  binding-previous-definition(binding) ~== #f
end function;

define compiler-open generic binding-constant-model-object
    (binding :: <binding>, #key error-if-circular? = #t)
 => (model-object, found? :: <boolean>);

// This mirrors the interface of the above function, but works only 
// on module bindings, constant or not.

define function binding-constant-type-model-object
    (binding :: <module-binding>, #key error-if-circular? = #t)
 => (model-object, found? :: <boolean>)
  let model-object =
    binding-type-model-object
      (binding, default: $unfound,
         error-if-circular?: error-if-circular?);
    if (found?(model-object)) // & ~instance?(model-object, <unknown>)
      values(model-object, #t)
    else
      values(#f, #f)
    end
end function;

// TODO: Make the access chain leading to this circular reference 
// accessible so that we can store it in the condition to help 
// explain the problem.

define program-warning <circular-reference>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "The definition of %= is circular.";
  format-arguments variable-name;
end program-warning;

define function binding-model-access-denied? 
    (binding :: <module-binding>) => (well? :: <boolean>)
  let ld = current-library-description();
  instance?(ld, <interactive-library-description>)
    & binding.binding-home.home-library ~== dylan-library()
end function;

define method binding-model-object (binding :: <module-binding>,
                                    #key default = #f,
                                    error-if-circular? = #t)
 => (model)
  let model = untracked-binding-model-object(binding, error-if-circular?);
  if (model == $binding-model-not-computed)
    note-binding-dependency(binding, dep$active-definition);
    default
  else
    note-binding-dependency
      (binding, dep$active-definition + dep$model-object);
    model
  end
end method;

// TODO: Loose binding between tight mode libraries may also hide model
// objects.

/*
define method binding-model-object-hidden?
    (binding :: <module-binding>) => (well? :: <boolean>)
  let definition = binding.binding-active-definition;
  form-dynamic?(definition)
    & ~form-compile-stage-only?(definition) 
end method;
*/

define method untracked-binding-model-object
    (binding :: <module-binding>, error-if-circular?)
  // Do a quickie test first.
  let value = binding-cached-model-object(binding);
  if (value ~== $binding-model-not-computed
        // & ~binding-model-object-hidden?(binding)
     )
    value
  else
    let definition = binding.binding-active-definition;
    if (~definition)
      $binding-model-not-computed
    elseif (form-models-installed?(definition))
      if (error-if-circular? &
            form-models-installed?(definition) == #"processing")
        note(<circular-reference>,
             source-location: form-source-location(definition),
             variable-name:   binding-variable-name(binding));
        gts-debug("circ", "ubmo, circular - returning <object>.\n");
        dylan-value(#"<object>");
      else
        $binding-model-not-computed
      end if;
    elseif (form-dynamic?(definition))
    // elseif (binding-model-object-hidden?(binding))
      // BREAK("Looking up models for dynamic form %s", definition);
      $binding-model-not-computed
    else
      compute-cached-binding-model-object-in
        (current-library-description(), binding, definition)
    end;
  end
end method;

define method binding-model-object-setter
    (model, binding :: <module-binding>) => (model)
  debug-assert(*current-dependent* == binding.untracked-binding-definition);
  binding-cached-model-object(binding) := model
end method;

define method binding-type-model-object
    (binding :: <module-binding>,
     #key default = #f,
     error-if-circular? = #t) => (model)
  if (binding-model-access-denied?(binding))
    note-binding-dependency(binding, dep$active-definition);
    default
  else
    let definition = untracked-binding-definition(binding, default: #f);
    let value = if (~definition)
                  $binding-model-not-computed
                elseif (form-models-installed?(definition) == #"processing")
                  if (error-if-circular?)
                    note(<circular-reference>,
                         source-location: form-source-location(definition),
                         variable-name:   binding-variable-name(binding));
                    break("Circularity");
                  end;
                  $binding-model-not-computed
                else
                  maybe-compute-and-install-form-model-objects(definition);
                  binding-cached-type-model-object(binding);
                end;
    if (value == $binding-model-not-computed)
      note-binding-dependency
        (binding, dep$active-definition);
      default
    else
      // TODO: CORRECTNESS: A new dependency id for the model class?
      note-binding-dependency
        (binding, dep$active-definition + dep$model-object);
      value
    end;
  end;
end method;

define method binding-type-model-object-setter
    (model, binding :: <module-binding>) => (model)
  debug-assert(*current-dependent* == binding.untracked-binding-definition);
  binding-cached-type-model-object(binding) := model
end method;


define function untracked-binding-model-object-if-computed
    (binding :: <module-binding>) => (model-object, computed? :: <boolean>)
  let value = binding-cached-model-object(binding);
  if (value == $binding-model-not-computed)
    values(#f, #f)
  else
    values(value, #t)
  end
end;

// Caller is responsible for recording a dependency on dep$modifying-models
// if the model is actually used.
define method untracked-ensure-form-model (form :: <modifying-form>)
  if (form-dynamic?(form))
    #f
  else
    form-model(form) 
      | compute-cached-form-model-in(form-library(form), form)
  end
end method;


define function untracked-retract-binding-model-object
    (binding :: <module-binding>) => ()
  binding-cached-model-object(binding) := $binding-model-not-computed;
  binding-cached-type-model-object(binding) := $binding-model-not-computed;
end function;

define function retract-binding-model-object
    (binding :: <module-binding>) => ()
  note-removing-model-object(binding);
  untracked-retract-binding-model-object(binding);
end function;

define method define-model-object (name, model) => ()
  // Don't need to track this lookup since only defined model if previously
  // defined a definition.
  let binding = untracked-lookup-binding(name);
  binding-model-object(binding) := mapped-model(model);
  // It's important to do the install above before looking up <object>
  // since <object> itself is defined through this route.
  binding-type-model-object(binding) := dylan-value(#"<object>");
end method;

define method define-model-object-and-type (name, model, type) => ()
  // Don't need to track this lookup since only defined model if previously
  // defined a definition.
  let binding = untracked-lookup-binding(name);
  binding-model-object(binding) := mapped-model(model);
  binding-type-model-object(binding) := mapped-model(type);
end method;

define method lookup-model-object
    (name, #key reference? = #t, 
                default = unsupplied(), 
                error-if-circular? = #t)
 => (model-object)
  let binding = lookup-binding (name, reference?: reference?);
  let model-object =
    binding-model-object
      (binding, default: $unfound, error-if-circular?: error-if-circular?);
  if (found?(model-object))
    model-object
  else
    debug-assert(supplied?(default),
                 "No model-object found for %= and no default supplied.",
                 name);
    default
  end
end method;

define function lookup-value (name) => (value)
  binding-model-object(lookup-binding(name))
end;

//// Canonical module bindings.

define class <binding-model-not-computed> (<object>) end;

define constant $binding-model-not-computed = make(<binding-model-not-computed>);

define class <dood-binding-model-not-computed-proxy> (<dood-proxy>)
end class;

define method dood-disk-object 
    (dood :: <dood>, object :: <binding-model-not-computed>) => (object)
  dood-as-proxy(dood, object, dood-make-binding-model-not-computed-proxy)
end method;

define method dood-make-binding-model-not-computed-proxy
    (dood :: <dood>, object :: <binding-model-not-computed>) => (object)
  make(<dood-binding-model-not-computed-proxy>)  
end method;

define method dood-restore-proxy 
    (dood :: <dood>, proxy :: <dood-binding-model-not-computed-proxy>) 
 => (object)
  $binding-model-not-computed
end method;

// These represent a binding at its point of creation.

define class <canonical-module-binding> (<module-binding>)
  constant slot name :: <symbol>, required-init-keyword: name:;
  constant slot binding-home :: <module>, required-init-keyword: home:;
  slot canonical-binding-properties :: false-or(<canonical-module-binding-properties>) = #f;
end;

define binding-properties of <canonical-module-binding>
  <canonical-module-binding-properties> (<module-binding-properties>)
  weak slot emitted-name = #f,
    reinit-expression: #f;
  slot binding-property-bits :: <integer> = 0;
  lazy slot binding-active-definition /* :: false-or(<top-level-form>) */ = #f,
    init-keyword: definition:;
  lazy slot %binding-cached-model-object = $binding-model-not-computed,
    init-keyword: cached-model:;
  lazy slot binding-cached-type-model-object = $binding-model-not-computed;
end;

// HACK: WARNINGS
ignore(binding-property-bits);
ignore(binding-property-bits-setter);
ignore(%binding-cached-model-object);
ignore(%binding-cached-model-object-setter);

binding-properties-accessors (<canonical-module-binding>)
  slot binding-previous-definition :: <object> = #f;
end;

define leaf packed-slots shadowable-binding-property-bits
    (<canonical-module-binding-properties>, <object>)
  boolean  slot shadowable-binding-hollow-model-object? = #f;
  tristate slot shadowable-binding-macro-class?         = #"unknown";
  boolean  slot shadowable-binding-defined?             = #f;
end packed-slots;

define method initialize
    (x :: <canonical-module-binding-properties>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, x, all-keys)
end method;

define inline method shadowable-binding-cached-hollow-model-object 
    (p :: <canonical-module-binding-properties>) => (model)
  if (shadowable-binding-hollow-model-object?(p))
    shadowable-%binding-cached-model-object(p)
  else
    $binding-model-not-computed
  end if
end method;

define inline method shadowable-binding-cached-hollow-model-object-setter
    (model, p :: <canonical-module-binding-properties>)
  shadowable-binding-hollow-model-object?(p) := #t;
  shadowable-%binding-cached-model-object(p) := model;
end method;

define inline method shadowable-binding-cached-model-object 
    (p :: <canonical-module-binding-properties>) => (model)
  if (shadowable-binding-hollow-model-object?(p))
    $binding-model-not-computed
  else
    shadowable-%binding-cached-model-object(p)
  end if
end method;

define inline method shadowable-binding-cached-model-object-setter
    (model, p :: <canonical-module-binding-properties>)
  shadowable-binding-hollow-model-object?(p) := #f;
  shadowable-%binding-cached-model-object(p) := model;
end method;

binding-properties-accessors (<canonical-module-binding>)
  slot binding-macro-class?               = #"unknown";
  slot binding-cached-model-object        = $binding-model-not-computed;
  slot binding-cached-hollow-model-object = $binding-model-not-computed;
  slot binding-defined?                   = #f;
end;

define method exported? (binding :: <module-binding>)
  exported-name?(binding.binding-home, binding.name)
end method;

define method created? (binding :: <module-binding>)
  created-name?(binding.binding-home, binding.name)
end method;

define class <dood-cross-module-binding-proxy> (<dood-proxy>)
  constant slot dood-proxy-module,        required-init-keyword: module:;
  constant slot dood-proxy-variable-name, required-init-keyword: variable-name:;
end class;

define sealed domain make (subclass(<dood-cross-module-binding-proxy>));
define sealed domain initialize (<dood-cross-module-binding-proxy>);

define method dood-make-cross-module-binding-proxy
    (dood :: <dood>, object :: <canonical-module-binding>) => (proxy)
  make(<dood-cross-module-binding-proxy>, 
       module:        binding-home(object),
       variable-name: name(object))
end method;

define method dood-disk-object 
    (dood :: <dood>, object :: <canonical-module-binding>)
 => (proxy :: type-union(<dood-cross-module-binding-proxy>, 
                         <canonical-module-binding>))
  if (dood.dood-root.language-definition == object.binding-home.home-library)
    next-method();
  else
    dood-as-proxy(dood, object, dood-make-cross-module-binding-proxy)
  end if
end method;

define method binding-identifier (binding :: <module-binding>)
 => (identifier)
  name(binding)
end method;

define method binding-variable-name
    (binding :: <module-binding>) => (variable-name)
  make-variable-name-fragment-in-module(binding.name,
                                        binding.binding-home)
end method;

define method untracked-binding-definition
    (binding :: <module-binding>, #key default = unsupplied()) => (definition)
  binding.binding-active-definition
    | begin
        debug-assert(supplied?(default),
                     "No definition found for %=.", binding);
        default
      end
end method;

define method binding-canonical-binding 
    (binding :: <canonical-module-binding>) => (binding)
  binding
end method;

define method untracked-binding-certain-modifying-models
    (binding :: <canonical-module-binding>, form-predicate :: <function>,
       #key imported-only? = #f)
 => (models :: <models>)
  if (imported-only?)
    #()
  else
    binding-certain-local-modifying-models(binding, form-predicate)
  end;
end method;

define method untracked-binding-modifying-definitions
    (binding :: <canonical-module-binding>) => (definitions :: <definitions>)
  binding-local-modifying-definitions(binding)
end method;

define method untracked-binding-all-definitions
    (binding :: <canonical-module-binding>) => (definitions)
  binding-local-definitions(binding)
end method;

define method binding-local-definitions (binding :: <canonical-module-binding>)
  let active = binding.binding-active-definition;
  let duplicate = binding.binding-local-duplicate-definitions;
  if (active)
    add(duplicate, active);
  else
    duplicate
  end;
end method;

define method untracked-binding-ignored-definitions
    (binding :: <canonical-module-binding>) => (definitions)
  binding-local-ignored-definitions(binding)
end method;

define method untracked-binding-all-modifying-definitions
    (binding :: <module-binding>) => (definitions :: <definitions>)
  let active = binding.untracked-binding-modifying-definitions;
  let ignored = binding.untracked-binding-ignored-definitions;
  if (empty?(ignored)) active else concatenate(active, ignored) end
end method;

define method retract-imported-binding
    (binding :: <canonical-module-binding>) => ()
end method;

define method binding-accessible-to-other-libraries?
    (binding :: <canonical-module-binding>) => (result :: <boolean>)
  let home-module = binding-home(binding);
  when (exported-name?(home-module, binding.name))
    let lib = current-library-model();
    exported-name?(lib, namespace-name(home-module)) |
      // The binding might be re-exported through some other module
      any?(method (lbinding)
             if (defined?(lbinding) & exported?(lbinding))
               let module = library-binding-value(lbinding);
               member?(home-module, module.all-used-namespaces)
                 // Argh...  Hopefully this won't happen too often
                 & member?(binding, module.exported-imports-table)
             end;
           end method,
           lib.namespace-local-bindings)
  end
end method;

define method remove-binding-definition 
    (binding :: <canonical-module-binding>, definition) => ()
  binding-macro-class?(binding) := #"unknown";
  binding-defined?(binding)     := #f;
  if (definition == binding.binding-active-definition)
    note-removing-definition(binding, definition);
    let dups = binding-local-duplicate-definitions(binding);
    if (empty?(dups))
      binding.binding-active-definition := #f;
    else
      let oldest = reduce1(method(old, def)
                               if (defined-before?(old, def)) def else old end
                           end, dups);
      remove-local-duplicate-definition(binding, oldest);
      // At this point nobody depends on binding having or not having any
      // definition, so don't need to note-adding-definition.
      binding.binding-active-definition := oldest;
    end;
  else
    remove-local-duplicate-definition(binding, definition);
  end;
end method;

//// Imported bindings.

// Imported bindings represent a local view on an imported binding. 
// They refer back (perhaps indirectly) to their canonical binding of 
// creation. They have their own set of definitions which are any
// modifying definitions defined in the importing library.
// Full definitions are also collected for error checking reasons. 

define binding-properties of <imported-module-binding-properties>
      <imported-module-binding-properties> (<module-binding-properties>)
  weak slot %binding-modifying-definitions :: false-or(<definitions>) = #f,
    reinit-expression: #f;
  weak slot %binding-defined-methods :: false-or(<list>) = #f,
    reinit-expression: #f;
  weak slot %binding-defined-domains :: false-or(<list>) = #f,
    reinit-expression: #f;
end;

define dood-class <imported-module-binding>
    (<module-binding>, <imported-module-binding-properties>)
  constant slot binding-canonical-binding, 
    required-init-keyword: canonical-binding:;
end;

define method retract-imported-binding
    (binding :: <imported-module-binding>) => ()
  %binding-modifying-definitions(binding) := #f;
  %binding-defined-methods(binding)       := #f;
  %binding-defined-domains(binding)       := #f;
end method;

define property-delegation 
    (<imported-module-binding>, binding-canonical-binding)
  name,
  binding-home,
end property-delegation;

define function imported-binding-delegated-property
    (binding :: <imported-module-binding>)
  let canonical-binding = binding-canonical-binding(binding);
  let home-ld = namespace-library-description(binding-home(canonical-binding));
  binding-properties-in-context(home-ld, canonical-binding, #f)
end;

define macro imported-binding-delegated-getter-definer
  { define imported-binding-delegated-getter ?:name = ?default:expression }
    => { define method ?name (b :: <imported-module-binding>)
           let p = imported-binding-delegated-property(b);
           if (p) "shadowable-" ## ?name(p)
           else ?default end;
         end method }
end macro;
           
define macro imported-binding-delegated-accessors-definer
  { define imported-binding-delegated-accessors ?:name = ?default:expression }
    => { define imported-binding-delegated-getter ?name = ?default;
         define method ?name ## "-setter" (v, b :: <imported-module-binding>)
           let p = imported-binding-delegated-property(b);
           if (p) "shadowable-" ## ?name(p) := v;
           else v end;
         end method }
end macro;
           
define imported-binding-delegated-accessors binding-macro-class? = #f;

define imported-binding-delegated-accessors binding-defined? = #f;

define imported-binding-delegated-accessors emitted-name = #f;

define imported-binding-delegated-getter binding-cached-model-object = $binding-model-not-computed;

define imported-binding-delegated-getter binding-cached-type-model-object = $binding-model-not-computed;

define imported-binding-delegated-getter binding-cached-hollow-model-object = $binding-model-not-computed;

define imported-binding-delegated-getter binding-active-definition = #f;

define imported-binding-delegated-getter binding-previous-definition = #f;


define method binding-imported-into-library? (binding :: <module-binding>)
  #f
end method;

define method binding-imported-into-library? (binding :: <imported-module-binding>)
  #t
end method;


define function collect-used-bindings (func :: <function>,
                                       binding :: <imported-module-binding>)
  debug-assert(valid-binding-home-library?(binding), "Bad binding");
  debug-assert(~*interactive-compilation-layer*, "Interactive?");
  let canonical = binding-canonical-binding(binding);
  let current = func(binding);
  concatenate!
    (reduce(method (result, ld)
              let imported
                = lookup-imported-binding(ld.language-definition, canonical);
              if (imported)
                with-library-context (ld)
                  concatenate!(result, copy-sequence(func(imported)))
                end;
              else
                result
              end
            end method,
            #(),
            all-used-library-descriptions(current-library-description())),
     copy-sequence(func(canonical)),
     copy-sequence(current))
end function;

define method untracked-binding-modifying-definitions
    (binding :: <imported-module-binding>) => (definitions :: <definitions>)
  %binding-modifying-definitions(binding)
    | (%binding-modifying-definitions(binding)
         := collect-modifying-objects
              (binding, binding-local-modifying-definitions, 
               shadowable-binding-local-modifying-definitions))
end method;

define method untracked-binding-certain-modifying-models
    (binding :: <imported-module-binding>, form-predicate :: <function>,
       #key imported-only? = #f)
 => (models :: <models>)
  // let all-local-models-computed? 
  //   = untracked-binding-ensure-certain-modifying-models(binding, form-predicate);
  let all-modifying-models
    = collect-modifying-objects // BUG: shadowable???
        (binding, 
         rcurry(binding-certain-local-modifying-models, form-predicate), 
         rcurry(shadowable-binding-certain-local-modifying-models, form-predicate),
         imported-only?: imported-only?);
  all-modifying-models
end method;

// Help subsequent method-numbering by placing most likely to-be-referenced
// definitions at the head of the list

define inline function collect-modifying-objects
    (binding :: <imported-module-binding>, 
     binding-current-local-objects :: <function>, 
     binding-local-objects :: <function>, #key imported-only? = #f)
 => (objects :: <definitions-or-models>)
  debug-assert(valid-binding-home-library?(binding));
  let current-defs 
    = if (imported-only?)
        #()
      else
        binding-current-local-objects(binding)
      end;
  let p = imported-binding-delegated-property(binding);
  let canonical-defs :: <list> = (p & p.binding-local-objects) | #();

  let canonical = binding-canonical-binding(binding);
  concatenate!
    (copy-sequence(current-defs),
     copy-sequence(canonical-defs),
     reduce(method (result, ld)
              let imported
                = lookup-imported-binding(ld.language-definition, canonical);
              if (imported)
                let p = binding-properties-in-context(ld, imported, #f);
                if (p)
                  let seq = p.binding-local-objects;
                  concatenate!(result, copy-sequence(seq))
                else
                  result
                end
              else
                result
              end;
            end method,
            #(),
            all-used-library-descriptions(current-library-description())))
end function;


// This is only used by the browser interface, so no need to cache - the
// env can cache if it needs to.
define method untracked-binding-all-definitions
    (binding :: <imported-module-binding>) => (definitions)
  collect-used-bindings(binding-local-definitions, binding);
end method;

define method untracked-binding-ignored-definitions
    (binding :: <imported-module-binding>) => (definitions)
  collect-used-bindings(binding-local-ignored-definitions, binding);
end method;

define method binding-local-definitions (binding :: <imported-module-binding>)
  binding.binding-local-duplicate-definitions
end method;

define method binding-accessible-to-other-libraries?
    (binding :: <imported-module-binding>) => (result :: <boolean>)
  #t // an imported binding object wouldn't have been created otherwise
end method;

define method remove-binding-definition 
    (binding :: <imported-module-binding>, definition) => ()
  remove-local-duplicate-definition(binding, definition);
end method;

define method untracked-binding-defined-methods
    (binding :: <imported-module-binding>) => (methods)
  %binding-defined-methods(binding)
end method;

define method untracked-binding-defined-methods-setter
    (new-methods, binding :: <imported-module-binding>) => (methods)
  %binding-defined-methods(binding) := new-methods
end method;

define method untracked-binding-defined-domains
    (binding :: <imported-module-binding>) => (domains)
  %binding-defined-domains(binding)
end method;

define method untracked-binding-defined-domains-setter
    (new-domains, binding :: <imported-module-binding>) => (domains)
  %binding-defined-domains(binding) := new-domains
end method;

//// USEFUL PREDICATES

define compiler-open generic constant? (binding) => (value);

define method defined? (binding :: <module-binding>) => (value :: <boolean>)
  note-binding-dependency(binding, dep$defined?);
  binding-defined?(binding)
    | begin
        let def = untracked-binding-definition(binding, default: $unfound);
        found?(def) & ~instance?(def, <missing-variable-defining-form>)
      end 
end method;

define method constant? (binding :: <module-binding>) => (value :: <boolean>)
  let definition = binding-definition(binding, default: $unfound);
  found?(definition) & constant?(definition)
end method;

define method compile-stage-only? (binding :: <module-binding>) => (value :: <boolean>)
  let definition = binding-definition(binding, default: $unfound);
  found?(definition) & form-compile-stage-only?(definition)
end method;

define method constant? 
    (binding :: <variable-defining-form>) => (value :: <boolean>)
  #t
end method;

//// INCREMENTAL ACCESS

define method define-hollow-object (name, model) => ()
  // Don't need to track this lookup since only defined model if previously
  // defined a definition.
  let binding = untracked-lookup-binding(name);
  binding-cached-hollow-model-object(binding) := model
end method;

define method binding-model-or-hollow-object
    (binding :: <module-binding>, #key default = #f) => (object)
  let definition = untracked-binding-definition(binding);
  if (form-dynamic?(definition)
        & form-binding-guaranteed-initialized?(definition))
    let model = binding-cached-hollow-model-object(binding);
    if (model == $binding-model-not-computed)
      note-binding-dependency(binding, dep$active-definition);
      default
    else
      note-binding-dependency
        (binding, dep$active-definition + dep$model-object);
      model
    end
  else
    binding-model-object(binding, default: default);
  end;
end method;

//// THREAD PREDICATES

define method binding-thread? 
    (binding :: <module-binding>) => (value :: <boolean>)
  let definition = binding-definition(binding, default: $unfound);
  found?(definition) & form-thread?(definition)
end method;

define method binding-locked? 
    (binding :: <module-binding>) => (value :: <boolean>)
  let definition = binding-definition(binding, default: $unfound);
  found?(definition) & form-locked?(definition)
end method;

define method binding-atomic? 
    (binding :: <module-binding>) => (value :: <boolean>)
  let definition = binding-definition(binding, default: $unfound);
  found?(definition) & form-atomic?(definition)
end method;

