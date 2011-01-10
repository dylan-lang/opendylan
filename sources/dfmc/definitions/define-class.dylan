Module:   dfmc-definitions
Synopsis: The class definition processor.
Author:   Paul Haahr, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Class definitions.

// Class definition objects.

define dood-class <class-definition> (<variable-defining-form>)

  lazy constant slot form-superclass-expressions,
    required-init-keyword: superclass-expressions:;
  lazy constant slot form-slot-specs,
    required-init-keyword: slot-specs:;
  lazy constant slot form-inherited-slot-specs,
    required-init-keyword: inherited-slot-specs:;
  lazy constant slot form-keyword-specs,
    required-init-keyword: keyword-specs:;
  lazy constant slot form-metaclass-spec,
    required-init-keyword: metaclass-spec:;

end dood-class;

define constant $form-sealing/sealed = 0;
define constant $form-sealing/compiler-open = 1;
define constant $form-sealing/open = 2;
define constant $form-sealing/dynamic = 3;

define leaf packed-slots form-properties
    (<class-definition>, <variable-defining-form>)
  boolean slot form-abstract? = #f, // vs. concrete
    init-keyword: abstract?:;
  boolean slot form-primary? = #f,  // vs. free
    init-keyword: primary?:;
  field slot form-sealing = $form-sealing/sealed, field-size: 2,
    init-keyword: sealing:;
  boolean slot form-made-inline? = #f, 
    init-keyword: made-inline?:;
end packed-slots;

define inline function form-declared-sealed? (form) => (well? :: <boolean>)
  form.form-sealing == $form-sealing/sealed
end;

define method form-compile-stage-only?
    (form :: <class-definition>) => (compile-stage-only?)
  let metaclass-spec = form-metaclass-spec(form);
  metaclass-spec &
    #"<virtual-class>" == spec-metaclass-name(metaclass-spec)
end method;

define method form-define-word
    (form :: <class-definition>) => (word :: <symbol>)
  #"class"
end method;

define constant form-concrete? = complement(form-abstract?);
define constant form-free?     = complement(form-primary?);

// If true, "open" and "dynamic" mean the same thing, as per DRM's definition of "open".
// If false, "open" forms are effectively treated as sealed if they are not exported,
//   and you have to use "dynamic" to force openness for unexported forms.
// TODO: the right semantics is to seal if not externally visible (e.g. macros), but
// that gets too hairy, so we make users handle stuff extensible by macros by hand.
define variable *open-is-dynamic?* = #t;


define inline function form-sealed-if-private? (form)
 => (well? :: <boolean>)
  select (form.form-sealing)
    $form-sealing/dynamic => #f;
    $form-sealing/open => ~*open-is-dynamic?*;
    otherwise => #t;
  end;
end;

define inline function form-compiler-open? (form)
 => (well? :: <boolean>)
  form.form-sealing == $form-sealing/compiler-open
end;


define function form-virtual? 
    (form :: <class-definition>) => (well? :: <boolean>)
  let meta-spec = form-metaclass-spec(form);
  meta-spec & spec-metaclass-name(meta-spec) == #"<virtual-class>"
end function;

// Conversion to a definition object.

define program-warning <no-superclass-expressions>
  slot condition-class-name,
    init-keyword: class-name:;
  format-string
    "No superclasses specified for the class %s - using <object>.";
  format-arguments
    class-name;
end program-warning;

define &definition class-definer
  { define ?mods:* class ?:name (?supers:*) ?slots:* end }
    => do-define-class(form, mods, name, supers, slots);
end &definition;

define method do-define-class (fragment, mods, name, supers, slots)
  let (initargs, adjectives) = parse-class-adjectives(name, mods);
  let (slot-specs, inherited-slot-specs, keyword-specs, metaclass-spec)
    = parse-class-clauses(name, slots);
  let super-exprs = parse-class-superclasses(supers);
  let super-exprs
    = if (empty?(super-exprs) & ~compiling-dylan-library?())
        // We allow zero superclasses in the Dylan library to accommodate
        // the definition of <object>.
        note(<no-superclass-expressions>,
             source-location: fragment-source-location(fragment),
             class-name:      name);
        list(dylan-variable-name(#"<object>"));
      else
        super-exprs
      end;
  let class-definition
    = apply(make, <class-definition>,
            source-location:        fragment-source-location(fragment),
            variable-name:          name,
            adjectives:             adjectives,
            superclass-expressions: super-exprs,
            slot-specs:             slot-specs,
            inherited-slot-specs:   inherited-slot-specs,
            keyword-specs:          keyword-specs,
            metaclass-spec:         metaclass-spec,
            initargs);
  for (slot in slot-specs) slot.form-parent-form := class-definition end;
  let method-definitions
    = generate-slot-method-definition-forms(slot-specs);
  let method-definitions
    = if (~(form-abstract?(class-definition) 
              | form-virtual?(class-definition)
              | metaclass-spec))
        let initializer-definitions
          = generate-initializer-definition-forms(class-definition);
        concatenate(initializer-definitions, method-definitions)
      else
        method-definitions
      end;
  pair(class-definition, method-definitions);
end method;

define program-warning <non-name-superclass-expression>
  slot condition-class-name,
    init-keyword: superclass-expression:;
  slot condition-superclass-expression,
    init-keyword: superclass-expression:;
  format-string
    "The superclass %= of %= is not a variable name - the compiler must "
    "allow for this class being a potential subclass of even sealed "
    "classes in this library."; 
  format-arguments
    superclass-expression, class-name;
end program-warning;

// TODO: If the superclasses of a class cannot be identified at 
// compile-time, doesn't that imply that you have to assume that
// it might be a subclass of any class at all? Something similarly
// bogus probably holds for define method on a variable/constant 
// rather than modifying a generic definition.

define method install-top-level-form-bindings
    (form :: <class-definition>) => ()
  next-method();
  if (form-ignored?(form))
    debug-out(#"gsb", ">>> Ignoring subparts of %=\n", form)
  else
  unless (form-compile-stage-only?(form))
    // hide virtual subclasses
    for (super-name in form-superclass-expressions(form))
      if (instance?(super-name, <variable-name-fragment>))
        let binding = lookup-binding(super-name);
        if (defined?(binding) 
              & instance?(binding-definition(binding), <class-definition>))
          add-modifying-definition(super-name, form);
        else
          add-modifying-definition(super-name, form);
          /*
          add-library-wildcard-subclass-definition
            (language-definition(form-library(form)), form);
          */
        end;
      else
        add-library-wildcard-subclass-definition
          (language-definition(form-library(form)), form);
      end;
    end;
  end;
  do(install-top-level-form-bindings, form-slot-specs(form));
  end;
end method;

define method uninstall-top-level-form-bindings
    (form :: <class-definition>) => ()
  next-method();
  unless (form-compile-stage-only?(form))
    for (super-name in form-superclass-expressions(form))
      if (instance?(super-name, <variable-name-fragment>))
	remove-modifying-definition(super-name, form);
      end;
    end;
  end;
  do(uninstall-top-level-form-bindings, form-slot-specs(form));
end method;

define method install-top-level-form-bindings
    (form :: <slot-definition>) => ()
  if (spec-virtual?(form) | spec-raw?(form))
    let getter-name = spec-getter(form);
    let setter-name = spec-setter(form);
    if (getter-name & ~variable-defined?(getter-name))
      // TODO: Lose this hygiene hack.
      with-fragment-info (getter-name)
	add-implicit-generic-definition-from-pattern
	  (form, getter-name, #"fixed",
	   list(as-name(#{ object })), #f);
      end;
    end;
    if (setter-name & ~variable-defined?(setter-name))
      // TODO: Lose this hygiene hack.
      with-fragment-info (setter-name)
	add-implicit-generic-definition-from-pattern
	  (form, setter-name, #"fixed", 
	   list(as-name(#{ new-value }), as-name(#{ object })), #f);
      end;
    end;
  end;
end method;

define method uninstall-top-level-form-bindings
    (form :: <slot-definition>) => ()
  if (spec-virtual?(form) | spec-raw?(form))
    local method retract-implicit (name)
	    name & retract-implicit-definition(name, form);
	  end method;
    retract-implicit(spec-getter(form));
    retract-implicit(spec-setter(form));
  end;
end method;


//// Class adjective parsing.

// Class properties:

define property <class-sealed-property> => sealing: = $form-sealing/sealed
  value sealed        = $form-sealing/sealed;
  value open          = $form-sealing/open;
  value compiler-open = $form-sealing/compiler-open;
  value dynamic       = $form-sealing/dynamic;
end property;

//define property <class-sealed-property-default-open> => sealing: = $form-sealing/open
//  value sealed        = $form-sealing/sealed;
//  value open          = $form-sealing/open;
//  value compiler-open = $form-sealing/compiler-open;
//  value dynamic       = $form-sealing/dynamic;
//end property;

define property <class-abstract-property> => abstract?: = #f
  value abstract = #t;
  value concrete = #f;
end property;

define property <class-primary-property> => primary?: = #f
  value primary = #t;
  value free    = #f;
end property;

define property <class-made-inline-property> => made-inline?: = #f
  value made-inline = #t;
end property;

define constant class-adjectives-default-sealed
    = list(<class-sealed-property>,
	   <class-abstract-property>,
	   <class-primary-property>,
           <class-made-inline-property>);

//define constant class-adjectives-default-open
//    = list(<class-sealed-property-default-open>,
//	   <class-abstract-property>,
//	   <class-primary-property>,
//           <class-made-inline-property>);

define function class-adjectives ()
//  let mode = current-compilation-mode();
//  if (mode == #"default-open") class-adjectives-default-open
//  else class-adjectives-default-sealed end
  class-adjectives-default-sealed
end function;

define method parse-class-adjectives (name, form)
 => (initargs, adjective-symbols)
  parse-property-adjectives(class-adjectives(), form, name);
end method;

//// Class superclasses parsing.

define method parse-class-superclasses (form) => (expressions)
  macro-case (form)
    { ?supers:* }
      => supers;
  supers:
    { } 
      => #();
    { ?super:expression, ... }
      => pair(super, ...);
  end;
end method;

//// Known subclasses computation.

define method binding-method-definitions
    (binding :: <module-binding>) => (method-definitions :: <sequence>)
  choose(method-definition?, binding-modifying-definitions(binding));
end;

define method binding-domain-definitions
    (binding :: <module-binding>) => (method-definitions :: <sequence>)
  choose(domain-definition?, binding-modifying-definitions(binding));
end;


define method binding-direct-subclass-definitions
    (binding :: <module-binding>) => (subclass-definitions :: <sequence>)
  choose-instances(<class-definition>, binding-modifying-definitions(binding))
end method;

define method form-model-object (form :: <class-definition>)
  binding-model-object(form-variable-binding (form));
end;

//// Initializer method generation.

define method generate-initializer-definition-forms
    (form :: <class-definition>) => (new-forms :: <sequence>)
  let class-name = form-variable-name(form);
  let class-binding = form-variable-binding(form);
  let constructor-name = #{ ?class-name ## " constructor" };
  let modifiers = if (form-made-inline?(form)) #{ inline } else #{ } end;
  let code
    = #{ define ?modifiers method ?constructor-name
             (class :: <class>, #rest init-args, #key, #all-keys) 
          => (object :: ?class-binding)
         end };
  let forms = top-level-convert(form, code);
  form-class(first(forms)) := #"initializer";
  forms
end method;
