Module:   dfmc-definitions
Synopsis: Slot conversion.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Class clause parsing.

// There are three kinds of class clause to handle - slot definitions, 
// inherited slot modifications, and init-arg specifications.

// TODO: Turn into a program error.

define program-warning <invalid-define-class-clause>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    "Unrecognized clause in the definition of class %= "
                   "- ignoring.";
  format-arguments variable-name;
end program-warning;

define function parse-class-clauses 
    (class-name, clauses) 
      => (slot-specs, inherited-slot-specs, initarg-specs, metaclass-spec)
  collecting (slot-specs, inherited-slot-specs, initargs-specs)
    // use collect-first, since macro pattern-matching runs right-hand sides
    // from bottom up (= right to left)
    let metaclass-spec = #f;
    macro-case (clauses)
      { ?clauses:* } 
        => values
             (collected(slot-specs), 
              collected(inherited-slot-specs), 
              collected(initargs-specs),
              metaclass-spec);
    clauses:
      { }
        => #f;
      { ?clause:*; ... }
        => macro-case (clause)
             { inherited slot ?spec:* }
               => collect-first-into
                  (inherited-slot-specs, 
                   parse-inherited-slot-clause(clause, class-name, spec));
             { ?adjectives:* slot ?spec:* }
               => collect-first-into
                   (slot-specs,
                    parse-slot-clause(clause, class-name, adjectives, spec));
             { required keyword ?spec:* }
	       => collect-first-into
                   (initargs-specs, 
                    parse-keyword-clause(clause, class-name, #t, spec));
             { keyword ?spec:* }
	       => collect-first-into
                   (initargs-specs, 
                    parse-keyword-clause(clause, class-name, #f, spec));
             { ?adjectives:* metaclass ?spec:* }
	       => metaclass-spec 
	             := parse-metaclass-clause(class-name, adjectives, spec);
	     { ?other:* }
               => note(<invalid-define-class-clause>,
                       source-location: fragment-source-location(other),
                       variable-name:   class-name);
           end macro-case;
    end macro-case;
  end;
end;

//// Slot definitions.

define abstract dood-class <slot-initial-value-spec> (<object>)
  slot spec-slot-properties :: <integer> = 0;
  lazy constant slot spec-init-expression = #f,
    init-keyword: expression:;
end dood-class;

define packed-slots spec-slot-properties
    (<slot-initial-value-spec>, <object>)
  boolean slot spec-init-supplied? = #f,
    init-keyword: init-supplied?:;
  boolean slot spec-init-expression? = #f,
    init-keyword: init-expression?:;
  boolean slot spec-init-value? = #f,
    init-keyword: init-value?:;
end packed-slots;

define method initialize
    (x :: <slot-initial-value-spec>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, x, all-keys)
end method;

define abstract dood-class <slot-keyword-initialization-spec> (<slot-initial-value-spec>)
  lazy constant slot spec-init-keyword :: false-or(<symbol>) = #f,
    init-keyword: init-keyword:;
  lazy constant slot spec-type-expression = make-variable-name-fragment(#"<object>"),
    init-keyword: type:;
end dood-class;

define packed-slots spec-slot-properties
    (<slot-keyword-initialization-spec>, <slot-initial-value-spec>)
  boolean slot spec-init-keyword-required? = #f,
    init-keyword: init-keyword-required?:;
end packed-slots;

define dood-class <slot-definition> (<variable-defining-form>,
				<slot-keyword-initialization-spec>)
  lazy slot form-getter-definition = #f; // #f for virtual slots.
  lazy slot form-setter-definition = #f;

  lazy constant slot spec-getter, required-init-keyword: getter:;
  lazy constant slot spec-setter, required-init-keyword: setter-name:;

  // TODO: COULD BE PACKED AS WELL
  constant slot spec-allocation = #"instance",
    init-keyword: allocation:;
end dood-class;

define packed-slots spec-slot-properties
    (<slot-definition>, <slot-keyword-initialization-spec>)
  boolean slot spec-sealed? = #f,
    init-keyword: sealed?:;
  boolean slot spec-constant? = #f,
    init-keyword: constant?:;
  boolean slot spec-volatile? = #f,
    init-keyword: volatile?:;
  boolean slot spec-atomic? = #f,
    init-keyword: atomic?:;
  boolean slot spec-raw? = #f,
    init-keyword: raw?:;
end packed-slots;

define dood-class <repeated-slot-definition> (<slot-definition>)
//  lazy slot form-size-getter-definition = #f;
  lazy constant slot spec-size-getter = #f,
    init-keyword: size-getter:;
  lazy constant slot spec-size-init-keyword = #f,
    init-keyword: size-init-keyword:;
  lazy constant slot spec-size-init-expression = #f,
    init-keyword: size-init-expression:;
end dood-class;

define leaf packed-slots spec-slot-properties
    (<repeated-slot-definition>, <slot-definition>)
  boolean slot spec-size-init-supplied? = #f,
    init-keyword: size-init-supplied?:;
end packed-slots;

define method form-variable-names
    (form :: <slot-definition>) => (seq :: <sequence>)
  form.form-variable-name-or-names
end method;

define method form-defined-bindings
    (form :: <slot-definition>) => (seq :: <sequence>)
  choose(method (binding)
	   let defn = untracked-binding-definition(binding, default: #f);
	   defn & ~form-ignored?(defn)
	 end,
	 map(untracked-lookup-binding, form.form-variable-names))
end;

define method form-variable-name
    (form :: <slot-definition>) => (name :: <variable-name-fragment>)
  // error("who's calling this?");
  form.form-variable-names.first
end method;

define generic make-slot-spec (class :: <class>, #key, #all-keys);

define method make-slot-spec (class == <slot-definition>,
			      #rest initargs,
			      #key allocation)
  if (fragment-repeated?(allocation))
    apply(make-slot-spec, <repeated-slot-definition>, initargs)
  else
    next-method()
  end;
end method;

define method make-slot-spec (class :: subclass(<slot-keyword-initialization-spec>),
			      #rest initargs,
			      #key required-init-keyword, init-keyword)

  if (required-init-keyword)
    apply(next-method, class,
	  init-keyword: as(<symbol>, required-init-keyword),
	  init-keyword-required?: #t,
	  initargs)
  elseif (init-keyword)
    apply(next-method, class,
	  init-keyword: as(<symbol>, init-keyword),
	  initargs)
  else
    next-method()
  end
end method;

define method make-slot-spec (class :: subclass(<slot-initial-value-spec>),
			      #rest initargs,
			      #key init-expression = unsupplied(),
			           init-value = unsupplied(),
			           init-function = unsupplied())

  if (supplied?(init-expression))
    apply(make, class,
	  init-supplied?: #t,
	  init-expression?: #t,
	  expression: init-expression,
	  initargs);
  elseif (supplied?(init-value))
    apply(make, class,
	  init-supplied?: #t,
	  init-value?: #t,
	  expression: init-value,
	  initargs)
  elseif (supplied?(init-function))
    apply(make, class,
	  init-supplied?: #t,
	  expression: init-function,
	  initargs)
  else
    apply(make, class, initargs)
  end;
end method;

define program-warning <constant-and-setter-both-specified>
  slot condition-slot-name,
    required-init-keyword: slot-name:;
  slot condition-setter-name,
    required-init-keyword: setter-name:;
  format-string    "Slot %s declared constant but with setter: %s "
                   "- ignoring the setter option.";
  format-arguments 
    slot-name, setter-name;
end program-warning;

define method make-slot-spec (class :: subclass(<slot-definition>),
			      #rest initargs,
			      #key getter, setter, size-getter, constant?)


  if (~constant? & ~setter)
    setter := as-name(#{ ?getter ## "-setter" });
  end;
  collecting (names)
    local method maybe-collect-name (name)
      if (name)
        macro-case (name) 
          { #f }     => #f;
          { ?:name } => begin collect-into(names, name); name end;
        end;
      end;
    end;
    collect-into(names, getter);
    let setter-name = maybe-collect-name(setter);
    maybe-collect-name(size-getter);
    if (constant? & setter-name)
      note(<constant-and-setter-both-specified>,
           source-location: fragment-source-location(setter),
           slot-name:       getter,
           setter-name:     setter);
      setter-name := #f;
    end;
    apply(next-method,    class,
	  setter-name:    setter-name,
	  variable-name:  collected(names),
          constant?:      constant? | ~setter-name,
	  initargs)
  end;
end method;

define method make-slot-spec (class :: subclass(<repeated-slot-definition>),
			      #rest initargs,
			      #key size-init-value = unsupplied())
  if (supplied?(size-init-value))
    apply(next-method, class,
	  size-init-supplied?: #t,
	  size-init-expression: size-init-value,
	  initargs)
  else
    next-method()
  end;
end method;

define function fragment-repeated? (fragment)
  macro-case (fragment)
    { #"repeated" } => #t;
    { ?other:* }    => #f;
  end;
end;

define function spec-repeated? (spec :: <slot-definition>)
  instance?(spec, <repeated-slot-definition>)
end;

define function spec-virtual? (spec :: <slot-definition>)
  spec-allocation(spec) == #"virtual"
end;

// Adjective specifications.

// TODO: constant

define property <slot-sealed-property> => sealed?: = #f
  value sealed = #t;
  value open   = #f;
end property;

define property <slot-allocation-property> => allocation: = #"instance"
  value instance      = #"instance";
  value class         = #"class";
  value each-subclass = #"each-subclass";
  value virtual       = #"virtual";
  value repeated      = #"repeated";
end property;

define property <slot-constant-property> => constant?: = #f
  value constant = #t;
end property;

define property <slot-inline-property> 
    => inline-policy: = #"default-inline"
  value inline         = #"inline";
  value may-inline     = #"may-inline";
  value default-inline = #"default-inline";
  value not-inline     = #"not-inline";
end property;

define property <slot-atomic-property> => atomic?: = #f
  value atomic = #t;
end property;

define property <slot-volatile-property> => volatile?: = #f
  value volatile = #f;
end property;

define property <slot-raw-property> => raw?: = #f
  value raw = #t;
end property;

define constant $slot-adjectives =
  list(<slot-sealed-property>, 
       <slot-allocation-property>,
       <slot-constant-property>,
       <slot-inline-property>,
       <slot-atomic-property>,
       <slot-volatile-property>,
       <slot-raw-property>);

// Option specifications.

define option <slot-setter-option> => setter: :: expression end;

define option <slot-init-value-option> => init-value: :: expression end;

define option <slot-init-function-option> => init-function: :: expression
  excludes <slot-init-value-option>;
end option;

define option <slot-init-keyword-option> => init-keyword: :: symbol end;

define option <slot-required-init-keyword-option> 
    => required-init-keyword: :: symbol
  excludes 
    <slot-init-value-option>, 
      <slot-init-function-option>, <slot-init-keyword-option>;
end option;

define option <slot-size-getter-option> 
    => size-getter: :: expression
end option;

define option <slot-size-init-value-option> 
    => size-init-value: :: expression
end option;

define option <slot-size-init-keyword-option> 
    => size-init-keyword: :: symbol
end option;

define option <slot-type-option> => type: :: expression end;

define constant $slot-options =
  list(<slot-setter-option>,
       <slot-init-value-option>,
       <slot-init-function-option>,
       <slot-init-keyword-option>,
       <slot-required-init-keyword-option>,
       <slot-size-getter-option>,
       <slot-size-init-value-option>,
       <slot-size-init-keyword-option>,
       <slot-type-option>);

// Slot parser.

// TODO: Cross check adjectives and options, or merge the two into
// a single kind of declaration.

// TODO: redundancy check on init-expression vs. rest
// TODO: redundancy check on :: type vs. type:
define function parse-slot-clause (clause, name, adjectives, spec)
  let (adjective-initargs, adjectives)
    = parse-property-adjectives($slot-adjectives, adjectives, name);
  let (getter-spec, option-initargs) =
    macro-case (spec)
    { ?getter:name = ?init:expression, ?options:* }
      => values(getter,
		concatenate(list(init-expression: init), options));
    { ?getter:name, ?options:* }
      => values(getter, options);
    { ?getter:name :: ?type:expression = ?init:expression, ?options:* }
      => values(getter,
		concatenate(list(type: type, init-expression: init), options));
    { ?getter:name :: ?type:expression, ?options:* }
      => values(getter, concatenate(list(type: type), options));
    options:
      { ?keys/vals:* } 
        => parse-options($slot-options, keys/vals, name);
    end macro-case;
  let all-initargs = concatenate(adjective-initargs, option-initargs);
  apply(make-slot-spec, <slot-definition>,
	source-location: fragment-source-location(clause),
	adjectives: adjectives,
	getter: getter-spec,
        all-initargs);
end;

// Slot method generation.

define function generate-slot-method-definition-forms 
    (slot-specs :: <sequence>) => (forms :: <sequence>)
  reduce(maybe-add-one-slot-method-definition-forms,
	 #(),
         slot-specs);
end;

define function maybe-add-one-slot-method-definition-forms
    (forms :: <sequence>, slot-spec :: <slot-definition>)
  case
    spec-virtual?(slot-spec) | spec-raw?(slot-spec)
      => forms;
    otherwise
      => with-expansion-source-form (slot-spec)
           add-one-slot-method-definition-forms(forms, slot-spec);
         end;
  end;
end function;

define method add-one-slot-method-definition-forms
    (forms :: <sequence>, slot-spec :: <slot-definition>)
      => (forms :: <sequence>)
  let class-binding = form-variable-binding(form-parent-form(slot-spec));
  let type-expression = spec-type-expression(slot-spec);
  let getter-name = spec-getter(slot-spec);
  let setter-name = spec-setter(slot-spec);
  let mods 
    = if (spec-sealed?(slot-spec)) #{ sealed } else #{ } end;
  let params
    = if (spec-repeated?(slot-spec))
	#{ object :: ?class-binding, index :: <integer> }
      else
	#{ object :: ?class-binding }
      end;
  let getter-fragment
    = #{ define ?mods method ?getter-name
	     (?params) => (value :: ?type-expression)
	 end };
  // TODO: This as-body forces the current hygiene context to be 
  // remembered since templates currently don't record that but
  // fragments do. 
  let getter-fragment = as-body(getter-fragment);
  let getter-forms = top-level-convert(slot-spec, getter-fragment);
  let getter-form = first(getter-forms);
  form-class(getter-form) := if (spec-repeated?(slot-spec))
			       #"repeated-getter"
			     else
			       #"getter"
			     end;
  form-getter-definition(slot-spec) := getter-form;
  forms := concatenate(getter-forms, forms);
  if (~spec-constant?(slot-spec) & setter-name)
    let setter-fragment
      = #{ define ?mods method ?setter-name
	       (value :: ?type-expression, ?params)
	    => (value :: ?type-expression)
	   end };
    // TODO: This as-body forces the current hygiene context to be 
    // remembered since templates currently don't record that but
    // fragments do. 
    let setter-fragment = as-body(setter-fragment);
    let setter-forms = top-level-convert(slot-spec, setter-fragment);
    let setter-form = first(setter-forms);
    form-class(setter-form) := if (spec-repeated?(slot-spec))
				 #"repeated-setter"
			       else
				 #"setter"
			       end;
    form-setter-definition(slot-spec) := setter-form;
    forms := concatenate(setter-forms, forms);
  end;
  forms
end method;

/*
define method add-one-slot-method-definition-forms
    (forms :: <sequence>, slot-spec :: <slot-definition>)
      => (forms :: <sequence>)
  let class-name = form-variable-name(form-parent-form(slot-spec));
  let type-expression = spec-type-expression(slot-spec);
  let getter-name = spec-getter(slot-spec);
  let setter-name = spec-setter(slot-spec);
  let sealed? = spec-sealed?(slot-spec);
  let adjectives = if (sealed?) list(#"adjectives") else #() end;
  let object-var-spec
    = make(<required-variable-spec>,
           variable-name:   #{ object },
           type-expression: class-name);
  let value-var-spec
    = make(<required-variable-spec>,
           variable-name:   #{ value },
           type-expression: type-expression);
  let getter-required
    = if (spec-repeated?(slot-spec))
        vector(object-var-spec, 
               make(<required-variable-spec>,
                    variable-name: #{ index },
                    type-expression: #{ <integer>}))
      else
        vector(object-var-spec)
      end;
  let getter-values-spec
    = make(<values-spec>,
           required-variable-specs: vector(value-var-spec),
           rest-variable-spec:      #f);
  let getter-sig
    = make(<method-signature-spec>,
           arguments-spec:
             make(<method-arguments-spec>,
                  required-variable-specs: getter-required,
                  next-variable-spec:      #f,
                  rest-variable-spec:      #f,
                  key?: #f, key-variable-specs: #()),
           values-spec: getter-values-spec);
  let getter-form
    = make(<method-definition>,
           parent: slot-spec,
           source-location: form-source-location(slot-spec),
           variable-name:   getter-name,
           adjectives:      adjectives,
           signature:       getter-sig,
           body:            #{ },
           signature-and-body-fragment: #f,
           inline-policy:   #"default-inline",
           sealed?:         sealed?,
           sideways?:       #f);
  form-class(getter-form) := if (spec-repeated?(slot-spec))
			       #"repeated-getter"
			     else
			       #"getter"
			     end;
  form-getter-definition(slot-spec) := getter-form;
  // TODO: Domain definitions.
  let getter-forms = list(getter-form);
  forms := concatenate(getter-forms, forms);
  if (~spec-constant?(slot-spec) & setter-name)
    let setter-sig
      = make(<method-signature-spec>,
             arguments-spec:
               make(<method-arguments-spec>,
                    required-variable-specs: 
                      concatenate(vector(value-var-spec), getter-required),
                    next-variable-spec:      #f,
                    rest-variable-spec:      #f,
                    key?: #f, key-variable-specs: #()),
             values-spec: getter-values-spec);
    let setter-form
      = make(<method-definition>,
             parent: slot-spec,
             source-location: form-source-location(slot-spec),
             variable-name:   setter-name,
             adjectives:      adjectives,
             signature:       setter-sig,
             body:            #{ },
             signature-and-body-fragment: #f,
             inline-policy:   #"default-inline",
             sealed?:         sealed?,
             sideways?:       #f);
    form-class(setter-form) := if (spec-repeated?(slot-spec))
				 #"repeated-setter"
			       else
				 #"setter"
			       end;
    form-setter-definition(slot-spec) := setter-form;
    let setter-forms = list(setter-form);
    forms := concatenate(setter-forms, forms);
  end;
  forms
end method;
*/

define method add-one-slot-method-definition-forms
    (forms :: <sequence>, slot-spec :: <repeated-slot-definition>)
      => (forms :: <sequence>)
  let forms = next-method();
  let size-getter-name = spec-size-getter(slot-spec);
  if (size-getter-name)
    let class-binding = form-variable-binding(form-parent-form(slot-spec));
    let mods 
      = if (spec-sealed?(slot-spec)) #{ sealed } else #{ } end;
    let size-getter-fragment
      = #{ define ?mods method ?size-getter-name 
             (object :: ?class-binding) => (size :: <integer>)
           end };
    let size-getter-fragment 
      = as-body(size-getter-fragment);
    let size-getter-forms 
      = top-level-convert(slot-spec, size-getter-fragment);
    let size-getter-form = first(size-getter-forms);
    form-class(size-getter-form) := #"getter";
    // form-size-getter-definition(slot-spec) := size-getter-form;
    concatenate(size-getter-forms, forms)
  else
    forms
  end
end method;

//// Inherited slot modifications.

define dood-class <inherited-slot-spec> (<slot-initial-value-spec>)
  lazy constant slot spec-getter, required-init-keyword: getter:;
end dood-class;

define constant $inherited-slot-options =
  list(<slot-init-value-option>,
       <slot-init-function-option>,
       <slot-init-keyword-option>,
       <slot-required-init-keyword-option>);

define program-warning <invalid-inherited-slot-type>
  slot condition-getter-name,
    required-init-keyword: getter-name:;
  slot condition-type-expression,
    required-init-keyword: type:;
  format-string    "Inherited slot clause may not specify a type %= :: %= "
                   "-- ignoring the type.";
  format-arguments getter-name, type;
end program-warning;

define function parse-inherited-slot-clause (clause, name, spec)
  let (getter, option-initargs) =
    macro-case (spec)
    { ?getter:name = ?init:expression, ?options:* }
      => values(getter,
		concatenate(list(init-expression: init), options));
    { ?getter:name, ?options:* }
      => values(getter, options);
    { ?getter:name :: ?type:expression = ?init:expression, ?options:* }
      => begin
	   note(<invalid-inherited-slot-type>,
		source-location: fragment-source-location(clause),
		getter-name:     getter,
		type:            type);
	   values(getter,
		  concatenate(list(init-expression: init), options));
	 end;
    { ?getter:name :: ?type:expression, ?options:* }
      => begin
	   note(<invalid-inherited-slot-type>,
		source-location: fragment-source-location(clause),
		getter-name:     getter,
		type:            type);
	   values(getter, options);
	 end;
    options:
      { ?keys/vals:* } 
        => parse-options($inherited-slot-options, keys/vals, name);
    end macro-case;
  apply(make-slot-spec, <inherited-slot-spec>, 
	getter: getter,
        option-initargs);
end;

//// Initarg definitons.

define dood-class <init-arg-spec> (<slot-keyword-initialization-spec>)
  lazy constant slot form-source-location,
    required-init-keyword: source-location:;
end dood-class;

define constant $initialization-argument-options =
  list(<slot-init-value-option>,
       <slot-init-function-option>,
       <slot-type-option>);

define program-warning <invalid-initialization-argument-type>
  slot condition-keyword,
    required-init-keyword: keyword:;
  slot condition-type-expression,
    required-init-keyword: type:;
  format-string    "Initialization argument specification may not specify a type %= :: %= "
                   "-- ignoring the type.  Perhaps you want to use the \"type:\" option.";
  format-arguments keyword, type;
end program-warning;

define function parse-keyword-clause (clause, name, required?, spec)
  let (keyword, option-init-args) =
    macro-case (spec)
      { ?keyword:token }
         => values(keyword, #());
      { ?keyword:token = ?init:expression, ?options }
         =>  values(keyword,
		    concatenate(list(init-expression:, init), options));
      { ?keyword:token, ?options  }
         => values(keyword, options);
      { ?keyword:token :: ?type:expression = ?init:expression, ?options }
         => begin
	      note(<invalid-initialization-argument-type>,
		   source-location: fragment-source-location(clause),
		   keyword:         keyword,
		   type:            type);
	      values(keyword,
		     concatenate(list(init-expression:, init), options))
	    end;
      { ?keyword:token :: ?type:expression, ?options }
         => begin
	      note(<invalid-initialization-argument-type>,
		   source-location: fragment-source-location(clause),
		   keyword:         keyword,
		   type:            type);
	      values(keyword, options)
	    end;
    options:
      { ?keys/vals:* }
        => parse-options($initialization-argument-options, keys/vals, name);
    end;
  apply(make-slot-spec, <init-arg-spec>,
	source-location: fragment-source-location(clause),
	init-keyword: keyword,
	init-keyword-required?: required?,
	option-init-args)
end;

//// Metaclass definitions.

define dood-class <metaclass-spec> (<object>)
  lazy constant slot spec-metaclass-name,
    required-init-keyword: metaclass-name:;
  lazy constant slot spec-metaclass-initargs,
    required-init-keyword: metaclass-initargs:;
end dood-class;

define function parse-metaclass-clause (name, adjectives, spec) => (spec)
  parse-metaclass-adjectives(name, adjectives);
  macro-case (spec)
    { ?metaclass:name, ?initargs:* }
      => make(<metaclass-spec>, 
              metaclass-name:     as(<symbol>, metaclass),
              metaclass-initargs: parse-property-list(initargs));
  end;
end;

define function parse-metaclass-adjectives (name, adjectives) => ()
  // Just check that there aren't any.
  parse-property-adjectives(#(), adjectives, name);
end;
