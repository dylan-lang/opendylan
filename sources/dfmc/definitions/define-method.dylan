Module:   dfmc-definitions
Synopsis: The method definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Method definitions.

define dood-class <method-defining-form> (<function-defining-form>) /* abstract */
  lazy slot form-body,
    required-init-keyword: body:;
end;

define packed-slots form-properties (<method-defining-form>, <function-defining-form>)
  boolean slot form-upgrade? = #t,    // vs. not-upgrade
    init-keyword: upgrade?:;  
end packed-slots;

define method form-class (form :: <method-defining-form>)
 => (class :: <symbol>)
  #"simple"
end;

define dood-class <method-definition> (<method-defining-form>, <modifying-form>)
  lazy slot form-class :: <symbol> = #"simple";
  // TODO: PERFORMANCE: This is only recorded for ease of macro-expansion
  // in the dynamic case. When that works off the parsed signature,
  // we can stop recording this.
  lazy slot form-signature-and-body-fragment,
    required-init-keyword: signature-and-body-fragment:;
end;

define inline method method-definition? (object) => (well? :: <boolean>)
  instance?(object, <method-definition>)
end method;

define method form-complete? (form :: <method-definition>) => (well? :: <boolean>)
  ~form-dynamic?(form)
end method;

define leaf packed-slots form-properties (<method-definition>, <method-defining-form>)
  boolean slot form-sealed? = #f,    // vs. open
    init-keyword: sealed?:;  
  boolean slot form-sideways? = #f, // vs. upwards (maybe!)
    init-keyword: sideways?:;  
end packed-slots;

/*
define method method-inlineable? (x :: <method-defining-form>)
  let policy = form-inline-policy(x);
  ~(policy == #"not-inline" | policy == #"default-inline")
    | form-compile-stage-only?(x)
end method;
*/

define method retract-body-fragments (x :: <method-defining-form>)
  next-method();
  // unless (method-inlineable?(x))
    form-body(x) := #f;
  // end unless;
end method;

define method retract-body-fragments (x :: <method-definition>)
  next-method();
  // unless (method-inlineable?(x))
    form-signature-and-body-fragment(x) := #f;
  // end unless;
end method;

define method strip-incremental-slots (x :: <method-defining-form>)
  next-method();
  retract-body-fragments(x);
end method;

define method form-define-word
    (form :: <method-definition>) => (word :: <symbol>)
  #"method"
end method;

define method form-compile-stage-only? 
    (form :: <method-definition>) => (well? :: <boolean>)
  let binding = form-variable-binding(form);
  let gf-def = untracked-binding-definition(binding);
  form-compile-stage-only?(gf-def)
end method;

// Conversion to a definition object.

define serious-program-warning <malformed-define-method>
  format-string "Missing variable name in method definition. Skipping."
end;

/*
define method fragment-context-id (name :: <variable-name-fragment>)
  as-lowercase(as(<string>,name.fragment-identifier))
end method;
*/

define &definition method-definer
  { define ?mods:* \method ?:name ?signature-and-body:* end }
    => do-define-method(form, mods, name, signature-and-body);
  { define ?mods:* \method ?other:* end }
    => begin
         note(<malformed-define-method>,
              source-location: fragment-source-location(form));
         #();
       end;
end &definition;

define function do-define-method (fragment, mods, name, signature-and-body)
  let (options, adjectives) = parse-method-adjectives(name, mods);
  let (signature, body) = parse-method-signature(name, signature-and-body);
  ensure-next-method-binding(signature);
  let method-definition
    = apply(make, <method-definition>,
            source-location: fragment-source-location(fragment),
            variable-name:   name,
            adjectives:      adjectives,
            signature:       signature,
            body:            body,
            signature-and-body-fragment: signature-and-body,
            options);
  if (form-sealed?(method-definition))
    let domain-fragment
      = generate-implicit-domain-definition-fragment(method-definition);
    pair(method-definition,
	 top-level-convert(method-definition, domain-fragment))
  else
    list(method-definition)
  end
end function;

define function generate-implicit-domain-definition-fragment
    (form :: <method-definition>) => (fragment)
  let name = form-variable-name(form);
  let required-specs 
    = spec-argument-required-variable-specs(form-signature(form));
  let type-expressions
    = map(spec-type-expression, required-specs);
  let modifiers
    = if (form-sideways?(form))
        #{ sideways }
      else
        #{ }
      end;
  // TODO: The need for this as-body is to work around dubious template 
  // hygiene.
  as-body
    (#{ define ?modifiers sealed domain ?name (??type-expressions, ...) });
end function;

// Modifier parsing.

define property <method-upgrade-property> => upgrade?: = #t
  value upgrade     = #t;
  value not-upgrade = #f;
end property;

define property <method-sealed-property> => sealed?: = #f
  value sealed = #t;
  value open   = #f;
end property;

define property <method-sideways-property> => sideways?: = #f
  value sideways = #t;
  // The following becomes #f when the compiler is being compiled as a
  // single component.
  value compiler-sideways = #t; 
end property;

define property <method-inline-property> 
    => inline-policy: = #"default-inline"
  value inline         = #"inline";
  value inline-only    = #"inline-only";
  value may-inline     = #"may-inline";
  value default-inline = #"default-inline";
  value not-inline     = #"not-inline";
end property;

define constant method-adjectives =
  list(<method-sealed-property>,
       <method-sideways-property>,
       <method-inline-property>,
       <method-upgrade-property>);

define function parse-method-adjectives
    (name, adjectives-form) => (initargs, adjectives)
  parse-property-adjectives(method-adjectives, adjectives-form, name)
end function;

// Signature parsing.

define function parse-method-signature 
    (name, sig-fragment) => (signature, body)
  let (sig-spec, body)
    = parse-signature-as
        (<method-signature-spec>, sig-fragment);
  verify-signature-spec(name, sig-spec, sig-fragment);
  values(sig-spec, as-body(body))
end;

define method verify-signature-spec (name, sig :: <method-signature-spec>, sig-fragment)
  // TODO: Do the checking.
end method;

// Default a #next next-method if necessary.

define function ensure-next-method-binding 
    (sig :: <method-signature-spec>) => ()
  unless (spec-argument-next-variable-spec(sig))
    spec-argument-next-variable-spec(sig)
      := make(<next-variable-spec>,
              variable-name: as-name(#{ ?=next-method }));
  end;
end function;

//// Definition protocol.

define serious-program-warning <method-not-on-generic-function>
  slot condition-definition,
    required-init-keyword: definition:;
  format-string 
    "This method extends the definition %= which does not define a "
    "generic function - ignoring.";
  format-arguments definition;
end serious-program-warning;

define serious-program-warning <method-on-undefined-variable>
  slot condition-variable,
    required-init-keyword: variable:;
  format-string
    "This method attempts to extend the definition of an undefined variable "
    "%s from another library - ignoring.";
  format-arguments variable;
end serious-program-warning;

define method install-top-level-form-bindings
    (form :: <method-definition>) => ()
  let name = form-variable-name(form);
  let binding = lookup-binding(name, reference?: #f);
  let def = binding-definition(binding, default: #f);
  let def = ~instance?(def, <missing-definition>) & def;
  if (~def & binding-imported-into-library?(binding))
    note(<method-on-undefined-variable>,
	 source-location: form-source-location(form),
	 variable: binding);
    ignore-modifying-definition(name, form);
  elseif (def & ~instance?(def, <generic-definition>))
    note(<method-not-on-generic-function>,
	 source-location: form-source-location(form),
	 definition:      def);
    ignore-modifying-definition(name, form);
  else
    if (~def)
      // Generate an implicit definition.  This must be the first method
      // definition, since otherwise would have generated an implicit def
      // last time around and name would be defined.
      // TODO: While hygiene is suspect, ensure the result of constructing
      // an implicit definition is in the same place as the generating 
      // form.
      with-fragment-info (name)
	add-implicit-generic-definition(form);
      end;
    end;
    add-modifying-definition(name, form);
  end;
end method;

define function retract-implicit-definition (name, form)
  let binding = untracked-lookup-binding(name);
  let def = untracked-binding-definition(binding, default: #f);
  if (def & (def.form-parent-form == form))
    assert(form-implicitly-defined?(def), "Non-implicit derived form %s?", def);
    // TODO: maybe should just move it to the next method def...
    // Except would need to move all the dependencies as well..
    retract-top-level-form(def);
  end;
end function;

define method uninstall-top-level-form-bindings
    (form :: <method-definition>) => ()
  let name = form-variable-name(form);
  remove-modifying-definition(name, form);
  retract-implicit-definition(name, form);
end method;

define method uninstall-form-models (form :: <modifying-form>)
  if (form.form-model)
    let binding = form-variable-binding(form);
    note-removing-modifying-models(binding);
    // At this point we should have no references to modifying models of
    // the binding.
    debug-assert((untracked-binding-model-object-if-computed(binding) ~==
		   form.form-model) |
		  // Could be in the middle of retracting...
		  // (really want to test stage-being-retracted? but that's
		  // not available here)
		  ~untracked-binding-definition(binding).form-models-installed?,
		 "Dangling single-method GF model!");
    form.form-model := #f;
  end;
end method;

// Generate an implicit definition.

define program-warning <implicit-generic-definition>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string "Implicitly defining the generic function %=.";
  format-arguments variable-name;
end program-warning;

define function add-implicit-generic-definition (form :: <method-definition>)
  let (pattern, required)= method-definition-argument-pattern(form);
  let sig-spec = form-signature(form);
  let required-fragments
    = map(spec-variable-name, spec-argument-required-variable-specs(sig-spec));
  let rest-fragment
    = pattern == #"variable" 
        & spec-variable-name(spec-argument-rest-variable-spec(sig-spec));
  add-implicit-generic-definition-from-pattern
    (form, form-variable-name(form), pattern, required-fragments, rest-fragment);
end function;

define function add-implicit-generic-definition-from-pattern
    (parent, name, pattern, required-fragments, rest-fragment) => (fragment)
  // TODO: Put back this note but under policy control.
  /*
  note(<implicit-generic-definition>,
       source-location: form-source-location(parent),
       variable-name:   name);
  */
  let optional-fragments
    = select (pattern)
        #"fixed"    
          => #{ };
        #"variable" 
          => #{ #rest ?rest-fragment };
        #"keyword"  
          => #{ #key };
      end;
  let template
    = if (empty?(required-fragments))
        #{ define generic ?name
               (?optional-fragments) }
      else
        #{ define generic ?name
               (??required-fragments, ..., ?optional-fragments) }
      end;
  add-derived-top-level-fragment(parent, as-body(template));
end function;

/*
define function method-definitions-congruent?
    (defs :: <sequence>) => (congruent? :: <boolean>)
  block (return)
    let pattern = #f;
    let required = #f;
    for (def in defs)
      let (def-pattern, def-required)
        = method-definition-argument-pattern(def);
      if (~pattern)
        pattern  := def-pattern;
        required := def-required;
      elseif (def-pattern ~== pattern | def-required ~= required)
        return(#f);
      end;
    finally
      return(#t);
    end;
  end;
end function;
*/

// Pattern is one of #"fixed", #"variable", and #"keyword".

define function method-definition-argument-pattern 
    (form :: <method-definition>) 
      => (pattern :: <symbol>, required :: <integer>)
  let sig-spec = form-signature(form);
  let pattern = case
                  spec-argument-key?(sig-spec)  => #"keyword";
                  spec-argument-rest?(sig-spec) => #"variable";
                  otherwise                     => #"fixed";
                end;
  values(pattern, spec-argument-number-required(sig-spec))
end function;

// Add generic-function method definitions in their defining order in the first place;
// Local modifying definitions are kept sorted in defining order;
// This makes method-number assignments in the back-end much more efficient

define method add-local-definition
  (definitions :: <definitions>, definition :: <method-definition>) => (new-definitions :: <definitions>)
  add-in-order(definitions, definition, test: defined-after?)
end method;

define inline method add-in-order(c :: <collection>, x,
				  #key test = \==)
  let found? = #f;
  collecting ()
    for (e in c)
      unless (found?)
        if (test(x, e))
          found? := #t;
          collect(x)
        end if;
      end unless;
      collect(e);
    finally unless (found?) collect(x) end;
    end for
  end collecting;
end method;

define method form-handled-by-make-when-dynamic?
    (form :: <method-definition>) => (well? :: <boolean>)
  form-class(form) == #"initializer"
    | begin
        let parent = form.form-parent-form;
        // most commonly parent == #f, so check that first for speed even 
        // though it's redundant with the other checks.
        parent & instance?(parent, <slot-definition>)
      end
end method;

//// Utilities.

define inline function choose-instances 
    (type :: <type>, sequence :: <sequence>) => (subsequence :: <sequence>)
  choose(rcurry(instance?, type), sequence)
end function;
