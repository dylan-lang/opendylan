Module:   dfmc-definitions
Synopsis: The constant definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Utility.

define function method-fragment? (fragment) => (well? :: <boolean>)
  instance?(fragment, <macro-call-fragment>) 
    & (lookup-binding(fragment-macro(fragment)) == dylan-binding(#"method"))
end function;

//// Constant definitions.

// Constant definition objects.

define class <constant-definition> (<binding-defining-form>) 
end class;

define method form-implicitly-defined? (form :: <constant-definition>) => (well?)
  let parent = form.form-parent-form;
  // most commonly parent == #f, so check that first for speed even though
  // it's redundant with the other check.
  parent & instance?(parent, <module-definition>);
end method;

define class <literal-value-constant-definition>
    (<constant-definition>, <literal-value-binding-defining-form>)
end class;

define method form-define-word 
    (form :: <constant-definition>) => (word :: <symbol>)
  #"constant"
end method;

define class <constant-method-definition> (<constant-definition>,
					   <method-defining-form>)
end;

define method strip-incremental-slots (x :: <constant-method-definition>)
  next-method();
  // unless (method-inlineable?(x))
    form-init-expression(x) := #f;
  // end unless;
end method;

define method form-compile-stage-only? 
    (form :: <constant-definition>) => (well? :: <boolean>)
  form-inline-policy(form) == #"inline-only" 
    // & ~form-dynamic?(form)
    & form-models-evaluated?(form)
end method;

define method form-models-evaluated? 
    (form :: <constant-definition>) => (well? :: <boolean>)
  let vars = form-defined-bindings(form);
  // Should just use untracked-binding-model-object, but that has
  // a weird return value convention, so until that's fixed, use a hammer..
  without-dependency-tracking
  every?(method (var)
	   found?(binding-model-object(var, default: $unfound))
	 end,
	 vars)
  end;
end method;


// Conversion to a definition object.

define &definition constant-definer
  { define ?mods:* constant ?:variable = ?:expression }
    => do-define-constant
         (form, mods, variable, expression, #f);
  { define ?mods:* constant (?bindings:*) = ?:expression }
    => do-define-constant
         (form, mods, bindings, expression, #f);
end &definition;

// Modifier parsing.

define property <constant-inline-property> 
    => inline-policy: = #"default-inline"
  value inline-only    = #"inline-only";
  value inline         = #"inline";
  value may-inline     = #"may-inline";
  value default-inline = #"default-inline";
  value not-inline     = #"not-inline";
end property;

define constant constant-adjectives
  = list(<constant-inline-property>);

define constant constant-method-adjectives
  = concatenate(constant-adjectives, list(<method-upgrade-property>));

define function do-define-constant
    (fragment, mods, bindings, init, booted?)
  let bindings-spec = parse-value-bindings(bindings);
  let required-specs = spec-value-required-variable-specs(bindings-spec);
  let single-required? =
    size(required-specs) = 1 &
    ~spec-value-rest?(bindings-spec);
  let first-variable-spec = single-required? & first(required-specs);
  let type-expression =
    single-required? & spec-type-expression(first-variable-spec);
  let constant-method?
    = single-required? &
        method-fragment?(init) &
        instance?(type-expression, <variable-name-fragment>) &
        lookup-binding(type-expression) == dylan-binding(#"<object>");
  let (initargs, adjectives) =
    if (constant-method?)     
      parse-property-adjectives
	(constant-method-adjectives, mods, "a constant method")
    else 
      parse-property-adjectives
	(constant-adjectives, mods, "a constant");
    end if;
  list(if (constant-method?)
	 let variable-name = spec-variable-name(first-variable-spec);
	 let (signature, body) =
	   parse-method-signature(variable-name, fragment-argument(init));
	 apply(make, <constant-method-definition>,
	       source-location:  fragment-source-location(fragment),
	       variable-name:    variable-name,
	       type-expressions: list(type-expression),
	       adjectives:       adjectives,
	       bindings-spec:    bindings-spec,
	       init-expression:  init,
	       signature:        signature,
	       body:             body,
	       initargs)
       else
	 let literal-value? =
	   single-required? &
	   instance?(init, <literal-constant-fragment>) &
	   instance?(type-expression, <variable-name-fragment>);
	 let variable-names = bound-variable-names(bindings-spec);
	 apply(make, if (booted?)
		       if (literal-value?)
			 <literal-value-booted-constant-definition>
		       else
			 <booted-constant-definition>
		       end
		     else
		       if (literal-value?)
			 <literal-value-constant-definition>
		       else
			 <constant-definition>
		       end
		     end,
	       source-location:  fragment-source-location(fragment),
	       variable-name:    if (size(variable-names) == 1)
				   variable-names.first
				 else
				   variable-names
				 end,
	       type-expressions: bound-type-expressions(bindings-spec),
	       adjectives:       adjectives,
	       bindings-spec:    bindings-spec,
	       init-expression:  init,
	       initargs)
       end)
end;

// Called by the boot code.

define class <booted-constant-definition> (<constant-definition>) end;

define class <literal-value-booted-constant-definition>
    (<booted-constant-definition>, <literal-value-binding-defining-form>)
end class;

define method define-booted-constant (variable-name, type, value)
 => (definition)
  do-define-constant
    (variable-name, 
     #{ }, #{ ?variable-name :: ?type }, value, #t).first;
end method;
