Module:   dfmc-conversion
Synopsis: Shared function processing.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function compute-signature-using-types
    (sig-spec :: <signature-spec>, 
     required-types, values-types, rest-value-type, keys, key-types)
 => (model)
  ^make(<&signature>,
        rest-value?:     spec-value-rest?(sig-spec),
	rest?:           spec-argument-rest?(sig-spec),
	all-keys?:       spec-argument-all-keys?(sig-spec),
	key?:            if (spec-argument-key?(sig-spec)) #t else #f end,
	number-values:   spec-value-number-required(sig-spec),
	number-required: spec-argument-number-required(sig-spec),
	required:        as-sig-types(required-types),
	values:          as-sig-types(values-types),
	rest-value:      rest-value-type,
	keys:            immutable-model(as(<simple-object-vector>, keys)),
	key-types:       as-sig-types(key-types))
end function;

define function compute-signature
    (form, sig-spec :: <signature-spec>) 
 => (model, static? :: <boolean>)
  // Try to evaluate each specializer in turn.
  let (required-types, required-types-static?) 
    = compute-variable-specs-types
        (form, spec-argument-required-variable-specs(sig-spec));
  // Keys are always static because they're syntactically constrained to
  // be literals.
  let keys
    = compute-variables-spec-keys(form, sig-spec);
  let (key-types, key-types-static?)
     = compute-variable-specs-types
         (form, spec-argument-key-variable-specs(sig-spec));
  let (values-types, values-types-static?)
    = compute-variable-specs-types
        (form, spec-value-required-variable-specs(sig-spec));
  let (rest-value-type, rest-value-type-static?)
    = compute-variables-spec-rest-value-type(form, sig-spec);
  let sig
    = compute-signature-using-types
       (sig-spec, required-types, values-types, rest-value-type,
          keys, key-types);
  let static?
    = required-types-static? 
        & key-types-static? 
        & values-types-static?
        & rest-value-type-static?;
  values(sig, static?)
end function;

// This old warning should no longer be necessary. We drop back
// to the dynamic case, in which case any problems are reported
// as for references in code.

/*
define program-warning <dynamic-specializer-expressions>
  slot condition-form,
    init-keyword: form:;
  slot condition-specializer-expressions,
    init-keyword: specializer-expressions:;
  format-string
    "The specializers %= of %= cannot be computed at compile-time -- optimizations may be missed.";
  format-arguments
    specializer-expressions, form;
end program-warning;
*/

define function compute-variable-specs-types
    (form, variable-specs :: <variable-specs>) 
 => (types :: <simple-object-vector>, static? :: <boolean>)
  let static-types = make(<vector>, size: size(variable-specs));
  collecting (dynamic-types)
    for (var-spec in variable-specs,
	 i :: <integer> from 0)
      let type 
        = ^top-level-eval-type
             (spec-type-expression(var-spec), on-failure: #f);
      static-types[i] :=
	if (type)
	  type
	else
	  collect-into(dynamic-types, spec-type-expression(var-spec));
	  dylan-value(#"<object>")
	end;
      check-signature-variable(form, var-spec, type);
    end;
    if (~empty?(collected(dynamic-types)))
      /*
      let collected-dynamic-types = collected(dynamic-types);
      note(<dynamic-specializer-expressions>,
           source-location: if (form)
			      form-source-location(form)
			    else
			      // kludge for anonymous methods
			      fragment-source-location(collected-dynamic-types[0])
			    end,
           form: form | "an unnamed method", 
           specializer-expressions: collected-dynamic-types,
	   format-arguments: list(collected-dynamic-types,
				  // kludge for anonymous methods
				  form | "an unnamed method"));
      */
      values(static-types, #f);
    else
      values(static-types, #t);
    end;
  end;
end;

define method compute-variables-spec-keys
    (form, spec :: <signature-spec>) => (keys)
  collecting (as <vector>)
    for (key-spec in spec-argument-key-variable-specs(spec))
      let key = ^top-level-eval(spec-keyword-expression(key-spec));
      collect(key);
    end;
  end;
end method;

define method compute-variables-spec-rest-value-type
    (form, sig-spec :: <signature-spec>) => (type, static? :: <boolean>)
  let spec-rest? = spec-value-rest?(sig-spec);
  if (spec-rest?)
    let rest-spec       = spec-value-rest-variable-spec(sig-spec);
    let type-expression = spec-type-expression(rest-spec);
    let (type, static?)
      = if (type-expression)
          let type 
            = ^top-level-eval-type(type-expression, on-failure: #f);
          if (type)
            values(type, #t)
          else
            // Warn.
            values(dylan-value(#"<object>"), #f)
          end
        else
          values(dylan-value(#"<object>"), #t)
        end;
    check-signature-variable(form, rest-spec, type);
    values(type, static?)
  else
    values(#f, #t) // Statically "known" by omission...
  end;
end method;

define program-warning <variable-name-looks-like-a-type>
  slot condition-function,
    required-init-keyword: function:;
  slot condition-type-name,
    required-init-keyword: type-name:;
  format-string
    "The untyped variable %= in the parameter list of %= looks like a type.";
  format-arguments
    type-name, function;
end program-warning;

define method check-signature-variable 
    (form, spec :: <variable-spec>, type)
  if (~type | ^id?(type, dylan-value(#"<object>")))
    let name = spec-variable-name(spec);
    if (name-uses-type-convention?(name))
      note(<variable-name-looks-like-a-type>,
           source-location: fragment-source-location(name),
           context-id: form-context-id(form),
           function: form,
           type-name: name);
    end;
  end;
end method;

define method name-uses-type-convention? (name :: <variable-name-fragment>)
  macro-case (name)
    { "<" ## ?:name ## ">" } => #t;
    { ?other:* } => #f;
  end;
end method;
