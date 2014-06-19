Module:   dfmc-conversion
Synopsis: The domain definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Domain modeling.


define compiler-sideways method compute-and-install-form-model-objects
    (form :: <domain-definition>) => ()
  form-evaluation-tried-and-failed?(form) := #f;
  if (form-dynamic?(form))
    compute-and-install-form-dynamic-init-method(form);
  else
    compute-and-install-form-model-objects-statically(form);
  end;
end method;


define method compute-and-install-form-model-objects-statically
    (form :: <domain-definition>) => ()
  let name = form-variable-name(form);
  let (gf-def, gf-static?) = get-form-generic-definition(form);
  let model = #f;
  // Make sure there is a definition for the domain.
  // TODO: if there isn't, should ignore this form (skip model
  //  computation, etc).
  if (~gf-def | ~gf-static? | ~(model := compute-form-model-object(form, name)))
    debug-out(#"dynamic", ">>> Retreating to the dynamic case for %=\n", form);
    form-evaluation-tried-and-failed?(form) := #t;
    compute-and-install-form-dynamic-init-method(form)
  else
    form-model(form) := model;
    let domain-locally-defined? = form-library(gf-def) == form-library(form);
    let gf-runtime-sealed? = (form-sealable?(gf-def) | form-compiler-open?(gf-def));

    // Don't do a domain add if:
    // (1) the gf is compile-time only
    // (2) it's going to be created on the gf model
    // (3) the gf isn't going to store a domain anyway
    // (4) the domain comes from a method definition so a method-domain will be added
    //     by the method adder
    // (5) the gf isn't implemented as a gf after all.
    unless (form-compile-stage-only?(gf-def)
	      | domain-locally-defined?
	      | gf-runtime-sealed?
	      | instance?(form-parent-form(form), <method-definition>)
	      | single-method-generic-function?(lookup-model-object(name, reference?: #f)))
      let code 
	= (with-expansion-source-form(form)

	     let definer = dylan-value(if (~all-types-known-imported?(model-library(model), model))
					 #"%add-nonsiblinged-domain"
				       else
					 #"%add-domain"
				       end if);
	   #{ ?definer(?name, ?model) }
	  end with-expansion-source-form);
      let init-model = convert-top-level-initializer(code);
      form-init-method(form) := init-model;
    end unless;
  end if;
end method;


define compiler-sideways method compute-form-model-object
    (form :: <domain-definition>, name :: <variable-name-fragment>)
 => (model :: false-or(<&domain>))
  let (domain-types, static?)
    = compute-type-specs-types(form, form-domain-type-expressions(form));
  if (static?)
    let parent = form.form-parent-form;
    let dbg-name = immutable-model(as-lowercase(as(<string>, name)));

    // @@@@ No!  Domain-types is not a model, just a vector used during compilation.
    // It is saved in the database but is in a compiler-only slot.
    // let domain-types = as-sig-types(domain-types);

    if (instance?(parent, <method-definition>))
      let parent :: <method-definition> = parent;
      maybe-compute-and-install-form-model-objects(parent);
      let meth = form-model(parent);
      if (meth)
	^make(<&method-domain>, 
	      definition: form,
	      debug-name: dbg-name,
	      domain-types: domain-types,
	      method: meth)
      else // else method dynamic
	#f
      end
    else
      let domain-object :: <&standalone-domain>
	= ^make(<&standalone-domain>,
		definition:   form,
		debug-name:   dbg-name,
		domain-types: domain-types);
      domain-object
    end if
  else
    #f
  end
end method;


// @@@@ This error message probably shouldn't say <object>, it should just say
// that the compiler might not have optimal information.  <object> "being used"
// instead makes it sound like the domain will be defined incorrectl.  --gsb
define program-warning <dynamic-domain-type-expressions>
  slot condition-form,
    init-keyword: form:;
  slot condition-type-expressions,
    init-keyword: type-expressions:;
  format-string
    "The sealed domain types %= of %= can not be computed at compile-time "
    "-- \"<object>\" used instead.";
  format-arguments
    type-expressions, form;
end program-warning;

define function compute-type-specs-types
    (form, type-specs :: <sequence>) 
 => (types :: <simple-object-vector>, types-static? :: <boolean>)
  let static-types = make(<vector>, size: size(type-specs));
  collecting (dynamic-types)
    for (type-spec in type-specs,
	 i from 0)
      let type = ^top-level-eval-type(type-spec, on-failure: #f);
      static-types[i] :=
	if (type)
	  type
	else
	  collect-into(dynamic-types, type-spec);
	  dylan-value(#"<object>")
	end;
    end;
    if (~empty?(collected(dynamic-types)))
      let collected-dynamic-types = collected(dynamic-types);
      note(<dynamic-domain-type-expressions>,
           source-location: if (form)
			      form-source-location(form)
			    else
			      // kludge for anonymous methods
			      fragment-source-location
                                (collected-dynamic-types[0])
			    end,
           form: form, 
           type-expressions: collected-dynamic-types);
      values(#[], #f);
    else
      values(static-types, #t);
    end;
  end;
end;


define method compute-form-dynamic-init-code (form :: <domain-definition>)
 => (computed-domain)
  let parent = form.form-parent-form;
  if (instance?(parent, <method-definition>))
    // The dynamic method adder does the favor for us.
    #f
  else
    let name = form-variable-name(form);
    let ld = form-library(form);
    let lib = library-description-model(ld);
    let definer = dylan-value(#"%define-domain");
    let types = form-domain-type-expressions(form);
    (with-expansion-source-form (form)
      #{ ?definer(?name, ?lib, ??types, ...) }
    end);
  end if;
end method;
