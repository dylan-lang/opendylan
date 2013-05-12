Module:   dfmc-conversion
Synopsis: The generic function definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Generic function modeling.


define compiler-sideways method compute-form-model-object
    (form :: <generic-definition>, variable-name :: <variable-name-fragment>)
 => (model)
  // Can we simplify this generic to a bare method?
  let equivalent-method-def = form-equivalent-method-definition(form);
  let model = equivalent-method-def & untracked-ensure-form-model(equivalent-method-def);
  if (model)
    let binding = form-variable-binding(form);
    note-binding-dependency(binding, dep$modifying-models);
    model
  else
    let signature-spec = form-signature(form);
    let (signature, static?)
      = compute-signature(form, signature-spec);
    if (~static?)
      // We couldn't fully compute its signature, so we retreat to the dynamic
      // case. This will force all its methods to be dynamic.
      debug-out(#"dynamic",
		">>> Retreating to the dynamic case for %=\n", form);
      form-evaluation-tried-and-failed?(form) := #t;
      #f
    else
      // The form is not dynamic (which better mean not being loosely compiled), so we
      // only need to make an incremental generic function if it is open, in which case
      // other libraries might be able to add sealed domains.
      let sealed? = form-sealable?(form);
      ^make(if (sealed? | form-compiler-open?(form))
	      <&sealed-generic-function>
	    else
	      <&incremental-generic-function>
	    end,
            definition:     form, 
            debug-name: 
              mapped-model(as-lowercase(as(<string>, variable-name))),
            signature:      signature,
            signature-spec: signature-spec,
            dynamic-extent: form-parameters-have-dynamic-extent?(form),
            sealed?:        sealed?);
    end;
  end;
end method;

define method form-equivalent-method-definition
    (form :: <generic-definition>)
 => (maybe-method-definition :: false-or(<method-definition>))
  // Is this generic function the product of a single method definition
  // that could just as well have been a function definition? Note that
  // the form-sealed? test isn't redundant with form-implicitly-defined?
  // because of our default-open mode hack.
  // TODO: CORRECTNESS: Do this analysis in the method computation, and
  // compute a different class of method that looks like a generic
  // function too so that the difference can't be determined at run-time.
  //---*** The default-open hack is long gone, is there something to
  //       clean up here?
  if (form-sealable?(form) & form-implicitly-defined?(form)
        & ~form-compile-stage-only?(form))
    form-single-modifying-simple-method-definition(form);
  end;
end method;

define method form-single-modifying-simple-method-definition
    (form :: <generic-definition>)
 => (maybe-method-definition :: false-or(<method-definition>))
  let binding = form-variable-binding(form);
  let definitions = binding-modifying-definitions(binding);
  let method-definition = #f;
  block (return)
    for (def in definitions)
      if (instance?(def, <domain-definition>))
        // No worries.
      elseif (instance?(def, <method-definition>) 
                & (form-class(def) == #"simple" 
                     | form-class(def) == #"initializer"))
        // It's not anything magic like a slot accessor.
        if (method-definition)
          // More than one method here, so no joy.
          return(#f);
        else
          method-definition := def;
        end;
      else
        // Bad luck.
        return(#f);
      end;
    finally
      method-definition
    end;
  end;
end method;

define method single-method-generic-function? 
    (gf :: <&generic-function>) => (well? :: <boolean>)
  #f
end method;

define method single-method-generic-function? 
    (gf :: <&method>) => (well? :: <boolean>)
  #t
end method;

define method binding-method-models
    (binding :: <module-binding>) => (method-models :: <models>)
  choose-instances
    (<&method>, binding-certain-modifying-models(binding, method-definition?))
end method;

define method binding-domain-models
    (binding :: <module-binding>) => (domain-models :: <models>)
  choose-instances
    (<&domain>, binding-certain-modifying-models(binding, domain-definition?))
end method;


define method binding-imported-domain-models
    (binding :: <module-binding>) => (domain-models :: <models>)
  choose-instances
    (<&domain>, 
       binding-certain-modifying-models
         (binding, domain-definition?, imported-only?: #t))
end method;


// define inline function map-collecting-unless-false 
//     (f :: <function>, s :: <sequence>) => (s :: <list>)
//   collecting ()
//     for (item in s)
//       let result = f(item);
//       if (result) collect(result) end;
//     end;
//   end;
// end function;

define method ^generic-function-imported-defined-domains 
    (function :: <&generic-function>) => (domains :: <sequence>)
  let binding = model-variable-binding(function);
  if (binding)
    note-binding-dependency(binding, dep$modifying-models);
    // We can't cache this because it's not necessarily the whole story.
    binding-imported-domain-models(binding);
  else
    #() // This generic function didn't have an explicit definition.
  end if
end method;

define method ^generic-function-explicitly-defined-domains 
    (function :: <&generic-function>) => (domains :: <sequence>)
  let binding = model-variable-binding(function);
  if (binding)
    note-binding-dependency(binding, dep$modifying-models);
    binding-defined-domains(binding)
      | ( binding-defined-domains(binding)
	   := binding-domain-models(binding) )
  else
    #() // This generic function didn't have an explicit definition.
  end if
end method;

define method ^generic-function-explicitly-defined-methods 
    (function :: <&generic-function>) => (methods :: <sequence>)
  let binding = model-variable-binding(function);
  if (binding)
    binding-defined-methods(binding)
      | begin
	  note-binding-dependency(binding, dep$modifying-models);
	  binding-defined-methods(binding)
	    := binding-method-models(binding);
        end
  else
    #() // This generic function didn't have an explicit definition.
  end if
end method;

define function ^generic-function-local-methods-known
    (function :: <&generic-function>) => (methods :: <sequence>)
  let binding = model-variable-binding(function);
  if (binding)
    note-binding-dependency(binding, dep$modifying-models);
    choose-instances
      (<&method>, untracked-lookup-certain-local-modifying-models
	            (binding, method-definition?));
  else
    #() // This generic function didn't have an explicit definition.
  end if
end function;

define compiler-sideways method ^generic-function-methods 
    (function :: <&generic-function>) => (methods :: <sequence>)
  unless (%generic-function-methods-initialized?(function))
    let form = model-definition(function);
    if (form)
      with-dependent-context ($compilation of form)
	let methods = ^generic-function-explicitly-defined-methods(function);
	function.%generic-function-methods
	  := mapped-model(as(<list>,methods));
      end;
    end;
    function.%generic-function-methods-initialized? := #t;
  end;
  function.%generic-function-methods
end method;

// Methods known in current library
define function ^generic-function-methods-known
    (function :: <&generic-function>) => (methods :: <sequence>)
  if (^generic-function-sealed?(function) |
	current-library-description?(model-library(function)))
    ^generic-function-methods(function)
  else
    ^generic-function-explicitly-defined-methods(function)
  end;
end function;

// TODO: Less redundant computation.

// Domains known in current library
define function ^generic-function-domains-known
    (function :: <&generic-function>) => (domains :: <sequence>)
  if (^generic-function-sealed?(function) |
	current-library-description?(model-library(function)))
    ^generic-function-domains(function)
  else
    ^generic-function-explicitly-defined-domains(function)
  end;
end function;

define compiler-sideways method ^generic-function-domains 
    (gf :: <&generic-function>) => (res :: <list>)
  unless (%generic-function-domains-initialized?(gf))
    let form = model-definition(gf);
    if (form)
      with-dependent-context ($compilation of form)
	let domains = ^generic-function-explicitly-defined-domains(gf);
	gf.%generic-function-domains
	  := mapped-model(as(<list>,domains));
        if (instance?(gf, <&incremental-generic-function>)
	      & form-inline-policy(form) ~== #"inline-only")
	  for (d :: <&domain> in domains, 
	       a = #f then begin ^domain-next(d) := a; d end)
	  finally ^incremental-gf-domain-info(gf) := a
	  end for
	end if;
      end;
    end;
    gf.%generic-function-domains-initialized? := #t;
  end;
  gf.%generic-function-domains
end method;

//// Model Finishing

define function finish-generic-function-models (ld :: <library-description>,
						form-mapper :: <function>)
 => (code)
//  let dtab :: <table> = make(<table>);
//  let ctab :: <table> = make(<table>);
  form-mapper(ld, method (gf :: <&generic-function>, form :: <top-level-form>) => ()
		    let doms = ^generic-function-domains(gf);
//		    select (form by instance?)
//		      <generic-definition> => ;
//                      <method-definition> =>
////			unless (form-complete?(form))
////			  if (current-library-description?(model-library(gf)))
////			    ^method-incomplete-count(gf) := ^method-incomplete-count(gf) + 1
////			  else
////			    ctab[gf] := element(ctab, gf, default: 0) + 1
////			  end if;
////			end unless
//			;
//		      <domain-definition> =>
//			let form :: <domain-definition> = form;
//			let d :: false-or(<&domain>) = form-model(form);
//			if ( d
//			     & instance?(gf, <&incremental-generic-function>) 
//			     & ld ~== model-library(gf)
//			     & ~form-compile-stage-only?(model-definition(gf)))
//			  let stuff :: <list> = element(dtab, gf, default: #());
//			  element(dtab, gf) := pair(d, stuff)
//			end if;
//		    end select
		  end method);
  let code = #{ };
//  for (stuff :: <list> keyed-by gf in dtab)
//    let gf = mapped-model(gf);
//    let doms = mapped-model(as(<simple-object-vector>, stuff));
//    code := #{ ?code %add-domains(?gf, ?doms); }
//  end for;
//  for (count :: <integer> keyed-by gf in ctab)
//    let gf = mapped-model(gf);
//    code := #{ ?code %tick-method-incomplete-count(?gf, ?count); }
//  end for;
  code
end function;


//// Incremental mode dynamic expansion

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <generic-definition>) => ()
  form-evaluation-tried-and-failed?(form) := #f;
  unless (form-sealed-if-private?(form) |
	    member?(#"dynamic", form-adjectives(form)) |
	    binding-accessible-to-other-libraries?(form-variable-binding(form)))
    note(<inaccessible-open-definition>,
	 binding: form-variable-binding(form),
	 source-location: form-source-location(form));
  end;
  if (form-dynamic?(form))
    compute-and-install-form-dynamic-init-method(form);
  else
    let name = form-variable-name(form);
    let model = compute-form-model-object(form, name);
    if (model)
      define-model-object(name, model);
    else
      compute-and-install-form-dynamic-init-method(form);
    end;
  end;
end method;

define method compute-form-dynamic-init-code
    (form :: <generic-definition>) => (computed-method)
  let name
    = form-variable-name(form);
  let sig-spec 
    = form-signature(form);
  let constructor 
    = if (form-in-place-redefinition?(form))
	let sealed
	  = form-sealable?(form);
	let module 
	  = form-dynamic?(form) & form-module-model(form);
	let definer
	  = dylan-value(#"%redefine-generic");
  	#{ ?definer(?name, ?"name", ?module, %signature(?sig-spec), ?sealed) }
      else
	let definer
	  = dylan-value(#"%define-generic");
	#{ ?definer(?name, %signature(?sig-spec)) }
      end if;
  // #{ %initialize-binding(?name, ?constructor); }
  constructor
end method;

define compiler-sideways method form-binding-guaranteed-initialized?
   (form :: <generic-definition>) => (well? :: <boolean>)
  #t
end method;

define compiler-sideways method compute-form-hollow-object
   (form :: <generic-definition>) => (model :: <&generic-function>)
  let variable-name = form-variable-name(form);
  let signature-spec = form-signature(form);
  ^make(<&incremental-generic-function>, 
        definition:     form, 
        debug-name:     mapped-model(as-lowercase(as(<string>, variable-name))),
	module:         form-module-model(form),
        signature:      #f,
	signature-spec: signature-spec,
        sealed?:        form-sealable?(form),
	type-complete?: #t,
        method-complete?: #t,
        signatured?:    #f);
end method;

define method form-in-place-redefinition?
    (form :: <generic-definition>) => (well? :: <boolean>)
  let binding = form-variable-binding(form);
  binding-previously-defined?(binding)
    & instance?(binding-previous-definition(binding), form.object-class);
end method;

define compiler-sideways method retract-form-model-objects (form :: <generic-definition>) => ()
  library-description-system-gf-init-code(form-library(form)) := #f;
  next-method()
end method;
