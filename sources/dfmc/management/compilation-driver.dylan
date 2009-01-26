module: dfmc-management
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Derived information generation.

define macro for-library-method
  { for-library-method (?doc:expression,
			?stage:expression of ?:variable in ?:expression)
      ?:body
    end }
    => { do-for-library-method
           (?doc, ?stage, method (?variable) ?body end, ?expression) }
end macro;

define macro compiling-forms
  { compiling-forms (?stage:expression of ?form:variable in ?cr:expression)
     ?:body
    end }
    => { let stage = ?stage;
	 for (?form in compilation-record-top-level-forms(?cr))
	   with-dependent (stage of ?form)
	     ?body
	   end
         end
       }
end macro;

//// Compilation to models.

define function ensure-library-models-computed (ld :: <compilation-context>)
  with-simple-abort-retry-restart
      ("Skip model object computation.", "Retry model object computation.")
    debug-out(#"internal", "Model object computation: %s\n", ld);
    // We want to compute the model object for each definition.
    timing-compilation-phase ("Model object computation" of ld)
      compute-and-install-model-objects(ld);
    end;
  end;
  debug-out(#"internal", "Model object computation complete.\n");
end function;

define function ensure-library-models-finished (ld :: <compilation-context>)
  with-simple-abort-retry-restart
      ("Skip model object finishing", "Retry model object finising")
    debug-out(#"internal", "Finishing off model objects:\n");
    timing-compilation-phase ("Model finishing" of ld)
      finish-models(ld);
    end;
  end;
  debug-out(#"internal", "Model checking complete.\n");
end function;


define function ensure-library-models-checked (ld :: <library-description>)
  with-simple-abort-retry-restart
      ("Skip model object checking", "Retry model object checking")
    debug-out(#"internal", "Model checking:\n");
    timing-compilation-phase ("Model checking" of ld)
      check-models(ld);
    end;
  end;
  debug-out(#"internal", "Model checking complete.\n");
end function;

//// Model object generation.

define function compute-and-install-model-objects
    (description :: <compilation-context>) => ()
  note-compilation-from-definitions-started(description);
  for (cr in description.compilation-context-records)
    let name = cr.compilation-record-source-record.source-record-name;
    source-record-progress-text("Computing data models for %s.dylan", name);
    unless (cr.compilation-record-model-heap)
      progress-line("Computing models for %s.dylan", name);
      compiling-forms ($compilation of form in cr)
	unless (form-ignored?(form))
	  maybe-compute-and-install-form-model-objects(form);
	  finish-installing-form-model-objects(form);
	end unless;
      end compiling-forms;
    end unless;
    source-record-progress-report();
  end for;
end function;

// Note that maybe-compute-and-install-form-model-objects is the client
// entry point, while compute-and-install-form-model-objects is for
// implementors of the protocol. This allows us to relieve specific
// implementing methods of setting up the appropriate context, since we
// can guarantee that this has been done before
// compute-and-install-form-model-objects is called.

define sideways method maybe-compute-and-install-form-model-objects
    (form :: <top-level-form>) => ()
  // No installation is required by default.
end method;

// We set up the dependent form in the following methods since they can
// be invoked directly through lazy compilation, as well as via
// compute-and-install-model-objects above.

define sideways method maybe-compute-and-install-form-model-objects
    (form :: <top-level-init-form>) => ()
  // A top-level initialization will never get touched recursively,
  // so just do it.
  unless (form.form-init-method)
    with-dependent ($compilation of form)
      debug-assert(~form-ignored?(form), "Compiling ignored form");
      compute-and-install-form-model-objects(form);
    end;
  end;
end method;

define sideways method maybe-compute-and-install-form-model-objects
    (form :: <variable-defining-form>) => ()
  unless (form.form-models-installed?)
    // The computation of named models may be lazily invoked, even from
    // other libraries, hence the following.
    with-dependent-context ($compilation of form)
      debug-assert(~form-ignored?(form), "Compiling ignored form");
      note-compilation-from-definitions-started(current-library-description());
      form.form-models-installed? := #"processing";
      block ()
	with-fragment-info (form-variable-names(form).first)
	  compute-and-install-form-model-objects(form);
	end;
      cleanup
	form.form-models-installed? := #f;
      end block;
      form.form-models-installed? := #t;
    end;
  end unless;
end method;

define sideways method compute-and-install-form-model-objects
    (form :: <variable-defining-form>) => ()
  // progress-line("  Form: %=", form);
  for (name in form-variable-names(form))
    if (form-defines-variable?(form, name))
      let model = compute-form-model-object(form, name);
      // progress-line("    => %=", model);
      define-model-object(name, model);
    end;
  end;
end method;

define sideways method finish-installing-form-model-objects
    (form :: <top-level-form>) => ()
  // No installation by default.
end;

//// Model object finalization.

//define function finish-models (ld :: <compilation-context>)
//  // format-out("finish-models on %=\n", ld);
//  for (cr in compilation-context-records(ld))
//    compiling-forms ($compilation of form in cr)
//      finish-form-models(form);
//    end;
//  end;

define function finish-models (ld :: <library-description>) => ()
  local method form-mapper (ld :: <library-description>,
			    model-selector :: <function>,
			    model-handler :: <function>)
	 => ()
	  with-library-context (ld)
	    for (cr in library-description-compilation-records(ld))
	      compiling-forms ($compilation of form in cr)
		  model-selector(form, model-handler)
	      end
	    end
	  end
	end method;
  with-library-context(ld)
    finish-library-models(ld);
    // There are no static models to finish in loose mode.
    unless (library-forms-dynamic?(ld))
      let form = ld.language-definition.namespace-definition;
      when (form)
	with-dependent-context ($compilation of form)
	  let lib = form-model(form);
	  size(library-accumulating-defined-generics(lib)) := 0;
	end with-dependent-context;
      end when;
      if (~library-description-system-class-init-code(ld))
	let cr = first(library-description-compilation-records(ld));
	with-dependent ($compilation of cr)
	  library-description-system-class-init-code(ld) 
	    := convert-top-level-initializer
		  (finish-class-models
		     (ld, method (ld :: <library-description>, 
			    model-handler :: <function>)
			    form-mapper
			      (ld, finish-class-model-forms, model-handler)
			  end method));
  	  library-description-system-gf-init-code(ld) 
	    := convert-top-level-initializer
	          (finish-generic-function-models
                     (ld, method (ld :: <library-description>, model-handler :: <function>)
			    form-mapper(ld, finish-gf-model-forms, model-handler)
			  end method));
	  finish-method-models
	    (ld, method (ld :: <library-description>, model-handler :: <function>)
		   form-mapper(ld, finish-method-model-forms, model-handler)
		 end method);
	end
      end if;
      let form = ld.language-definition.namespace-definition;
      when (form)
	with-dependent-context ($compilation of form)
	  let lib :: <&library> = form-model(form);
	  ^library-defined-generics(lib)
	    := mapped-model(as(<simple-object-vector>, library-accumulating-defined-generics(lib)));
	  size(library-accumulating-defined-generics(lib)) := 0;
	end with-dependent-context;
      end when;
    end unless;
  end
end function;


define method finish-method-model-forms 
     (form :: <top-level-form>, model-handler :: <function>)
  => ()
end method;


define method finish-method-model-forms (form :: <generic-definition>, 
					 model-handler :: <function>)
  => ()
  map-definition-variable-models
    (form, method (gf) 
             if (instance?(gf, <&generic-function>))
               do(model-handler, ^generic-function-methods(gf)) 
             else
               model-handler(gf)
             end
           end)
end method;

define constant $major-minor-checks-only-environment-variable-name
  = "OPEN_DYLAN_MAJOR_MINOR_CHECKS_ONLY";

define function library-description-build-count
    (ld :: <project-library-description>) => (build-count :: <integer>)
  let change-count
    = library-description-change-count(ld);
  let major-minor-checks-only?
    = environment-variable($major-minor-checks-only-environment-variable-name);
  if (major-minor-checks-only?)
    $library-build-count-only-wildcard
  else
    change-count
  end if;
end function;


define method finish-library-models (ld :: <library-description>) => ()
  let form = ld.language-definition.namespace-definition;
  when (form)
    with-dependent-context ($compilation of form)
      let lib :: <&library> = form-model(form);
      ^library-major-version(lib) := library-description-major-version(ld);
      ^library-minor-version(lib) := library-description-minor-version(ld);
      ^library-build-count(lib)   := library-description-build-count(ld);
      let used-libraries
        = map-as(<simple-object-vector>,
		 method (uld)
		   ^make(<&used-library>, 
			 used-library: library-description-model(uld),
			 binding: if (library-dynamically-bound-in?(ld, uld))
				    #"loose"
				  else
				    #"tight"
				  end)
		 end,
		 library-description-used-descriptions(ld));
      ^used-libraries(lib) := mapped-model(used-libraries);
    end with-dependent-context;
  end when;
end method;


// define variable *register-subclass-dependent-generics-at-runtime?* = #t;
// define constant *register-subclass-dependent-generics?*            = #f;

// Add to something like conversion/define-method.dylan

// Unlike the class finisher, this one doesn't (currently) return any code to be
// executed, nor does it do anything which is a function of all the methods, hence
// it's overly simple for the complicated mechanism that invokes it.
define function finish-method-models
    (ld :: <library-description>, form-mapper :: <function>)
 => ()
  local method walk-it (m :: <&method>) => ()
          let gf = ^method-generic-function(m);
	  // HACK: SEEMS TOO LOW LEVEL -- DETAILS ABOUT SINGLE SEALED GF-METHODS
          let name 
            = if (gf == m)
                mapped-model(as-lowercase(as(<string>, debug-name(m))))
              else
                gf
              end;
  	  // ^debug-name(m) := name;
	  /*
	  when (*register-subclass-dependent-generics?*)
	    let m-library = model-library(m);
	    when (gf ~== m & m-library == model-library(gf))
	      let dependent-classes
		= collecting ()
		    let sig = ^function-signature(m);
		    ^map-congruency-classes-sov
		       (method (class :: <&class>)
			  unless (class == dylan-value(#"<object>"))
			    let iclass = ^class-implementation-class(class);
			    unless (^iclass-subclasses-fixed?(iclass))
			      if (*register-subclass-dependent-generics-at-runtime?*
				    | m-library == form-library(model-definition(class)))
				^iclass-subclass-dependent-generics(iclass)
				  := mapped-model
				       (add-new(^iclass-subclass-dependent-generics(iclass), gf));
			      else 
				collect(class);
			      end if;
			    end unless;
			  end unless;
			end method,
			^signature-required(sig),
			^signature-number-required(sig));
		  end collecting;
	      unless (empty?(dependent-classes))
		let form
		  = model-definition(m);
		let dependent-classes
		  = mapped-model(as(<simple-object-vector>, dependent-classes));
		let code
		  = (with-expansion-source-form(form)
		       let register = dylan-value(#"%register-subclasses-dependent-generic");
		       #{ ?register(?gf, ?dependent-classes) }
		     end with-expansion-source-form);
		let init-model = convert-top-level-initializer(code);
		form-init-method(form) := init-model;
	      end unless;
	    end when;
  	  end when;
          */
	end method;
  form-mapper(ld, walk-it);
end function;
  

define method finish-class-model-forms (form :: <top-level-form>, model-handler :: <function>)
 => ()
end method;

define method finish-class-model-forms (form :: <class-definition>, model-handler :: <function>)
  map-definition-variable-models(form, model-handler)
end method;


define method finish-gf-model-forms (form :: <top-level-form>, model-handler :: <function>)
 => ()
end method;


define method finish-gf-model-forms (form :: <generic-definition>, model-handler :: <function>) => ()
  // map-definition-variable-models(form, model-handler)
  let model = binding-model-object(form-variable-binding(form));
  if (instance?(model, <&generic-function>))
    when (*profile-all-calls?*)
      unless (model-compile-stage-only?(model) | ~model-has-definition?(model))
	let ld   = model-library(model);
	let lib  = language-definition(model-library(ld));
	let &lib = namespace-model(lib);
	add!(library-accumulating-defined-generics(&lib), model);
      end unless;
    end when;
    model-handler(model, form)
  end;
end method;


define method finish-gf-model-forms (form :: <domain-definition>, model-handler :: <function>) => ()
  let gf-model = binding-model-object(form-variable-binding(form));
  if (instance?(gf-model, <&generic-function>)) 
    // Filter out conversions to non-generics.
    model-handler(gf-model, form)
  end;
end method;


define method finish-gf-model-forms (form :: <method-definition>, model-handler :: <function>) => ()
  // Find implicit generic function definition, if any.
  let model = binding-model-object(form-variable-binding(form));
  if (instance?(model, <&generic-function>))
    // Only do this is it hasn't been converted to a singular method definition.
    model-handler(model, form)
  end if;
end method;


define method finish-gf-model-forms (form :: <class-definition>, model-handler :: <function>) => ()
  for (slot-spec :: <slot-definition> in form-slot-specs(form))
    finish-gf-model-forms(slot-spec, model-handler)
  end for
end method;

define method finish-gf-model-forms (form :: <slot-definition>, model-handler :: <function>) => ()
  let getter-def = form-getter-definition(form);
  let setter-def = form-setter-definition(form);
//  break("slot def %= - getter = %= setter = %=",form,  getter-def, setter-def);
  if (getter-def) finish-gf-model-forms(getter-def, model-handler) end;
  if (setter-def) finish-gf-model-forms(setter-def, model-handler) end;
end method;



define function map-definition-variable-models (form :: <variable-defining-form>, 
						model-handler :: <function>)
 => ()
  for (binding in form-defined-bindings(form))
    let model = binding-model-object(binding, default: #f);
    if (model) model-handler(model) end;
  end for
end function;


//define function finish-models (ld :: <library-description>) => ()
//  // format-out("finish-models on %=\n", ld);
//  with-library-context (ld)
//    let classes = library-description-class-models(ld);
//    let gfs = library-description-generic-function-models(ld);
//    library-description-models-closed-for-finishing?(ld) := #t;
//    let pick-one = if (size(classes) > 0) first(classes)
//		   elseif (size(gfs) > 0) first(gfs)
//		   else #f
//		   end if;
//    if (pick-one)
//      let model-def = model-definition(pick-one);
//      assert(~(form-init-method(model-def)));
//      with-dependent ($compilation of model-def)
//	let class-code = finish-class-models(ld, classes);
//	let gf-code = finish-generic-function-models(ld, gfs);
//	form-init-method(model-def) := convert-top-level-initializer(#{ ?class-code ; ?gf-code });
//      end
//    end if
//  end;
//end function;


//define method finish-form-models (form :: <variable-defining-form>) => ();
//  // format-out("finish-form-models on %=\n", form);
//  for (binding in form-defined-bindings(form))
//    let model = binding-model-object(binding, default: #f);
//    if (model) finish-model(model) end;
//  end for
//end method;

//define method finish-form-models
//    (form :: <top-level-form>) => ()
//  // Nothing to do by default?
//  // format-out("default finish-form-models on %=\n", form);
//end method;

//define method finish-form-models (form :: <variable-defining-form>) => ();
//  // format-out("finish-form-models on %=\n", form);
//  for (binding in form-defined-variables(form))
//    let model = binding-model-object(binding, default: #f);
//    if (model) finish-model(model) end;
//  end for
//end method;

//// @@@@ Is this necessary in light of the above?
//define method finish-form-models
//    (form :: <method-definition>) => ()
//  // format-out("finish-form-models on %=\n", form);
//  with-fragment-info(form-body(form))
//    finish-model(form-model(form));
//  end;
//end method;

//define method finish-form-models
//    (form :: <domain-definition>) => ()
//  // format-out("finish-form-models on %=\n", form);
//  finish-model(form-model(form));
//end method;

//define method finish-model (model)
//  model;
//  // format-out("default finish-model on %=\n", model);
//  #f
//end method;


//// Model object consistency checking.

// Really just does congruency / sealing violation detection on generic
// functions.

// TODO: Shouldn't this just be done in model object generation for
// define method? Well, maybe, except that you really want to look at
// the complete set of methods for duplicates for example, whereas not 
// all methods are going to be available method-by-method.
define function check-models (ld :: <compilation-context>)
  for (cr in compilation-context-records(ld))
    compiling-forms ($compilation of form in cr)
      if (~form-dynamic?(form))
        check-form-models(form);
      end;
    end;
  end;
end function;

define method check-form-models
    (form :: <top-level-form>) => ()
  // Nothing to check by default.
end method;

define method check-form-models
    (form :: <method-definition>) => ()
  with-fragment-info(form-body(form))
    let model = form-model(form);
    if (model) check-model(model) end;
  end;
end method;

define method check-form-models
    (form :: <domain-definition>) => ()
  let model = form-model(form);
  if (model) check-model(model) end;
end method;

//// Code walking utilities.

define function do-for-library-method (doc, stage, f, ld)
  for (cr in compilation-context-records(ld), firstp = #t then #f)
    unless (cr.compilation-record-model-heap)
      if (doc)
	let sr = cr.compilation-record-source-record;
	let name = sr.source-record-name;
	progress-line("%s %s.dylan", doc, name);
	source-record-progress-text("%s %s.dylan", doc, name);
      end;
      // TODO: CORRECTNESS: What's the recommended way to disable this
      // stuff in interactive mode?
      if (firstp & ~instance?(ld, <interactive-layer>))
	let class-init-code = library-description-system-class-init-code(ld);
	let gf-init-code = library-description-system-gf-init-code(ld);
	with-dependent(stage of cr)
	  if (class-init-code) f(class-init-code) end;
	  if (gf-init-code) f(gf-init-code) end;
        end
      end if;
      compiling-forms (stage of form in cr)
	do(f, form-top-level-methods(form));
      end;
    end;
    source-record-progress-report();
  end;
end function;

//// DFM generation.

define function ensure-library-dfm-computed (ld :: <compilation-context>)
  debug-out(#"internal", "DFM generation: %s\n", ld);
  let i :: <integer> = 0;
  timing-compilation-phase ("DFM generation" of ld)
    for-library-method ("Computing code models for", $compilation of m in ld)
      ensure-method-dfm-or-heap(m);
      let sexp = print-method(make(<string-stream>), m, output-format: #"sexp");
      visualization-report(#"initial-dfm", list(i, sexp));
      i := i + 1;
    end;
  end;
  visualization-report(#"finished", #());
  debug-out(#"internal", "DFM generation complete.\n");
end function;

//// Binding checking.

define function ensure-library-bindings-checked
    (ld :: <project-library-description>)
  debug-out(#"internal", "Checking bindings:\n");
  timing-compilation-phase ("Checking bindings" of ld)
    check-bindings(ld);
  end;
  debug-out(#"internal", "Bindings checking done.\n");
end function;

define program-warning <module-exported-but-not-defined>
  slot condition-library,
    required-init-keyword: library:;
  slot condition-module,
    required-init-keyword: module:;
  format-string
    "The module %= is exported from %= but is never defined.";
  format-arguments
    module, library;
end program-warning;

define program-warning <binding-exported-but-not-defined>
  slot condition-module,
    required-init-keyword: module:;
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string "The binding %s is exported from %= but never defined.";
  format-arguments variable-name, module;
end program-warning;

define program-warning <binding-created-but-not-defined>
  slot condition-module,
    required-init-keyword: module:;
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string "The binding %s is created in %= but never defined.";
  format-arguments variable-name, module;
end program-warning;

/*
define serious-program-warning <binding-referenced-but-not-defined>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    
    "The binding %s is referenced but not defined or imported.";
  format-arguments 
    variable-name;
end serious-program-warning;
*/

define program-warning <binding-defined-but-not-used>
  slot condition-variable-name,
    required-init-keyword: variable-name:;
  format-string    
    "The binding %s is defined but not referenced or exported.";
  format-arguments 
    variable-name;
end program-warning;

// generated-definition? is just used to decide whether to issue
// defined but not used warnings.  Skip the warnings if the definition
// was implicitly generated by some other form.

// A definition is generated if it has a parent other than a macro call, or
// if it has a macro call as a parent, and the macro call itself is
// generated.
define method generated-definition? (form :: <top-level-form>)
  let parent = form.form-parent-form;
  parent & (~instance?(parent, <macro-call-form>)
	      | generated-definition?(parent))
end;

// Except for slot definitions, which have a class definition for a parent
// but still come from user code...
define method generated-definition? (form :: <slot-definition>)
  let parent = form.form-parent-form;
  parent & generated-definition?(parent)
end method;

// Implicitly defined generics still represent a user-defined variable...
define method generated-definition? (form :: <generic-definition>)
  if (form-implicitly-defined?(form))
    generated-definition?(form.form-parent-form)
  else
    next-method()
  end
end method;  

// Implicitly defined accessor methods still represent a user-defined variable
define method generated-definition? (form :: <method-definition>)
  let parent = form.form-parent-form;
  if (parent & instance?(parent, <slot-definition>))
    generated-definition?(parent)
  else
    next-method()
  end;
end;

define function form-containing-source-location
    (form :: false-or(<top-level-form>))
  form & (form.form-source-location
	    | form-containing-source-location(form.form-parent-form))
end function;

define method check-bindings
    (description :: <project-library-description>) => ()
  let library = language-definition(description);
  let library-def = library.namespace-definition;
  if (library-def)
    remove-dependent-program-conditions(library-def, $compilation-mask);
    with-dependent ($compilation of library-def)
      for (binding in undefined-module-bindings-in(library))
	note(<module-exported-but-not-defined>, 
	     library: library,
	     module:  binding.name);
      end;
      // TODO: should we check something about imported bindings?
      // Really only want to check if explicitly imported rather than
      // implicitly.  But might want a low-priority note even if implicitly
      // imported, for users who want a list for switching to an explicit
      // import list...
    end;
    // TODO: a note about imported modules none of whose bindings are
    // actually referenced.
    for (module in defined-modules-in(library))
      let module-def = module.namespace-definition | library-def;
      // We don't actually track access to bindings here, since then we would
      // have to record a dependency in every binding.  So those are handled
      // by retracting on every compile.  We still need the with-dependent to
      // find the warnings.
      // TODO: make this really be incremental.  Need to retract if
      // any binding definition or any binding local reference changes.
      remove-dependent-program-conditions(module-def, $compilation-mask);
      with-dependent ($compilation of module-def)
	let defined-but-not-used = #();
	// TODO: There's currently no way to tell whether a binding is both
	// referenced and exported when undefined.
	for (binding in module.namespace-local-bindings)
	  let def? = untracked-binding-definition(binding, default: #f);
	  if (def? & ~instance?(def?, <missing-variable-defining-form>))
	    unless (exported?(binding) |
		    generated-definition?(def?) |
		    binding-local-references?(binding))
	      defined-but-not-used := pair(pair(def?, binding.name),
					   defined-but-not-used);
	    end unless;
	  else // else undefined
	   /* This just duplicates warnings at the point of reference
	    if (binding-local-references?(binding))
	      note(<binding-referenced-but-not-defined>,
		   variable-name: binding.name);
	    end;
	   */
	    if (exported?(binding))
	      let mod-loc = form-source-location(namespace-definition(module));
	      note(if (created?(binding)) <binding-created-but-not-defined>
		   else <binding-exported-but-not-defined> end,
		   source-location: mod-loc,
		   module:          module,
		   variable-name:   binding.name);
              // if (def?)
              //   install-missing-definition(binding);
              // end;
	    end;
	  end if;
	end for;
	do(method (info :: <pair>)
	     note(<binding-defined-but-not-used>,
		  source-location: form-containing-source-location(info.head),
		  variable-name:   info.tail);
	   end,
	   sort!(defined-but-not-used,
		 test: method (info1 :: <pair>, info2 :: <pair>)
			 let form1 :: <top-level-form> = info1.head;
			 let form2 :: <top-level-form> = info2.head;
			 defined-before?(form2, form1)
		       end));
      end with-dependent;
    end for;
  else
    // TODO: is this a good place to give a "no library definition" warning?
  end;
end method;

/// TYPE INFERENCE

define function ensure-library-type-estimated (ld :: <compilation-context>)
  // Run the typist over the forms of this library.
  debug-out(#"internal", "Inferring library types: %s\n", ld);
  // The cache was established when the <compilation-context> was made,
  // because some early-bird optimizers want it.
  timing-compilation-phase ("Initial type inference" of ld)
    // Loop over all the top-level forms, estimating types.
    for (cr in ld.compilation-context-records)
      // progress-line("Compilation Record: %=", cr);
      compiling-forms ($compilation of form in cr)
        // progress-line("  Top Level Form: %=", form);
        with-simple-abort-retry-restart
            ("Skip type inferring this form", 
             "Retry type inferring this form")
          type-estimate-top-level-form(form);
        end
      end
    end for;
  end timing-compilation-phase;
  debug-out(#"internal", "Done inferring library types: %s\n", ld);
end function;

/// OPTIMIZING

define function ensure-library-optimized (ld :: <compilation-context>)
  debug-out(#"internal", "Optimizing library: %s.\n", ld);
  let i :: <integer> = 0;
  timing-compilation-phase ("Optimization" of ld)
    for-library-method ("Optimizing", $compilation of m in ld)
      visualization-report(#"optimizing", i);
      optimize-method(m);
      let sexp = print-method(make(<string-stream>), m, output-format: #"sexp");
      visualization-report(#"optimized-dfm", list(i, sexp));
      i := i + 1;
    end;
    do(compact-coloring-info, compilation-context-records(ld));
    dispatch-decisions-progress-line(ld);
  end;
  visualization-report(#"finished", #());
  debug-out(#"internal", "Done optimizing library: %s.\n", ld);
end function;

/* TODO: OBSOLETE?
// Debug-only, not used anywhere.
define method optimize-compilation-record
    (desc :: <library-description>, cr :: <compilation-record>)
      => ()
  compiling-forms ($compilation of form in cr)
    optimize-form(form);
  end;
end method;

define method optimize-form
    (form :: <top-level-form>) => ()
  if (form.form-init-method) optimize-method(form.form-init-method); end;
end method;

*/

define method optimize-method (m :: <&method>)
  with-simple-abort-retry-restart
      ("Skip optimizing this method", 
       "Retry optimizing for this method")
    progress-line("<<<<");
    progress-line("  Method before: %=.", m);
    progress-line("  ----");
    run-compilation-passes(m);
    progress-line("  Method after: %=.", m);
    progress-line(">>>>");
  end;
end method;

define method compact-coloring-info (cr :: <compilation-record>)
  let dds = cr.compilation-record-dispatch-decisions;
  when (empty?(dds))
    dds := (cr.compilation-record-dispatch-decisions := #[]);
  end;
  // Unless already compacted
  unless (instance?(dds, <vector>))
    // sort by location, and remove any entries shadowed by another entry
    // with same location but "lower" type.
    local method less? (dd1 :: <simple-object-vector>,
			dd2 :: <simple-object-vector>)
	    let ordered-types = #[#"not-all-methods-known",
				  #"failed-to-select-where-all-known", 
				  #"lambda-call", 
				  #"inlining", 
				  #"slot-accessor-fixed-offset", 
				  #"eliminated", 
				  #"dynamic-extent",
				  #"bogus-upgrade"];
	    let start-offset-1 = dd1[0];
	    let start-offset-2 = dd2[0];
	    start-offset-1 < start-offset-2 |
	      (start-offset-1 = start-offset-2 &
		 begin
		   let end-offset-1 = dd1[1];
		   let end-offset-2 = dd2[1];
		   end-offset-2 < end-offset-1 |
		     (end-offset-2 = end-offset-1 &
			begin
			  let dispatch-1 = dd1[2]; // dispatch type
			  let dispatch-2 = dd2[2];
			  dispatch-1 ~== dispatch-2 &
			    dispatch-1 ~== any?(method (dt)
						  (dt == dispatch-1 |
						     dt == dispatch-2)
						    & dt
						end, ordered-types)
			end)
		 end)
	  end;
    let v = sort!(as(<vector>, dds), test: less?);
    let count = 3 * v.size;
    for (i from 1 below v.size,
	 dd = v[0] then begin
			  let new-dd = v[i];
			  if (new-dd[0] = dd[0] & new-dd[1] = dd[1])
			    count := count - 3;
			    v[i] := #f;
			    dd
			  else
			    new-dd
			  end
			end)
    end;
    let dds = make(<vector>, size: count);
    for (i from 0 below v.size,
	 j = 0 then begin
		      let dd = v[i];
		      if (dd)
			dds[j] := dd[0];
			dds[j + 1] := dd[1];
			dds[j + 2] := dd[2];
			j + 3
		      else
			j
		      end if
		    end)
    end for;
    cr.compilation-record-dispatch-decisions := dds;
  end;
end;

define method dispatch-decisions-progress-line (ld :: <compilation-context>)
  let calls-unoptimized = 0;
  let calls-optimized = 0;
  for (cr in ld.compilation-context-records)
    let dds = cr.compilation-record-dispatch-decisions;
    for (i from 2 below dds.size by 3)
      let type = dds[i];
      if (member?(type, #[#"not-all-methods-known",
			  #"failed-to-select-where-all-known"]))
	calls-unoptimized := calls-unoptimized + 1;	
      elseif (member?(type, #[#"lambda-call", 
			      #"inlining", 
			      #"slot-accessor-fixed-offset", 
			      #"eliminated", 
			      #"dynamic-extent"]))
	calls-optimized := calls-optimized + 1;
      end;
    end;
  end;
  let calls-processed = calls-optimized + calls-unoptimized;
  if (calls-processed > 0)
    progress-line("A total of %= calls were processed, and %= of these (%=%%) were optimized.",
      calls-processed, calls-optimized, 
      round/(calls-optimized * 100, calls-processed))
  end if;
end dispatch-decisions-progress-line;

