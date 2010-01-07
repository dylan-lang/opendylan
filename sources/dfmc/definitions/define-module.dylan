Module:   dfmc-definitions
Synopsis: The module definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Module definitions.

define class <module-definition> (<namespace-defining-form>) end;

define method form-define-word 
    (form :: <module-definition>) => (word :: <symbol>)
  #"module"
end method;

// TODO: These definition objects are really stubs. The real story for
// definition-based namespace processing is yet to come.

define &definition module-definer
  { define ?mods:* module ?:name ?clauses:* end }
    => do-define-module(form, mods, name, clauses);
end &definition;


define sideways method make-module-definition (#key name,
					       source-location = #f,
					       parent-form = #f,
					       use-clauses,
					       create-clauses = #(),
					       export-clauses = #())
  make(<module-definition>,
       source-location: source-location,
       adjectives: #(),
       name: name,
       parent-form: parent-form,
       use-clauses: use-clauses,
       create-clauses: create-clauses,
       export-clauses: export-clauses);
end method;

define method do-define-module (form, mods, name, clauses)
  let (uses, creates, exports)
    = parse-namespace-clauses(name, clauses);
  let definition
    = define-parsed-module(name.fragment-identifier,
                           source-location: fragment-source-location(form),
                           use-clauses:     uses,
                           create-clauses:  creates,
                           export-clauses:  exports);
  list(definition)
end method;

define method generate-initializer-source-with-namespace
    (form :: <module-definition>, module :: <module>) 
 => (source)
  with-expansion-source-form (form)
    let model      = namespace-model(module);
    let var-name   = namespace-model-variable(module);
    let code       = #{ define constant ?var-name = ?model; ?var-name };
    code
  end;
end method;

// also used by boot code.
define sideways method define-parsed-module (name, #key source-location = #f,
					                use-clauses,
					                create-clauses,
					                export-clauses)
 => (module :: <module-definition>, module :: false-or(<module>))
  let definition = make(<module-definition>,
			source-location: source-location,
			adjectives:      #(),
			name:            name,
			use-clauses:     use-clauses,
			create-clauses:  create-clauses,
			export-clauses:  export-clauses);
  /*
  let module     = compiling-dylan-library?()
                     & install-top-level-form(definition);
  */
  let module 
    = if (compiling-dylan-library?() | single-file-project-hack?())
        pre-install-top-level-form(definition);
      else
        #f
      end;
  values(definition, module)
end method;

define method single-file-project-hack? () => (well? :: <boolean>)
  // If our source record name doesn't match our actual module, we're
  // pre-switch in a single file project hack.
  let cr = current-compilation-record();
  let sr = compilation-record-source-record(cr);
  let cr-module = compilation-record-module(cr);
  namespace-name(cr-module) ~== source-record-module-name(sr)
end method;

define serious-program-warning <library-not-yet-defined>
  slot condition-module-name,
    required-init-keyword: module-name:;
  format-string "No define library seen for the current library - "
                "skipping definition of the %s module.";
  format-arguments module-name;
end serious-program-warning;

define method pre-install-top-level-form (form :: <module-definition>)
  with-dependent ($top-level-processing of form)
    if (current-library-defined?())
      let module =
        define-and-install-module(definition:     form,
                                  name:           form-namespace-name(form),
				  use-clauses:    form-use-clauses(form),
				  create-clauses: form-create-clauses(form),
				  export-clauses: form-export-clauses(form));
      module
    end if;
  end with-dependent;
end method;

define method install-top-level-form (form :: <module-definition>)
  with-dependent ($top-level-processing of form)
    if (~current-library-defined?())
      note(<library-not-yet-defined>,
           source-location: form-source-location(form),
           module-name:     form-namespace-name(form));
      #f
    else
      let module 
        = lookup-module(form-namespace-name(form), default: #f)
            | pre-install-top-level-form(form);
      if (module)
	let ld = current-library-description();
        let module-cr = form-compilation-record(form);
	// Force reparsing of any cr's with undefined modules (except
        // ourselves, since we must be attempting the single-file 
        // project hack if we're doing anything at all).
	for (cr in ld.library-description-compilation-records)
	  if (cr ~== module-cr & ~cr.compilation-record-module)
	    ld.compiled-to-definitions? := #f;
	    cr.compilation-record-definitions-installed? := #f;
	  end;
	end;
	// For some reason no longer remembered by anybody, we don't create
	// the constant variable definitions for bootstrapped modules in
	// the dylan library...
	unless (booted-module?(module))
	  let initializer-source
	    = generate-initializer-source-with-namespace(form, module);
	  add-derived-top-level-fragment(form, initializer-source);
	end;
      end;
      form.form-top-level-installed? := #t;
      module
    end if;
  end with-dependent;
end method;

define method retract-using-modules (module :: <module>)
  let name = namespace-name(module);
  for (m in defined-modules-in(home-library(module)))
    if (namespace-uses?(m, name))
      // Note that 'm' can't be a <dylan-user-module> since that doesn't
      // use anything in its own library (i.e. the one in the Dylan library
      // doesn't use anything, and the ones in other libraries only use Dylan).
      // All other modules have namespace-definition's so it's safe to use it.
      retract-top-level-form(m.namespace-definition);
    end;
  end;
end method;

define function retract-derived-definitions (definition :: <module-definition>)
  let cr = form-compilation-record(definition);
  for (form in compilation-record-top-level-forms(cr))
    when (form.form-parent-form == definition)
      retract-top-level-form(form);
    end;
  end;
end function;

define method uninstall-top-level-form (form :: <module-definition>)
  retract-derived-definitions(form);
  let ld = form-library(form);
  let library = ld.language-definition;
  let name = form-namespace-name(form);
  let module = lookup-module-in(library, name, default: #f);
  if (module & module.namespace-definition == form)
    retract-using-modules(module);
    // Force rescan of cr's so find the delete module
    ld.compiled-to-definitions? := #f;
    for (cr in ld.library-description-compilation-records)
      if (cr.compilation-record-module == module)
	cr.compilation-record-definitions-installed? := #f;
      end;
    end;
    undefine-module!(module);
    namespace-model(module) := #f;
  end;
  form.form-top-level-installed? := #f;
end method;
