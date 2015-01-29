Module:   dfmc-definitions
Synopsis: The library definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Library definitions.

define dood-class <library-definition> (<namespace-defining-form>)
end;

define method form-model (form :: <library-definition>)
  form.form-library.language-definition.namespace-model
end;

define method form-define-word
    (form :: <library-definition>) => (word :: <symbol>)
  #"library"
end method;

// TODO: These definition objects are really stubs. The real story for
// definition-based namespace processing is yet to come.

define &definition library-definer
  { define ?mods:* library ?:name ?clauses:* end }
    => do-define-library(form, mods, name, clauses);
end &definition;

define method do-define-library (form, mods, name, clauses)
  let (uses, creates, exports)
    = parse-namespace-clauses(name, clauses);
  let (definition, library)
    = define-parsed-library(name.fragment-identifier,
                            source-location: fragment-source-location(form),
                            use-clauses:     uses,
                            create-clauses:  creates,
                            export-clauses:  exports);
  let initializer-definitions
    = if (library)
        let initializer-source
          = generate-initializer-source-with-namespace(definition, library);
        top-level-convert(definition, initializer-source);
      else
        #()
      end;
  pair(definition, initializer-definitions)
end method;

define method generate-initializer-source-with-namespace
    (form :: <library-definition>, library :: <library>)
 => (source)
  with-expansion-source-form (form)
    let model      = namespace-model(library);
    let var-name   = namespace-model-variable(library);
    let code       = #{ define constant ?var-name = ?model;
                        %library-version-check(?var-name,
                                               primitive-runtime-module-handle()) };
    code
  end;
end method;

// also used by boot code
define sideways method define-parsed-library (name, #key source-location = #f,
                                                use-clauses,
                                                create-clauses,
                                                export-clauses)
 => (defn :: <library-definition>, library :: false-or(<library>))
  let definition = make(<library-definition>,
                        source-location: source-location,
                        adjectives:      #(),
                        name:            name,
                        use-clauses:     use-clauses,
                        create-clauses:  create-clauses,
                        export-clauses:  export-clauses);
  let library
    = with-expansion-source-location (#f, #f)
        // This macro wrapper prevents the source location of the define
        // library being inherited by locationless forms in libraries it
        // uses if caused to be compiled here.
        install-top-level-form(definition);
      end;
  values(definition, library)
end method;

define program-warning <duplicate-library-definition>
  slot condition-definition,
    init-keyword: definition:;
  slot condition-project,
    init-keyword: project:;
  format-string "Duplicate definition %= for library of %s ignored";
  format-arguments definition, project;
end program-warning;

define method install-top-level-form (form :: <library-definition>)
  with-dependent ($top-level-processing of form)
    let ld = current-library-description();
    let library =
      if (ld.language-definition.namespace-definition)
        note(<duplicate-library-definition>,
             project: ld.library-description-project,
             definition: form);
        #f
      else
        let name = form-namespace-name(form);
        let model = ^make-<&library>(name);
        let library
          = make-namespace(<library>,
                           definition:     form,
                           debug-name:     name,
                           use-clauses:    form-use-clauses(form),
                           create-clauses: form-create-clauses(form),
                           export-clauses: form-export-clauses(form),
                           model:          model);
        define-library!(library);
        ^library-description(model) := ld;
        library
      end;
    form.form-top-level-installed? := #t;
    library
  end with-dependent;
end method;

define method uninstall-top-level-form (form :: <library-definition>)
  let description = form-library(form);
//   let library = description.language-definition;
//   // TODO: Have to do this first to prevent circularity, since retracting
//   // the dylan-user module will attempt to uninstall this form.  Shouldn't
//   // need this once do library redefinition properly.
//   form.form-top-level-installed? := #f;
//   if (library.namespace-definition == form)
//     do(retract-module, defined-modules-in(library));
//     description.language-definition := #f;
//     ensure-language-definition(description);
//   end;
  retract-library-parsing(description);
  ensure-language-definition(description);
end method;
