Module:   dfmc-definitions
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: Converters
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method as-body (t :: <template>)
  let (failure, parsed-f)
    = parse-template-fragments-as
        ($start-body-constraint, template-fragments(t));
  if (failure)
    error("Template %= wouldn't parse as a body: %s.", 
          t, failure);
  end;
  parsed-f
end method;

define method as-body (f :: <body-fragment>)
  f
end method;

define method as-body (form)
  macro-case (form) { ?:body! } => body! end
end method;

define method as-name (form)
  macro-case (form) { ?:name } => name end
end method;

//// Boot hooks.

// Hack!!! Very fragile.

define method boot-definitions-form? (form)
  instance?(form, <function-call-fragment>)
    & begin 
        let func = fragment-function(form);
        instance?(func, <variable-name-fragment>)
          & func.fragment-identifier = #"boot-dylan-definitions"
      end;
end method;

//// Top-level conversion.

define method top-level-convert* (parent, fragments :: <sequence>)
  reduce(method (forms, fragment)
           concatenate(forms, top-level-convert(parent, fragment))
         end,
         #(),
         fragments)
end method;

define method top-level-convert (parent, fragment :: <fragment>)
  list(make(<top-level-init-form>, 
	    parent-form: parent,
            source-location: fragment-source-location(fragment),
            body:            fragment))
end method;

define method top-level-convert (parent, fragment)
  top-level-convert(parent, as-body(fragment))
end method;

// Bodies preserve top-levelness.

define method top-level-convert (parent, fragment :: <body-fragment>)
  let constituents = fragment-constituents(fragment);
  if (parent)
    top-level-convert*(parent, constituents);
  else
    // Source level, make a form for the browser to show.
    let parent = make(<macro-call-form>, // Like, statement macro, dig?
		      define-word: #f,
		      parent-form: #f,
		      source-location: fragment-source-location(fragment));
    let forms = top-level-convert*(parent, constituents);
    parent.form-derived-forms := as(<vector>, forms);
    pair(parent, forms)
  end if;
end method;

define serious-program-warning <skipping-form>
  slot condition-macro-name, required-init-keyword: macro-name:;
  format-string "Skipping %s macro call due to previous syntax error.";
  format-arguments macro-name;
end serious-program-warning;

define method top-level-convert (parent, fragment :: <macro-call-fragment>)
  let macro-variable = fragment.fragment-macro;
  let definition = macro-definition(macro-variable);
  with-fragment-info (fragment)
    // TODO: Real error handling and more formal recovery.
    handling-parse-errors
      top-level-convert-using-definition(parent, definition, fragment);
    on-error (condition)
      // Pass it on for collection.
      signal(condition); 
      // Warn about what we're skipping.
      note(<skipping-form>,
           source-location: fragment-source-location(fragment),
           macro-name:      macro-variable);
      // Skip by faking no definitions.
      #();
    end;
  end
end method;

define function let-fragment? 
    (fragment :: <local-declaration-fragment>) => (well? :: <boolean>)
  lookup-binding(fragment-macro(fragment)) == dylan-binding(#"let")
end function;

define method top-level-convert 
    (parent, fragment :: <local-declaration-fragment>)
 => (forms :: <sequence>)
  if (let-fragment?(fragment))
    let bindings-inits 
      = make(<sequence-fragment>, fragments: fragment-list-fragment(fragment));
    with-expansion-source-location 
        (fragment-record(fragment), fragment-source-position(fragment))
      top-level-convert(parent, #{ define variable ?bindings-inits });
    end;
  else
    next-method();
  end;
end method;

define serious-program-warning <interactive-tight-mode-definition>
  constant slot condition-macro-name, 
    required-init-keyword: macro-name:;
  constant slot condition-library-name, 
    required-init-keyword: library-name:;
  format-string 
    "Skipping interactive definition %s in tight mode library %s.";
  format-arguments 
    macro-name, library-name;
end serious-program-warning;

define method top-level-convert-using-definition
    (parent, 
       definition :: <&definition-definition>, 
       fragment :: <macro-call-fragment>)
  // Are definitions allowed?
  if (*interactive-compilation-layer* 
        & ~library-forms-dynamic?(current-library-description()))
    note(<interactive-tight-mode-definition>,
         source-location: fragment-source-location(fragment),
         macro-name:      fragment-macro(fragment),
         library-name:    library-description-emit-name
                            (current-library-description()));
    // Skip by faking no definitions.
    #();
  else
    let forms 
      = with-expansion-source-location 
            (fragment-record(fragment), fragment-source-position(fragment))
          form-expander(definition)(#f, fragment);
        end;
    for (form in forms)
      unless (form.form-parent-form)
        form.form-parent-form := parent
      end;
    end;
    forms
  end;
end method;

define method top-level-convert-using-definition
    (parent,
     definition :: <&converter-definition>,
     fragment :: <macro-call-fragment>)
  // !@#$ this is really sort of a hack
  if (lookup-binding(fragment-macro(fragment)) == dylan-binding(#"begin"))
    // strip off the begin
    macro-case (fragment-argument(fragment))
      { ?:body } => begin 
                      top-level-convert(parent, body);
                    end;
    end;
  else
    list(make(<top-level-init-form>, 
             parent-form: parent,
             source-location: fragment-source-location(fragment),
             body: fragment));
  end;
end method;


define method top-level-convert-using-definition
    (parent, 
     definition :: <expander-defining-form>,
     fragment :: <macro-call-fragment>)
  let expander = form-expander(definition);
  let expansion 
    = with-expansion-source-location 
          (fragment-record(fragment), fragment-source-position(fragment))
        expander(#f, fragment); 
      end;
  if (compiling-for-macroexpansion?())
    // Descend no further.
    list(make(<top-level-init-form>, 
              parent-form: parent,
              source-location: fragment-source-location(fragment),
              body: fragment));
  elseif (parent)
    top-level-convert(parent, expansion);
  else
    let (word, kind) = macro-definition-word(definition);
    let parent = make(<macro-call-form>,
		      define-word: if (kind == #"define") word end,
		      parent-form: #f,
		      source-location: fragment-source-location(fragment));
    let forms = top-level-convert(parent, expansion);
    parent.form-derived-forms := as(<vector>, forms);
    pair(parent, forms);
  end if;
end method;

//// Conversion entry points.

define method top-level-convert-forms 
    (cr :: <compilation-record>, fragment) => (forms)
  // Is this the magic boot marker? Definitions and sources are booted
  // separately because the definitions have to be installed in order 
  // to parse the sources.
  if (boot-definitions-form?(fragment))
    let parent = make(<macro-call-form>,
		      source-location: fragment-source-location(fragment),
		      define-word: #f,
		      parent-form: #f);
    let forms = booted-definition-sequence();
    for (form in forms)
      unless (form.form-parent-form) form.form-parent-form := parent end;
    end for;
    // Don't be tempted to give these source locations bogus source 
    // locations - it break hygienic resolution after database 
    // dumping because the current dylan-user is fake.
    let sources = booted-source-sequence();
    dynamic-bind (*fragment-context* = dylan-implementation-module())
      let source-forms = top-level-convert*(parent, sources);
      forms := concatenate!(forms, source-forms);
      parent.form-derived-forms := as(<vector>, forms);
    end;
    pair(parent, forms)
  else
    top-level-convert(#f, fragment)
  end;
end method;

define method add-derived-top-level-fragment
    (form :: <top-level-form>, fragment) => ()
  // assert(*current-stage* == $top-level-processing);
  let fragment-forms = top-level-convert(form, fragment);
  add-derived-top-level-forms(form-compilation-record(form), fragment-forms);
  install-top-level-forms(fragment-forms);
end method;

// Top level installation/deinstallation

define method install-top-level-form (form :: <top-level-form>)
  // No installation is required by default.
end method;

define method install-top-level-form (form :: <variable-defining-form>)
  with-dependent ($top-level-processing of form)
    if (form-ignored?(form))
      debug-out(#"gsb", "ignoring top level form %=\n", form)
    else
      install-top-level-form-bindings(form)
    end;
    form.form-top-level-installed? := #t;
  end;
end;

define method uninstall-top-level-form (form :: <variable-defining-form>)
  uninstall-top-level-form-bindings(form);
  form.form-top-level-installed? := #f;
end;

define method install-top-level-form-bindings (form :: <variable-defining-form>)
  // This default methods registers the form as a definition of each of the
  // variables involved. It may that some definitions are definitions for
  // some of their variables and/or modifying definitions for others.
  // They must provide their own implementations.
  for (name in form-variable-names(form))
    add-definition(name, form);
  end;
end method;

define method uninstall-top-level-form-bindings (form :: <variable-defining-form>)
  for (name in form-variable-names(form))
    remove-definition(name, form);
  end;
end method;
