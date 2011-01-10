Module:   dylan-user
Synopsis: Library and module definitions for compiler namespaces.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-namespace
  use functional-dylan;
  use dfmc-common;
  use dfmc-conditions;
  use dfmc-reader;
  use dfmc-macro-expander;
  export dfmc-namespace;
end library;

//// The DFMC namespace library.

define module dfmc-namespace
  use functional-dylan;
  use dfmc-common;
  use dfmc-imports;
  use dfmc-conditions;
  use dfmc-reader,
    import: { <fragment>,
	        fragment-record,          fragment-record-setter,
	        fragment-source-position, fragment-source-position-setter,
	      <variable-name-fragment>,
	      fragment-identifier,
	      fragment-module,
	      make-variable-name-fragment-in-module,
	      dylan-variable-name,
	      definer-token-class?,
	      merge-token-classes,
	      classify-word-in,
	      <fragment-copier>,
	      <variable-name-table>
	    };

  use dfmc-macro-expander; 
  // Debugging
  export
    read-databases?, read-databases?-setter,
    write-databases?, write-databases?-setter,
    use-databases?, use-databases?-setter;

  // Abstract environments.
  export <environment>,
    define-name, 
    undefine-name, 
    lookup-name;

  // Namespace clauses.
  export <use-clause>, used-name,
         <create-clause>,
         <export-clause>;

  // The class of abstract namespaces.
  export <namespace>,
    make-namespace,
    namespace-local-bindings, defined-name?,
    exported-names, exported-name?, created-name?,
    exported-imports-table,
    namespace-definition,
    namespace-library-description,
    namespace-original-library,
    namespace-name,
    namespace-uses?,
    namespace-model, namespace-model-setter,
    namespace-model-variable,
    do-imported-names,
    implicitly-exported-name-collection,
    implicitly-exported-value-collection,
    lookup-implicitly-exported-name,
    all-used-namespaces,
    directly-used-namespaces;

  export
    <dfmc-dood>,
      \with-dood-context, *library-description*,
      dood-dfmc-initial-segments,
    <library-description>,
      library-description-current-call-site-id, 
	library-description-current-call-site-id-setter,
      library-generate-call-site-id,
    <dylan-library-description>,
    <project-library-description>,
    <dylan-project-library-description>,
    detach-interactive-namespaces,
    interactive-namespaces-detached?,
    models-in-interactive-use?,
    interactive-library-shadowed?,
    <interactive-library-description>,
      lookup-interactive-context,
    <interactive-layer>,
      interactive-layer-base,
    outer-lexical-environment,
      active-lexical-variables, // DM callback
    merge-interactive-layer,
    define-library!,
    make-library-description,
    close-library-description,
    language-definition,
    ensure-language-definition,
    install-library-description-sources,
    dood-boot-mapped-objects,
    compilation-context-object-names,
    library-description-emit-name,
    library-description-compiler-back-end-name,
      library-description-compiler-back-end-name-setter,
    library-description-os-name,
      library-description-os-name-setter,
    library-description-processor-name,
      library-description-processor-name-setter,
    library-description-compilation-mode,
      library-description-compilation-mode-setter,
    library-description-build-location,
      library-description-build-location-setter,
    library-description-stripped?,
      library-description-stripped?-setter,
    library-description-build-settings,
    library-description-project,
    library-description-dylan-library,
    library-combined-back-end-data,
    library-description-combined-record,
      library-description-combined-record-setter,
    library-description-dood,
    dylan-library-library-description?,
    library-forms-dynamic?,
    library-dynamically-bound-in?,
    library-conditions-table-setter,
    record-library-build,
    note-compilation-from-definitions-started,
    library-description-database-location,
    library-description-profile-location,
    \with-build-area-output, call-with-build-area-output,
    build-area-output-locator,
    \with-profile-area-output, call-with-profile-area-output,
    profile-area-output-locator,
    project-record-id-source-record,
    project-source-record-id,
    project-source-record-name,
    retract-library-parsing,
    retract-library-warnings,
    retract-library-compilation,
    used-library-context,
    project-library-version,
    project-inter-library-binding,
    project-used-library-version;
  export
    install-dylan-boot-constants;
  export 
    strip-library-model-properties,
    install-owned-model-properties-in,
    lookup-owned-model-properties-in,
    clear-dependent-model-properties,
    clear-library-model-properties,
    new-mapped-model;
  export dylan-library, dylan-module, dylan-implementation-module, 
    dylan-library-description, 
    dylan-value, dylan-definition, dylan-binding, dylan-canonical-binding,
    library-description-dylan-value-cache;
  export \with-top-level-library-description, 
            do-with-top-level-library-description,
         \with-library-context, do-with-library-context,
         \with-dependent-context, do-with-dependent-context,
         \with-interactive-layer, do-with-interactive-layer;
  export
    library-description-model,
    library-description-compilation-records, 
    library-description-record-table,
    library-description-interface-version,
      library-description-interface-version-setter,
    library-description-interface-spec,
      library-description-interface-spec-setter,
    library-description-change-count,
      library-description-change-count-setter,
    library-description-models-change-count,
      library-description-models-change-count-setter,
    library-description-major-version,
      library-description-major-version-setter,
    library-description-minor-version,
      library-description-minor-version-setter,
    library-description-library-pack,
      library-description-library-pack-setter,
    library-description-compilation-aborted?,
      library-description-compilation-aborted?-setter,
    library-description-built?,
      library-description-built?-setter,
    library-description-dfm-copier, 
      library-description-dfm-copier-setter,
    library-description-value-model-copier, 
      library-description-value-model-copier-setter,
    %library-description-object-expression,
      %library-description-object-expression-setter,
    %library-description-false-expression,
      %library-description-false-expression-setter,
    %library-description-default-value-rest-spec,
      %library-description-default-value-rest-spec-setter,
    record-booted-model-properties,
    record-all-booted-model-properties,
    library-description-model,
    retract-library-copiers,
    save-definition-database,
    ensure-database-saved,
    save-namespace-database,
    ensure-namespace-database-saved,
    report-definition-database-statistics,
    report-recursive-definition-database-statistics,
    report-diff-definition-database-statistics,
    dood-statistics-filter-set,
    dood-statistics-aggregate-set,
    verify-library-definition,
    *verify-used-libraries-strictly?*,
    verify-used-libraries,
    library-description-used-descriptions,
    all-library-descriptions,
    all-used-library-descriptions,
    directly-used-library-descriptions,
    library-references-retracted-models?,
    library-deleted-modules,
    library-type-cache, library-type-cache-setter, 
    library-type-estimate-disjoint?-cache, 
      library-type-estimate-disjoint?-cache-setter, 
    library-type-estimate-cons-cache, 
      library-type-estimate-cons-cache-setter, 
    library-type-estimate-dispatch-cache, 
      library-type-estimate-dispatch-cache-setter, 
    library-external-model-cache,
      library-external-model-cache-setter,
    initialize-typist-library-caches,
    current-library-defined?,
    current-library-stripable?,
    library-description-defined?;

  export
    <dfmc-namespace-dood>,
      binding-model-not-computed-proxy,
        binding-model-not-computed-proxy-setter,
    ensure-export-only,	      

    // EXPORTED MERELY TO SUPPORT EXPORT-ONLY DOODS

    shadowable-binding-local-dependents,
      shadowable-binding-local-dependents-setter,

    binding-local-modifying-definitions,
      binding-local-modifying-definitions-setter,
    retract-modifying-models,
    retract-imported-binding,
    $binding-model-not-computed,
    imported-bindings-tables,
      imported-bindings-tables-setter,
    library-definer-references,
    imported-name-cache;

  export
    ^library-description, ^library-description-setter,
    ^all-used-libraries, ^all-used-libraries-setter,
    ^used-libraries, ^used-libraries-setter;

  export
    library-description-system-class-init-code,
      library-description-system-class-init-code-setter,
    library-description-system-gf-init-code,
      library-description-system-gf-init-code-setter;
  export
    retract-compilation-timings,
    record-compilation-timing-property,
    compilation-timing-properties,
    compilation-timing-property?;

  export <library>,
    ^make-<&library>,
    ^all-used-libraries, ^all-used-libraries-setter,
    ^library-description, ^library-description-setter,
    <interactive-library>,
    lookup-module-in,
    lookup-module,
    define-and-install-module,
    undefine-module!,
    module-defined?,
    defined-modules-in,
    undefined-module-bindings-in,
    library-binding-value,
    definer-references,
    do-imported-bindings,
    lookup-imported-binding,
    retract-library-imported-bindings;

  // A little utility function, maybe belongs elsewhere?
  export name-definer-word;

  export <module>,
    ^make-<&module>,
    <interactive-module>,
    make-module-definition,
    <dylan-user-module>,
    dylan-user-module,
    dylan-user-module-variable,
    home-library,
    lookup-binding, untracked-lookup-binding,
    form-variable-binding, form-defined-bindings,
    model-variable-binding,
    lookup-binding-in, untracked-lookup-binding-in,
    untracked-lookup-canonical-binding,
    untracked-lookup-canonical-binding-in,
    local-binding-in,
    local-binding-in-requesting-library,
    *cross-module-access-abort*;

  export
    <definitions>,
    <models>,
    add-definition,
    add-missing-definition,
    add-local-definition,
    macro-definition,
    binding-defined?,
    variable-defined?,
    remove-definition,
    form-defines-variable?,
    form-ignored?, form-ignored-internal?,
    add-modifying-definition,
    ignore-modifying-definition,
    untracked-lookup-local-modifying-definitions,
    untracked-lookup-certain-local-modifying-models,
    lookup-certain-modifying-models,
    remove-modifying-definition,
    binding-defined-methods, binding-defined-methods-setter,
    binding-defined-domains, binding-defined-domains-setter,
    define-model-object,
    define-model-object-and-type,
    lookup-model-object,
    lookup-value,
    lookup-compilation-record-module;

  export
    add-library-wildcard-subclass-definition,
    remove-library-wildcard-subclass-definition,
    library-contains-wildcard-subclasses?;

  export <binding>,
    binding-value-slot, binding-value-slot-setter, // should be obsolete
    rest-variable?,
    keyword-variable?;

  export
    defined?, exported?, created?, constant?;

  export
    enable-library-externally-visible-elements,
    library-externally-visible-models,
    model-externally-visible?, model-externally-visible?-setter;

  export <module-binding>,
    binding-variable-name,
    binding-canonical-binding,
    canonical-binding-properties,
    binding-definition, untracked-binding-definition, 
    binding-local-modifying-definitions,
    binding-modifying-definitions, untracked-binding-modifying-definitions,
    binding-certain-modifying-models, untracked-binding-certain-modifying-models,
    untracked-binding-all-definitions, untracked-binding-all-modifying-definitions,
    binding-identifier,
    binding-home,
    binding-accessible-to-other-libraries?,
    binding-imported-into-library?,
    binding-interactive?,
    valid-binding-home-library-in?, // For debugging only
    valid-binding-home-library?, // For debugging only
    binding-constant-model-object,
    binding-constant-type-model-object,
    binding-model-object,
    binding-model-object-setter, binding-cached-model-object-setter,
    binding-type-model-object,
    binding-type-model-object-setter, binding-cached-type-model-object-setter,
    untracked-binding-model-object,
    untracked-binding-model-object-if-computed,
    binding-compilation-record,
    retract-binding-model-object,
    untracked-retract-binding-model-object,
    binding-local-dependents,
    register-binding-dependent, unregister-binding-dependent,
    compile-stage-only?,
    binding-thread?, binding-locked?, binding-atomic?,
    define-hollow-object,
    binding-model-or-hollow-object,
    untracked-ensure-form-model,
    binding-previously-defined?,
    binding-previous-definition;

  export <interactor-binding>,
    binding-interactor-id;

end module;
