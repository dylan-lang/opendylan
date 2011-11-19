Module: dylan-user
Synopsis: Library and module definitions for compiler browser support.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-browser-support
  use functional-dylan;
  use dood;
  use source-records;
  use dfmc-core;
  use dfmc-reader;
  use dfmc-conversion;
  use dfmc-management;
  use dfmc-namespace;
  use dfmc-back-end;
  use dfmc-back-end-protocol;

  use dfmc-linker;
  use dfmc-c-ffi;

  // Theory is these are for debugging/testing only so we can do without
  // them... Let's test the theory...
  // use dfmc-testing;
  // use dfmc-debug-back-end;

  export
    dfmc-progress-reports,
    dfmc-project-compilation,
    dfmc-interactive-execution,
    dfmc-derived-information,
    dfmc-browser-back-end;


end library;

define module browser-used-modules
  use source-records, export: all;
  use dood, export: all;
  use dfmc-core, export: all;
  use dfmc-reader, export: all;
  use dfmc-conversion, export: all;
  use dfmc-management, export: all;
  use dfmc-linker, export: all;
  use dfmc-back-end, export: all;
  use dfmc-back-end-protocol, export: all;
end module;

define macro interface-module-definer
  { define interface-module ?:name (?used:name) ?specs end }
    =>  { define interface-module-aux ?name (?used)
            (?specs) (?specs)
          end }

  specs:
    { }                         => { }
    { export ?bindings:*; ... } => { ?bindings, ... }
end macro;

define macro interface-module-aux-definer
  { define interface-module-aux ?:name (?used:name)
      (?create-specs) (?use-specs)
    end }
    => { define module ?name
            create ?create-specs;
	    use ?used, import: { ?use-specs }, export: all;
	 end module; }

  create-specs:
    { }                       => { }
    { ?:name, ... }           => { ?name, ... }
    { ?:name = ?orig:name, ... } => { ... }
  use-specs:
    { }                       => { }
    { ?:name, ... }           => { ... }
    { ?:name = ?orig:name, ... } => {  ?orig => ?name, ... }
end macro;

define interface-module dfmc-derived-information (browser-used-modules)
  export
    /*  Namespace interface */
    <variable> = <variable-name>,
    make-variable,
    same-variable-name?,
    variable-name,
    variable-home,
    variable-exported?,
    same-variable?,
    dylan-variable,
    project-library-definition,
    do-library-modules,
    find-module-definition,
    module-exported?,
    do-module-variables;
  export
    /* top-level parsing info */
    <type-expression>,
    <source-locator> = <source-location>,
    <invalid-canonical-source-record-error>,
      source-locator-positions,
      source-locator-lines,
      source-locator-source-record = source-location-source-record,
    <source-form> = <top-level-form>,
      source-form-parent-form = form-parent-form,
      source-form-location = form-source-location,
      source-form-context = form-original-library,
      source-form-defined-variables,
      source-form-variable-type,
      source-form-adjectives = form-adjectives,
      source-form-define-word = form-define-word,
    <source-form-sequence> = <top-level-form-sequence>,
      source-record-top-level-forms,
      source-record-dispatch-decisions,
    <macro-form> = <macro-call-form>,
      macro-form-expanded-forms,
    <init-form> = <top-level-init-form>,
    <module-definition> = <module-definition>,
    module-definition-name,
    module-definition-used-modules,
    <library-definition> = <library-definition>,
    library-definition-name,
    library-definition-used-libraries,
    <macro-definition> = <expander-defining-form>,
      macro-definition-word = macro-definition-word,
    <class-definition> = <class-definition>,
      class-definition-slot-definitions = form-slot-specs,
      class-definition-superclass-types,
      class-definition-init-keywords,
      class-definition-init-keyword-required?,
      class-definition-init-keyword-init-kind,
      class-definition-init-keyword-type,
    <functional-definition> = <function-defining-form>,
      functional-parameters,
      functional-keys,
      functional-parameter-types,
    <generic-definition> = <generic-definition>,
      generic-definition-options,
    <domain-definition> = <domain-definition>,
      domain-definition-domain-types,
    <constant-definition> = <constant-definition>,
    <variable-definition> = <variable-definition>,
    <function-definition> = <function-definition>,
    <primitive-definition> = <primitive-definition>,
    <method-definition> = <method-definition>,
    <constant-method-definition> = <constant-method-definition>,
    <slot-definition> = <slot-definition>,
      slot-definition-allocation = spec-allocation,
      slot-definition-init-kind,
      slot-definition-keyword,
      slot-definition-class-definition,
      slot-definition-getter = form-getter-definition,
      slot-definition-setter = form-setter-definition,
      slot-definition-type;

  export
    <program-note> = <program-condition>,
    program-note-message,
    program-note-location = condition-source-location,
    program-note-creator,
    compilation-context-notes,
    source-record-notes,
    source-form-notes;
    
  export
    /* global derived info */
    variable-all-definitions,
    variable-active-definition,
    variable-active-method-definitions,
    source-form-browsing-context,
    class-direct-subclass-definitions,
    class-all-superclass-definitions,
    class-all-slot-definitions,
    class-direct-method-definitions,
    source-form-referenced-macros,
    source-form-referenced-variables,
    variable-referencing-forms;

  export
    /* debug info */
    <compiled-lambda> = <lambda-compiled-data>,
    source-form-symbolic-name,
    symbolic-name-source-form,
    variable-symbolic-name,
    symbolic-name-variable,
    compiled-lambda-symbolic-name,
    symbolic-name-compiled-lambda,
    symbolic-name-component-name,
    source-position-compiled-lambda,
    source-form-compiled-lambda,
    compiled-lambda-source-form,
    source-form-compiled-lambda-symbolic-name,
    compiled-lambda-symbolic-name-source-form,
    compiled-lambda-source-location,
    compiled-lambda-code-offset,
    compiled-lambda-mapped-source-locations,
    compiled-lambda-mapped-code-offsets,
    <local-variable> = <local-variable>,
    local-variable-debug-name,
    local-variable-debug-name-dylan-name,
    local-variable-type,
    local-variable-location,
    local-variable-argument?,
    compiled-lambda-local-variables,
    compiled-lambda-frame-boundaries,
    compilation-context-initializer-symbolic-name,
    compilation-context-library-name,
    compilation-context-component-name,
    compilation-context-dylan-component-name,
    compilation-context-runtime-component-name,
    component-name-context,
    library-name-context;

  export
    <compiler-database-error> = <dood-proxy-error>;

end interface-module dfmc-derived-information;

define module dfmc-progress-reports
  use functional-dylan;
  use dfmc-management,
    export: {
	     // Progress reporting API
	     progress-line,
	     progress-report,
	     \with-progress-reports,
	     \with-library-progress,
	     internal-reporting-setter,
	     library-progress-text,
	     library-stage-text,
	     internal-progress-text,
	     library-progress-report,
	     library-condition-report,
	     conditions-for,

	     // Needed for macro hygiene in the emulator
	     *current-progress*,
	     *previous-progress*,
	     *progress-library*,
	     *library-increment*,
	     current-progress,
	     do-with-library-progress
	     };
  use dfmc-namespace, // same as compilation-context-project
    export: {library-description-project};
  // for debugging only
  use dfmc-core,
    export: {\debug-out,
	     // emulator non-hygiene!!
	     *debug-out*, \without-dependency-tracking, *current-stage*, *current-dependent*, $no-dependent
             };
end module;

define interface-module dfmc-project-compilation (browser-used-modules)
  export
    open-compilation-context,
    close-compilation-context,
    <compiler-source-record-sequence>,
    compilation-context-sources,
    compilation-context-object-names = compilation-context-object-names,
    compilation-context-version,
    compilation-context-compiler-settings,
    compilation-context-compiler-settings-setter,
    compilation-context-project,
    compilation-context-database-location,
    // unsafe option, for internal use.
    *verify-used-libraries-strictly?* = *verify-used-libraries-strictly?*,
    install-project-sources = install-project-sources,
    parse-project-sources = parse-project-sources,
    note-definitions-updated = note-definitions-updated,
    compile-project-definitions,
    used-compilation-contexts,
    all-known-compilation-contexts,
    compilation-context-built?,
    compilation-definitions-inconsistent? = compilation-definitions-inconsistent?,
    dylan-library-compilation-context?,
    save-compilation-context,
    <abort-compilation> = <abort-compilation>,
    <database-corruption-warning> = <dood-corruption-warning>,
    <database-version-warning> = <dood-version-warning>,
    <database-user-version-warning> = <dood-user-version-warning>,
    condition-database-name,
    <library-pack-not-installed>,
    condition-project,
    condition-library-pack;
  export // Callouts
    used-library-context = used-library-context,
    project-record-id-source-record = project-record-id-source-record,
    project-source-record-id = project-source-record-id,
    project-source-record-name = project-source-record-name,
    project-library-version = project-library-version,
    project-inter-library-binding = project-inter-library-binding,
    project-used-library-version = project-used-library-version;
end interface-module dfmc-project-compilation;

define interface-module dfmc-interactive-execution (browser-used-modules)
  export
    find-execution-context,
    establish-execution-context,
    release-execution-context,
    execution-transaction-notes,
    execute-source = execute-source,
    source-complete? = source-complete?,
    macroexpand-source = macroexpand-source,
    execute-definition-removal = execute-definition-removal;
  export
    execution-context-interactive? = interactive-library-shadowed?;
  export // Callouts
    active-lexical-variables = active-lexical-variables;
  export
    link-and-download = link-and-download,
    download-for-interactive-execution = download-for-interactive-execution;
end interface-module dfmc-interactive-execution;

define module dfmc-browser-back-end
  create
    context-back-end,
    back-end-symbolic-name-compiled-lambda,
    back-end-source-position-compiled-lambda,
    back-end-source-form-compiled-lambda,
    back-end-source-form-compiled-lambda-symbolic-name,
    back-end-compiled-lambda-symbolic-name-source-form,
    back-end-local-variable-debug-name-dylan-name,
    back-end-compilation-context-initializer-symbolic-name
    ;
end module;

define module dfmc-browser-support
  use functional-dylan;
  use dfmc-imports;
  use dfmc-project-compilation;
  use dfmc-derived-information;
  use dfmc-interactive-execution;
  use dfmc-browser-back-end;
  use dfmc-back-end-protocol;
  // Use prefix to avoid conflicts with interface names.
  use browser-used-modules, prefix: "dfmc-";
end module;
